#' Risk Analysis Orchestrator
#'
#' Main entry point for position risk analysis. Combines Monte Carlo simulation,
#' RQuantLib pricing, and stress testing into comprehensive risk profile.
#'
#' @name risk-analysis
#' @importFrom logger log_info log_warn log_debug log_success
#' @importFrom dplyr %>% mutate
#' @importFrom tibble tibble
NULL

################################################################################
# MAIN ORCHESTRATOR
################################################################################

#' Analyze complete risk profile for a position
#'
#' Main entry point that combines all risk analysis components.
#' Reuses existing dividend projection and market data fetching logic.
#'
#' @param ticker Stock ticker symbol
#' @param strike Option strike price
#' @param expiration Option expiration date (Date object or string)
#' @param premium_received Premium received from selling call (total, not per share)
#' @param current_price Current stock price (if NULL, fetches live)
#' @param simulation_paths Number of Monte Carlo paths (default 10000)
#' @param use_monte_carlo Run full Monte Carlo simulation (TRUE)
#' @param use_rquantlib Calculate Greeks with RQuantLib (TRUE)
#' @param is_aristocrat Is this a dividend aristocrat (affects dividend stress assumptions)
#' @return List with complete risk analysis results
#' @export
analyze_position_risk <- function(ticker,
                                  strike,
                                  expiration,
                                  premium_received,
                                  current_price = NULL,
                                  simulation_paths = RISK_CONFIG$default_simulation_paths,
                                  use_monte_carlo = TRUE,
                                  use_rquantlib = TRUE,
                                  is_aristocrat = FALSE) {

  log_info("Starting risk analysis for {ticker}")

  # Validate inputs
  validate_ticker(ticker)
  validate_price(strike, "strike")

  # Convert expiration to Date if string
  if (is.character(expiration)) {
    expiration <- as.Date(expiration)
  }

  # Fetch current price if not provided
  if (is.null(current_price)) {
    quote <- fetch_current_quote(ticker, fields = "Last Trade (Price Only)")
    current_price <- as.numeric(quote$Last)

    if (is.na(current_price) || current_price <= 0) {
      stop("Could not fetch valid current price for ", ticker)
    }
  }

  # Calculate days to expiration
  days_to_expiry <- as.numeric(difftime(expiration, Sys.Date(), units = "days"))

  if (days_to_expiry <= 0) {
    stop("Option has already expired")
  }

  log_debug("{ticker}: Days to expiry: {days_to_expiry}")

  # Build dividend schedule (reuses existing logic)
  dividend_schedule <- build_dividend_schedule(ticker, days_to_expiry, is_aristocrat)

  log_debug("{ticker}: Found {nrow(dividend_schedule)} projected dividends")

  # Initialize results list
  results <- list(
    # Position details
    ticker = ticker,
    strike = strike,
    expiration = as.character(expiration),
    days_to_expiry = days_to_expiry,
    current_price = current_price,
    premium_received = premium_received,
    is_aristocrat = is_aristocrat,
    simulation_paths = simulation_paths,

    # Dividend schedule
    dividend_schedule = dividend_schedule,

    # To be populated
    monte_carlo = NULL,
    rquantlib = NULL,
    stress_tests = NULL,
    risk_score = NULL
  )

  # Run Monte Carlo simulation
  if (use_monte_carlo) {
    log_info("{ticker}: Running Monte Carlo simulation ({simulation_paths} paths)...")

    tryCatch({
      mc_result <- run_monte_carlo_simulation(
        ticker = ticker,
        current_price = current_price,
        strike = strike,
        expiration_date = expiration,
        premium_received = premium_received,
        n_paths = simulation_paths,
        model = "jump_diffusion",
        is_aristocrat = is_aristocrat
      )

      results$monte_carlo <- mc_result
      log_success("{ticker}: Monte Carlo complete. Early exercise prob: {sprintf('%.1f%%', mc_result$early_exercise_prob * 100)}")

    }, error = function(e) {
      log_warn("{ticker}: Monte Carlo failed - {e$message}")
      results$monte_carlo <- list(error = e$message)
    })
  }

  # Run RQuantLib analysis
  if (use_rquantlib) {
    log_info("{ticker}: Calculating Greeks with RQuantLib...")

    tryCatch({
      rql_result <- calculate_early_exercise_probability(
        ticker = ticker,
        current_price = current_price,
        strike = strike,
        days_to_expiry = days_to_expiry,
        dividend_schedule = dividend_schedule
      )

      results$rquantlib <- rql_result

      if (rql_result$success) {
        log_success("{ticker}: RQuantLib complete. Delta: {sprintf('%.2f', rql_result$delta)}")
      } else {
        log_warn("{ticker}: RQuantLib failed - {rql_result$error}")
      }

    }, error = function(e) {
      log_warn("{ticker}: RQuantLib failed - {e$message}")
      results$rquantlib <- list(success = FALSE, error = e$message)
    })
  }

  # Calculate risk-adjusted return
  if (!is.null(results$monte_carlo) && !is.null(results$monte_carlo$expected_return)) {
    # Convert to annualized
    T_years <- days_to_expiry / 365.25
    mc_annualized <- ((1 + results$monte_carlo$expected_return)^(1/T_years)) - 1

    results$risk_adjusted_return_annualized <- mc_annualized
  } else {
    results$risk_adjusted_return_annualized <- NA
  }

  # Run stress tests
  results$stress_tests <- run_position_stress_tests(
    ticker = ticker,
    current_price = current_price,
    strike = strike,
    premium_received = premium_received,
    is_aristocrat = is_aristocrat
  )

  # Calculate overall risk score (0-100, higher = riskier)
  results$risk_score <- calculate_position_risk_score(results)

  log_success("{ticker}: Risk analysis complete. Risk score: {results$risk_score}")

  results
}

################################################################################
# STRESS TESTING
################################################################################

#' Run stress tests on position
#'
#' Applies pre-built scenarios to estimate position performance under stress.
#'
#' @param ticker Stock ticker
#' @param current_price Current stock price
#' @param strike Strike price
#' @param premium_received Premium received
#' @param is_aristocrat Is dividend aristocrat
#' @return Tibble with stress test results
#' @noRd
run_position_stress_tests <- function(ticker,
                                      current_price,
                                      strike,
                                      premium_received,
                                      is_aristocrat) {

  # Get ticker sector for scenario-specific impacts
  sector <- get_ticker_sector(ticker)

  # Scenario names
  scenario_names <- c("financial_crisis_2008", "covid_crash_2020", "rising_rates",
                      "stagflation", "volatility_spike")

  # Run each scenario
  stress_results <- lapply(scenario_names, function(scenario_name) {
    scenario <- get_stress_scenario(scenario_name)

    # Get stock price change for this sector
    if (!is.null(scenario$sector_returns[[sector]])) {
      price_change_pct <- scenario$sector_returns[[sector]]
    } else {
      price_change_pct <- scenario$default_return
    }

    stressed_price <- current_price * (1 + price_change_pct)

    # Calculate P&L under stress
    # Covered call: stock P&L + premium
    shares <- 100

    if (stressed_price >= strike) {
      # Assigned
      stock_pnl <- (strike - current_price) * shares
      total_pnl <- stock_pnl + premium_received
    } else {
      # Not assigned
      stock_pnl <- (stressed_price - current_price) * shares
      total_pnl <- stock_pnl + premium_received
    }

    return_pct <- total_pnl / (current_price * shares)

    # Early exercise probability estimate (simplified)
    # If price drops significantly, assignment risk decreases
    exercise_prob_change <- if (price_change_pct < -0.15) {
      "Decreased significantly"
    } else if (price_change_pct > 0.10) {
      "Increased significantly"
    } else {
      "Relatively unchanged"
    }

    tibble(
      scenario = scenario$name,
      stock_price_change_pct = price_change_pct,
      stressed_stock_price = stressed_price,
      position_pnl = total_pnl,
      position_return_pct = return_pct,
      early_exercise_impact = exercise_prob_change
    )
  })

  bind_rows(stress_results)
}

################################################################################
# RISK SCORING
################################################################################

#' Calculate overall risk score for position
#'
#' Combines multiple risk factors into single 0-100 score.
#' Higher score = higher risk.
#'
#' @param analysis_results Results from analyze_position_risk()
#' @return Numeric risk score (0-100)
#' @noRd
calculate_position_risk_score <- function(analysis_results) {

  score <- 0

  # Factor 1: Early exercise probability (0-40 points)
  if (!is.null(analysis_results$monte_carlo$early_exercise_prob)) {
    ee_prob <- analysis_results$monte_carlo$early_exercise_prob
    score <- score + (ee_prob * 40)
  } else if (!is.null(analysis_results$rquantlib$overall_early_exercise_prob)) {
    ee_prob <- analysis_results$rquantlib$overall_early_exercise_prob
    if (!is.na(ee_prob)) {
      score <- score + (ee_prob * 40)
    }
  }

  # Factor 2: Moneyness (0-30 points)
  # Deep ITM = higher assignment risk
  moneyness <- analysis_results$current_price / analysis_results$strike

  if (moneyness > 1.15) {
    score <- score + 30  # Deep ITM
  } else if (moneyness > 1.05) {
    score <- score + 20  # ITM
  } else if (moneyness > 0.95) {
    score <- score + 10  # ATM
  } else {
    score <- score + 5   # OTM (low risk)
  }

  # Factor 3: Time to expiration (0-20 points)
  # Longer time = more uncertainty = higher risk
  days <- analysis_results$days_to_expiry

  if (days > 365) {
    score <- score + 20  # LEAPS = high uncertainty
  } else if (days > 180) {
    score <- score + 15
  } else if (days > 90) {
    score <- score + 10
  } else {
    score <- score + 5   # Near expiration = low uncertainty
  }

  # Factor 4: Stress test vulnerability (0-10 points)
  if (!is.null(analysis_results$stress_tests)) {
    # Average loss across stress scenarios
    avg_stress_return <- mean(analysis_results$stress_tests$position_return_pct, na.rm = TRUE)

    if (avg_stress_return < -0.20) {
      score <- score + 10  # Vulnerable to stress
    } else if (avg_stress_return < -0.10) {
      score <- score + 5
    }
  }

  # Ensure score is 0-100
  score <- min(100, max(0, score))

  round(score, 1)
}

################################################################################
# ADAPTIVE THRESHOLD CALCULATION
################################################################################

#' Calculate adaptive alert thresholds for portfolio
#'
#' Analyzes all positions in portfolio to determine "normal" ranges,
#' then sets yellow/red thresholds relative to portfolio norms.
#'
#' @param portfolio_positions List of positions with risk scores
#' @return List with yellow_threshold and red_threshold
#' @export
calculate_adaptive_thresholds <- function(portfolio_positions) {

  if (length(portfolio_positions) == 0) {
    # Default thresholds if no portfolio history
    return(list(
      yellow_threshold = RISK_CONFIG$risk_score_yellow_threshold,
      red_threshold = RISK_CONFIG$risk_score_red_threshold
    ))
  }

  # Extract risk scores
  risk_scores <- sapply(portfolio_positions, function(p) p$risk_score)
  risk_scores <- risk_scores[!is.na(risk_scores)]

  if (length(risk_scores) < 3) {
    # Not enough data, use defaults
    return(list(
      yellow_threshold = RISK_CONFIG$risk_score_yellow_threshold,
      red_threshold = RISK_CONFIG$risk_score_red_threshold
    ))
  }

  # Calculate portfolio statistics
  mean_score <- mean(risk_scores)
  sd_score <- sd(risk_scores)

  # Yellow = mean + 1 SD
  # Red = mean + 2 SD
  list(
    yellow_threshold = min(mean_score + sd_score, 80),  # Cap at 80
    red_threshold = min(mean_score + 2*sd_score, 90)    # Cap at 90
  )
}
