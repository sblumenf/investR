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
#' @param cost_basis Actual cost basis per share (if NULL, uses current_price for return calculations)
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
                                  cost_basis = NULL,
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

  # Determine entry price for return calculations
  entry_price <- if (!is.null(cost_basis)) cost_basis else current_price

  # Initialize results list
  results <- list(
    # Position details
    ticker = ticker,
    strike = strike,
    expiration = as.character(expiration),
    days_to_expiry = days_to_expiry,
    current_price = current_price,
    purchase_price = entry_price,  # Show what price was used for return calculations
    premium_received = premium_received,
    is_aristocrat = is_aristocrat,
    simulation_paths = simulation_paths,

    # Dividend schedule
    dividend_schedule = dividend_schedule,

    # To be populated
    monte_carlo = NULL,
    rquantlib = NULL,
    stress_tests = NULL
  )

  # Run Monte Carlo simulation
  if (use_monte_carlo) {
    log_info("{ticker}: Running Monte Carlo simulation ({simulation_paths} paths)...")

    tryCatch({
      mc_result <- run_monte_carlo_simulation(
        ticker = ticker,
        current_price = current_price,
        entry_price = entry_price,
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

  # Run stress tests (reuse entry_price already calculated above)
  results$stress_tests <- run_position_stress_tests(
    ticker = ticker,
    current_price = current_price,
    entry_price = entry_price,
    strike = strike,
    premium_received = premium_received,
    is_aristocrat = is_aristocrat
  )

  log_success("{ticker}: Risk analysis complete")

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
#' @param current_price Current stock price (for stress scenario calculation)
#' @param entry_price Entry/cost basis price (for P&L calculation)
#' @param strike Strike price
#' @param premium_received Premium received
#' @param is_aristocrat Is dividend aristocrat
#' @return Tibble with stress test results
#' @noRd
run_position_stress_tests <- function(ticker,
                                      current_price,
                                      entry_price,
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

    # Calculate P&L under stress (based on actual entry price)
    # Covered call: stock P&L + premium
    shares <- 100

    if (stressed_price >= strike) {
      # Assigned
      stock_pnl <- (strike - entry_price) * shares
      total_pnl <- stock_pnl + premium_received
    } else {
      # Not assigned
      stock_pnl <- (stressed_price - entry_price) * shares
      total_pnl <- stock_pnl + premium_received
    }

    return_pct <- total_pnl / (entry_price * shares)

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

