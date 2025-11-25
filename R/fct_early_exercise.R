#' Early Exercise Probability Analysis
#'
#' Functions for calculating early exercise probabilities and critical prices
#' for American options with discrete dividends using RQuantLib.
#'
#' @name early-exercise
#' @importFrom tibble tibble
NULL

################################################################################
# RQUANTLIB INTEGRATION
################################################################################

#' Calculate early exercise probability using RQuantLib
#'
#' Uses RQuantLib's American option pricer with discrete dividends
#' to determine exercise boundary and probability.
#'
#' @param ticker Stock ticker
#' @param current_price Current stock price
#' @param strike Strike price
#' @param days_to_expiry Days until expiration
#' @param dividend_schedule Tibble with dividend_date, dividend_amount
#' @param volatility Implied volatility (if NULL, estimates from history)
#' @param option_type Type of option: "call" (default) or "put"
#' @return List with early exercise metrics
#' @export
calculate_early_exercise_probability <- function(ticker,
                                                 current_price,
                                                 strike,
                                                 days_to_expiry,
                                                 dividend_schedule,
                                                 volatility = NULL,
                                                 option_type = "call") {

  # Estimate volatility if not provided - use integrated volatility function
  if (is.null(volatility)) {
    use_implied <- RISK_CONFIG$features$use_implied_volatility %||% TRUE

    volatility <- get_volatility(
      ticker = ticker,
      days_to_expiry = days_to_expiry,
      use_implied = use_implied,
      fallback_to_historical = TRUE,
      blend_weight = RISK_CONFIG$advanced$implied_vol_blend_weight %||% 0.70
    )
  }

  # Get risk-free rate
  r <- RISK_CONFIG$risk_free_rate

  # Convert to years
  T <- days_to_expiry / 365.25

  # Prepare discrete dividends for RQuantLib
  if (nrow(dividend_schedule) > 0) {
    # Calculate time until each dividend (in years from today)
    div_times <- as.numeric(difftime(
      dividend_schedule$dividend_date,
      Sys.Date(),
      units = "days"
    )) / 365.25

    # Filter out dividends that have already passed
    valid_divs <- div_times > 0 & div_times < T
    discrete_dividends <- dividend_schedule$dividend_amount[valid_divs]
    discrete_dividend_times <- div_times[valid_divs]
  } else {
    discrete_dividends <- numeric(0)
    discrete_dividend_times <- numeric(0)
  }

  # Price American option with RQuantLib (call or put)
  tryCatch({
    result <- RQuantLib::AmericanOption(
      type = option_type,
      underlying = current_price,
      strike = strike,
      dividendYield = 0,  # Using discrete dividends instead
      riskFreeRate = r,
      maturity = T,
      volatility = volatility,
      engine = "CrankNicolson",  # Required for discrete dividends
      discreteDividends = if (length(discrete_dividends) > 0) discrete_dividends else NULL,
      discreteDividendsTimeUntil = if (length(discrete_dividend_times) > 0) discrete_dividend_times else NULL
    )

    # Calculate per-dividend exercise probabilities
    dividend_exercise_probs <- numeric(nrow(dividend_schedule))

    if (nrow(dividend_schedule) > 0) {
      for (i in seq_len(nrow(dividend_schedule))) {
        div_date <- dividend_schedule$dividend_date[i]
        div_amount <- dividend_schedule$dividend_amount[i]

        # Critical price calculation depends on option type
        delta <- result$delta

        if (option_type == "call") {
          # Call: exercise if S > K + dividend/delta
          critical_price <- strike + (div_amount / max(delta, 0.5))
        } else {
          # Put: exercise if S < K - dividend/abs(delta)
          critical_price <- strike - (div_amount / max(abs(delta), 0.5))
        }

        # Estimate probability of early exercise
        # Using lognormal distribution
        days_to_div <- as.numeric(difftime(div_date, Sys.Date(), units = "days"))
        t_div <- days_to_div / 365.25

        if (t_div > 0) {
          # Expected stock price at dividend date (geometric Brownian motion)
          mu_ln <- log(current_price) + (r - 0.5 * volatility^2) * t_div
          sigma_ln <- volatility * sqrt(t_div)

          if (option_type == "call") {
            # Call: P(S > critical_price)
            prob <- 1 - pnorm(log(critical_price), mean = mu_ln, sd = sigma_ln)
          } else {
            # Put: P(S < critical_price)
            prob <- pnorm(log(critical_price), mean = mu_ln, sd = sigma_ln)
          }
          dividend_exercise_probs[i] <- prob
        }
      }
    }

    # Overall early exercise probability is max across dividend dates
    overall_prob <- if (length(dividend_exercise_probs) > 0) {
      max(dividend_exercise_probs)
    } else {
      0
    }

    list(
      value = result$value,
      delta = result$delta,
      gamma = result$gamma,
      vega = result$vega,
      theta = result$theta,
      rho = result$rho,
      overall_early_exercise_prob = overall_prob,
      dividend_exercise_probs = dividend_exercise_probs,
      dividend_schedule = dividend_schedule,
      volatility_used = volatility,
      success = TRUE,
      error = NULL
    )

  }, error = function(e) {
    # If RQuantLib fails, return NA results with error
    list(
      value = NA,
      delta = NA,
      gamma = NA,
      vega = NA,
      theta = NA,
      rho = NA,
      overall_early_exercise_prob = NA,
      dividend_exercise_probs = rep(NA, nrow(dividend_schedule)),
      dividend_schedule = dividend_schedule,
      volatility_used = volatility,
      success = FALSE,
      error = e$message
    )
  })
}

################################################################################
# RISK-ADJUSTED RETURN CALCULATIONS
################################################################################

#' Calculate risk-adjusted return accounting for early exercise
#'
#' Combines annualized return (if held to expiration) with early exercise
#' probability to get expected return.
#'
#' @param annualized_return Return if held to expiration
#' @param early_exercise_prob Probability of early assignment
#' @param days_to_expiry Days until expiration
#' @param current_price Current stock price
#' @param strike Strike price
#' @param premium_received Premium from selling call
#' @return Risk-adjusted annualized return
#' @export
calculate_risk_adjusted_return <- function(annualized_return,
                                           early_exercise_prob,
                                           days_to_expiry,
                                           current_price,
                                           strike,
                                           premium_received) {

  # If no early exercise risk, return unadjusted
  if (is.na(early_exercise_prob) || early_exercise_prob < 0.01) {
    return(annualized_return)
  }

  # Estimate average time to early exercise (assume midpoint of option life)
  avg_days_to_exercise <- days_to_expiry * 0.5

  # Return if exercised early (shorter time period)
  # Profit = premium + (strike - current_price) if assigned
  shares <- 100
  profit_if_exercised <- premium_received + (strike - current_price) * shares
  return_if_exercised <- profit_if_exercised / (current_price * shares)
  annualized_if_exercised <- (1 + return_if_exercised)^(365.25 / avg_days_to_exercise) - 1

  # Weighted average
  risk_adjusted <- (early_exercise_prob * annualized_if_exercised) +
                   ((1 - early_exercise_prob) * annualized_return)

  risk_adjusted
}
