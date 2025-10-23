#' Least Squares Monte Carlo (LSM) Engine
#'
#' Implements Longstaff-Schwartz (2001) method for American option early exercise.
#' This is the TRUE LSM implementation (not the simple approximation).
#'
#' Key innovation: Uses regression to estimate continuation value, learning
#' optimal exercise boundary directly from the simulation.
#'
#' @name lsm-engine
#' @importFrom stats lm predict poly
#' @importFrom logger log_info log_debug log_warn
NULL

################################################################################
# MAIN LSM INTERFACE
################################################################################

#' Run LSM algorithm to determine early exercise decisions
#'
#' Analyzes simulated price paths and determines optimal early exercise
#' at dividend dates using least squares regression.
#'
#' @param price_paths Matrix of simulated stock prices (n_steps × n_paths)
#' @param strike Strike price of the call option
#' @param dividend_schedule Data frame with dividend_date, dividend_amount, days_until
#' @param risk_free_rate Annual risk-free rate
#' @param days_to_expiry Days until option expiration
#' @param config Configuration list (uses RISK_CONFIG if NULL)
#' @return List with exercise_matrix, early_exercise_prob, exercise_by_dividend
#' @export
run_lsm_early_exercise <- function(price_paths,
                                   strike,
                                   dividend_schedule,
                                   risk_free_rate,
                                   days_to_expiry,
                                   config = NULL) {

  if (is.null(config)) {
    config <- RISK_CONFIG
  }

  n_steps <- nrow(price_paths) - 1  # Rows include t=0
  n_paths <- ncol(price_paths)

  log_info("LSM Engine: Analyzing {n_paths} paths for early exercise at {nrow(dividend_schedule)} dividend dates")

  # No dividends = no early exercise for call options
  if (nrow(dividend_schedule) == 0) {
    return(list(
      exercise_matrix = matrix(FALSE, nrow = 0, ncol = n_paths),
      early_exercise_prob = 0,
      exercise_by_dividend = numeric(0),
      method = "lsm",
      paths_analyzed = n_paths
    ))
  }

  # Map dividend dates to simulation steps
  div_steps <- map_dividend_dates_to_steps(
    dividend_schedule$days_until,
    days_to_expiry,
    n_steps
  )

  # Initialize exercise decision matrix
  # Rows = dividend dates, Cols = paths
  # TRUE = exercise at this dividend, FALSE = hold
  exercise_matrix <- matrix(FALSE, nrow = nrow(dividend_schedule), ncol = n_paths)

  # Initialize continuation values (value of holding the option)
  # Start from expiration and work backward
  continuation_values <- calculate_expiration_values(
    price_paths[n_steps + 1, ],
    strike
  )

  # BACKWARD INDUCTION: Work from last dividend to first
  for (div_idx in rev(seq_len(nrow(dividend_schedule)))) {

    div_step <- div_steps[div_idx]
    div_amount <- dividend_schedule$dividend_amount[div_idx]
    days_to_div <- dividend_schedule$days_until[div_idx]

    log_debug("LSM: Processing dividend {div_idx}, step {div_step}, amount ${div_amount}")

    # Stock prices at this dividend date
    stock_prices_at_div <- price_paths[div_step, ]

    # Find in-the-money paths (only these might exercise early)
    itm_paths <- which(stock_prices_at_div > strike)

    min_paths <- config$advanced$lsm_min_itm_paths %||% 10

    if (length(itm_paths) < min_paths) {
      log_debug("LSM: Only {length(itm_paths)} ITM paths at dividend {div_idx}, skipping regression")
      next  # Not enough data for regression
    }

    # STEP 1: Calculate immediate exercise value
    # If exercise now: capture dividend + intrinsic value
    immediate_value <- (stock_prices_at_div[itm_paths] - strike) + div_amount

    # STEP 2: Estimate continuation value using regression
    # This is the LSM innovation: learn continuation value from paths
    estimated_continuation <- estimate_continuation_value_lsm(
      stock_prices = stock_prices_at_div[itm_paths],
      future_values = continuation_values[itm_paths],
      time_to_expiry = days_to_div,
      risk_free_rate = risk_free_rate,
      config = config
    )

    # STEP 3: Optimal decision - exercise if immediate > continuation
    should_exercise <- immediate_value > estimated_continuation

    exercise_matrix[div_idx, itm_paths] <- should_exercise

    # STEP 4: Update continuation values for paths that exercised
    # If exercised now, continuation value = exercise value
    # If held, continuation value stays as future expected value
    exercised_paths <- itm_paths[should_exercise]

    if (length(exercised_paths) > 0) {
      # Discount exercise value back to today
      time_to_div_years <- days_to_div / 365
      discount_factor <- exp(-risk_free_rate * time_to_div_years)

      continuation_values[exercised_paths] <-
        immediate_value[should_exercise] * discount_factor
    }
  }

  # Calculate statistics
  early_exercise_count <- rowSums(exercise_matrix)
  early_exercise_prob <- sum(early_exercise_count > 0) / n_paths

  list(
    exercise_matrix = exercise_matrix,
    early_exercise_prob = early_exercise_prob,
    exercise_by_dividend = early_exercise_count,
    method = "lsm",
    paths_analyzed = n_paths,
    itm_dividend_dates = sum(early_exercise_count > 0)
  )
}

################################################################################
# LSM REGRESSION CORE
################################################################################

#' Estimate continuation value using least squares regression
#'
#' This is the heart of the LSM algorithm: use regression on polynomial
#' basis functions to approximate the continuation value function.
#'
#' @param stock_prices Stock prices for ITM paths at current time
#' @param future_values Future cashflows if held (continuation values)
#' @param time_to_expiry Time remaining until expiration (days)
#' @param risk_free_rate Risk-free rate
#' @param config Configuration
#' @return Estimated continuation values
#' @noRd
estimate_continuation_value_lsm <- function(stock_prices,
                                           future_values,
                                           time_to_expiry,
                                           risk_free_rate,
                                           config) {

  # Discount future values to present
  time_years <- time_to_expiry / 365
  discount_factor <- exp(-risk_free_rate * time_years)
  discounted_future <- future_values * discount_factor

  # Get polynomial degree from config
  degree <- config$advanced$lsm_polynomial_degree %||% 3
  use_orthogonal <- config$advanced$lsm_use_orthogonal_poly %||% TRUE

  # Build regression using polynomial basis functions
  # Orthogonal polynomials are more numerically stable
  if (use_orthogonal) {
    # poly() with raw=FALSE gives orthogonal polynomials
    X <- poly(stock_prices, degree = degree, raw = FALSE)
  } else {
    # Raw polynomials: S, S^2, S^3, ...
    X <- poly(stock_prices, degree = degree, raw = TRUE)
  }

  # Least squares regression: continuation_value ~ f(stock_price)
  tryCatch({
    model <- lm(discounted_future ~ X)

    # Predict continuation values
    predicted_continuation <- predict(model)

    # Ensure non-negative (can't have negative option value)
    pmax(predicted_continuation, 0)

  }, error = function(e) {
    # If regression fails, fall back to mean (robust)
    log_warn("LSM regression failed: {e$message}, using mean continuation value")
    rep(mean(discounted_future, na.rm = TRUE), length(stock_prices))
  })
}

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Map dividend dates to simulation time steps
#'
#' @param days_until Vector of days until each dividend
#' @param total_days Total days to expiration
#' @param n_steps Number of simulation steps
#' @return Vector of step indices for each dividend
#' @noRd
map_dividend_dates_to_steps <- function(days_until, total_days, n_steps) {
  # Map continuous time to discrete steps
  steps <- round((days_until / total_days) * n_steps)

  # Ensure valid range [1, n_steps]
  pmax(1, pmin(steps, n_steps))
}

#' Calculate option values at expiration
#'
#' @param final_prices Stock prices at expiration
#' @param strike Strike price
#' @return Call option payoffs
#' @noRd
calculate_expiration_values <- function(final_prices, strike) {
  # Call option payoff at expiration: max(S - K, 0)
  pmax(final_prices - strike, 0)
}

################################################################################
# DIAGNOSTIC AND COMPARISON FUNCTIONS
################################################################################

#' Compare LSM vs simple approximation
#'
#' Useful for validation: shows how much LSM improves over simple method
#'
#' @param price_paths Price path matrix
#' @param strike Strike price
#' @param dividend_schedule Dividend schedule
#' @param risk_free_rate Risk-free rate
#' @param volatility Volatility (for simple approximation)
#' @param days_to_expiry Days to expiry
#' @return List comparing both methods
#' @export
compare_lsm_vs_simple <- function(price_paths, strike, dividend_schedule,
                                  risk_free_rate, volatility, days_to_expiry) {

  # Run LSM
  lsm_result <- run_lsm_early_exercise(
    price_paths, strike, dividend_schedule,
    risk_free_rate, days_to_expiry
  )

  # Run simple approximation (dividend > time value)
  simple_result <- run_simple_early_exercise(
    price_paths, strike, dividend_schedule,
    volatility, risk_free_rate, days_to_expiry
  )

  # Compare
  list(
    lsm_prob = lsm_result$early_exercise_prob,
    simple_prob = simple_result$early_exercise_prob,
    difference = abs(lsm_result$early_exercise_prob - simple_result$early_exercise_prob),
    lsm_by_dividend = lsm_result$exercise_by_dividend,
    simple_by_dividend = simple_result$exercise_by_dividend,
    recommendation = if (abs(lsm_result$early_exercise_prob - simple_result$early_exercise_prob) > 0.05) {
      "Significant difference - LSM provides more accurate estimate"
    } else {
      "Methods agree closely - simple approximation is adequate"
    }
  )
}

#' Run simple early exercise approximation (original method)
#'
#' For comparison purposes - the old "dividend > time value" method
#'
#' @noRd
run_simple_early_exercise <- function(price_paths, strike, dividend_schedule,
                                     volatility, risk_free_rate, days_to_expiry) {

  n_steps <- nrow(price_paths) - 1
  n_paths <- ncol(price_paths)

  if (nrow(dividend_schedule) == 0) {
    return(list(early_exercise_prob = 0, exercise_by_dividend = numeric(0)))
  }

  div_steps <- map_dividend_dates_to_steps(
    dividend_schedule$days_until,
    days_to_expiry,
    n_steps
  )

  exercise_matrix <- matrix(FALSE, nrow = nrow(dividend_schedule), ncol = n_paths)

  for (div_idx in seq_len(nrow(dividend_schedule))) {
    div_step <- div_steps[div_idx]
    div_amount <- dividend_schedule$dividend_amount[div_idx]
    days_remaining <- days_to_expiry - dividend_schedule$days_until[div_idx]

    stock_prices <- price_paths[div_step, ]

    # Simple check: dividend > time value
    for (path in seq_len(n_paths)) {
      S <- stock_prices[path]

      if (S <= strike) next  # Not ITM

      intrinsic <- S - strike
      time_to_exp <- max(days_remaining / 365, 0.001)

      # Black-Scholes approximation for time value
      d1 <- (log(S / strike) + (risk_free_rate + 0.5 * volatility^2) * time_to_exp) /
            (volatility * sqrt(time_to_exp))
      d2 <- d1 - volatility * sqrt(time_to_exp)

      N_d1 <- pnorm(d1)
      N_d2 <- pnorm(d2)

      call_value <- S * N_d1 - strike * exp(-risk_free_rate * time_to_exp) * N_d2
      time_value <- max(0, call_value - intrinsic)

      # Exercise if dividend > time value
      if (div_amount > time_value) {
        exercise_matrix[div_idx, path] <- TRUE
      }
    }
  }

  early_exercise_count <- rowSums(exercise_matrix)
  early_exercise_prob <- sum(early_exercise_count > 0) / n_paths

  list(
    early_exercise_prob = early_exercise_prob,
    exercise_by_dividend = early_exercise_count
  )
}
