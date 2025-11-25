#' Implied Volatility Module
#'
#' Fetches options market data and calculates implied volatility using
#' Black-Scholes inverse solver. Provides market-based volatility estimates
#' that complement historical volatility.
#'
#' Data sources: Questrade API (primary), Yahoo Finance (fallback)
#'
#' @name implied-volatility
#' @importFrom logger log_info log_warn log_debug
#' @importFrom stats pnorm dnorm optimize
NULL

################################################################################
# MAIN INTERFACE (DRY - Single volatility function)
################################################################################

#' Get volatility with implied vol integration
#'
#' DRY PRINCIPLE: Single function replaces all volatility calculations.
#' Uses research-backed three-tier horizon strategy:
#' - Very short-term (≤90 days): Pure IV (strongest research evidence)
#' - Medium-term (90-365 days): Blend IV + historical (mixed evidence)
#' - Long-term (>365 days): Pure historical (IV unreliable, mean reversion dominates)
#'
#' Research evidence:
#' - IV has strongest predictive power for <90 day horizons
#' - IV competitive but mixed results for 90-365 day horizons
#' - Historical volatility preferred for LEAPs >365 days (sparse IV, mean reversion)
#'
#' @param ticker Stock ticker symbol
#' @param days_to_expiry Days until position expires
#' @param use_implied Logical: try to fetch implied volatility
#' @param fallback_to_historical Logical: use historical if implied fails
#' @param blend_weight If both available, weight for implied (0-1), used for 90-365 day range
#' @return Annualized volatility estimate
#' @export
get_volatility <- function(ticker,
                          days_to_expiry,
                          use_implied = TRUE,
                          fallback_to_historical = TRUE,
                          blend_weight = 0.70) {

  implied_vol <- NA
  historical_vol <- NA

  # Get horizon thresholds
  short_horizon <- RISK_CONFIG$implied_vol_short_horizon %||% 90
  max_horizon <- RISK_CONFIG$implied_vol_max_horizon %||% 365

  # TIER 3: Long-dated positions (>365 days) - use pure historical
  if (!is.null(days_to_expiry) && !is.na(days_to_expiry) && days_to_expiry > max_horizon) {
    log_info("Volatility ({ticker}): Long-dated position ({days_to_expiry} days > {max_horizon}), using pure historical volatility")
    historical_vol <- calculate_adaptive_volatility(ticker, days_to_expiry)
    return(historical_vol)
  }

  # Try to fetch implied volatility for short and medium-term positions
  if (use_implied && !is.null(days_to_expiry) && !is.na(days_to_expiry) &&
      days_to_expiry > 0 && days_to_expiry <= max_horizon) {

    implied_vol <- fetch_implied_volatility(ticker, days_to_expiry)

    # Sanity check
    if (!is.na(implied_vol) && implied_vol > 0.05 && implied_vol < 3.0) {

      # TIER 1: Very short-term (≤90 days) - use pure IV
      if (days_to_expiry <= short_horizon) {
        log_info("Volatility ({ticker}): Short-term position ({days_to_expiry} days ≤ {short_horizon}), using pure IV = {round(implied_vol * 100, 1)}%")
        return(implied_vol)
      }

      # TIER 2: Medium-term (90-365 days) - will blend below
      log_info("Volatility ({ticker}): Medium-term position ({days_to_expiry} days), will blend IV with historical")

    } else {
      implied_vol <- NA
    }
  }

  # Get historical volatility (for blending in medium-term, or as fallback)
  if (fallback_to_historical || !is.na(implied_vol)) {
    historical_vol <- calculate_adaptive_volatility(ticker, days_to_expiry)
  }

  # Decision logic
  if (!is.na(implied_vol) && !is.na(historical_vol)) {
    # TIER 2: Medium-term (90-365 days) - blend both
    blended <- blend_weight * implied_vol + (1 - blend_weight) * historical_vol
    log_info("Volatility ({ticker}): Blending implied ({round(implied_vol * 100, 1)}%) and historical ({round(historical_vol * 100, 1)}%) → {round(blended * 100, 1)}%")
    return(blended)

  } else if (!is.na(implied_vol)) {
    # Only implied available (shouldn't reach here due to tier 1 early return, but keep for safety)
    return(implied_vol)

  } else if (!is.na(historical_vol)) {
    # IV fetch failed, fall back to historical
    log_info("Volatility ({ticker}): IV unavailable, using historical fallback = {round(historical_vol * 100, 1)}%")
    return(historical_vol)

  } else {
    # Both failed - use default
    log_warn("Volatility ({ticker}): All methods failed, using default {RISK_CONFIG$volatility_default * 100}%")
    return(RISK_CONFIG$volatility_default)
  }
}

################################################################################
# IMPLIED VOLATILITY FETCHING
################################################################################

#' Fetch implied volatility from options market
#'
#' Tries Questrade first, falls back to Yahoo Finance
#'
#' @param ticker Stock ticker
#' @param target_days_to_expiry Target days to expiration
#' @return Implied volatility (annualized) or NA if failed
#' @noRd
fetch_implied_volatility <- function(ticker, target_days_to_expiry) {

  # Try Questrade first
  implied_vol <- tryCatch({
    fetch_implied_vol_questrade(ticker, target_days_to_expiry)
  }, error = function(e) {
    log_debug("Questrade implied vol failed for {ticker}: {e$message}")
    NA
  })

  if (!is.na(implied_vol)) {
    return(implied_vol)
  }

  # Fallback to Yahoo
  implied_vol <- tryCatch({
    fetch_implied_vol_yahoo(ticker, target_days_to_expiry)
  }, error = function(e) {
    log_debug("Yahoo implied vol failed for {ticker}: {e$message}")
    NA
  })

  implied_vol
}

#' Fetch implied volatility from Questrade API
#'
#' Fetches options chain from Questrade and extracts IV from ATM options
#' for the expiration closest to target_days.
#'
#' @param ticker Stock ticker symbol
#' @param target_days Target days to expiration
#' @return Implied volatility (annualized) or NA if failed
#' @noRd
fetch_implied_vol_questrade <- function(ticker, target_days) {

  # Fetch full options chain from Questrade
  chain <- tryCatch({
    fetch_questrade_options_chain(ticker, expiration = NULL)
  }, error = function(e) {
    log_debug("Questrade options chain fetch failed for {ticker}: {e$message}")
    return(NULL)
  })

  if (is.null(chain) || length(chain) == 0) {
    log_debug("Questrade implied vol: No options chain data for {ticker}")
    return(NA)
  }

  # Get current stock price for ATM selection
  stock_price <- tryCatch({
    quote <- fetch_current_quote(ticker, fields = "Last Trade (Price Only)")
    as.numeric(quote$Last)
  }, error = function(e) {
    log_warn("Failed to get stock price for {ticker} in IV calculation")
    return(NA)
  })

  if (is.na(stock_price) || stock_price <= 0) {
    log_debug("Questrade implied vol: Invalid stock price for {ticker}")
    return(NA)
  }

  # Find expiration closest to target_days
  # chain is a list with date strings as names (format: "Nov.07.2025" from Questrade)
  exp_dates <- names(chain)

  # Parse dates - Questrade returns format like "Nov.07.2025"
  exp_dates_parsed <- as.Date(exp_dates, format = "%b.%d.%Y")

  if (any(is.na(exp_dates_parsed))) {
    log_debug("Questrade implied vol: Failed to parse expiration dates for {ticker}")
    return(NA)
  }

  days_to_exps <- as.numeric(exp_dates_parsed - Sys.Date())

  if (length(days_to_exps) == 0) {
    log_debug("Questrade implied vol: No expiration dates found for {ticker}")
    return(NA)
  }

  closest_idx <- which.min(abs(days_to_exps - target_days))
  chosen_exp <- exp_dates[closest_idx]
  chosen_days <- days_to_exps[closest_idx]

  log_debug("Questrade implied vol: Using expiration {chosen_exp} ({chosen_days} days) for target {target_days} days")

  # Extract call options for this expiration
  calls <- chain[[chosen_exp]]$calls

  if (is.null(calls) || nrow(calls) == 0) {
    log_debug("Questrade implied vol: No call options for {ticker} expiration {chosen_exp}")
    return(NA)
  }

  # Filter to ATM options (within configured range of current price)
  atm_range <- RISK_CONFIG$advanced$implied_vol_atm_range %||% 0.10
  atm_calls <- calls[abs(calls$Strike - stock_price) / stock_price < atm_range, ]

  if (nrow(atm_calls) == 0) {
    # No options in ATM range, use closest strike
    closest_strike_idx <- which.min(abs(calls$Strike - stock_price))
    atm_calls <- calls[closest_strike_idx, , drop = FALSE]
    log_debug("Questrade implied vol: No ATM options, using closest strike {atm_calls$Strike[1]}")
  }

  # Extract IV values (column is 'IV' from Questrade chain)
  iv_values <- atm_calls$IV

  # Remove NA values
  iv_values <- iv_values[!is.na(iv_values)]

  if (length(iv_values) == 0) {
    log_debug("Questrade implied vol: No valid IV values for {ticker}")
    return(NA)
  }

  log_debug("Questrade implied vol ({ticker}): Raw IV values range from {min(iv_values)} to {max(iv_values)}")

  # Questrade returns IV as percentage (e.g., 23.74 for 23.74%)
  # Convert to decimal form (e.g., 0.2374)
  iv_values <- iv_values / 100

  # Filter outliers (IV should be reasonable volatility in decimal, typically 0.01 to 3.0)
  outlier_threshold <- RISK_CONFIG$advanced$implied_vol_outlier_threshold %||% 3.0
  iv_values <- iv_values[iv_values > 0.01 & iv_values < outlier_threshold]

  if (length(iv_values) == 0) {
    log_debug("Questrade implied vol: All IV values filtered as outliers for {ticker}")
    return(NA)
  }

  # Return median (robust to outliers)
  median_iv <- median(iv_values)

  log_info("Questrade implied vol ({ticker}): {round(median_iv * 100, 1)}% from {length(iv_values)} ATM options")

  return(median_iv)
}

#' Fetch implied volatility from Yahoo Finance
#'
#' Uses quantmod to fetch options chain
#'
#' @noRd
fetch_implied_vol_yahoo <- function(ticker, target_days) {

  # Fetch options chain using quantmod
  options_data <- tryCatch({
    quantmod::getOptionChain(ticker, NULL)  # NULL = all expirations
  }, error = function(e) {
    log_warn("Yahoo options chain failed for {ticker}: {e$message}")
    return(NULL)
  })

  if (is.null(options_data) || length(options_data) == 0) {
    return(NA)
  }

  # Get current stock price
  stock_price <- tryCatch({
    quote <- quantmod::getQuote(ticker)
    as.numeric(quote$Last)
  }, error = function(e) {
    log_warn("Failed to get stock price for {ticker}")
    return(NA)
  })

  if (is.na(stock_price) || stock_price <= 0) {
    return(NA)
  }

  # Find expiration closest to target
  exp_dates <- names(options_data)
  days_to_exps <- as.numeric(as.Date(exp_dates) - Sys.Date())

  closest_idx <- which.min(abs(days_to_exps - target_days))
  chosen_exp <- exp_dates[closest_idx]

  log_debug("Yahoo implied vol: Using expiration {chosen_exp} for target {target_days} days")

  # Get call options for this expiration
  calls <- options_data[[chosen_exp]]$calls

  if (is.null(calls) || nrow(calls) == 0) {
    return(NA)
  }

  # Filter to ATM options (within 10% of current price)
  atm_range <- RISK_CONFIG$advanced$implied_vol_atm_range %||% 0.10
  atm_calls <- calls[abs(calls$Strike - stock_price) / stock_price < atm_range, ]

  if (nrow(atm_calls) == 0) {
    # No ATM options, use closest to ATM
    closest_strike_idx <- which.min(abs(calls$Strike - stock_price))
    atm_calls <- calls[closest_strike_idx, , drop = FALSE]
  }

  # Calculate implied vol for each ATM option
  time_to_exp <- days_to_exps[closest_idx] / 365

  implied_vols <- sapply(seq_len(nrow(atm_calls)), function(i) {
    call <- atm_calls[i, ]

    # Use mid price (bid + ask) / 2 if available, else last
    option_price <- if (!is.null(call$Bid) && !is.null(call$Ask)) {
      (call$Bid + call$Ask) / 2
    } else {
      call$Last
    }

    if (is.na(option_price) || option_price <= 0) {
      return(NA)
    }

    calculate_implied_vol_from_price(
      option_price = option_price,
      stock_price = stock_price,
      strike = call$Strike,
      time_to_expiry = time_to_exp,
      risk_free_rate = RISK_CONFIG$risk_free_rate,
      option_type = "call"
    )
  })

  # Remove outliers and take median (robust)
  implied_vols <- implied_vols[!is.na(implied_vols)]

  if (length(implied_vols) == 0) {
    return(NA)
  }

  # Filter out extreme values
  outlier_threshold <- RISK_CONFIG$advanced$implied_vol_outlier_threshold %||% 3.0
  implied_vols <- implied_vols[implied_vols < outlier_threshold]

  if (length(implied_vols) == 0) {
    return(NA)
  }

  # Return median (robust to outliers)
  median(implied_vols)
}

################################################################################
# BLACK-SCHOLES IMPLIED VOL SOLVER
################################################################################

#' Calculate implied volatility from option price
#'
#' Uses Newton-Raphson iteration to invert Black-Scholes formula
#'
#' @param option_price Market price of option
#' @param stock_price Current stock price
#' @param strike Strike price
#' @param time_to_expiry Time to expiration (years)
#' @param risk_free_rate Risk-free rate
#' @param option_type "call" or "put"
#' @return Implied volatility or NA if solver fails
#' @noRd
calculate_implied_vol_from_price <- function(option_price,
                                             stock_price,
                                             strike,
                                             time_to_expiry,
                                             risk_free_rate,
                                             option_type = "call") {

  # Handle edge cases
  if (time_to_expiry <= 0) return(NA)
  if (option_price <= 0) return(NA)

  # Initial guess using Brenner-Subrahmanyam approximation
  # This provides a good starting point for Newton-Raphson
  vol_guess <- sqrt(2 * pi / time_to_expiry) * (option_price / stock_price)
  vol_guess <- max(0.01, min(vol_guess, 2.0))  # Constrain to reasonable range

  # Newton-Raphson iteration
  for (iter in 1:20) {

    # Calculate Black-Scholes price and vega
    bs_result <- black_scholes(stock_price, strike, time_to_expiry,
                               vol_guess, risk_free_rate, option_type)

    bs_price <- bs_result$price
    vega <- bs_result$vega

    # Check convergence
    price_diff <- bs_price - option_price

    if (abs(price_diff) < 0.01 || vega < 1e-6) {
      # Converged or vega too small
      return(vol_guess)
    }

    # Newton-Raphson update: vol_new = vol - f(vol) / f'(vol)
    vol_guess <- vol_guess - price_diff / vega

    # Keep in reasonable range
    vol_guess <- max(0.01, min(vol_guess, 5.0))
  }

  # Failed to converge after 20 iterations
  log_debug("Implied vol solver did not converge")
  return(NA)
}

#' Black-Scholes option pricing formula
#'
#' @return List with price and vega
#' @noRd
black_scholes <- function(S, K, T, sigma, r, option_type = "call") {

  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)

  if (option_type == "call") {
    price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else {
    price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  }

  # Vega (same for call and put)
  vega <- S * dnorm(d1) * sqrt(T)

  list(price = price, vega = vega)
}

################################################################################
# DIAGNOSTICS
################################################################################

#' Compare implied vs historical volatility
#'
#' Useful diagnostic to see how market vol differs from realized vol
#'
#' @param ticker Stock ticker
#' @param days_to_expiry Days to expiration
#' @return Data frame with comparison
#' @export
compare_implied_vs_historical <- function(ticker, days_to_expiry = 30) {

  implied <- fetch_implied_volatility(ticker, days_to_expiry)
  historical <- calculate_adaptive_volatility(ticker, days_to_expiry)

  data.frame(
    ticker = ticker,
    days_to_expiry = days_to_expiry,
    implied_vol = ifelse(is.na(implied), NA, round(implied * 100, 2)),
    historical_vol = ifelse(is.na(historical), NA, round(historical * 100, 2)),
    difference = ifelse(is.na(implied) || is.na(historical), NA,
                       round((implied - historical) * 100, 2)),
    interpretation = ifelse(is.na(implied) || is.na(historical), "N/A",
                           ifelse(implied > historical * 1.2, "Market pricing higher risk",
                           ifelse(implied < historical * 0.8, "Market pricing lower risk",
                           "Market and historical agree"))),
    stringsAsFactors = FALSE
  )
}
