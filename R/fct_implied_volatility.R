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
#' Tries implied vol first, falls back to historical adaptive method.
#'
#' @param ticker Stock ticker symbol
#' @param days_to_expiry Days until position expires
#' @param use_implied Logical: try to fetch implied volatility
#' @param fallback_to_historical Logical: use historical if implied fails
#' @param blend_weight If both available, weight for implied (0-1)
#' @return Annualized volatility estimate
#' @export
get_volatility <- function(ticker,
                          days_to_expiry,
                          use_implied = TRUE,
                          fallback_to_historical = TRUE,
                          blend_weight = 0.70) {

  implied_vol <- NA
  historical_vol <- NA

  # Try implied volatility (only for positions within 1 year)
  if (use_implied && !is.null(days_to_expiry) && !is.na(days_to_expiry) &&
      days_to_expiry > 0 && days_to_expiry <= 365) {

    implied_vol <- fetch_implied_volatility(ticker, days_to_expiry)

    # Sanity check
    if (!is.na(implied_vol) && implied_vol > 0.05 && implied_vol < 3.0) {
      log_info("Volatility ({ticker}): Using implied vol = {round(implied_vol * 100, 1)}%")

      # If we have implied vol and not blending, return it
      if (!fallback_to_historical) {
        return(implied_vol)
      }
    } else {
      implied_vol <- NA
    }
  }

  # Get historical volatility (as backup or for blending)
  if (fallback_to_historical || !is.na(implied_vol)) {
    historical_vol <- calculate_adaptive_volatility(ticker, days_to_expiry)
  }

  # Decision logic
  if (!is.na(implied_vol) && !is.na(historical_vol)) {
    # Blend both
    blended <- blend_weight * implied_vol + (1 - blend_weight) * historical_vol
    log_info("Volatility ({ticker}): Blending implied ({round(implied_vol * 100, 1)}%) and historical ({round(historical_vol * 100, 1)}%) → {round(blended * 100, 1)}%")
    return(blended)

  } else if (!is.na(implied_vol)) {
    # Only implied available
    return(implied_vol)

  } else if (!is.na(historical_vol)) {
    # Only historical available
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
#' @noRd
fetch_implied_vol_questrade <- function(ticker, target_days) {
  # TODO: Implement Questrade options chain fetching
  # This requires Questrade API integration (if available)
  # For now, return NA and rely on Yahoo fallback

  log_debug("Questrade implied vol not yet implemented")
  return(NA)
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
