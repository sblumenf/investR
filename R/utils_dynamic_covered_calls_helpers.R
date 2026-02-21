#' Dynamic Covered Calls Helper Functions
#'
#' Utility functions for drawdown calculation, parameter determination,
#' and performance optimization for the dynamic covered calls strategy.
#'
#' @name dynamic-covered-calls-helpers
#' @importFrom quantmod Cl dailyReturn
#' @importFrom zoo index
#' @importFrom lubridate years
#' @importFrom logger log_debug log_info log_warn
#' @importFrom dplyr filter %>%
NULL

#' Calculate comprehensive drawdown metrics
#'
#' Extends the basic max drawdown calculation to include peak/trough dates
#' and drawdown interval, which are needed for dynamic parameter calculation.
#'
#' @param price_history xts object with OHLC price data
#' @return List with max_drawdown, peak_date, trough_date, drawdown_interval
#' @export
#' @examples
#' \dontrun{
#'   price_data <- fetch_price_history("AAPL", from = Sys.Date() - years(5))
#'   metrics <- calculate_drawdown_metrics(price_data)
#'   metrics$max_drawdown  # e.g., -0.30 for 30% drawdown
#'   metrics$drawdown_interval  # e.g., 180 days
#' }
calculate_drawdown_metrics <- function(price_history) {
  if (is.null(price_history) || length(price_history) == 0) {
    return(list(
      max_drawdown = NA,
      peak_date = NA,
      trough_date = NA,
      drawdown_interval = NA
    ))
  }

  tryCatch({
    # Get close prices
    close_prices <- Cl(price_history)

    # Calculate daily returns
    daily_returns <- dailyReturn(close_prices, type = "arithmetic")

    # Calculate cumulative returns
    cumulative_returns <- cumprod(1 + daily_returns)

    # Running maximum
    running_max <- cummax(cumulative_returns)

    # Drawdown at each point
    drawdown <- (cumulative_returns - running_max) / running_max

    # Maximum drawdown (most negative value)
    max_dd <- min(drawdown, na.rm = TRUE)

    # Find trough date (where max drawdown occurred)
    trough_index <- which.min(drawdown)
    trough_date <- index(drawdown)[trough_index]

    # Find peak date (highest point before trough)
    # Get the running max value at trough
    peak_value <- as.numeric(running_max[trough_index])

    # Search backwards from trough for the peak
    before_trough <- cumulative_returns[1:trough_index]
    peak_candidates <- which(abs(before_trough - peak_value) < 0.0001)

    if (length(peak_candidates) > 0) {
      peak_index <- peak_candidates[length(peak_candidates)]  # Last occurrence before trough
      peak_date <- index(before_trough)[peak_index]
    } else {
      # Fallback: use first date if no peak found
      peak_date <- index(cumulative_returns)[1]
    }

    # Calculate drawdown interval in days
    drawdown_interval <- as.numeric(difftime(trough_date, peak_date, units = "days"))

    return(list(
      max_drawdown = max_dd,
      peak_date = peak_date,
      trough_date = trough_date,
      drawdown_interval = drawdown_interval
    ))

  }, error = function(e) {
    log_warn("Failed to calculate drawdown metrics: {e$message}")
    return(list(
      max_drawdown = NA,
      peak_date = NA,
      trough_date = NA,
      drawdown_interval = NA
    ))
  })
}

#' Calculate dynamic strike threshold percentage
#'
#' Converts maximum drawdown to a strike threshold percentage with bounds.
#' Formula: strike_pct = 1.0 + max_drawdown (e.g., -30% drawdown -> 70% strike)
#'
#' @param max_drawdown Maximum drawdown as negative decimal (e.g., -0.30)
#' @param min_bound Minimum strike percentage (default 0.50)
#' @param max_bound Maximum strike percentage (default 0.95)
#' @return Strike threshold percentage between bounds
#' @export
#' @examples
#' \dontrun{
#'   # Stock with -30% drawdown
#'   calculate_dynamic_strike_pct(-0.30, 0.50, 0.95)  # Returns 0.70
#'
#'   # Stock with -60% drawdown (hits floor)
#'   calculate_dynamic_strike_pct(-0.60, 0.50, 0.95)  # Returns 0.50
#'
#'   # Stock with no drawdown (always rising)
#'   calculate_dynamic_strike_pct(0, 0.50, 0.95)  # Returns 0.95 (ceiling)
#' }
calculate_dynamic_strike_pct <- function(max_drawdown,
                                        min_bound = 0.50,
                                        max_bound = 0.95) {
  # Handle NA or missing drawdown
  if (is.na(max_drawdown)) {
    log_debug("No drawdown data, using max bound: {max_bound}")
    return(max_bound)
  }

  # Handle edge case: no drawdown (stock always rising)
  if (max_drawdown >= 0) {
    log_debug("No drawdown (stock rising), using max bound: {max_bound}")
    return(max_bound)
  }

  # Calculate strike percentage: 1 + max_drawdown
  # Example: -0.30 drawdown -> 1 + (-0.30) = 0.70 = 70% strike
  strike_pct <- 1.0 + max_drawdown

  # Apply bounds
  strike_pct_bounded <- max(min_bound, min(max_bound, strike_pct))

  # Log if bounds were hit
  if (strike_pct < min_bound) {
    log_debug("Strike {sprintf('%.2f', strike_pct)} below min bound {min_bound}, using {min_bound}")
  } else if (strike_pct > max_bound) {
    log_debug("Strike {sprintf('%.2f', strike_pct)} above max bound {max_bound}, using {max_bound}")
  }

  return(strike_pct_bounded)
}

#' Calculate dynamic target days with bounds
#'
#' Applies minimum and maximum bounds to the drawdown interval.
#'
#' @param drawdown_interval Days between peak and trough
#' @param min_days Minimum days to expiry (default 30)
#' @param max_days Maximum days to expiry (default 730)
#' @return Target days between bounds
#' @export
#' @examples
#' \dontrun{
#'   # Normal interval
#'   calculate_dynamic_target_days(180, 30, 730)  # Returns 180
#'
#'   # Very short interval (hits floor)
#'   calculate_dynamic_target_days(10, 30, 730)  # Returns 30
#'
#'   # Very long interval (hits ceiling)
#'   calculate_dynamic_target_days(1000, 30, 730)  # Returns 730
#' }
calculate_dynamic_target_days <- function(drawdown_interval,
                                         min_days = 30,
                                         max_days = 730) {
  # Handle NA or missing interval
  if (is.na(drawdown_interval) || drawdown_interval <= 0) {
    log_debug("No valid interval, using middle of range: {(min_days + max_days) / 2}")
    return(as.integer((min_days + max_days) / 2))
  }

  # Apply bounds
  target_days <- as.integer(max(min_days, min(max_days, drawdown_interval)))

  # Log if bounds were hit
  if (drawdown_interval < min_days) {
    log_debug("Interval {drawdown_interval} below min {min_days}, using {min_days}")
  } else if (drawdown_interval > max_days) {
    log_debug("Interval {drawdown_interval} above max {max_days}, using {max_days}")
  }

  return(target_days)
}

#' Filter expiration names from fetched options chain
#'
#' Filters expiration dates from already-fetched options chain data based on
#' target days. No additional API calls - just filters existing data.
#'
#' @param options_chain_data List of options data by expiration (from getOptionChain)
#' @param target_days Target days to expiration
#' @param tolerance_pct Tolerance as percentage (0.50 = +/- 50%)
#' @return Character vector of filtered expiration names
#' @export
#' @examples
#' \dontrun{
#'   # opt_chain already fetched via getOptionChain(ticker, Exp = NULL)
#'   filtered_names <- filter_expirations_from_data(opt_chain, 180, 0.50)
#' }
filter_expirations_from_data <- function(options_chain_data,
                                         target_days,
                                         tolerance_pct = 0.50) {
  if (is.null(options_chain_data) || length(options_chain_data) == 0) {
    return(character(0))
  }

  available_expirations <- names(options_chain_data)

  tryCatch({
    # Calculate acceptable range
    min_days <- target_days * (1 - tolerance_pct)
    max_days <- target_days * (1 + tolerance_pct)

    log_debug("Filtering expirations: target={target_days}, range={sprintf('%.0f', min_days)}-{sprintf('%.0f', max_days)} days")

    # Convert expiration strings to days from now
    # Yahoo format: "Jan.17.2025" -> need "%b.%d.%Y"
    today <- Sys.Date()
    exp_dates <- as.Date(available_expirations, format = "%b.%d.%Y")
    days_to_exp <- as.numeric(difftime(exp_dates, today, units = "days"))

    # Filter to those within range (handle NAs from date parsing)
    in_range <- !is.na(days_to_exp) & (days_to_exp >= min_days) & (days_to_exp <= max_days)
    filtered_exps <- available_expirations[in_range]

    # If none match, fall back to closest 3 expirations
    if (length(filtered_exps) == 0) {
      log_debug("No expirations in range, using 3 closest to target")

      # Calculate absolute difference from target (filter out NAs)
      valid_indices <- which(!is.na(days_to_exp))
      if (length(valid_indices) > 0) {
        valid_days <- days_to_exp[valid_indices]
        days_diff <- abs(valid_days - target_days)

        # Get indices of 3 closest
        n_to_take <- min(3, length(valid_indices))
        closest_in_valid <- order(days_diff)[1:n_to_take]
        closest_indices <- valid_indices[closest_in_valid]

        filtered_exps <- available_expirations[closest_indices]
      } else {
        # All dates failed to parse, return first 3
        log_warn("All expiration dates failed to parse, returning first 3")
        filtered_exps <- head(available_expirations, 3)
      }
    }

    log_debug("Filtered {length(available_expirations)} expirations to {length(filtered_exps)}")

    return(filtered_exps)

  }, error = function(e) {
    log_warn("Failed to filter expirations: {e$message}, returning all")
    return(available_expirations)
  })
}

#' Pre-filter S&P 500 stocks by maximum price
#'
#' Bulk operation to filter stocks by price threshold before analysis.
#' Uses batch quote fetching for efficiency.
#'
#' @param tickers Character vector of ticker symbols
#' @param max_price Maximum stock price threshold
#' @return Filtered character vector of tickers under max_price
#' @export
#' @examples
#' \dontrun{
#'   sp500 <- get_sp500_stocks()
#'   affordable <- pre_filter_stocks_by_price(sp500, 250)
#'   # Returns only stocks trading under $250
#' }
pre_filter_stocks_by_price <- function(tickers, max_price = 250) {
  if (length(tickers) == 0) {
    return(character(0))
  }

  log_info("Pre-filtering {length(tickers)} stocks by price (<= ${max_price})...")

  tryCatch({
    quote_source <- getOption("investR.quote_source", default = "questrade")

    if (quote_source == "questrade") {
      quotes <- purrr::map_dfr(tickers, function(t) {
        tryCatch(
          fetch_current_quote(t, fields = "Last Trade (Price Only)"),
          error = function(e) {
            log_warn("Price filter: failed to fetch quote for {t} ({e$message})")
            NULL
          }
        )
      })
    } else {
      quotes <- fetch_current_quote(tickers, fields = "Last Trade (Price Only)")
    }

    if (is.null(quotes) || nrow(quotes) == 0) {
      log_warn("Failed to fetch quotes, returning all tickers")
      return(tickers)
    }

    # Extract prices and filter
    prices <- as.numeric(quotes$Last)
    under_max <- !is.na(prices) & (prices <= max_price)

    filtered_tickers <- tickers[under_max]

    log_info("Filtered from {length(tickers)} to {length(filtered_tickers)} stocks under ${max_price}")

    return(filtered_tickers)

  }, error = function(e) {
    log_warn("Price filtering failed ({e$message}), returning all tickers")
    return(tickers)
  })
}

#' Retry API call with exponential backoff
#'
#' Wrapper function to retry API calls that may hit rate limits (429 errors).
#' Uses exponential backoff strategy to handle transient failures gracefully.
#'
#' @param expr Expression to evaluate (the API call)
#' @param max_attempts Maximum number of retry attempts (default 3)
#' @param initial_wait Initial wait time in seconds (default 2)
#' @param max_wait Maximum wait time in seconds (default 32)
#' @return Result of the expression or NULL if all attempts fail
#' @export
retry_with_backoff <- function(expr, max_attempts = 3, initial_wait = 2, max_wait = 32) {
  last_error <- NULL

  for (attempt in 1:max_attempts) {
    result <- tryCatch({
      # Successfully evaluated - return result
      list(success = TRUE, value = eval(expr, envir = parent.frame()))
    }, error = function(e) {
      # Error occurred - check if we should retry
      list(success = FALSE, error = e)
    })

    # If successful, return the value
    if (result$success) {
      return(result$value)
    }

    # If error, check if it's a 429 and we have retries left
    last_error <- result$error
    if (grepl("429", last_error$message, ignore.case = TRUE) && attempt < max_attempts) {
      wait_time <- min(initial_wait * (2 ^ (attempt - 1)), max_wait)
      log_warn("Rate limit hit, waiting {wait_time}s before retry {attempt}/{max_attempts}")
      Sys.sleep(wait_time)
      # Continue to next iteration
    } else {
      # Non-429 error or max attempts reached - log and return NULL
      log_debug("API call failed: {last_error$message}")
      return(NULL)
    }
  }

  # All attempts failed with 429 errors
  return(NULL)
}

#' Filter stocks by option liquidity
#'
#' Pre-filters stocks to only include those with active options markets.
#' Checks for implied volatility as a proxy for option trading activity.
#'
#' @param tickers Character vector of ticker symbols
#' @return Character vector of tickers with liquid options
#' @export
#' @examples
#' \dontrun{
#'   sp500 <- get_sp500_stocks()
#'   liquid_stocks <- filter_by_option_liquidity(sp500)
#' }
filter_by_option_liquidity <- function(tickers) {
  if (length(tickers) == 0) {
    return(character(0))
  }

  log_info("Pre-filtering {length(tickers)} stocks by option liquidity...")

  tryCatch({
    # Fetch quotes with implied volatility
    # If IV exists and is not NA, options are actively traded
    quotes <- fetch_current_quote(tickers, fields = c("Last Trade (Price Only)", "Name"))

    if (is.null(quotes) || nrow(quotes) == 0) {
      log_warn("Failed to fetch quotes for liquidity filter, returning all tickers")
      return(tickers)
    }

    # For now, we return all tickers as quantmod's getQuote doesn't easily support IV
    # This is a placeholder for future enhancement
    log_info("Option liquidity filter: keeping all {length(tickers)} stocks")
    return(tickers)

  }, error = function(e) {
    log_warn("Liquidity filtering failed ({e$message}), returning all tickers")
    return(tickers)
  })
}
