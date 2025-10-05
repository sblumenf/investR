#' Dividend Capture Shared Utilities
#'
#' Core functions for dividend capture strategies (weekly, monthly, etc.)
#' Strategy-agnostic business logic for analyzing dividend events.
#'
#' @name dividend-capture-utils
#' @importFrom tibble tibble
#' @importFrom zoo index
#' @importFrom quantmod Op Cl
#' @importFrom purrr map_dfr compact
#' @importFrom dplyr bind_rows
NULL

#' Analyze individual dividend events
#'
#' For each historical dividend, calculates the return from buying at close
#' before ex-dividend and selling at open on ex-dividend day.
#'
#' THIS IS STRATEGY-AGNOSTIC - works for weekly, monthly, or any frequency
#'
#' @param price_history xts object with OHLCV data
#' @param dividends xts object with dividend amounts and dates
#' @return Tibble with per-trade results
#' @export
analyze_dividend_events <- function(price_history, dividends) {
  if (is.null(dividends) || length(dividends) == 0) {
    return(tibble())
  }

  if (is.null(price_history) || nrow(price_history) == 0) {
    return(tibble())
  }

  # Extract dates and amounts
  div_dates <- index(dividends)
  div_amounts <- as.numeric(dividends)

  # Price history dates
  price_dates <- index(price_history)

  # Analyze each dividend event
  results <- map_dfr(seq_along(div_dates), function(i) {
    div_date <- div_dates[i]
    div_amount <- div_amounts[i]

    tryCatch({
      # Find buy date (last trading day before ex-dividend)
      before_dates <- price_dates[price_dates < div_date]
      if (length(before_dates) == 0) {
        return(NULL)
      }
      buy_date <- tail(before_dates, 1)

      # Find sell date (ex-dividend date)
      on_after_dates <- price_dates[price_dates >= div_date]
      if (length(on_after_dates) == 0) {
        return(NULL)
      }
      sell_date <- on_after_dates[1]

      # Extract prices
      buy_price <- as.numeric(Cl(price_history[buy_date]))
      sell_price <- as.numeric(Op(price_history[sell_date]))

      if (is.na(buy_price) || is.na(sell_price) || buy_price <= 0 || sell_price <= 0) {
        return(NULL)
      }

      # Calculate return
      total_received <- sell_price + div_amount
      pct_return <- ((total_received - buy_price) / buy_price) * 100
      overnight_move <- sell_price - buy_price

      # Day of week
      buy_day <- weekdays(buy_date)
      ex_div_day <- weekdays(sell_date)

      # Return results
      tibble(
        div_date = as.Date(div_date),
        buy_date = as.Date(buy_date),
        sell_date = as.Date(sell_date),
        buy_day = buy_day,
        ex_div_day = ex_div_day,
        dividend = div_amount,
        buy_price = buy_price,
        sell_price = sell_price,
        overnight_move = overnight_move,
        pct_return = pct_return,
        profitable = pct_return > 0
      )
    }, error = function(e) {
      return(NULL)
    })
  })

  # Remove NULL results
  results <- compact(results)

  if (length(results) > 0) {
    return(bind_rows(results))
  } else {
    return(tibble())
  }
}

#' Get most common value from vector
#'
#' Generic function to find mode (most frequent value).
#' Works for day names, dates, or any categorical data.
#'
#' @param values Character or numeric vector
#' @return Most common value
#' @export
get_most_common_value <- function(values) {
  if (length(values) == 0) {
    return("Unknown")
  }

  value_counts <- table(values)
  most_common <- names(value_counts)[which.max(value_counts)]

  return(most_common)
}

#' Get day of week order for sorting
#'
#' Converts day names to numeric order (Monday=1, Tuesday=2, etc.)
#' Used for sorting weekly dividend capture strategies.
#'
#' @param day_name Character day name(s)
#' @return Numeric order value(s)
#' @export
get_day_order <- function(day_name) {
  day_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                  "Friday", "Saturday", "Sunday")
  match(day_name, day_levels)
}
