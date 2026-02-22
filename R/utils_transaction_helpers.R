#' Transaction Helper Utilities
#'
#' Helper functions for analyzing and categorizing account activities.
#' Used by pattern matching logic to detect position grouping opportunities.
#'
#' @name transaction-helpers
#' @importFrom stringr str_detect
NULL

#' Get calendar date from trade_date timestamp
#'
#' Extracts date portion (ignoring time) from trade_date field
#'
#' @param activity Single activity row (tibble) with trade_date field
#' @return Date object
#' @noRd
get_trade_date_calendar <- function(activity) {
  as.Date(as.POSIXct(activity$trade_date, origin = "1970-01-01", tz = "UTC"))
}

#' Check if two activities occurred on same calendar day
#'
#' Compares trade_date fields as calendar dates (ignoring time)
#'
#' @param activity1 First activity row
#' @param activity2 Second activity row
#' @return Logical TRUE if same day
#' @noRd
activities_same_calendar_day <- function(activity1, activity2) {
  date1 <- get_trade_date_calendar(activity1)
  date2 <- get_trade_date_calendar(activity2)
  return(date1 == date2)
}

#' Check if activity is a stock purchase (vectorized)
#'
#' @param data Dataframe or single row with action and symbol fields
#' @return Logical vector: TRUE for each row that is a stock buy
#' @noRd
is_stock_purchase <- function(data) {
  # Handle NA values safely (return FALSE for NAs)
  action_is_buy <- !is.na(data$action) & data$action == "Buy"
  not_option <- !is_option_symbol(data$symbol)

  # Vectorized AND operation
  action_is_buy & not_option
}

#' Check if activity is an option sale (vectorized)
#'
#' @param data Dataframe or single row with action and symbol fields
#' @return Logical vector: TRUE for each row that is an option sell
#' @noRd
is_option_sale <- function(data) {
  # Handle NA values safely (return FALSE for NAs)
  action_is_sell <- !is.na(data$action) & data$action == "Sell"
  is_option <- is_option_symbol(data$symbol)

  # Vectorized AND operation
  action_is_sell & is_option
}

#' Check if sell occurred within 1-2 days after buy (overnight hold)
#'
#' @param buy_activity Buy activity row
#' @param sell_activity Sell activity row
#' @return Logical TRUE if overnight hold pattern
#' @noRd
is_overnight_hold <- function(buy_activity, sell_activity) {
  buy_date <- get_trade_date_calendar(buy_activity)
  sell_date <- get_trade_date_calendar(sell_activity)

  days_diff <- as.numeric(sell_date - buy_date)

  # Overnight hold: sold 1 or 2 business days after purchase
  return(days_diff >= 1 && days_diff <= 2)
}

#' Detect if an option sale is part of a roll transaction
#'
#' A roll occurs when you close an existing option (via buy-to-close OR expiration)
#' and sell-to-open a new option (different strike/expiry) on the same underlying
#' stock on the same day. This function checks if a sell-to-open option transaction
#' has a matching closing transaction in the same group.
#'
#' @param activity Single row tibble with columns: symbol, action, trade_date, type
#' @param group_activities Tibble of all activities in the same group
#' @return List with is_roll (logical), old_symbol (character), new_symbol (character)
#' @noRd
detect_option_roll <- function(activity, group_activities) {
  # Default: not a roll
  no_roll <- list(is_roll = FALSE, old_symbol = NULL, new_symbol = NULL)

  # Must be a sell option transaction
  if (nrow(activity) == 0 || !is_option_sale(activity)) {
    return(no_roll)
  }

  new_symbol <- activity$symbol[1]
  trade_date <- as.Date(as.POSIXct(activity$trade_date[1], origin = "1970-01-01", tz = "UTC"))

  # Extract underlying ticker from the new option
  underlying <- parse_option_symbol(new_symbol)
  if (is.na(underlying)) {
    return(no_roll)
  }

  # Look for closing event on same date: buy-to-close OR expiration
  # Closing events: Buy action (buy-to-close) or EXP action (expired worthless)
  close_activity <- group_activities %>%
    filter(
      !is.na(action),
      action %in% c("Buy", "EXP"),  # Handle both buy-to-close and expirations
      !is.na(symbol),
      is_option_symbol(symbol),
      as.Date(as.POSIXct(trade_date, origin = "1970-01-01", tz = "UTC")) == !!trade_date,
      symbol != new_symbol  # Different option symbol (different strike/expiry)
    )

  # Check if any of the closing transactions are for the same underlying
  if (nrow(close_activity) > 0) {
    for (i in seq_len(nrow(close_activity))) {
      old_symbol <- close_activity$symbol[i]
      old_underlying <- parse_option_symbol(old_symbol)

      if (!is.na(old_underlying) && old_underlying == underlying) {
        # Found a roll: closing old option (buy OR exp), selling new option, same underlying, same day
        return(list(
          is_roll = TRUE,
          old_symbol = old_symbol,
          new_symbol = new_symbol
        ))
      }
    }
  }

  return(no_roll)
}
