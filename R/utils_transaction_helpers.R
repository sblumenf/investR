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
  as.Date(activity$trade_date)
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
