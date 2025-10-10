#' Pattern Matching Logic
#'
#' Pure functions to detect transaction patterns for position grouping.
#' Identifies three main patterns: same-day strategy positions, dividend
#' capture positions, and delayed covered calls.
#'
#' @name pattern-matching
#' @import dplyr
#' @importFrom purrr map_dfr map_chr
#' @importFrom tibble tibble
NULL

################################################################################
# PATTERN 1: SAME-DAY STRATEGY (Stock + Option Same Day)
################################################################################

#' Detect same-day stock and option purchases
#'
#' Finds pairs of stock purchases and option sales that occurred on the
#' same calendar day (based on trade_date). This indicates one of your
#' implemented strategy positions.
#'
#' @param activities Tibble with unprocessed activities (type = "Trades")
#' @return Tibble with stock_activity_id, option_activity_id, ticker, trade_date
#' @noRd
detect_same_day_strategy <- function(activities) {
  # Define empty result schema (DRY - used for all early returns)
  EMPTY_RESULT <- tibble(
    stock_activity_id = character(),
    option_activity_id = character(),
    ticker = character(),
    trade_date = character()
  )

  # Filter to trades only
  trades <- activities %>% filter(type == "Trades")
  if (nrow(trades) == 0) return(EMPTY_RESULT)

  # Find stock purchases (vectorized)
  stock_buys <- trades %>% filter(is_stock_purchase(.))

  # Find option sales (vectorized)
  option_sells <- trades %>% filter(is_option_sale(.))

  if (nrow(stock_buys) == 0 || nrow(option_sells) == 0) return(EMPTY_RESULT)

  # Prepare stock buys for join
  stock_data <- stock_buys %>%
    mutate(trade_date = as.Date(trade_date)) %>%
    select(
      stock_activity_id = activity_id,
      ticker = symbol,
      trade_date
    )

  # Prepare option sells for join (extract underlying ticker)
  option_data <- option_sells %>%
    mutate(
      ticker = map_chr(symbol, parse_option_symbol),
      trade_date = as.Date(trade_date)
    ) %>%
    filter(!is.na(ticker)) %>%  # Remove options we couldn't parse
    select(
      option_activity_id = activity_id,
      ticker,
      trade_date
    )

  # Join on ticker and trade_date (replaces nested loops)
  # Allow many-to-many: multiple stock purchases can match multiple option sales
  matches <- stock_data %>%
    inner_join(option_data, by = c("ticker", "trade_date"), relationship = "many-to-many") %>%
    mutate(trade_date = as.character(trade_date))

  if (nrow(matches) == 0) return(EMPTY_RESULT)

  matches
}

################################################################################
# PATTERN 2: DIVIDEND CAPTURE (Overnight Hold)
################################################################################

#' Detect dividend capture positions
#'
#' Finds pairs of stock buy and sell that occurred 1-2 days apart
#' (overnight hold pattern). This indicates a dividend capture strategy.
#'
#' @param activities Tibble with unprocessed activities (type = "Trades")
#' @return Tibble with buy_activity_id, sell_activity_id, ticker, buy_date, sell_date
#' @noRd
detect_dividend_capture <- function(activities) {
  # Define empty result schema (DRY)
  EMPTY_RESULT <- tibble(
    buy_activity_id = character(),
    sell_activity_id = character(),
    ticker = character(),
    buy_date = character(),
    sell_date = character()
  )

  # Filter to trades only
  trades <- activities %>% filter(type == "Trades")
  if (nrow(trades) == 0) return(EMPTY_RESULT)

  # Find stock purchases (vectorized)
  stock_buys <- trades %>% filter(is_stock_purchase(.))

  # Find stock sales (vectorized - sell action, not option)
  stock_sells <- trades %>%
    filter(
      action == "Sell",
      !is_option_symbol(symbol)
    )

  if (nrow(stock_buys) == 0 || nrow(stock_sells) == 0) return(EMPTY_RESULT)

  # Prepare buys for join
  buy_data <- stock_buys %>%
    mutate(buy_date = as.Date(trade_date)) %>%
    select(
      buy_activity_id = activity_id,
      ticker = symbol,
      buy_quantity = quantity,
      buy_date
    )

  # Prepare sells for join
  sell_data <- stock_sells %>%
    mutate(sell_date = as.Date(trade_date)) %>%
    select(
      sell_activity_id = activity_id,
      ticker = symbol,
      sell_quantity = quantity,
      sell_date
    )

  # Join on ticker, then filter for overnight holds (1-2 days)
  matches <- buy_data %>%
    inner_join(sell_data, by = "ticker") %>%
    mutate(days_diff = as.numeric(sell_date - buy_date)) %>%
    filter(
      days_diff >= 1,
      days_diff <= 2,
      buy_quantity == abs(sell_quantity)
    ) %>%
    mutate(
      buy_date = as.character(buy_date),
      sell_date = as.character(sell_date)
    ) %>%
    select(buy_activity_id, sell_activity_id, ticker, buy_date, sell_date)

  if (nrow(matches) == 0) return(EMPTY_RESULT)

  matches
}

################################################################################
# PATTERN 3: DELAYED COVERED CALL (Option on Existing Stock)
################################################################################

#' Detect delayed covered calls
#'
#' Finds option sales on stocks that are currently owned in positions.
#' The stock was purchased before (not in transaction history), but option
#' sale is recent.
#'
#' @param activities Tibble with unprocessed activities (type = "Trades")
#' @param current_positions Tibble with latest positions
#' @return Tibble with option_activity_id, ticker, position_data (list column)
#' @noRd
detect_delayed_covered_call <- function(activities, current_positions) {
  # Define empty result schema (DRY)
  EMPTY_RESULT <- tibble(
    option_activity_id = character(),
    ticker = character(),
    position_quantity = numeric(),
    position_avg_price = numeric()
  )

  # Filter to trades only
  trades <- activities %>% filter(type == "Trades")
  if (nrow(trades) == 0 || nrow(current_positions) == 0) return(EMPTY_RESULT)

  # Find option sales (vectorized)
  option_sells <- trades %>% filter(is_option_sale(.))
  if (nrow(option_sells) == 0) return(EMPTY_RESULT)

  # Prepare options for join (extract underlying ticker)
  option_data <- option_sells %>%
    mutate(ticker = map_chr(symbol, parse_option_symbol)) %>%
    filter(!is.na(ticker)) %>%
    select(
      option_activity_id = activity_id,
      ticker
    )

  # Prepare positions for join
  position_data <- current_positions %>%
    filter(open_quantity > 0) %>%
    select(
      ticker = symbol,
      position_quantity = open_quantity,
      position_avg_price = average_entry_price
    )

  # Join options to positions on ticker (replaces loop)
  matches <- option_data %>%
    inner_join(position_data, by = "ticker")

  if (nrow(matches) == 0) return(EMPTY_RESULT)

  matches
}

################################################################################
# PATTERN 4: LATE DIVIDENDS (Dividend After Position Closed)
################################################################################

#' Detect late dividends for closed groups
#'
#' Finds dividend transactions that match recently closed dividend capture
#' groups by ticker and timing.
#'
#' @param activities Tibble with unprocessed activities (type = "Dividends")
#' @param closed_groups Tibble with recently closed groups (dividend capture)
#' @return Tibble with dividend_activity_id, potential_group_id, ticker, dividend_date
#' @noRd
detect_late_dividends <- function(activities, closed_groups) {
  # Define empty result schema (DRY)
  EMPTY_RESULT <- tibble(
    dividend_activity_id = character(),
    potential_group_id = character(),
    ticker = character(),
    dividend_date = character()
  )

  # Filter to dividends only
  dividends <- activities %>% filter(type == "Dividends")
  if (nrow(dividends) == 0 || nrow(closed_groups) == 0) return(EMPTY_RESULT)

  # Filter closed_groups to dividend capture strategy only
  div_capture_groups <- closed_groups %>%
    filter(grepl("Dividend Capture", group_name, ignore.case = TRUE))

  if (nrow(div_capture_groups) == 0) return(EMPTY_RESULT)

  # Prepare dividends for join
  div_data <- dividends %>%
    mutate(dividend_date = as.Date(trade_date)) %>%
    select(
      dividend_activity_id = activity_id,
      ticker = symbol,
      dividend_date
    )

  # Prepare groups for join (extract ticker from group name)
  group_data <- div_capture_groups %>%
    mutate(
      ticker = map_chr(group_name, ~strsplit(.x, " ")[[1]][1]),
      days_since_close = as.numeric(Sys.Date() - as.Date(updated_at))
    ) %>%
    filter(days_since_close <= 30) %>%
    select(
      potential_group_id = group_id,
      ticker
    )

  # Join on ticker (replaces nested loops)
  matches <- div_data %>%
    inner_join(group_data, by = "ticker") %>%
    mutate(dividend_date = as.character(dividend_date)) %>%
    select(dividend_activity_id, potential_group_id, ticker, dividend_date)

  if (nrow(matches) == 0) return(EMPTY_RESULT)

  matches
}

################################################################################
# AMBIGUOUS MATCH DETECTION
################################################################################

#' Check for ambiguous matches
#'
#' Detects when multiple transactions of the same ticker occurred on the
#' same day, making it unclear which transactions should be grouped together.
#'
#' @param same_day_matches Tibble from detect_same_day_strategy()
#' @return Tibble with ticker, trade_date, stock_ids (list), option_ids (list), is_ambiguous
#' @noRd
check_ambiguous_match <- function(same_day_matches) {
  if (nrow(same_day_matches) == 0) {
    return(tibble(
      ticker = character(),
      trade_date = character(),
      stock_activity_ids = list(),
      option_activity_ids = list(),
      is_ambiguous = logical()
    ))
  }

  # Group by ticker and trade_date
  grouped <- same_day_matches %>%
    group_by(ticker, trade_date) %>%
    summarise(
      stock_activity_ids = list(stock_activity_id),
      option_activity_ids = list(option_activity_id),
      count = n(),
      .groups = "drop"
    ) %>%
    mutate(is_ambiguous = count > 1)

  # Return only ambiguous matches
  grouped %>% filter(is_ambiguous)
}
