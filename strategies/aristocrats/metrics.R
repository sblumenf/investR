################################################################################
# Metrics Calculation Functions
#
# Modular functions for calculating covered call metrics
################################################################################

#' Calculate dividend projections for holding period
#'
#' Projects future dividend payments based on historical payment frequency
#'
#' @param dividends xts object with dividend history
#' @param days_to_expiry Number of days until option expiration
#' @param reinvest_rate Reinvestment rate for dividend income
#' @return List with dividend_income and reinvestment_income
calculate_dividend_projections <- function(dividends,
                                          days_to_expiry,
                                          reinvest_rate = get_reinvestment_rate()) {

  # Default values
  dividend_income <- 0
  reinvestment_income <- 0

  if (is.null(dividends) || nrow(dividends) < 2) {
    return(list(
      dividend_income = 0,
      reinvestment_income = 0
    ))
  }

  # Get latest dividend amount
  latest_dividend <- as.numeric(tail(dividends, 1))

  # Calculate average days between payments from last 6 dividends
  recent_divs <- tail(dividends, min(6, nrow(dividends)))

  if (nrow(recent_divs) >= 2) {
    div_dates <- index(recent_divs)
    days_between <- as.numeric(diff(div_dates))
    avg_days_between <- mean(days_between)

    # Project dividend payments during holding period
    expected_payments <- days_to_expiry / avg_days_between
    dividend_income <- latest_dividend * expected_payments * CONFIG$shares_per_contract

    # Calculate reinvestment income
    # Assume dividends received evenly throughout holding period
    avg_days_to_invest <- days_to_expiry / 2
    years_to_invest <- avg_days_to_invest / CONFIG$days_per_year
    reinvestment_income <- dividend_income * ((1 + reinvest_rate)^years_to_invest - 1)
  }

  list(
    dividend_income = dividend_income,
    reinvestment_income = reinvestment_income
  )
}

#' Calculate option value components
#'
#' @param current_price Current stock price
#' @param strike Option strike price
#' @param bid_price Option bid price
#' @return List with intrinsic_value and extrinsic_value
calculate_option_values <- function(current_price, strike, bid_price) {
  validate_price(current_price, "current_price")
  validate_price(strike, "strike")
  validate_price(bid_price, "bid_price")

  intrinsic_value <- max(0, current_price - strike)
  extrinsic_value <- bid_price - intrinsic_value

  list(
    intrinsic_value = intrinsic_value,
    extrinsic_value = extrinsic_value
  )
}

#' Calculate protection metrics
#'
#' @param current_price Current stock price
#' @param bid_price Option bid price
#' @return List with breakeven_price and downside_protection_pct
calculate_protection_metrics <- function(current_price, bid_price) {
  validate_price(current_price, "current_price")
  validate_price(bid_price, "bid_price")

  breakeven_price <- current_price - bid_price
  downside_protection_pct <- (current_price - breakeven_price) / current_price

  list(
    breakeven_price = breakeven_price,
    downside_protection_pct = downside_protection_pct
  )
}

#' Calculate return metrics
#'
#' @param net_profit Total profit amount
#' @param net_outlay Capital required (after premium received)
#' @param days_to_expiry Days until expiration
#' @return List with total_return and annualized_return
calculate_return_metrics <- function(net_profit, net_outlay, days_to_expiry) {
  if (!is.numeric(net_outlay)) stop("net_outlay must be numeric")
  if (days_to_expiry <= 0) stop("days_to_expiry must be positive")

  total_return <- if (net_outlay > 0) net_profit / net_outlay else 0
  annualized_return <- calculate_annualized_return(net_profit, net_outlay, days_to_expiry)

  list(
    total_return = total_return,
    annualized_return = annualized_return
  )
}

#' Calculate cash flows for covered call position
#'
#' @param current_price Current stock price
#' @param strike Option strike price
#' @param bid_price Option bid price
#' @param dividend_income Projected dividend income
#' @param reinvestment_income Projected reinvestment income
#' @return List with all cash flow components
calculate_cash_flows <- function(current_price, strike, bid_price,
                                dividend_income, reinvestment_income) {

  investment <- current_price * CONFIG$shares_per_contract
  premium_received <- bid_price * CONFIG$shares_per_contract
  net_outlay <- investment - premium_received
  exercise_proceeds <- strike * CONFIG$shares_per_contract

  # Net profit = all income minus what we paid
  net_profit <- premium_received + dividend_income +
                reinvestment_income + exercise_proceeds - investment

  list(
    investment = investment,
    premium_received = premium_received,
    net_outlay = net_outlay,
    exercise_proceeds = exercise_proceeds,
    net_profit = net_profit
  )
}

#' Calculate all covered call metrics (orchestrator)
#'
#' Comprehensive metric calculation that delegates to specialized functions
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @param option_row Selected option (tibble row)
#' @param stock_data Stock data list from get_stock_data()
#' @param warning_flag Warning flag from option selection
#' @return Tibble row with all calculated metrics
calculate_metrics <- function(ticker, current_price, option_row,
                             stock_data, warning_flag) {

  # Validate inputs
  validate_ticker(ticker)
  validate_price(current_price, "current_price")
  validate_columns(option_row, c("Strike", "Bid", "days_to_expiry", "expiration", "OI"),
                  "option_row")

  # Extract option data
  strike <- option_row$Strike
  bid_price <- option_row$Bid
  days_to_expiry <- option_row$days_to_expiry
  expiration <- option_row$expiration
  open_interest <- option_row$OI

  # Calculate components using modular functions
  option_vals <- calculate_option_values(current_price, strike, bid_price)

  dividend_proj <- calculate_dividend_projections(
    stock_data$dividends,
    days_to_expiry
  )

  cash_flows <- calculate_cash_flows(
    current_price, strike, bid_price,
    dividend_proj$dividend_income,
    dividend_proj$reinvestment_income
  )

  return_metrics <- calculate_return_metrics(
    cash_flows$net_profit,
    cash_flows$net_outlay,
    days_to_expiry
  )

  protection <- calculate_protection_metrics(current_price, bid_price)

  # Assemble final result
  tibble(
    ticker = ticker,
    company_name = stock_data$company_name,
    current_price = current_price,
    strike = strike,
    expiration = as.character(expiration),
    days_to_expiry = days_to_expiry,
    bid_price = bid_price,
    open_interest = open_interest,
    # Cash flows
    investment = cash_flows$investment,
    premium_received = cash_flows$premium_received,
    dividend_income = dividend_proj$dividend_income,
    reinvestment_income = dividend_proj$reinvestment_income,
    exercise_proceeds = cash_flows$exercise_proceeds,
    net_profit = cash_flows$net_profit,
    net_outlay = cash_flows$net_outlay,
    # Returns
    total_return = return_metrics$total_return,
    annualized_return = return_metrics$annualized_return,
    # Risk metrics
    max_drawdown = stock_data$max_drawdown,
    current_yield = stock_data$current_yield,
    # Protection
    breakeven_price = protection$breakeven_price,
    downside_protection_pct = protection$downside_protection_pct,
    # Additional values
    intrinsic_value = option_vals$intrinsic_value,
    extrinsic_value = option_vals$extrinsic_value,
    annual_dividend = stock_data$annual_dividend,
    # Flags
    warning_flag = warning_flag,
    is_aristocrat = TRUE
  )
}