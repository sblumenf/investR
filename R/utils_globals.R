#' Global variables for R CMD check
#'
#' Declare global variables used in dplyr/tidyverse pipelines and data.table operations
#' to avoid R CMD check NOTEs about undefined global variables
#'
#' @noRd
utils::globalVariables(c(
  # Column names used in dplyr operations (Aristocrats)
  "Strike", "ITM", "Bid", "OI", "expiration", "days_to_expiry",
  "ticker", "company_name", "current_price", "strike",
  "premium_received", "dividend_income", "total_return",
  "annualized_return", "downside_protection_pct", "max_drawdown",
  "current_yield", "low_dividend_flag", "days_diff",

  # Column names for Dividend Capture Weekly
  "div_date", "buy_date", "sell_date", "buy_day", "ex_div_day",
  "dividend", "buy_price", "sell_price", "overnight_move",
  "pct_return", "profitable", "success_rate", "avg_return",
  "std_deviation", "best_return", "worst_return", "avg_dividend",
  "avg_overnight_move", "drop_ratio", "simple_annual_return",
  "compound_annual_return", "annual_income_per_10k", "sharpe_ratio",
  "annual_sharpe", "sortino_ratio", "annual_sortino", "profitable_trades", "losing_trades",
  "recent_success_rate", "recent_avg_return", "date_range",
  "total_events", "median_return", "avg_profit_per_10k", "sofr_annual",
  "day_order", "ex_dividend_day",

  # Shared variables
  "SOFR", "DATE"
))