# Extracted from test-fct_cash_flow_projection.R:88

# prequel ----------------------------------------------------------------------
library(testthat)
library(dplyr)
library(lubridate)
library(mockery)
mock_db_connection <- function() {
  list(
    db = "mock_db",
    disconnect = function() { }
  )
}
mock_db_get_query <- function(conn, query) {
  # Mock account_activities data for a Legacy Covered Call roll
  if (grepl("FROM account_activities", query)) {
    tibble(
      trade_date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-05")),
      symbol = c("O240119C00075000", "O240216C00075000", "BAC"),
      action = c("Buy", "Sell", "Buy"),
      type = c("Trades", "Trades", "Trades"),
      net_amount = c(-100, 150, -500),
      description = c("BUY TO CLOSE O", "SELL TO OPEN O", "BUY BAC"),
      group_id = c("CC_O_LEGACY", "CC_O_LEGACY", "IGNORED_GROUP"),
      group_name = c("O - Legacy Covered Call", "O - Legacy Covered Call", "Ignored Group"),
      role = c(NA, NA, NA),
      strategy_type = c("Legacy Covered Call", "Legacy Covered Call", "Some Strategy")
    )
  } else if (grepl("FROM position_groups", query)) {
    tibble(
      group_id = c("CC_O_LEGACY", "IGNORED_GROUP", "IGNORED_LONG_STOCK"),
      group_name = c("O - Legacy Covered Call", "Ignored Group", "BAC - Ignored Long Stock"),
      strategy_type = c("Legacy Covered Call", "Some Strategy", "Long Stock"),
      status = c("open", "ignored", "ignored")
    )
  } else if (grepl("FROM position_group_members", query)) {
    tibble(
      group_id = c("CC_O_LEGACY", "IGNORED_LONG_STOCK"),
      symbol = c("O", "BAC"),
      role = c("underlying_stock", "underlying_stock")
    )
  } else if (grepl("FROM position_group_cash_flows", query)) {
    # Mock data for position_group_cash_flows
    tibble(
      event_date = as.Date(c("2024-02-01")),
      event_type = c("stock_purchase"),
      amount = c(-1000),
      confidence = c("high"),
      status = c("actual"),
      group_id = c("IGNORED_LONG_STOCK")
    )
  } else {
    tibble() # Return empty tibble for other queries
  }
}
mock_is_option_symbol <- function(symbol) {
  grepl("^[A-Z]+[0-9]{6}[CP][0-9]+$", symbol)
}
mock_parse_option_symbol <- function(symbol) {
  "O" # Always return "O" for simplicity
}
library(here)
source(here("R/fct_cash_flow_projection.R"))
source(here("R/utils_calculations.R"))

# test -------------------------------------------------------------------------
stub(get_actual_cash_flows_from_activities, "get_portfolio_db_connection", mock_db_connection)
stub(get_actual_cash_flows_from_activities, "dbGetQuery", mock_db_get_query)
stub(get_actual_cash_flows_from_activities, "is_option_symbol", mock_is_option_symbol)
stub(get_actual_cash_flows_from_activities, "parse_option_symbol", mock_parse_option_symbol)
stub(get_actual_cash_flows_from_activities, "dbDisconnect", function(...) NULL)
cash_flows <- get_actual_cash_flows_from_activities()
expect_true(inherits(cash_flows, "tbl_df"))
expect_equal(nrow(cash_flows), 2)
expect_equal(sum(cash_flows$amount), 50)
expect_equal(cash_flows$type, c("option_premium", "option_premium"))
