# Extracted from test-fct_cash_secured_puts.R:738

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)
library(tibble)
library(dplyr)

# test -------------------------------------------------------------------------
option_row <- tibble(
    Strike = 95,
    Bid = 1.50,
    days_to_expiry = 90,
    expiration = as.Date("2025-03-21"),
    OI = 150
  )
stock_data <- list(
    company_name = "Dividend Aristocrat Inc",
    current_price = 100,
    max_drawdown = -0.18,
    current_yield = 0.025,
    annual_dividend = 2.50
  )
result <- calculate_put_metrics(
    ticker = "DARC",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )
expect_equal(result$ticker, "DARC")
expect_equal(result$company_name, "Dividend Aristocrat Inc")
expect_equal(result$current_price, 100)
expect_equal(result$strike, 95)
expect_equal(result$bid_price, 1.50)
expect_equal(result$days_to_expiry, 90)
expect_equal(result$open_interest, 150)
expect_equal(result$cash_required, 9500)
expect_equal(result$premium_received, 150)
expect_equal(result$net_outlay, 9350)
expect_equal(result$return_on_cash, 150 / 9500, tolerance = 0.0001)
expected_ann <- (150 / 9500) * (365 / 90)
expect_equal(result$annualized_return, expected_ann, tolerance = 0.0001)
