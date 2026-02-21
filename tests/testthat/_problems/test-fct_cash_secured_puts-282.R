# Extracted from test-fct_cash_secured_puts.R:282

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)
library(tibble)
library(dplyr)

# test -------------------------------------------------------------------------
test_options <- tibble(
    Strike = c(85, 90, 95, 96, 97, 98, 99, 100, 105),
    Bid = c(0.50, 0.75, 1.50, 1.60, 1.70, 1.80, 2.00, 3.00, 5.00),
    OI = c(50, 75, 100, 100, 100, 100, 100, 150, 200),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )
result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )
expect_false(is.null(result))
expect_equal(result$option$Strike, 105)
