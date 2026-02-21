# Extracted from test-fct_cash_secured_puts.R:489

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)
library(tibble)
library(dplyr)

# test -------------------------------------------------------------------------
test_options <- tibble(
    Strike = c(85, 90),  # All below 95% threshold
    Bid = c(0.50, 0.75),
    OI = c(50, 75),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )
result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95  # Needs strikes >= 95
  )
expect_null(result)
