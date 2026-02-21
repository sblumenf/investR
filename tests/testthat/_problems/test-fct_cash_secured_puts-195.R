# Extracted from test-fct_cash_secured_puts.R:195

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)
library(tibble)
library(dplyr)

# test -------------------------------------------------------------------------
result <- calculate_put_return_metrics(
    premium_received = 150,
    cash_required = 9500,
    days_to_expiry = 45
  )
return_on_cash <- 150 / 9500
expect_equal(result$return_on_cash, return_on_cash, tolerance = 0.0001)
expected_annualized <- return_on_cash * (365 / 45)
expect_equal(result$annualized_return, expected_annualized, tolerance = 0.0001)
