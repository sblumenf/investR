# Extracted from test-fct_cash_secured_puts.R:65

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)
library(tibble)
library(dplyr)

# test -------------------------------------------------------------------------
expect_error(
    calculate_put_cash_flows(strike = 0, bid_price = 2),
    "price"
  )
