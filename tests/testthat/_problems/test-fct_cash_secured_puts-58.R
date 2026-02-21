# Extracted from test-fct_cash_secured_puts.R:58

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)
library(tibble)
library(dplyr)

# test -------------------------------------------------------------------------
expect_error(
    calculate_put_cash_flows(strike = -100, bid_price = 2),
    "price"
  )
