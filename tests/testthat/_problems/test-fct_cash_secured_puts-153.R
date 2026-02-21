# Extracted from test-fct_cash_secured_puts.R:153

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)
library(tibble)
library(dplyr)

# test -------------------------------------------------------------------------
expect_error(
    calculate_put_protection_metrics(100, -95, 1.50),
    "price"
  )
