# Extracted from test-fct_cash_secured_puts.R:827

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)
library(tibble)
library(dplyr)

# test -------------------------------------------------------------------------
skip_on_cran()
skip_if_offline()
result <- analyze_single_stock_put("KO", strike_threshold_pct = 0.85)
expect_true(nrow(result) > 0,
              info = "Should find put opportunities for KO")
expected_cols <- c("symbol", "Strike", "Bid", "intrinsic_value",
                     "time_value", "premium_yield", "annualized_return")
expect_true(all(expected_cols %in% names(result)),
              info = "Expected analysis columns should be present")
