# Extracted from test-fct_cash_secured_puts.R:773

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)
library(tibble)
library(dplyr)

# test -------------------------------------------------------------------------
skip_on_cran()
skip_if_offline()
current_price <- 65.00
result <- get_options_chain_puts("KO", current_price)
expect_true(nrow(result) > 0,
              info = "Should find put options for KO")
