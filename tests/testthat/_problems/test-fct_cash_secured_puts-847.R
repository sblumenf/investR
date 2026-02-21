# Extracted from test-fct_cash_secured_puts.R:847

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)
library(tibble)
library(dplyr)

# test -------------------------------------------------------------------------
skip_on_cran()
skip_if_offline()
result_invalid <- get_options_chain_puts("INVALID_SYMBOL_123", 100)
