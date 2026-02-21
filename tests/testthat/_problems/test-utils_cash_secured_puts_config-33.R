# Extracted from test-utils_cash_secured_puts_config.R:33

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)

# test -------------------------------------------------------------------------
expect_equal(CASH_SECURED_PUTS_CONFIG$strike_threshold_pct, 0.95)
