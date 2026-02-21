# Extracted from test-utils_cash_secured_puts_config.R:39

# prequel ----------------------------------------------------------------------
library(testthat)
library(investR)

# test -------------------------------------------------------------------------
expect_equal(CASH_SECURED_PUTS_CONFIG$strike_threshold_pct, 0.95)
expect_equal(CASH_SECURED_PUTS_CONFIG$min_days, 45)
expect_equal(CASH_SECURED_PUTS_CONFIG$max_days, 120)
expect_equal(CASH_SECURED_PUTS_CONFIG$max_workers, 10)
expect_equal(CASH_SECURED_PUTS_CONFIG$min_option_bid, 0.01)
expect_equal(CASH_SECURED_PUTS_CONFIG$min_open_interest, 10)
expect_equal(CASH_SECURED_PUTS_CONFIG$max_stock_price, 250)
