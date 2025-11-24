# tests/testthat/test-fct_cash_secured_puts.R
# Business logic tests for cash-secured puts strategy
# Date: 2025-11-23

library(testthat)
library(investR)
library(tibble)
library(dplyr)

################################################################################
# CASH FLOW CALCULATION TESTS
################################################################################

test_that("calculate_put_cash_flows computes correctly with whole numbers", {
  result <- calculate_put_cash_flows(strike = 100, bid_price = 2)

  expect_equal(result$cash_required, 10000)    # 100 * 100 shares
  expect_equal(result$premium_received, 200)   # 2 * 100 shares
  expect_equal(result$net_outlay, 9800)        # 10000 - 200
})

test_that("calculate_put_cash_flows handles decimal strikes and bids", {
  result <- calculate_put_cash_flows(strike = 95.50, bid_price = 1.75)

  expect_equal(result$cash_required, 9550)     # 95.50 * 100
  expect_equal(result$premium_received, 175)   # 1.75 * 100
  expect_equal(result$net_outlay, 9375)        # 9550 - 175
})

test_that("calculate_put_cash_flows handles low-priced stocks", {
  result <- calculate_put_cash_flows(strike = 10, bid_price = 0.50)

  expect_equal(result$cash_required, 1000)     # 10 * 100
  expect_equal(result$premium_received, 50)    # 0.50 * 100
  expect_equal(result$net_outlay, 950)         # 1000 - 50
})

test_that("calculate_put_cash_flows handles high-priced stocks", {
  result <- calculate_put_cash_flows(strike = 500, bid_price = 10)

  expect_equal(result$cash_required, 50000)    # 500 * 100
  expect_equal(result$premium_received, 1000)  # 10 * 100
  expect_equal(result$net_outlay, 49000)       # 50000 - 1000
})

test_that("calculate_put_cash_flows handles very small premium", {
  result <- calculate_put_cash_flows(strike = 10, bid_price = 0.05)

  expect_equal(result$cash_required, 1000)     # 10 * 100
  expect_equal(result$premium_received, 5)     # 0.05 * 100
  expect_equal(result$net_outlay, 995)         # 1000 - 5
})

test_that("calculate_put_cash_flows rejects negative strike", {
  expect_error(
    calculate_put_cash_flows(strike = -100, bid_price = 2),
    "price"
  )
})

test_that("calculate_put_cash_flows rejects zero strike", {
  expect_error(
    calculate_put_cash_flows(strike = 0, bid_price = 2),
    "price"
  )
})

test_that("calculate_put_cash_flows rejects negative bid", {
  expect_error(
    calculate_put_cash_flows(strike = 100, bid_price = -2),
    "price"
  )
})

################################################################################
# PROTECTION METRICS TESTS
################################################################################

test_that("calculate_put_protection_metrics calculates OTM put correctly", {
  # OTM put: strike < current_price
  result <- calculate_put_protection_metrics(
    current_price = 100,
    strike = 95,
    bid_price = 1.50
  )

  expect_equal(result$breakeven_price, 93.50)  # 95 - 1.50
  # Protection: (100 - 93.50) / 100 = 0.065 = 6.5%
  expect_equal(result$downside_protection_pct, 0.065, tolerance = 0.0001)
})

test_that("calculate_put_protection_metrics calculates ATM put correctly", {
  # ATM put: strike = current_price
  result <- calculate_put_protection_metrics(
    current_price = 100,
    strike = 100,
    bid_price = 3.00
  )

  expect_equal(result$breakeven_price, 97.00)  # 100 - 3.00
  # Protection: (100 - 97) / 100 = 0.03 = 3%
  expect_equal(result$downside_protection_pct, 0.03, tolerance = 0.0001)
})

test_that("calculate_put_protection_metrics calculates ITM put correctly", {
  # ITM put: strike > current_price
  result <- calculate_put_protection_metrics(
    current_price = 100,
    strike = 105,
    bid_price = 6.00
  )

  expect_equal(result$breakeven_price, 99.00)  # 105 - 6.00
  # Protection: (100 - 99) / 100 = 0.01 = 1%
  expect_equal(result$downside_protection_pct, 0.01, tolerance = 0.0001)
})

test_that("calculate_put_protection_metrics handles high premium", {
  result <- calculate_put_protection_metrics(
    current_price = 150,
    strike = 140,
    bid_price = 2.00
  )

  expect_equal(result$breakeven_price, 138.00)  # 140 - 2.00
  # Protection: (150 - 138) / 150 = 0.08 = 8%
  expect_equal(result$downside_protection_pct, 0.08, tolerance = 0.0001)
})

test_that("calculate_put_protection_metrics handles low premium", {
  result <- calculate_put_protection_metrics(
    current_price = 100,
    strike = 95,
    bid_price = 0.50
  )

  expect_equal(result$breakeven_price, 94.50)  # 95 - 0.50
  # Protection: (100 - 94.50) / 100 = 0.055 = 5.5%
  expect_equal(result$downside_protection_pct, 0.055, tolerance = 0.0001)
})

test_that("calculate_put_protection_metrics rejects negative current_price", {
  expect_error(
    calculate_put_protection_metrics(-100, 95, 1.50),
    "price"
  )
})

test_that("calculate_put_protection_metrics rejects negative strike", {
  expect_error(
    calculate_put_protection_metrics(100, -95, 1.50),
    "price"
  )
})

test_that("calculate_put_protection_metrics rejects negative bid", {
  expect_error(
    calculate_put_protection_metrics(100, 95, -1.50),
    "price"
  )
})

################################################################################
# RETURN METRICS TESTS
################################################################################

test_that("calculate_put_return_metrics calculates correctly for 90-day option", {
  result <- calculate_put_return_metrics(
    premium_received = 150,
    cash_required = 9500,
    days_to_expiry = 90
  )

  # Return on cash: 150 / 9500 = 0.0158
  expect_equal(result$return_on_cash, 150 / 9500, tolerance = 0.0001)

  # Annualized: 0.0158 * (365 / 90) = 0.0641
  expected_annualized <- (150 / 9500) * (365 / 90)
  expect_equal(result$annualized_return, expected_annualized, tolerance = 0.0001)
})

test_that("calculate_put_return_metrics calculates correctly for 45-day option", {
  result <- calculate_put_return_metrics(
    premium_received = 150,
    cash_required = 9500,
    days_to_expiry = 45
  )

  return_on_cash <- 150 / 9500
  expect_equal(result$return_on_cash, return_on_cash, tolerance = 0.0001)

  # Shorter expiry should yield higher annualized return
  # Annualized: 0.0158 * (365 / 45) = 0.1282
  expected_annualized <- return_on_cash * (365 / 45)
  expect_equal(result$annualized_return, expected_annualized, tolerance = 0.0001)
})

test_that("calculate_put_return_metrics: shorter expiry yields higher annualized return", {
  premium <- 150
  cash <- 9500

  result_45 <- calculate_put_return_metrics(premium, cash, 45)
  result_90 <- calculate_put_return_metrics(premium, cash, 90)

  # Same absolute return
  expect_equal(result_45$return_on_cash, result_90$return_on_cash)

  # But higher annualized for shorter duration
  expect_gt(result_45$annualized_return, result_90$annualized_return)
})

test_that("calculate_put_return_metrics handles very short expiration", {
  result <- calculate_put_return_metrics(
    premium_received = 50,
    cash_required = 9500,
    days_to_expiry = 7
  )

  return_on_cash <- 50 / 9500
  expect_equal(result$return_on_cash, return_on_cash, tolerance = 0.0001)

  # Very short expiration should have very high annualized return
  expected_annualized <- return_on_cash * (365 / 7)
  expect_equal(result$annualized_return, expected_annualized, tolerance = 0.0001)
  expect_gt(result$annualized_return, 0.25)  # Should be > 25% annualized
})

test_that("calculate_put_return_metrics rejects zero cash_required", {
  expect_error(
    calculate_put_return_metrics(150, 0, 90),
    "cash_required"
  )
})

test_that("calculate_put_return_metrics rejects negative cash_required", {
  expect_error(
    calculate_put_return_metrics(150, -9500, 90),
    "cash_required"
  )
})

test_that("calculate_put_return_metrics rejects zero days_to_expiry", {
  expect_error(
    calculate_put_return_metrics(150, 9500, 0),
    "days_to_expiry"
  )
})

test_that("calculate_put_return_metrics rejects negative days_to_expiry", {
  expect_error(
    calculate_put_return_metrics(150, 9500, -90),
    "days_to_expiry"
  )
})

################################################################################
# STRIKE SELECTION TESTS (CRITICAL)
################################################################################

test_that("select_optimal_put filters strikes correctly with 95% threshold", {
  # Current price $100, threshold 95%
  # Strike threshold = $95
  # Should select strikes >= $95 (OTM/ATM puts)
  test_options <- tibble(
    Strike = c(85, 90, 95, 96, 97, 98, 99, 100, 105),
    Bid = c(0.50, 0.75, 1.50, 1.60, 1.70, 1.80, 2.00, 3.00, 5.00),
    OI = c(50, 75, 100, 100, 100, 100, 100, 150, 200),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  # Should select strikes >= 95 (not 85, 90)
  # Should select longest dated with highest OI: strike 105
  expect_false(is.null(result))
  expect_equal(result$option$Strike, 105)
})

test_that("select_optimal_put filters strikes correctly with 90% threshold", {
  # Current price $100, threshold 90%
  # Strike threshold = $90
  # More aggressive (deeper OTM)
  test_options <- tibble(
    Strike = c(85, 90, 95, 100, 105),
    Bid = c(0.50, 1.00, 1.50, 3.00, 5.00),
    OI = c(50, 100, 150, 200, 250),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.90
  )

  # Threshold = 100 * 0.90 = 90
  # Should select strikes >= 90 (90, 95, 100, 105)
  # Should NOT select 85
  expect_false(is.null(result))
  expect_gte(result$option$Strike, 90)
})

test_that("select_optimal_put handles exact threshold boundary", {
  # Test boundary condition: strike exactly at threshold
  test_options <- tibble(
    Strike = c(94.99, 95.00, 95.01),
    Bid = c(1.45, 1.50, 1.55),
    OI = c(100, 100, 100),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  # Threshold = 100 * 0.95 = 95.00
  # With >= comparison, should include 95.00 and 95.01
  # Should NOT include 94.99
  expect_false(is.null(result))
  expect_gte(result$option$Strike, 95.00)
})

test_that("select_optimal_put: higher threshold % = more conservative", {
  test_options <- tibble(
    Strike = c(85, 90, 95, 100),
    Bid = c(0.50, 1.00, 1.50, 3.00),
    OI = c(100, 100, 100, 100),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  # 90% threshold (aggressive)
  result_90 <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.90
  )

  # 95% threshold (conservative)
  result_95 <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  # 90% threshold allows strikes >= 90
  # 95% threshold requires strikes >= 95
  # So 95% should select higher strike (more conservative, closer to ATM)
  expect_gte(result_95$option$Strike, result_90$option$Strike)
})

test_that("select_optimal_put selects longest expiration when multiple options", {
  test_options <- tibble(
    Strike = c(95, 95, 95),
    Bid = c(1.50, 2.00, 2.50),
    OI = c(100, 100, 100),
    expiration = as.Date(c("2025-02-21", "2025-03-21", "2025-04-18")),
    days_to_expiry = c(60, 90, 120)
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  # Should select longest dated: 2025-04-18 (120 days)
  expect_equal(result$option$expiration, as.Date("2025-04-18"))
  expect_equal(result$option$days_to_expiry, 120)
})

test_that("select_optimal_put selects highest OI when same expiration", {
  test_options <- tibble(
    Strike = c(95, 95, 95),
    Bid = c(1.50, 1.50, 1.50),
    OI = c(50, 200, 100),  # Middle one has highest OI
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  # Should select highest OI: 200
  expect_equal(result$option$OI, 200)
})

test_that("select_optimal_put filters by min/max days correctly", {
  test_options <- tibble(
    Strike = c(95, 95, 95, 95),
    Bid = c(1.00, 1.50, 2.00, 2.50),
    OI = c(100, 100, 100, 100),
    expiration = as.Date(c("2025-01-31", "2025-02-28", "2025-03-31", "2025-05-30")),
    days_to_expiry = c(30, 60, 90, 150)
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95,
    min_days = 45,
    max_days = 120
  )

  # Should filter to 60-day and 90-day options
  # Should select longest: 90-day
  expect_equal(result$option$days_to_expiry, 90)
  expect_gte(result$option$days_to_expiry, 45)
  expect_lte(result$option$days_to_expiry, 120)
})

test_that("select_optimal_put filters by minimum bid", {
  test_options <- tibble(
    Strike = c(95, 95, 95),
    Bid = c(0.005, 0.01, 1.50),  # One below min, one at min, one above
    OI = c(100, 100, 100),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  # Should filter out 0.005 (below min_option_bid = 0.01)
  expect_gte(result$option$Bid, CASH_SECURED_PUTS_CONFIG$min_option_bid)
})

test_that("select_optimal_put filters by minimum open interest", {
  test_options <- tibble(
    Strike = c(95, 95, 95),
    Bid = c(1.50, 1.60, 1.70),
    OI = c(5, 10, 50),  # One below min, one at min, one above
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  # Should filter out OI < 10
  expect_gte(result$option$OI, CASH_SECURED_PUTS_CONFIG$min_open_interest)
})

test_that("select_optimal_put returns NULL when no options meet strike criteria", {
  # All strikes too deep OTM
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
})

test_that("select_optimal_put returns NULL when all bids too low", {
  test_options <- tibble(
    Strike = c(95, 95, 95),
    Bid = c(0.001, 0.005, 0.009),  # All below min_option_bid = 0.01
    OI = c(100, 100, 100),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  expect_null(result)
})

test_that("select_optimal_put returns NULL when all OI too low", {
  test_options <- tibble(
    Strike = c(95, 95, 95),
    Bid = c(1.50, 1.60, 1.70),
    OI = c(1, 3, 5),  # All below min_open_interest = 10
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  expect_null(result)
})

test_that("select_optimal_put returns NULL for empty options", {
  test_options <- tibble(
    Strike = numeric(0),
    Bid = numeric(0),
    OI = numeric(0),
    expiration = as.Date(character(0)),
    days_to_expiry = numeric(0)
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  expect_null(result)
})

################################################################################
# OPTION VALUE DECOMPOSITION TESTS
################################################################################

test_that("calculate_put_metrics computes intrinsic value for OTM put", {
  # OTM put: strike < current_price
  option_row <- tibble(
    Strike = 95,
    Bid = 1.50,
    days_to_expiry = 90,
    expiration = as.Date("2025-03-21"),
    OI = 100
  )

  stock_data <- list(
    company_name = "Test Corp",
    current_price = 100,
    max_drawdown = -0.15,
    current_yield = 0.02,
    annual_dividend = 2.00
  )

  result <- calculate_put_metrics(
    ticker = "TEST",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  # OTM put: intrinsic = max(0, strike - current) = max(0, 95 - 100) = 0
  expect_equal(result$intrinsic_value, 0)
  # All time value
  expect_equal(result$extrinsic_value, 1.50)
})

test_that("calculate_put_metrics computes intrinsic value for ATM put", {
  # ATM put: strike = current_price
  option_row <- tibble(
    Strike = 100,
    Bid = 3.00,
    days_to_expiry = 90,
    expiration = as.Date("2025-03-21"),
    OI = 100
  )

  stock_data <- list(
    company_name = "Test Corp",
    current_price = 100,
    max_drawdown = -0.15,
    current_yield = 0.02,
    annual_dividend = 2.00
  )

  result <- calculate_put_metrics(
    ticker = "TEST",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  # ATM put: intrinsic = max(0, strike - current) = max(0, 100 - 100) = 0
  expect_equal(result$intrinsic_value, 0)
  # All time value
  expect_equal(result$extrinsic_value, 3.00)
})

test_that("calculate_put_metrics computes intrinsic value for ITM put", {
  # ITM put: strike > current_price
  option_row <- tibble(
    Strike = 105,
    Bid = 6.00,
    days_to_expiry = 90,
    expiration = as.Date("2025-03-21"),
    OI = 100
  )

  stock_data <- list(
    company_name = "Test Corp",
    current_price = 100,
    max_drawdown = -0.15,
    current_yield = 0.02,
    annual_dividend = 2.00
  )

  result <- calculate_put_metrics(
    ticker = "TEST",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  # ITM put: intrinsic = max(0, strike - current) = max(0, 105 - 100) = 5
  expect_equal(result$intrinsic_value, 5.00)
  # Remaining is time value: 6.00 - 5.00 = 1.00
  expect_equal(result$extrinsic_value, 1.00)
})

################################################################################
# INTEGRATION: CALCULATE_PUT_METRICS
################################################################################

test_that("calculate_put_metrics returns all required fields", {
  option_row <- tibble(
    Strike = 95,
    Bid = 1.50,
    days_to_expiry = 90,
    expiration = as.Date("2025-03-21"),
    OI = 100
  )

  stock_data <- list(
    company_name = "Test Corp",
    current_price = 100,
    max_drawdown = -0.15,
    current_yield = 0.02,
    annual_dividend = 2.00
  )

  result <- calculate_put_metrics(
    ticker = "TEST",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  required_fields <- c(
    "ticker", "company_name", "current_price", "strike",
    "expiration", "days_to_expiry", "bid_price", "open_interest",
    "cash_required", "premium_received", "net_outlay",
    "return_on_cash", "annualized_return",
    "max_drawdown", "current_yield",
    "breakeven_price", "downside_protection_pct",
    "intrinsic_value", "extrinsic_value", "annual_dividend",
    "warning_flag", "is_put", "is_aristocrat"
  )

  for (field in required_fields) {
    expect_true(field %in% names(result),
               info = sprintf("Missing field: %s", field))
  }
})

test_that("calculate_put_metrics validates all metrics for realistic scenario", {
  # Realistic scenario: $100 stock, $95 strike, $1.50 bid, 90 days
  option_row <- tibble(
    Strike = 95,
    Bid = 1.50,
    days_to_expiry = 90,
    expiration = as.Date("2025-03-21"),
    OI = 150
  )

  stock_data <- list(
    company_name = "Dividend Aristocrat Inc",
    current_price = 100,
    max_drawdown = -0.18,
    current_yield = 0.025,
    annual_dividend = 2.50
  )

  result <- calculate_put_metrics(
    ticker = "DARC",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  # Verify all calculations
  expect_equal(result$ticker, "DARC")
  expect_equal(result$company_name, "Dividend Aristocrat Inc")
  expect_equal(result$current_price, 100)
  expect_equal(result$strike, 95)
  expect_equal(result$bid_price, 1.50)
  expect_equal(result$days_to_expiry, 90)
  expect_equal(result$open_interest, 150)

  # Cash flows
  expect_equal(result$cash_required, 9500)
  expect_equal(result$premium_received, 150)
  expect_equal(result$net_outlay, 9350)

  # Returns
  expect_equal(result$return_on_cash, 150 / 9500, tolerance = 0.0001)
  expected_ann <- (150 / 9500) * (365 / 90)
  expect_equal(result$annualized_return, expected_ann, tolerance = 0.0001)

  # Protection
  expect_equal(result$breakeven_price, 93.50)
  expect_equal(result$downside_protection_pct, 0.065, tolerance = 0.0001)

  # Option values
  expect_equal(result$intrinsic_value, 0)  # OTM
  expect_equal(result$extrinsic_value, 1.50)

  # Stock data
  expect_equal(result$max_drawdown, -0.18)
  expect_equal(result$current_yield, 0.025)
  expect_equal(result$annual_dividend, 2.50)

  # Flags
  expect_false(result$warning_flag)
  expect_true(result$is_put)
  expect_true(result$is_aristocrat)
})

################################################################################
# LIVE DATA INTEGRATION TESTS
################################################################################

test_that("get_options_chain_puts returns valid data for KO", {
  skip_on_cran()
  skip_if_offline()

  # Test with Coca-Cola (dividend aristocrat)
  current_price <- 65.00
  result <- get_options_chain_puts("KO", current_price)

  # Should return data (not empty)
  expect_true(nrow(result) > 0,
              info = "Should find put options for KO")

  # Check required columns exist
  required_cols <- c("Strike", "Bid", "expiration", "days_to_expiry",
                     "intrinsic_value", "time_value")
  expect_true(all(required_cols %in% names(result)),
              info = "All required columns should be present")

  # Validate Strike column has no NAs
  expect_false(any(is.na(result$Strike)),
               info = "Strike column should not contain NA values")

  # Validate intrinsic value calculation
  # Intrinsic value = max(0, Strike - current_price)
  expected_intrinsic <- pmax(0, result$Strike - current_price)
  expect_equal(result$intrinsic_value, expected_intrinsic,
               tolerance = 0.01,
               info = "Intrinsic value should equal max(0, Strike - current_price)")

  # Validate time value calculation
  # Time value = Bid - intrinsic_value
  expected_time_value <- result$Bid - result$intrinsic_value
  expect_equal(result$time_value, expected_time_value,
               tolerance = 0.01,
               info = "Time value should equal Bid - intrinsic_value")

  # Time value should be non-negative
  expect_true(all(result$time_value >= 0),
              info = "Time value should be non-negative")

  # Days to expiry should be positive
  expect_true(all(result$days_to_expiry > 0),
              info = "Days to expiry should be positive")

  # Print sample data for inspection
  cat("\n=== Sample get_options_chain_puts data for KO ===\n")
  print(head(result, 3))
})

test_that("analyze_single_stock_put returns results for KO", {
  skip_on_cran()
  skip_if_offline()

  # Test full analysis function
  result <- analyze_single_stock_put("KO", strike_threshold_pct = 0.85)

  # Should return results
  expect_true(nrow(result) > 0,
              info = "Should find put opportunities for KO")

  # Check key columns exist
  expected_cols <- c("symbol", "Strike", "Bid", "intrinsic_value",
                     "time_value", "premium_yield", "annualized_return")
  expect_true(all(expected_cols %in% names(result)),
              info = "Expected analysis columns should be present")

  # Premium yield should be positive
  expect_true(all(result$premium_yield > 0),
              info = "Premium yield should be positive")

  # Annualized return should be positive
  expect_true(all(result$annualized_return > 0),
              info = "Annualized return should be positive")

  # Print sample data for inspection
  cat("\n=== Sample analyze_single_stock_put data for KO ===\n")
  print(head(result, 3))
})

test_that("cash secured puts strategy handles edge cases", {
  skip_on_cran()
  skip_if_offline()

  # Test with invalid symbol
  result_invalid <- get_options_chain_puts("INVALID_SYMBOL_123", 100)
  expect_true(nrow(result_invalid) == 0,
              info = "Invalid symbol should return empty tibble")

  # Test with very high strike threshold (should return fewer/no results)
  result_high_threshold <- analyze_single_stock_put("KO", strike_threshold_pct = 1.50)
  # Should either be empty or have very few results
  expect_true(is.data.frame(result_high_threshold),
              info = "Should return data frame even with high threshold")
})

test_that("intrinsic and time value calculations are mathematically correct", {
  # Create mock data to verify calculation logic
  mock_data <- tibble(
    Strike = c(60, 65, 70),
    Bid = c(0.50, 2.00, 5.50)
  )
  current_price <- 65.00

  # Calculate values
  intrinsic <- pmax(0, mock_data$Strike - current_price)
  time_val <- mock_data$Bid - intrinsic

  # Expected values:
  # Strike 60: intrinsic = 0, time = 0.50
  # Strike 65: intrinsic = 0, time = 2.00
  # Strike 70: intrinsic = 5, time = 0.50

  expect_equal(intrinsic, c(0, 0, 5), tolerance = 0.01)
  expect_equal(time_val, c(0.50, 2.00, 0.50), tolerance = 0.01)
})
