################################################################################
# Unit Tests for metrics.R
################################################################################

library(testthat)
library(tibble)
library(xts)

test_that("calculate_option_values works correctly", {
  # Deep ITM option
  result <- calculate_option_values(100, 80, 22)
  expect_equal(result$intrinsic_value, 20)
  expect_equal(result$extrinsic_value, 2)

  # ATM option
  result <- calculate_option_values(100, 100, 3)
  expect_equal(result$intrinsic_value, 0)
  expect_equal(result$extrinsic_value, 3)

  # OTM option (should not happen in our strategy)
  result <- calculate_option_values(100, 105, 1)
  expect_equal(result$intrinsic_value, 0)
  expect_equal(result$extrinsic_value, 1)
})

test_that("calculate_option_values validates inputs", {
  expect_error(calculate_option_values(-100, 80, 22), "positive")
  expect_error(calculate_option_values(100, -80, 22), "positive")
  expect_error(calculate_option_values(100, 80, -22), "positive")
  expect_error(calculate_option_values(NA, 80, 22), "not be NA")
})

test_that("calculate_protection_metrics works correctly", {
  result <- calculate_protection_metrics(100, 5)
  expect_equal(result$breakeven_price, 95)
  expect_equal(result$downside_protection_pct, 0.05)

  result <- calculate_protection_metrics(100, 20)
  expect_equal(result$breakeven_price, 80)
  expect_equal(result$downside_protection_pct, 0.20)
})

test_that("calculate_return_metrics works correctly", {
  # 10% return over 365 days
  result <- calculate_return_metrics(100, 1000, 365)
  expect_equal(result$total_return, 0.1)
  expect_equal(result$annualized_return, 0.1, tolerance = 0.0001)

  # 10% return over ~6 months should be ~21% annualized
  result <- calculate_return_metrics(100, 1000, 180)
  expect_equal(result$total_return, 0.1)
  expect_gt(result$annualized_return, 0.20)
  expect_lt(result$annualized_return, 0.22)
})

test_that("calculate_cash_flows works correctly", {
  result <- calculate_cash_flows(
    current_price = 100,
    strike = 80,
    bid_price = 22,
    dividend_income = 100,
    reinvestment_income = 5
  )

  expect_equal(result$investment, 10000)  # 100 * 100 shares
  expect_equal(result$premium_received, 2200)  # 22 * 100
  expect_equal(result$net_outlay, 7800)  # 10000 - 2200
  expect_equal(result$exercise_proceeds, 8000)  # 80 * 100
  expect_equal(result$net_profit, 305)  # 2200 + 100 + 5 + 8000 - 10000
})

test_that("calculate_dividend_projections with no dividends returns zero", {
  result <- calculate_dividend_projections(NULL, 90)
  expect_equal(result$dividend_income, 0)
  expect_equal(result$reinvestment_income, 0)

  # Empty xts object
  empty_divs <- xts(numeric(0), order.by = as.Date(character(0)))
  result <- calculate_dividend_projections(empty_divs, 90)
  expect_equal(result$dividend_income, 0)
  expect_equal(result$reinvestment_income, 0)
})

test_that("calculate_dividend_projections with quarterly dividends", {
  # Create mock quarterly dividend history
  dates <- as.Date(c("2024-01-15", "2024-04-15", "2024-07-15", "2024-10-15"))
  amounts <- c(0.50, 0.50, 0.50, 0.50)
  divs <- xts(amounts, order.by = dates)

  # 180 days = ~2 quarters = ~$100 in dividends (2 * $0.50 * 100 shares)
  result <- calculate_dividend_projections(divs, 180, reinvest_rate = 0.05)

  expect_gt(result$dividend_income, 90)  # Should be close to 100
  expect_lt(result$dividend_income, 110)

  # Reinvestment should be small but positive
  expect_gt(result$reinvestment_income, 0)
  expect_lt(result$reinvestment_income, 10)
})

test_that("calculate_metrics returns tibble with all required columns", {
  # Create mock data
  option_row <- tibble(
    Strike = 80,
    Bid = 22,
    days_to_expiry = 90,
    expiration = as.Date("2025-12-31"),
    OI = 100
  )

  stock_data <- list(
    ticker = "TEST",
    company_name = "Test Company",
    current_price = 100,
    dividends = NULL,
    max_drawdown = -0.20,
    current_yield = 0.02,
    annual_dividend = 2.00
  )

  result <- calculate_metrics(
    ticker = "TEST",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  # Check it returns a tibble
  expect_s3_class(result, "tbl_df")

  # Check key columns exist
  required_cols <- c("ticker", "current_price", "strike", "annualized_return",
                    "total_return", "dividend_income", "net_profit")
  expect_true(all(required_cols %in% names(result)))

  # Check values are reasonable
  expect_equal(result$ticker, "TEST")
  expect_equal(result$strike, 80)
  expect_equal(result$investment, 10000)
  expect_equal(result$warning_flag, FALSE)
})

test_that("calculate_metrics validates inputs", {
  option_row <- tibble(
    Strike = 80,
    Bid = 22,
    days_to_expiry = 90,
    expiration = as.Date("2025-12-31"),
    OI = 100
  )

  stock_data <- list(
    ticker = "TEST",
    company_name = "Test Company",
    current_price = 100,
    dividends = NULL,
    max_drawdown = -0.20,
    current_yield = 0.02,
    annual_dividend = 2.00
  )

  # Invalid ticker
  expect_error(
    calculate_metrics("invalid", 100, option_row, stock_data, FALSE),
    "uppercase"
  )

  # Invalid price
  expect_error(
    calculate_metrics("TEST", -100, option_row, stock_data, FALSE),
    "positive"
  )

  # Missing columns in option_row
  bad_option <- tibble(Strike = 80)
  expect_error(
    calculate_metrics("TEST", 100, bad_option, stock_data, FALSE),
    "missing required columns"
  )
})