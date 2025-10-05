test_that("calculate_option_values works correctly", {
  # Test basic calculation
  result <- calculate_option_values(
    current_price = 100,
    strike = 80,
    bid_price = 22
  )

  expect_equal(result$intrinsic_value, 20)
  expect_equal(result$extrinsic_value, 2)

  # Test when out of the money
  result2 <- calculate_option_values(
    current_price = 100,
    strike = 110,
    bid_price = 3
  )

  expect_equal(result2$intrinsic_value, 0)
  expect_equal(result2$extrinsic_value, 3)
})

test_that("calculate_option_values validates inputs", {
  expect_error(
    calculate_option_values(current_price = -10, strike = 80, bid_price = 5),
    "current_price must be positive"
  )

  expect_error(
    calculate_option_values(current_price = 100, strike = -80, bid_price = 5),
    "strike must be positive"
  )

  expect_error(
    calculate_option_values(current_price = 100, strike = 80, bid_price = 0),
    "bid_price must be positive"
  )
})

test_that("calculate_protection_metrics works correctly", {
  result <- calculate_protection_metrics(
    current_price = 100,
    bid_price = 20
  )

  expect_equal(result$breakeven_price, 80)
  expect_equal(result$downside_protection_pct, 0.2)  # 20%

  # Test with different values
  result2 <- calculate_protection_metrics(
    current_price = 50,
    bid_price = 10
  )

  expect_equal(result2$breakeven_price, 40)
  expect_equal(result2$downside_protection_pct, 0.2)
})

test_that("calculate_return_metrics works correctly", {
  result <- calculate_return_metrics(
    net_profit = 1000,
    net_outlay = 10000,
    days_to_expiry = 365
  )

  expect_equal(result$total_return, 0.1)  # 10%
  expect_equal(result$annualized_return, 0.1, tolerance = 0.001)

  # Test with 180 days
  result2 <- calculate_return_metrics(
    net_profit = 1000,
    net_outlay = 10000,
    days_to_expiry = 182.5  # Half year
  )

  expect_equal(result2$total_return, 0.1)
  # Annualized should be approximately (1.1)^2 - 1 = 0.21
  expect_equal(result2$annualized_return, 0.21, tolerance = 0.01)
})

test_that("calculate_return_metrics validates inputs", {
  expect_error(
    calculate_return_metrics(net_profit = 100, net_outlay = "invalid", days_to_expiry = 365),
    "net_outlay must be numeric"
  )

  expect_error(
    calculate_return_metrics(net_profit = 100, net_outlay = 1000, days_to_expiry = 0),
    "days_to_expiry must be positive"
  )

  expect_error(
    calculate_return_metrics(net_profit = 100, net_outlay = 1000, days_to_expiry = -10),
    "days_to_expiry must be positive"
  )
})

test_that("calculate_cash_flows works correctly", {
  result <- calculate_cash_flows(
    current_price = 100,
    strike = 80,
    bid_price = 22,
    dividend_income = 200,
    reinvestment_income = 10
  )

  expect_equal(result$investment, 10000)  # 100 * 100 shares
  expect_equal(result$premium_received, 2200)  # 22 * 100 shares
  expect_equal(result$net_outlay, 7800)  # 10000 - 2200
  expect_equal(result$exercise_proceeds, 8000)  # 80 * 100 shares
  expect_equal(result$net_profit, 410)  # 2200 + 200 + 10 + 8000 - 10000
})

test_that("calculate_dividend_projections returns zero for null dividends", {
  result <- calculate_dividend_projections(
    dividends = NULL,
    days_to_expiry = 365,
    reinvest_rate = 0.05
  )

  expect_equal(result$dividend_income, 0)
  expect_equal(result$reinvestment_income, 0)
})

test_that("calculate_dividend_projections returns zero for insufficient dividend history", {
  # Create a minimal xts object with 1 dividend (need at least 2)
  div_dates <- as.Date("2024-01-01")
  div_values <- 2.50
  dividends <- xts::xts(div_values, order.by = div_dates)

  result <- calculate_dividend_projections(
    dividends = dividends,
    days_to_expiry = 365,
    reinvest_rate = 0.05
  )

  expect_equal(result$dividend_income, 0)
  expect_equal(result$reinvestment_income, 0)
})

test_that("calculate_dividend_projections works with real dividend data", {
  # Create realistic quarterly dividends
  div_dates <- seq(as.Date("2023-01-01"), as.Date("2024-10-01"), by = "3 months")
  div_values <- rep(2.50, length(div_dates))
  dividends <- xts::xts(div_values, order.by = div_dates)

  # Test for 1 year holding period
  result <- calculate_dividend_projections(
    dividends = dividends,
    days_to_expiry = 365,
    reinvest_rate = 0.05
  )

  # Should project approximately 4 payments * 2.50 * 100 shares = 1000
  expect_gt(result$dividend_income, 900)
  expect_lt(result$dividend_income, 1100)

  # Reinvestment income should be positive but small
  expect_gt(result$reinvestment_income, 0)
  expect_lt(result$reinvestment_income, result$dividend_income * 0.1)
})

test_that("calculate_metrics orchestrates all calculations correctly", {
  # Create test option row
  option_row <- tibble::tibble(
    Strike = 80,
    Bid = 22,
    days_to_expiry = 365,
    expiration = as.Date("2025-12-31"),
    OI = 1000
  )

  # Create mock stock data
  div_dates <- seq(as.Date("2023-01-01"), as.Date("2024-10-01"), by = "3 months")
  div_values <- rep(2.50, length(div_dates))
  dividends <- xts::xts(div_values, order.by = div_dates)

  stock_data <- list(
    company_name = "Test Corp",
    dividends = dividends,
    max_drawdown = -0.15,
    current_yield = 0.03,
    annual_dividend = 10
  )

  result <- calculate_metrics(
    ticker = "TEST",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  # Verify structure
  expect_s3_class(result, "tbl_df")
  expect_true("ticker" %in% names(result))
  expect_true("annualized_return" %in% names(result))
  expect_true("total_return" %in% names(result))

  # Verify values
  expect_equal(result$ticker, "TEST")
  expect_equal(result$current_price, 100)
  expect_equal(result$strike, 80)
  expect_equal(result$company_name, "Test Corp")
  expect_equal(result$warning_flag, FALSE)
  expect_equal(result$is_aristocrat, TRUE)

  # Verify calculations are reasonable
  expect_gt(result$annualized_return, 0)
  expect_gt(result$downside_protection_pct, 0)
  expect_lt(result$downside_protection_pct, 1)
})