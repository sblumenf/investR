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

test_that("finalize_results returns properly structured empty tibble when no results", {
  # Test with empty list
  result <- finalize_results(list())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)

  # Verify all expected columns are present
  expected_columns <- c(
    "ticker", "company_name", "current_price", "strike", "expiration",
    "days_to_expiry", "bid_price", "open_interest", "investment",
    "premium_received", "dividend_income", "reinvestment_income",
    "exercise_proceeds", "net_profit", "net_outlay", "total_return",
    "annualized_return", "max_drawdown", "current_yield", "breakeven_price",
    "downside_protection_pct", "intrinsic_value", "extrinsic_value",
    "annual_dividend", "warning_flag"
  )

  expect_true(all(expected_columns %in% names(result)))

  # Verify column types
  expect_type(result$ticker, "character")
  expect_type(result$company_name, "character")
  expect_type(result$expiration, "character")
  expect_type(result$current_price, "double")
  expect_type(result$strike, "double")
  expect_type(result$warning_flag, "logical")
})

test_that("finalize_results returns properly structured empty tibble when all NULLs", {
  # Test with list of NULLs (simulating all stock analyses returning NULL)
  result <- finalize_results(list(NULL, NULL, NULL))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true("expiration" %in% names(result))
})

test_that("finalize_results sorts non-empty results by annualized return", {
  # Create mock results
  result1 <- tibble::tibble(
    ticker = "AAPL",
    annualized_return = 0.15,
    company_name = "Apple Inc",
    current_price = 100,
    strike = 90,
    expiration = "2025-12-31",
    days_to_expiry = 365,
    bid_price = 12,
    open_interest = 1000,
    investment = 10000,
    premium_received = 1200,
    dividend_income = 100,
    reinvestment_income = 5,
    exercise_proceeds = 9000,
    net_profit = 305,
    net_outlay = 8800,
    total_return = 0.035,
    max_drawdown = -0.2,
    current_yield = 0.02,
    breakeven_price = 88,
    downside_protection_pct = 0.12,
    intrinsic_value = 10,
    extrinsic_value = 2,
    annual_dividend = 4,
    warning_flag = FALSE
  )

  result2 <- tibble::tibble(
    ticker = "MSFT",
    annualized_return = 0.25,  # Higher return
    company_name = "Microsoft",
    current_price = 200,
    strike = 180,
    expiration = "2025-12-31",
    days_to_expiry = 365,
    bid_price = 24,
    open_interest = 2000,
    investment = 20000,
    premium_received = 2400,
    dividend_income = 200,
    reinvestment_income = 10,
    exercise_proceeds = 18000,
    net_profit = 610,
    net_outlay = 17600,
    total_return = 0.035,
    max_drawdown = -0.15,
    current_yield = 0.015,
    breakeven_price = 176,
    downside_protection_pct = 0.12,
    intrinsic_value = 20,
    extrinsic_value = 4,
    annual_dividend = 8,
    warning_flag = FALSE
  )

  results <- finalize_results(list(result1, result2))

  expect_equal(nrow(results), 2)
  # Should be sorted by annualized_return descending, so MSFT first
  expect_equal(results$ticker[1], "MSFT")
  expect_equal(results$ticker[2], "AAPL")
})