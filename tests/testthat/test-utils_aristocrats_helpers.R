test_that("validate_ticker works correctly", {
  # Valid tickers
  expect_silent(validate_ticker("AAPL"))
  expect_silent(validate_ticker("BRK.B"))
  expect_silent(validate_ticker("ABC"))

  # Invalid tickers
  expect_error(validate_ticker(123), "ticker must be character")
  expect_error(validate_ticker(c("A", "B")), "ticker must be length 1")
  expect_error(validate_ticker(""), "ticker must not be empty")
  expect_error(validate_ticker("aapl"), "ticker must be uppercase")
  expect_error(validate_ticker("AAP-L"), "ticker must be uppercase letters/dots")
})

test_that("validate_price works correctly", {
  # Valid prices
  expect_silent(validate_price(100))
  expect_silent(validate_price(0.01))
  expect_silent(validate_price(10000))

  # Invalid prices
  expect_error(validate_price(NA), "price must not be NA")
  expect_error(validate_price("100"), "price must be numeric")
  expect_error(validate_price(0), "price must be positive")
  expect_error(validate_price(-10), "price must be positive")

  # Custom name in error message
  expect_error(validate_price(-10, "stock_price"), "stock_price must be positive")
})

test_that("validate_percentage works correctly", {
  # Valid percentages
  expect_silent(validate_percentage(0))
  expect_silent(validate_percentage(0.5))
  expect_silent(validate_percentage(1))

  # Invalid percentages
  expect_error(validate_percentage(NA), "percentage must not be NA")
  expect_error(validate_percentage("0.5"), "percentage must be numeric")
  expect_error(validate_percentage(-0.1), "percentage must be between 0 and 1")
  expect_error(validate_percentage(1.1), "percentage must be between 0 and 1")

  # Custom name in error message
  expect_error(validate_percentage(1.5, "threshold"), "threshold must be between 0 and 1")
})

test_that("validate_columns works correctly", {
  # Valid data frame
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  expect_silent(validate_columns(df, c("a", "b")))
  expect_silent(validate_columns(df, c("a", "b", "c")))

  # Invalid inputs
  expect_error(validate_columns("not_a_df", c("a")), "data frame must be a data frame")
  expect_error(validate_columns(df, c("x", "y")), "missing required columns: x, y")
  expect_error(validate_columns(df, c("a", "x")), "missing required columns: x")

  # Custom name in error message
  expect_error(validate_columns(df, c("x"), "my_table"), "my_table missing required columns: x")
})

test_that("calculate_annualized_return works correctly", {
  # Note: calculate_annualized_return is in utils_calculations.R
  # It takes total_return (as decimal) and days, not total_profit/capital_required
  # Comprehensive tests are in test-fct_aristocrats_analysis.R

  # Basic test: 10% return over 1 year
  result <- calculate_annualized_return(
    total_return = 0.1,
    days = 365
  )
  expect_equal(result, 0.1, tolerance = 0.001)

  # Test: 10% return over 6 months should annualize to ~21%
  result2 <- calculate_annualized_return(
    total_return = 0.1,
    days = 182.5
  )
  expect_equal(result2, 0.21, tolerance = 0.01)

  # Test: 10% return over 2 years should annualize to ~4.88%
  result3 <- calculate_annualized_return(
    total_return = 0.1,
    days = 730
  )
  expect_equal(result3, 0.0488, tolerance = 0.001)

  # Edge cases
  expect_equal(calculate_annualized_return(0, 365), 0)  # 0% return
  expect_equal(calculate_annualized_return(0.1, 0), 0)  # 0 days
})

test_that("find_ticker_column works correctly", {
  # Test with standard column names
  expect_equal(find_ticker_column(c("Name", "Ticker symbol", "Price")), "Ticker symbol")
  expect_equal(find_ticker_column(c("Name", "Symbol", "Price")), "Symbol")
  expect_equal(find_ticker_column(c("Name", "Ticker", "Price")), "Ticker")

  # Test with no match
  expect_null(find_ticker_column(c("Name", "Price", "Volume")))

  # Test with multiple matches (should return first)
  expect_equal(find_ticker_column(c("Ticker symbol", "Symbol", "Ticker")), "Ticker symbol")

  # Test with custom possible names
  expect_equal(
    find_ticker_column(c("Stock", "Price"), possible_names = c("Stock")),
    "Stock"
  )
})

test_that("truncate_error works correctly", {
  # Short message (no truncation)
  short_msg <- "Error occurred"
  expect_equal(truncate_error(short_msg), short_msg)

  # Long message (truncation)
  long_msg <- paste(rep("a", 100), collapse = "")
  result <- truncate_error(long_msg, max_chars = 50)
  expect_equal(nchar(result), 53)  # 50 chars + "..."
  expect_true(grepl("\\.\\.\\.$", result))

  # Edge case: exactly max_chars
  exact_msg <- paste(rep("a", 50), collapse = "")
  expect_equal(truncate_error(exact_msg, max_chars = 50), exact_msg)

  # Custom max_chars
  result2 <- truncate_error(long_msg, max_chars = 20)
  expect_equal(nchar(result2), 23)  # 20 chars + "..."
})

test_that("get_reinvestment_rate returns valid rate", {
  # This test may fail if market is closed or data unavailable
  # We mainly test that it returns a valid number
  rate <- get_reinvestment_rate()

  expect_true(is.numeric(rate))
  expect_gte(rate, 0)
  expect_lte(rate, 0.20)  # Should be less than 20%
})