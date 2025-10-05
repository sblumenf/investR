################################################################################
# Unit Tests for utils.R
################################################################################

library(testthat)

test_that("find_ticker_column returns correct column name", {
  table_names1 <- c("Date", "Symbol", "Price")
  expect_equal(find_ticker_column(table_names1), "Symbol")

  table_names2 <- c("Date", "Ticker symbol", "Price")
  expect_equal(find_ticker_column(table_names2), "Ticker symbol")

  table_names3 <- c("Date", "Price", "Volume")
  expect_null(find_ticker_column(table_names3))
})

test_that("validate_ticker accepts valid tickers", {
  expect_true(validate_ticker("AAPL"))
  expect_true(validate_ticker("BRK.B"))
  expect_true(validate_ticker("A"))
})

test_that("validate_ticker rejects invalid tickers", {
  expect_error(validate_ticker("aapl"), "uppercase")
  expect_error(validate_ticker(c("AAPL", "MSFT")), "length 1")
  expect_error(validate_ticker(""), "not be empty")
  expect_error(validate_ticker(123), "character")
  expect_error(validate_ticker("AAPL123"), "uppercase letters")
})

test_that("validate_price accepts valid prices", {
  expect_true(validate_price(100.50))
  expect_true(validate_price(0.01))
  expect_true(validate_price(1000000))
})

test_that("validate_price rejects invalid prices", {
  expect_error(validate_price(-10), "positive")
  expect_error(validate_price(0), "positive")
  expect_error(validate_price(NA), "not be NA")
  expect_error(validate_price("100"), "numeric")
})

test_that("validate_percentage accepts valid percentages", {
  expect_true(validate_percentage(0.5))
  expect_true(validate_percentage(0))
  expect_true(validate_percentage(1))
  expect_true(validate_percentage(0.8))
})

test_that("validate_percentage rejects invalid percentages", {
  expect_error(validate_percentage(-0.1), "between 0 and 1")
  expect_error(validate_percentage(1.5), "between 0 and 1")
  expect_error(validate_percentage(NA), "not be NA")
  expect_error(validate_percentage("0.5"), "numeric")
})

test_that("validate_columns detects missing columns", {
  df <- data.frame(a = 1, b = 2, c = 3)
  expect_true(validate_columns(df, c("a", "b")))
  expect_error(validate_columns(df, c("a", "d")), "missing required columns: d")
  expect_error(validate_columns(df, c("d", "e")), "missing required columns: d, e")
})

test_that("calculate_annualized_return works correctly", {
  # 10% return over 365 days should be 10% annualized
  result <- calculate_annualized_return(100, 1000, 365)
  expect_equal(result, 0.1, tolerance = 0.0001)

  # 10% return over 182.5 days should be ~21% annualized
  result <- calculate_annualized_return(100, 1000, 182.5)
  expect_gt(result, 0.20)
  expect_lt(result, 0.22)

  # Zero or negative capital returns 0
  expect_equal(calculate_annualized_return(100, 0, 365), 0)
  expect_equal(calculate_annualized_return(100, -100, 365), 0)

  # Zero days returns 0
  expect_equal(calculate_annualized_return(100, 1000, 0), 0)
})

test_that("format_timestamp returns correct format", {
  ts <- format_timestamp()
  expect_match(ts, "^\\d{8}_\\d{6}$")
  expect_equal(nchar(ts), 15)
})

test_that("truncate_error truncates long messages", {
  short_msg <- "Short error"
  expect_equal(truncate_error(short_msg), short_msg)

  long_msg <- paste(rep("A", 100), collapse = "")
  truncated <- truncate_error(long_msg, 50)
  expect_equal(nchar(truncated), 53)  # 50 chars + "..."
  expect_match(truncated, "\\.\\.\\.$")
})