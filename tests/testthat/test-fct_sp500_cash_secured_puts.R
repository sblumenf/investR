# Tests for S&P 500 Cash-Secured Puts Analysis

# Test Setup ----

test_that("analyze_sp500_cash_secured_puts function exists and is exported", {
  expect_true(exists("analyze_sp500_cash_secured_puts"))
  expect_true("analyze_sp500_cash_secured_puts" %in% getNamespaceExports("investR"))
})

# Dividend Filter Tests ----

test_that("analyze_sp500_cash_secured_puts validates dividend_filter parameter", {
  expect_error(
    analyze_sp500_cash_secured_puts(
      dividend_filter = "invalid_filter",
      limit = 5,
      max_workers = 1
    ),
    "Invalid dividend_filter"
  )

  expect_error(
    analyze_sp500_cash_secured_puts(
      dividend_filter = "",
      limit = 5,
      max_workers = 1
    ),
    "Invalid dividend_filter"
  )
})

test_that("analyze_sp500_cash_secured_puts accepts valid dividend filter values", {
  valid_filters <- c("all", "dividend_paying", "zero_dividend")

  for (filter_value in valid_filters) {
    expect_silent({
      result <- analyze_sp500_cash_secured_puts(
        dividend_filter = filter_value,
        limit = 5,
        max_workers = 1
      )
    })
  }
})

test_that("analyze_sp500_cash_secured_puts filters by 'all' correctly", {
  skip_on_cran()

  result <- analyze_sp500_cash_secured_puts(
    dividend_filter = "all",
    limit = NULL,  # No limit for "all"
    strike_threshold_pct = 0.85,
    min_days = 30,
    max_days = 45,
    max_workers = 1
  )

  # Should return tibble (may be empty if markets closed)
  expect_s3_class(result, "tbl_df")

  # If results exist, check result flags
  if (nrow(result) > 0) {
    expect_true(all(result$is_put))
    expect_false(any(result$is_aristocrat))
    expect_false(any(result$is_etf))
    expect_false(any(result$is_yfscreen))
    expect_true(all(result$is_sp500))
    expect_equal(unique(result$dividend_filter), "all")
  }
})

test_that("analyze_sp500_cash_secured_puts filters by 'dividend_paying' correctly", {
  skip_on_cran()

  result <- analyze_sp500_cash_secured_puts(
    dividend_filter = "dividend_paying",
    limit = 10,
    strike_threshold_pct = 0.85,
    min_days = 30,
    max_days = 45,
    max_workers = 1
  )

  expect_s3_class(result, "tbl_df")

  if (nrow(result) > 0) {
    expect_true(all(result$is_put))
    expect_false(any(result$is_aristocrat))
    expect_false(any(result$is_etf))
    expect_false(any(result$is_yfscreen))
    expect_true(all(result$is_sp500))
    expect_equal(unique(result$dividend_filter), "dividend_paying")
  }
})

test_that("analyze_sp500_cash_secured_puts filters by 'zero_dividend' correctly", {
  skip_on_cran()

  result <- analyze_sp500_cash_secured_puts(
    dividend_filter = "zero_dividend",
    limit = 10,
    strike_threshold_pct = 0.90,
    min_days = 30,
    max_days = 45,
    max_workers = 1
  )

  expect_s3_class(result, "tbl_df")

  if (nrow(result) > 0) {
    expect_true(all(result$is_put))
    expect_false(any(result$is_aristocrat))
    expect_false(any(result$is_etf))
    expect_false(any(result$is_yfscreen))
    expect_true(all(result$is_sp500))
    expect_equal(unique(result$dividend_filter), "zero_dividend")
  }
})

# Parameter Passing Tests ----

test_that("analyze_sp500_cash_secured_puts respects limit parameter", {
  skip_on_cran()

  # For "all" filter, limit should be ignored (NULL passed to universe function)
  result_all <- analyze_sp500_cash_secured_puts(
    dividend_filter = "all",
    limit = 10,  # Should be ignored
    max_workers = 1
  )
  expect_s3_class(result_all, "tbl_df")

  # For filtered lists, limit should be applied
  result_dividend <- analyze_sp500_cash_secured_puts(
    dividend_filter = "dividend_paying",
    limit = 5,
    max_workers = 1
  )
  expect_s3_class(result_dividend, "tbl_df")

  result_zero <- analyze_sp500_cash_secured_puts(
    dividend_filter = "zero_dividend",
    limit = 5,
    max_workers = 1
  )
  expect_s3_class(result_zero, "tbl_df")
})

test_that("analyze_sp500_cash_secured_puts passes strike_threshold_pct correctly", {
  skip_on_cran()

  result <- analyze_sp500_cash_secured_puts(
    dividend_filter = "all",
    limit = NULL,
    strike_threshold_pct = 0.80,  # 20% OTM
    min_days = 30,
    max_days = 45,
    max_workers = 1
  )

  expect_s3_class(result, "tbl_df")

  # If results exist, strikes should be near 80% of current price
  if (nrow(result) > 0) {
    expect_true(all(result$strike_pct_of_price >= 0.70 & result$strike_pct_of_price <= 0.90))
  }
})

test_that("analyze_sp500_cash_secured_puts passes days range correctly", {
  skip_on_cran()

  result <- analyze_sp500_cash_secured_puts(
    dividend_filter = "all",
    limit = NULL,
    min_days = 20,
    max_days = 60,
    max_workers = 1
  )

  expect_s3_class(result, "tbl_df")

  # If results exist, days should be within range
  if (nrow(result) > 0) {
    expect_true(all(result$days_to_expiry >= 20 & result$days_to_expiry <= 60))
  }
})

test_that("analyze_sp500_cash_secured_puts passes max_workers correctly", {
  skip_on_cran()

  # Test with single worker
  result_1 <- analyze_sp500_cash_secured_puts(
    dividend_filter = "all",
    limit = NULL,
    max_workers = 1
  )
  expect_s3_class(result_1, "tbl_df")

  # Test with multiple workers
  result_4 <- analyze_sp500_cash_secured_puts(
    dividend_filter = "all",
    limit = NULL,
    max_workers = 4
  )
  expect_s3_class(result_4, "tbl_df")

  # Results should be equivalent (order may differ)
  if (nrow(result_1) > 0 && nrow(result_4) > 0) {
    expect_equal(nrow(result_1), nrow(result_4))
  }
})

# Result Structure Tests ----

test_that("analyze_sp500_cash_secured_puts returns correct result structure", {
  skip_on_cran()

  result <- analyze_sp500_cash_secured_puts(
    dividend_filter = "dividend_paying",
    limit = 5,
    max_workers = 1
  )

  expect_s3_class(result, "tbl_df")

  # If results exist, check required columns
  if (nrow(result) > 0) {
    required_cols <- c(
      "ticker", "strike", "strike_pct_of_price", "expiration",
      "days_to_expiry", "bid", "premium_pct", "annualized_return_pct",
      "intrinsic_value", "time_value", "open_interest",
      "is_put", "is_aristocrat", "is_etf", "is_yfscreen", "is_sp500",
      "dividend_filter"
    )

    for (col in required_cols) {
      expect_true(col %in% names(result), info = paste("Missing column:", col))
    }
  }
})

test_that("analyze_sp500_cash_secured_puts sets result flags correctly", {
  skip_on_cran()

  filters <- c("all", "dividend_paying", "zero_dividend")

  for (filter_value in filters) {
    result <- analyze_sp500_cash_secured_puts(
      dividend_filter = filter_value,
      limit = 5,
      max_workers = 1
    )

    if (nrow(result) > 0) {
      # Check boolean flags
      expect_true(all(result$is_put), info = paste("Filter:", filter_value))
      expect_false(any(result$is_aristocrat), info = paste("Filter:", filter_value))
      expect_false(any(result$is_etf), info = paste("Filter:", filter_value))
      expect_false(any(result$is_yfscreen), info = paste("Filter:", filter_value))
      expect_true(all(result$is_sp500), info = paste("Filter:", filter_value))

      # Check dividend_filter value
      expect_equal(unique(result$dividend_filter), filter_value)
    }
  }
})

# Result Sorting Tests ----

test_that("analyze_sp500_cash_secured_puts sorts by annualized return descending", {
  skip_on_cran()

  result <- analyze_sp500_cash_secured_puts(
    dividend_filter = "dividend_paying",
    limit = 10,
    max_workers = 1
  )

  if (nrow(result) > 1) {
    # Check that annualized return is sorted in descending order
    returns <- result$annualized_return_pct
    expect_equal(returns, sort(returns, decreasing = TRUE))
  }
})

# Data Type Tests ----

test_that("analyze_sp500_cash_secured_puts returns correct data types", {
  skip_on_cran()

  result <- analyze_sp500_cash_secured_puts(
    dividend_filter = "all",
    limit = NULL,
    max_workers = 1
  )

  if (nrow(result) > 0) {
    expect_type(result$ticker, "character")
    expect_type(result$strike, "double")
    expect_type(result$strike_pct_of_price, "double")
    expect_s3_class(result$expiration, "Date")
    expect_type(result$days_to_expiry, "integer")
    expect_type(result$bid, "double")
    expect_type(result$premium_pct, "double")
    expect_type(result$annualized_return_pct, "double")
    expect_type(result$intrinsic_value, "double")
    expect_type(result$time_value, "double")
    expect_type(result$open_interest, "double")
    expect_type(result$is_put, "logical")
    expect_type(result$is_aristocrat, "logical")
    expect_type(result$is_etf, "logical")
    expect_type(result$is_yfscreen, "logical")
    expect_type(result$is_sp500, "logical")
    expect_type(result$dividend_filter, "character")
  }
})

# Edge Case Tests ----

test_that("analyze_sp500_cash_secured_puts handles empty universe", {
  # This is difficult to test directly, but we can test that
  # the function returns empty tibble when no opportunities found
  result <- analyze_sp500_cash_secured_puts(
    dividend_filter = "zero_dividend",
    limit = 1,
    strike_threshold_pct = 0.50,  # Very low threshold
    min_days = 1,
    max_days = 2,  # Very narrow range
    max_workers = 1
  )

  # Should return tibble (may be empty)
  expect_s3_class(result, "tbl_df")
})

test_that("analyze_sp500_cash_secured_puts uses config defaults", {
  skip_on_cran()

  # Call without optional parameters to test defaults
  result <- analyze_sp500_cash_secured_puts(
    dividend_filter = "all",
    limit = NULL,
    max_workers = 1
  )

  expect_s3_class(result, "tbl_df")

  # If results exist, verify they respect default config values
  if (nrow(result) > 0) {
    config <- CASH_SECURED_PUTS_CONFIG

    # Days should be within default range
    expect_true(all(result$days_to_expiry >= config$min_days))
    expect_true(all(result$days_to_expiry <= config$max_days))

    # Strike should be near default threshold
    threshold <- config$strike_threshold_pct
    expect_true(all(result$strike_pct_of_price >= threshold * 0.90))
    expect_true(all(result$strike_pct_of_price <= threshold * 1.10))
  }
})

# Integration Tests ----

test_that("analyze_sp500_cash_secured_puts integrates with stock universe functions", {
  # Test that the function calls the appropriate stock universe functions
  # based on dividend_filter parameter

  # All S&P 500 stocks
  sp500_all <- get_sp500_stocks()
  expect_true(length(sp500_all) > 0)
  expect_true(length(sp500_all) >= 400)  # Should be ~500 stocks

  # Dividend-paying stocks
  sp500_dividend <- get_dividend_paying_sp500(limit = 10, max_workers = 1)
  expect_true(length(sp500_dividend) > 0)
  expect_true(length(sp500_dividend) <= 10)

  # Zero-dividend stocks
  sp500_zero <- get_zero_dividend_stocks(limit = 10, max_workers = 1)
  expect_true(length(sp500_zero) > 0)
  expect_true(length(sp500_zero) <= 10)
})

test_that("analyze_sp500_cash_secured_puts produces valid put options", {
  skip_on_cran()

  result <- analyze_sp500_cash_secured_puts(
    dividend_filter = "dividend_paying",
    limit = 5,
    max_workers = 1
  )

  if (nrow(result) > 0) {
    # All should be puts (strike < current price for OTM)
    expect_true(all(result$is_put))

    # Strike should be less than current price (OTM puts)
    expect_true(all(result$strike_pct_of_price < 1.0))

    # Bid should be positive
    expect_true(all(result$bid > 0))

    # Premium percentage should be positive
    expect_true(all(result$premium_pct > 0))

    # Annualized return should be positive
    expect_true(all(result$annualized_return_pct > 0))

    # Time value should be positive (since these are OTM puts)
    expect_true(all(result$time_value > 0))

    # Intrinsic value should be zero (OTM puts have no intrinsic value)
    expect_true(all(result$intrinsic_value == 0))

    # Open interest should be non-negative
    expect_true(all(result$open_interest >= 0))
  }
})
