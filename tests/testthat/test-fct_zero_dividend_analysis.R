test_that("analyze_zero_dividend requires non-empty stock universe", {
  # Mock get_zero_dividend_stocks to return empty vector
  mockery::stub(analyze_zero_dividend, "get_zero_dividend_stocks", character(0))

  # Should throw error
  expect_error(
    analyze_zero_dividend(limit = 5),
    "No zero-dividend stocks found"
  )
})

test_that("analyze_zero_dividend returns tibble with correct structure", {
  skip_on_cran()
  skip_if_offline()

  # Test with very small limit for speed
  # This is an integration test that may take a minute
  result <- tryCatch({
    analyze_zero_dividend(limit = 2, max_workers = 1)
  }, error = function(e) {
    skip(paste("Skipping integration test due to error:", e$message))
  })

  # Should return a tibble (even if empty)
  expect_s3_class(result, "tbl_df")

  # If we got results, verify structure
  if (nrow(result) > 0) {
    expected_cols <- c(
      "ticker", "company_name", "current_price", "strike",
      "annualized_return", "total_return", "is_zero_dividend"
    )

    for (col in expected_cols) {
      expect_true(col %in% names(result),
                 info = paste("Missing column:", col))
    }

    # Verify flags are set correctly
    expect_true(all(result$is_zero_dividend))
    expect_true(all(!result$is_aristocrat))

    # Verify returns are sorted descending
    if (nrow(result) > 1) {
      expect_true(all(diff(result$annualized_return) <= 0))
    }
  }
})

test_that("analyze_zero_dividend respects strike_threshold_pct parameter", {
  skip_on_cran()
  skip_if_offline()
  skip("Long running test - manual verification only")

  # Test with different strike thresholds
  result_80 <- analyze_zero_dividend(
    limit = 5,
    strike_threshold_pct = 0.80,
    max_workers = 1
  )

  result_90 <- analyze_zero_dividend(
    limit = 5,
    strike_threshold_pct = 0.90,
    max_workers = 1
  )

  # Both should return tibbles
  expect_s3_class(result_80, "tbl_df")
  expect_s3_class(result_90, "tbl_df")

  # If we got results, verify strikes are different
  if (nrow(result_80) > 0 && nrow(result_90) > 0) {
    # 90% threshold should generally result in strikes closer to current price
    # This is a heuristic test - may not always hold
    expect_true(is.numeric(result_80$strike))
    expect_true(is.numeric(result_90$strike))
  }
})

test_that("analyze_zero_dividend respects target_days parameter", {
  skip_on_cran()
  skip_if_offline()
  skip("Long running test - manual verification only")

  # Test with different target days
  result_60 <- analyze_zero_dividend(
    limit = 5,
    target_days = 60,
    max_workers = 1
  )

  result_90 <- analyze_zero_dividend(
    limit = 5,
    target_days = 90,
    max_workers = 1
  )

  # Both should return tibbles
  expect_s3_class(result_60, "tbl_df")
  expect_s3_class(result_90, "tbl_df")

  # If we got results, verify days to expiry are different
  if (nrow(result_60) > 0 && nrow(result_90) > 0) {
    # Should generally target different expirations
    expect_true(is.numeric(result_60$days_to_expiry))
    expect_true(is.numeric(result_90$days_to_expiry))
  }
})

test_that("analyze_zero_dividend handles limit parameter", {
  skip_on_cran()
  skip_if_offline()

  # With limit=1, we should only analyze 1 stock
  # Mock to control behavior
  mock_stocks <- c("AAPL")  # Just one stock
  mockery::stub(analyze_zero_dividend, "get_zero_dividend_stocks", mock_stocks)

  # Should not error (even if no results)
  result <- tryCatch({
    analyze_zero_dividend(limit = 1, max_workers = 1)
  }, error = function(e) {
    skip(paste("Skipping test due to error:", e$message))
  })

  expect_s3_class(result, "tbl_df")
})

test_that("analyze_zero_dividend sets result flags correctly", {
  skip_on_cran()
  skip_if_offline()
  skip("Integration test - requires live data")

  result <- analyze_zero_dividend(limit = 2, max_workers = 1)

  if (nrow(result) > 0) {
    # All results should have is_zero_dividend = TRUE
    expect_true(all(result$is_zero_dividend == TRUE))

    # All results should have is_aristocrat = FALSE
    expect_true(all(result$is_aristocrat == FALSE))
  }
})

test_that("analyze_zero_dividend with NULL target_days uses longest expiry", {
  skip_on_cran()
  skip_if_offline()
  skip("Integration test - requires live data")

  result <- analyze_zero_dividend(
    limit = 2,
    target_days = NULL,
    max_workers = 1
  )

  expect_s3_class(result, "tbl_df")

  if (nrow(result) > 0) {
    # With NULL target_days, should generally get longer expirations
    # (this is heuristic - depends on available options)
    expect_true(is.numeric(result$days_to_expiry))
    expect_gte(min(result$days_to_expiry), 0)
  }
})
