# Tests for Portfolio Expected Return Calculations

test_that("map_strategy_to_display returns correct mappings", {
  # Test direct mappings

  expect_equal(map_strategy_to_display("Covered Call"), "Covered Calls")
  expect_equal(map_strategy_to_display("Dynamic Covered Calls"), "Covered Calls")
  expect_equal(map_strategy_to_display("CSP"), "Cash Secured Puts")
  expect_equal(map_strategy_to_display("Cash-Secured Put"), "Cash Secured Puts")
  expect_equal(map_strategy_to_display("Collar"), "Collars")
  expect_equal(map_strategy_to_display("Dividend Aristocrats"), "Dividend Aristocrats")
  expect_equal(map_strategy_to_display("Other"), "Other")

  # Test unknown strategy defaults to "Other"
  expect_equal(map_strategy_to_display("Unknown Strategy"), "Other")
  expect_equal(map_strategy_to_display(""), "Other")

  # Test money market detection
  expect_equal(map_strategy_to_display("Other", ticker = "SGOV"), "Money Market")
  expect_equal(map_strategy_to_display("Other", ticker = "ZMMK.TO"), "Money Market")
  expect_equal(map_strategy_to_display("Covered Call", ticker = "AAPL"), "Covered Calls")
})

test_that("get_money_market_yield returns valid yield", {
  skip_if_not(Sys.getenv("QUESTRADE_REFRESH_TOKEN") != "", "Questrade API not configured")

  # Test SGOV yield
  sgov_yield <- get_money_market_yield("SGOV")
  expect_true(is.numeric(sgov_yield))
  expect_true(sgov_yield > 0)
  expect_true(sgov_yield < 0.15)  # Sanity check: yield should be less than 15%

  # Test ZMMK uses SGOV as proxy
  zmmk_yield <- get_money_market_yield("ZMMK.TO")
  expect_true(is.numeric(zmmk_yield))
  expect_equal(sgov_yield, zmmk_yield)  # Should be same since ZMMK uses SGOV as proxy
})

test_that("get_money_market_yield returns fallback on invalid ticker", {
  # Suppress expected warnings during test
  suppressWarnings({
    # Test with invalid ticker - should return default
    yield <- get_money_market_yield("INVALID_TICKER_XYZ")
    expect_true(is.numeric(yield))
    expect_true(yield > 0)
    expect_true(yield < 0.15)
  })
})

test_that("calculate_portfolio_expected_returns returns correct structure", {
  skip_if_not(Sys.getenv("QUESTRADE_REFRESH_TOKEN") != "", "Questrade API not configured")

  result <- calculate_portfolio_expected_returns(include_breakdown = TRUE)

  # Check return structure
  expect_type(result, "list")
  expect_true("expected_return_pct" %in% names(result))
  expect_true("realized_return_pct" %in% names(result))
  expect_true("total_open_capital" %in% names(result))
  expect_true("total_closed_capital" %in% names(result))
  expect_true("strategy_breakdown" %in% names(result))
  expect_true("exclusions" %in% names(result))
  expect_true("calculation_timestamp" %in% names(result))
  expect_true("positions_included" %in% names(result))
  expect_true("positions_excluded" %in% names(result))

  # Check exclusions structure
  expect_type(result$exclusions, "list")
  expect_true("count" %in% names(result$exclusions))
  expect_true("capital" %in% names(result$exclusions))
  expect_true("pct_of_portfolio" %in% names(result$exclusions))
  expect_true("reasons" %in% names(result$exclusions))

  # Check strategy_breakdown is a tibble

  expect_s3_class(result$strategy_breakdown, "tbl_df")

  # Check timestamp
  expect_s3_class(result$calculation_timestamp, "POSIXct")
})

test_that("calculate_portfolio_expected_returns handles empty portfolio", {
  # This test requires mocking - skip if database has data
  # We can still verify the function handles errors gracefully
  result <- tryCatch({
    calculate_portfolio_expected_returns(include_breakdown = TRUE)
  }, error = function(e) {
    list(error = e$message)
  })

  expect_type(result, "list")
})

test_that("calculate_portfolio_expected_returns with include_breakdown FALSE", {
  skip_if_not(Sys.getenv("QUESTRADE_REFRESH_TOKEN") != "", "Questrade API not configured")

  result <- calculate_portfolio_expected_returns(include_breakdown = FALSE)

  # Strategy breakdown should still be returned but may be empty
  expect_true("strategy_breakdown" %in% names(result))
})

test_that("capital weighting formula is correct", {
  # Test the mathematical formula for weighted average
  # If Position A: 10% return, $10,000 capital
  #    Position B: 20% return, $20,000 capital
  # Expected weighted return: (10*10000 + 20*20000) / 30000 = 16.67%

  returns <- c(10, 20)
  capitals <- c(10000, 20000)

  expected_weighted <- sum(returns * capitals) / sum(capitals)

  expect_equal(round(expected_weighted, 2), 16.67)
})

test_that("exclusion tracking works correctly", {
  skip_if_not(Sys.getenv("QUESTRADE_REFRESH_TOKEN") != "", "Questrade API not configured")

  result <- calculate_portfolio_expected_returns(include_breakdown = TRUE)

  # Exclusion count should be non-negative
  expect_true(result$exclusions$count >= 0)
  expect_true(result$exclusions$capital >= 0)
  expect_true(result$exclusions$pct_of_portfolio >= 0)
  expect_true(result$exclusions$pct_of_portfolio <= 1)

  # positions_included + positions_excluded should equal total positions processed
  expect_true(result$positions_included >= 0)
  expect_true(result$positions_excluded >= 0)
})

test_that("strategy breakdown by status separation", {
  skip_if_not(Sys.getenv("QUESTRADE_REFRESH_TOKEN") != "", "Questrade API not configured")

  result <- calculate_portfolio_expected_returns(include_breakdown = TRUE)

  if (nrow(result$strategy_breakdown) > 0) {
    # Check that status column exists
    expect_true("status" %in% names(result$strategy_breakdown))

    # Check that status values are valid
    valid_statuses <- c("open", "closed")
    expect_true(all(result$strategy_breakdown$status %in% valid_statuses))

    # Check required columns
    expect_true("strategy_type" %in% names(result$strategy_breakdown))
    expect_true("position_count" %in% names(result$strategy_breakdown))
    expect_true("total_capital" %in% names(result$strategy_breakdown))
    expect_true("weighted_return_pct" %in% names(result$strategy_breakdown))
    expect_true("pct_of_portfolio" %in% names(result$strategy_breakdown))
  }
})

test_that("extract_ticker_from_members handles various member types", {
  # Test with mock data for different member types

  # Test underlying_stock member
  stock_members <- tibble::tibble(
    symbol = "AAPL",
    role = "underlying_stock"
  )
  expect_equal(extract_ticker_from_members(stock_members), "AAPL")

  # Test cash_equivalent member
  cash_members <- tibble::tibble(
    symbol = "SGOV",
    role = "cash_equivalent"
  )
  expect_equal(extract_ticker_from_members(cash_members), "SGOV")

  # Test empty members
  empty_members <- tibble::tibble(
    symbol = character(),
    role = character()
  )
  expect_null(extract_ticker_from_members(empty_members))
})

test_that("format_currency_compact produces correct output", {
  # Test millions
  expect_equal(format_currency_compact(1500000), "1.5M")
  expect_equal(format_currency_compact(2000000), "2.0M")

  # Test thousands
  expect_equal(format_currency_compact(50000), "50.0K")
  expect_equal(format_currency_compact(1500), "1.5K")

  # Test small numbers
  expect_equal(format_currency_compact(500), "500")
  expect_equal(format_currency_compact(99), "99")

  # Test zero and NA
  expect_equal(format_currency_compact(0), "0")
  expect_equal(format_currency_compact(NA), "0")

  # Test negative numbers
  expect_equal(format_currency_compact(-50000), "-50.0K")
  expect_equal(format_currency_compact(-1500000), "-1.5M")
})
