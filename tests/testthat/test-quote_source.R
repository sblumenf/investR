test_that("get_quote_source returns default questrade when not set", {
  # Clear any existing option
  options(investR.quote_source = NULL)

  result <- get_quote_source()

  expect_equal(result, "questrade")
})

test_that("get_quote_source returns configured source", {
  options(investR.quote_source = "questrade")

  result <- get_quote_source()

  expect_equal(result, "questrade")

  # Cleanup
  options(investR.quote_source = NULL)
})

test_that("fetch_current_quote routes to Questrade by default", {
  # Clear any existing option
  options(investR.quote_source = NULL)

  # Mock the Questrade-specific function
  local_mocked_bindings(
    fetch_questrade_quote = function(ticker, fields) {
      data.frame(Last = 150.25, Name = "Test Stock", row.names = ticker)
    }
  )

  result <- fetch_current_quote("TEST")

  expect_s3_class(result, "data.frame")
  expect_equal(result$Last, 150.25)
  expect_equal(result$Name, "Test Stock")
})

test_that("fetch_current_quote routes to Yahoo when configured", {
  options(investR.quote_source = "yahoo")

  # Mock the Yahoo-specific function
  local_mocked_bindings(
    fetch_current_quote_yahoo = function(ticker, fields) {
      data.frame(Last = 175.50, Name = "Yahoo Stock", row.names = ticker)
    }
  )

  result <- fetch_current_quote("TEST")

  expect_s3_class(result, "data.frame")
  expect_equal(result$Last, 175.50)
  expect_equal(result$Name, "Yahoo Stock")

  # Cleanup
  options(investR.quote_source = NULL)
})

test_that("quote source toggle UI generates radio buttons", {
  ns <- NS("test")
  ui <- quote_source_toggle_ui(ns)

  # Check that it returns a Shiny input element
  expect_s3_class(ui, "shiny.tag")

  # Convert to HTML and check for key components
  html <- as.character(ui)
  expect_match(html, "quote_source")
  expect_match(html, "Yahoo Finance")
  expect_match(html, "Questrade API")
})

test_that("convert_quote_to_yahoo_format creates correct structure", {
  # Create sample Questrade quote data
  questrade_quote <- list(
    symbol = "AAPL",
    lastTradePrice = 150.25,
    description = "Apple Inc"
  )

  # Use the internal function
  result <- investR:::convert_quote_to_yahoo_format(questrade_quote, "AAPL")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$Last, 150.25)
  expect_equal(result$Name, "Apple Inc")
  expect_equal(rownames(result), "AAPL")
})

test_that("convert_quote_to_yahoo_format handles missing data", {
  # Create sample Questrade quote with missing description
  questrade_quote <- list(
    symbol = "TEST",
    lastTradePrice = 100.00
  )

  result <- investR:::convert_quote_to_yahoo_format(questrade_quote, "TEST")

  expect_s3_class(result, "data.frame")
  expect_equal(result$Last, 100.00)
  expect_equal(result$Name, "TEST")  # Falls back to ticker
})

test_that("fetch_questrade_quote falls back to Yahoo on auth failure", {
  # Mock get_questrade_auth to return NULL (auth failure)
  local_mocked_bindings(
    get_questrade_auth = function() NULL,
    fetch_current_quote_yahoo = function(ticker, fields) {
      data.frame(Last = 150.25, Name = "Yahoo Fallback", row.names = ticker)
    }
  )

  result <- fetch_questrade_quote("TEST")

  expect_s3_class(result, "data.frame")
  expect_equal(result$Name, "Yahoo Fallback")
})

test_that("fetch_questrade_quote falls back to Yahoo on symbol search failure", {
  # Mock get_questrade_auth to succeed but symbol search to fail
  local_mocked_bindings(
    get_questrade_auth = function() list(access_token = "test", api_server = "http://test/"),
    search_questrade_symbol = function(ticker, auth) NULL,
    fetch_current_quote_yahoo = function(ticker, fields) {
      data.frame(Last = 150.25, Name = "Yahoo Fallback", row.names = ticker)
    }
  )

  result <- fetch_questrade_quote("TEST")

  expect_s3_class(result, "data.frame")
  expect_equal(result$Name, "Yahoo Fallback")
})

test_that("null coalescing operator works correctly", {
  `%||%` <- investR:::`%||%`

  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% "default", 0)
  expect_equal(FALSE %||% "default", FALSE)
})

test_that("reset_fallback_tracker clears fallback data", {
  # Ensure clean state
  reset_fallback_tracker()

  # Record some fallbacks first
  investR:::record_fallback("TEST1", "Auth failed")
  investR:::record_fallback("TEST2", "Symbol not found")

  # Verify they were recorded
  summary_before <- get_fallback_summary()
  expect_equal(summary_before$count, 2)

  # Reset
  reset_fallback_tracker()

  # Verify they're cleared
  summary_after <- get_fallback_summary()
  expect_equal(summary_after$count, 0)
  expect_equal(summary_after$tickers, character(0))
  expect_null(summary_after$message)
})

test_that("record_fallback tracks ticker and reason", {
  reset_fallback_tracker()

  investR:::record_fallback("AAPL", "Authentication failed")
  investR:::record_fallback("MSFT", "Symbol not found")

  summary <- get_fallback_summary()

  expect_equal(summary$count, 2)
  expect_true("AAPL" %in% summary$tickers)
  expect_true("MSFT" %in% summary$tickers)
  expect_match(summary$message, "AAPL")
  expect_match(summary$message, "MSFT")
})

test_that("get_fallback_summary handles duplicate tickers", {
  reset_fallback_tracker()

  # Record same ticker multiple times
  investR:::record_fallback("AAPL", "Auth failed")
  investR:::record_fallback("AAPL", "Quote failed")
  investR:::record_fallback("MSFT", "Symbol not found")

  summary <- get_fallback_summary()

  # Should only count unique tickers
  expect_equal(summary$count, 2)
  expect_equal(length(summary$tickers), 2)
})

test_that("get_fallback_summary returns null message when no fallbacks", {
  reset_fallback_tracker()

  summary <- get_fallback_summary()

  expect_equal(summary$count, 0)
  expect_null(summary$message)
})

test_that("get_fallback_summary formats message correctly for few tickers", {
  reset_fallback_tracker()

  investR:::record_fallback("AAPL", "Test")
  investR:::record_fallback("MSFT", "Test")
  investR:::record_fallback("GOOGL", "Test")

  summary <- get_fallback_summary()

  expect_match(summary$message, "3 tickers")
  expect_match(summary$message, "AAPL")
  expect_match(summary$message, "MSFT")
  expect_match(summary$message, "GOOGL")
})

test_that("get_fallback_summary formats message correctly for many tickers", {
  reset_fallback_tracker()

  # Record 10 different tickers
  for (i in 1:10) {
    investR:::record_fallback(paste0("TICK", i), "Test")
  }

  summary <- get_fallback_summary()

  expect_equal(summary$count, 10)
  # Message should show first 5 and mention "and X more"
  expect_match(summary$message, "and 5 more")
})

test_that("check_and_notify_fallbacks does nothing when Yahoo is selected", {
  reset_fallback_tracker()
  options(investR.quote_source = "yahoo")

  # Record a fallback
  investR:::record_fallback("TEST", "Test")

  # Should not error, should return invisibly
  result <- check_and_notify_fallbacks()

  expect_null(result)

  # Cleanup
  options(investR.quote_source = NULL)
})

test_that("check_and_notify_fallbacks does nothing when no fallbacks occurred", {
  reset_fallback_tracker()
  options(investR.quote_source = "questrade")

  # No fallbacks recorded
  result <- check_and_notify_fallbacks()

  expect_null(result)

  # Cleanup
  options(investR.quote_source = NULL)
})
