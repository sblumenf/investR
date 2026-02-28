test_that("fetch_iwb_holdings parses cached CSV and returns equity tickers", {
  tickers <- fetch_iwb_holdings()

  # Should return a non-empty character vector
  expect_type(tickers, "character")
  expect_gt(length(tickers), 500)

  # Should not contain NA or empty strings
  expect_false(any(is.na(tickers)))
  expect_false(any(!nzchar(tickers)))

  # Well-known Russell 1000 constituents should be present
  expect_true("AAPL" %in% tickers)
  expect_true("MSFT" %in% tickers)
  expect_true("NVDA" %in% tickers)
})

test_that("compute_iv_skew_ratio returns NULL for missing quote data", {
  local_mocked_bindings(
    fetch_current_quote = function(ticker, ...) NULL,
    .package = "investR"
  )
  result <- compute_iv_skew_ratio("FAKE")
  expect_null(result)
})

test_that("compute_iv_skew_ratio computes correct iv_ratio from mock data", {
  mock_quote <- data.frame(Last = "150.0", Name = "Apple Inc", stringsAsFactors = FALSE)

  today <- Sys.Date()
  # Pick an expiry 50 days from today (within 45-60 day window)
  target_expiry_key <- format(today + 50, "%b.%d.%Y")

  mock_chain <- list()
  mock_chain[[target_expiry_key]] <- list(
    calls = data.frame(
      Strike = c(145, 150, 155),
      IV = c(0.30, 0.32, 0.28),
      Bid = c(6.5, 4.2, 2.8),
      Ask = c(6.7, 4.4, 3.0),
      Vol = c(100L, 200L, 80L),
      OI = c(500L, 1000L, 400L),
      stringsAsFactors = FALSE
    ),
    puts = data.frame(
      Strike = c(145, 150, 155),
      IV = c(0.22, 0.20, 0.25),
      Bid = c(1.0, 2.5, 4.0),
      Ask = c(1.2, 2.7, 4.2),
      Vol = c(50L, 150L, 60L),
      OI = c(300L, 800L, 250L),
      stringsAsFactors = FALSE
    )
  )

  local_mocked_bindings(
    fetch_current_quote = function(ticker, ...) mock_quote,
    fetch_options_chain = function(ticker, ...) mock_chain,
    .package = "investR"
  )

  result <- compute_iv_skew_ratio("AAPL")

  expect_type(result, "list")
  expect_equal(result$ticker, "AAPL")
  expect_equal(result$call_iv, 0.32)
  expect_equal(result$put_iv, 0.20)
  expect_equal(result$iv_ratio, 0.32 / 0.20)
  expect_equal(result$current_price, 150.0)
  # expiry_date is now normalized to ISO format
  expect_equal(result$expiry_date, as.character(today + 50))
})

test_that("compute_iv_skew_ratio skips tickers with no expiry in window", {
  mock_quote <- data.frame(Last = "150.0", Name = "Apple Inc", stringsAsFactors = FALSE)

  today <- Sys.Date()
  # Expiry outside the 45-60 day window (e.g. 30 days away)
  outside_expiry_key <- format(today + 30, "%b.%d.%Y")

  mock_chain <- list()
  mock_chain[[outside_expiry_key]] <- list(
    calls = data.frame(Strike = 150, IV = 0.30, stringsAsFactors = FALSE),
    puts  = data.frame(Strike = 150, IV = 0.20, stringsAsFactors = FALSE)
  )

  local_mocked_bindings(
    fetch_current_quote = function(ticker, ...) mock_quote,
    fetch_options_chain = function(ticker, ...) mock_chain,
    .package = "investR"
  )

  result <- compute_iv_skew_ratio("AAPL")
  expect_null(result)
})

test_that("compute_iv_skew_ratio returns NULL when put IV is zero", {
  mock_quote <- data.frame(Last = "150.0", Name = "Apple Inc", stringsAsFactors = FALSE)

  today <- Sys.Date()
  target_expiry_key <- format(today + 50, "%b.%d.%Y")

  mock_chain <- list()
  mock_chain[[target_expiry_key]] <- list(
    calls = data.frame(Strike = 150, IV = 0.30, stringsAsFactors = FALSE),
    puts  = data.frame(Strike = 150, IV = 0.0,  stringsAsFactors = FALSE)
  )

  local_mocked_bindings(
    fetch_current_quote = function(ticker, ...) mock_quote,
    fetch_options_chain = function(ticker, ...) mock_chain,
    .package = "investR"
  )

  result <- compute_iv_skew_ratio("AAPL")
  expect_null(result)
})
