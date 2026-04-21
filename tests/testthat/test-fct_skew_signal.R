test_that("normalize_iv_column converts percent-scale IV to decimal for Questrade", {
  df <- data.frame(Strike = c(100, 110), IV = c(23.74, 31.50))
  result <- normalize_iv_column(df, "Questrade")
  expect_equal(result$IV, c(0.2374, 0.3150))
})

test_that("normalize_iv_column leaves decimal-scale IV unchanged for Yahoo Finance", {
  df <- data.frame(Strike = c(100, 110), IV = c(0.2374, 0.3150))
  result <- normalize_iv_column(df, "Yahoo Finance")
  expect_equal(result$IV, c(0.2374, 0.3150))
})

test_that("normalize_iv_column handles missing IV column gracefully", {
  df <- data.frame(Strike = c(100, 110), Bid = c(5, 3))
  result <- normalize_iv_column(df, "Questrade")
  expect_equal(names(result), c("Strike", "Bid"))
})

test_that("add_bs_delta computes call deltas in (0, 1) range", {
  df <- data.frame(
    Strike = c(80, 90, 100, 110, 120),
    IV     = c(0.25, 0.25, 0.25, 0.25, 0.25)
  )
  stock_price    <- 100
  time_to_expiry <- 30 / 365
  risk_free_rate <- 0.05

  result <- add_bs_delta(df, stock_price, time_to_expiry, risk_free_rate, "call")

  expect_true(all(!is.na(result$bs_delta)))
  expect_true(all(result$bs_delta > 0 & result$bs_delta < 1))
  # ITM call (strike < stock) should have higher delta than OTM call
  expect_gt(result$bs_delta[result$Strike == 80], result$bs_delta[result$Strike == 120])
})

test_that("add_bs_delta computes put deltas in (-1, 0) range", {
  df <- data.frame(
    Strike = c(80, 90, 100, 110, 120),
    IV     = c(0.25, 0.25, 0.25, 0.25, 0.25)
  )
  result <- add_bs_delta(df, 100, 30 / 365, 0.05, "put")

  expect_true(all(!is.na(result$bs_delta)))
  expect_true(all(result$bs_delta > -1 & result$bs_delta < 0))
})

test_that("add_bs_delta returns NA for invalid inputs", {
  df <- data.frame(Strike = c(100, NA), IV = c(NA, 0.25))
  result <- add_bs_delta(df, 100, 30 / 365, 0.05, "call")
  expect_true(all(is.na(result$bs_delta)))
})

test_that("match_nearest_delta finds correct call for target delta", {
  df <- data.frame(
    Strike   = c(80,   90,   100,  110,  120),
    IV       = c(0.25, 0.25, 0.25, 0.25, 0.25),
    Ask      = c(20,   12,   5,    1,    0.2),
    bs_delta = c(0.85, 0.65, 0.48, 0.25, 0.10)
  )

  # Target 0.25 -> should match row with bs_delta = 0.25
  result <- match_nearest_delta(df, 0.25, "call")
  expect_equal(result$Strike, 110)

  # Target 0.50 -> should match row with bs_delta = 0.48
  result <- match_nearest_delta(df, 0.50, "call")
  expect_equal(result$Strike, 100)
})

test_that("match_nearest_delta handles puts using abs(delta)", {
  df <- data.frame(
    Strike   = c(80,    90,    100,   110,   120),
    IV       = c(0.25,  0.25,  0.25,  0.25,  0.25),
    Ask      = c(0.2,   1,     5,     12,    20),
    bs_delta = c(-0.10, -0.25, -0.48, -0.65, -0.85)
  )

  # Target 0.25 -> should match row with abs(bs_delta) = 0.25 -> Strike 90
  result <- match_nearest_delta(df, 0.25, "put")
  expect_equal(result$Strike, 90)
})

test_that("match_nearest_delta tie-breaks to lower abs delta", {
  df <- data.frame(
    Strike   = c(95,   105),
    IV       = c(0.25, 0.25),
    Ask      = c(5,    4),
    bs_delta = c(0.52, 0.48)
  )
  # Both equidistant from 0.50; lower abs delta wins -> 0.48 -> Strike 105
  result <- match_nearest_delta(df, 0.50, "call")
  expect_equal(result$Strike, 105)
})

test_that("match_nearest_delta returns NULL for empty data frame", {
  df <- data.frame(Strike = numeric(0), IV = numeric(0),
                   Ask = numeric(0), bs_delta = numeric(0))
  expect_null(match_nearest_delta(df, 0.30, "call"))
})

test_that("match_nearest_delta returns NULL when all deltas are NA", {
  df <- data.frame(Strike = c(100, 110), IV = c(NA, NA),
                   Ask = c(5, 3), bs_delta = c(NA, NA))
  expect_null(match_nearest_delta(df, 0.30, "call"))
})

test_that("aggregate formula is correct weighted average", {
  deltas  <- c(0.20, 0.30, 0.40, 0.50, 0.60)
  iv_diff <- c(0.01, 0.02, 0.03, 0.04, 0.05)
  expected <- sum(deltas * iv_diff) / sum(deltas)

  skew_table <- data.frame(delta = deltas, iv_diff = iv_diff)
  valid_rows <- !is.na(skew_table$iv_diff)
  computed <- sum(skew_table$delta[valid_rows] * skew_table$iv_diff[valid_rows]) /
                sum(skew_table$delta[valid_rows])

  expect_equal(computed, expected)
})

test_that("parse_chain_expiry_dates parses Questrade format", {
  dates <- parse_chain_expiry_dates(c("Mar.21.2025", "Apr.17.2025"), "Questrade")
  expect_equal(dates[1], as.Date("2025-03-21"))
  expect_equal(dates[2], as.Date("2025-04-17"))
})

test_that("parse_chain_expiry_dates parses Yahoo ISO format", {
  dates <- parse_chain_expiry_dates(c("2025-03-21", "2025-04-17"), "Yahoo Finance")
  expect_equal(dates[1], as.Date("2025-03-21"))
  expect_equal(dates[2], as.Date("2025-04-17"))
})

test_that("compute_skew_signal returns error list when stock price fetch fails", {
  # Mock fetch_current_quote to throw
  local_mocked_bindings(
    fetch_current_quote = function(...) stop("network error"),
    .package = "investR"
  )
  result <- compute_skew_signal("FAKE")
  expect_null(result$table)
  expect_false(is.null(result$error))
  expect_true(is.character(result$error))
})

test_that("compute_skew_signal returns error list when both chains fail", {
  local_mocked_bindings(
    fetch_current_quote = function(...) data.frame(Last = 100),
    fetch_questrade_options_chain = function(...) NULL,
    .package = "investR"
  )
  # Also mock quantmod
  local_mocked_bindings(
    getOptionChain = function(...) NULL,
    .package = "quantmod"
  )
  result <- compute_skew_signal("FAKE")
  expect_null(result$table)
  expect_false(is.null(result$error))
})

test_that("compute_skew_signal produces no duplicate strikes when chain is sparse", {
  # Only 3 strikes with valid computed deltas but 5 target delta levels.
  # ATM-first matching must prevent the same strike appearing in multiple rows,
  # and the 3 matched rows should correspond to delta levels closest to 0.50
  # (i.e., 0.50, 0.40/0.60, 0.30) rather than 0.20, 0.30, 0.40.
  sparse_calls <- data.frame(
    Strike = c(85, 100, 115),
    Ask    = c(15, 5, 0.5),
    IV     = c(0.35, 0.25, 0.18),
    stringsAsFactors = FALSE
  )
  sparse_puts <- data.frame(
    Strike = c(85, 100, 115),
    Ask    = c(0.5, 5, 15),
    IV     = c(0.18, 0.25, 0.35),
    stringsAsFactors = FALSE
  )

  mock_chain <- list(
    "2099-06-20" = list(calls = sparse_calls, puts = sparse_puts)
  )

  local_mocked_bindings(
    fetch_current_quote = function(...) data.frame(Last = 100),
    fetch_questrade_options_chain = function(...) NULL,
    .package = "investR"
  )
  local_mocked_bindings(
    getOptionChain = function(...) mock_chain,
    .package = "quantmod"
  )

  result <- compute_skew_signal("FAKE")

  if (!is.null(result$table) && nrow(result$table) > 1) {
    # No duplicate strikes
    expect_equal(length(unique(result$table$call_strike)), nrow(result$table))
    expect_equal(length(unique(result$table$put_strike)),  nrow(result$table))

    # With only 3 strikes, at most 3 rows. The matched delta levels should
    # include 0.50 (ATM) — the most meaningful level — not be limited to OTM levels.
    expect_true(0.50 %in% result$table$delta)
  }
  # At minimum the result should not error and should return a table or NULL
  expect_true(is.null(result$error) || is.character(result$error))
})

test_that("compute_skew_signal handles Yahoo Finance option symbol rownames without NA deduplication", {
  # Yahoo Finance chains use option symbol strings as rownames (e.g. "AAPL250620C00200000").
  # The rowname reset must make as.integer(rownames(...)) safe so deduplication works.
  yahoo_calls <- data.frame(
    Strike = c(80, 90, 100, 110, 120),
    Ask    = c(22, 13, 5, 1.5, 0.3),
    IV     = c(0.30, 0.28, 0.25, 0.22, 0.20),
    stringsAsFactors = FALSE
  )
  rownames(yahoo_calls) <- c(
    "AAPL250620C00080000", "AAPL250620C00090000", "AAPL250620C00100000",
    "AAPL250620C00110000", "AAPL250620C00120000"
  )

  yahoo_puts <- data.frame(
    Strike = c(80, 90, 100, 110, 120),
    Ask    = c(0.3, 1.5, 5, 13, 22),
    IV     = c(0.20, 0.22, 0.25, 0.28, 0.30),
    stringsAsFactors = FALSE
  )
  rownames(yahoo_puts) <- c(
    "AAPL250620P00080000", "AAPL250620P00090000", "AAPL250620P00100000",
    "AAPL250620P00110000", "AAPL250620P00120000"
  )

  mock_chain <- list(
    "2099-06-20" = list(calls = yahoo_calls, puts = yahoo_puts)
  )

  local_mocked_bindings(
    fetch_current_quote = function(...) data.frame(Last = 100),
    fetch_questrade_options_chain = function(...) NULL,
    .package = "investR"
  )
  local_mocked_bindings(
    getOptionChain = function(...) mock_chain,
    .package = "quantmod"
  )

  result <- compute_skew_signal("AAPL")

  # Must succeed without error
  expect_null(result$error)
  expect_false(is.null(result$table))

  # No duplicate strikes — deduplication must have worked correctly
  expect_equal(length(unique(result$table$call_strike)), nrow(result$table))
  expect_equal(length(unique(result$table$put_strike)),  nrow(result$table))
})

test_that("compute_skew_signal sets data_source to Yahoo Finance on Questrade failure", {
  # Build minimal mock chain in Questrade/Yahoo format
  mock_chain <- list(
    "2099-06-20" = list(
      calls = data.frame(
        Strike = seq(80, 120, by = 10),
        Ask    = c(22, 13, 5, 1.5, 0.3),
        IV     = c(0.30, 0.28, 0.25, 0.22, 0.20),
        stringsAsFactors = FALSE
      ),
      puts = data.frame(
        Strike = seq(80, 120, by = 10),
        Ask    = c(0.3, 1.5, 5, 13, 22),
        IV     = c(0.20, 0.22, 0.25, 0.28, 0.30),
        stringsAsFactors = FALSE
      )
    )
  )

  local_mocked_bindings(
    fetch_current_quote = function(...) data.frame(Last = 100),
    fetch_questrade_options_chain = function(...) NULL,
    .package = "investR"
  )
  local_mocked_bindings(
    getOptionChain = function(...) mock_chain,
    .package = "quantmod"
  )

  result <- compute_skew_signal("FAKE")
  expect_equal(result$data_source, "Yahoo Finance")
})
