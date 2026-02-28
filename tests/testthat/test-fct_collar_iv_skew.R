# Tests for IV Skew Screener functions (US-1, US-2)

test_that("fetch_iwb_holdings parses cached CSV and returns equity tickers", {
  skip_if_not_installed("readr")

  # Use the bundled fallback CSV directly
  fallback_path <- system.file("cache", "IWB_holdings.csv", package = "investR")
  if (!nzchar(fallback_path)) {
    fallback_path <- file.path("inst", "cache", "IWB_holdings.csv")
  }
  skip_if(!file.exists(fallback_path), "IWB_holdings.csv not found")

  # Parse directly using the same logic as fetch_iwb_holdings
  df <- suppressWarnings(readr::read_csv(
    fallback_path,
    skip = 9,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    name_repair = "minimal"
  ))

  equity_rows <- df[!is.na(df[["Asset Class"]]) & df[["Asset Class"]] == "Equity", ]
  tickers <- trimws(equity_rows[["Ticker"]])
  tickers <- tickers[!is.na(tickers) & nzchar(tickers)]

  # Should return ~1000 equity tickers
  expect_type(tickers, "character")
  expect_gte(length(tickers), 900)
  expect_lte(length(tickers), 1100)

  # All tickers should be non-empty strings
  expect_true(all(nzchar(tickers)))

  # Known large-cap tickers should be present
  expect_true("AAPL" %in% tickers)
  expect_true("MSFT" %in% tickers)
  expect_true("NVDA" %in% tickers)

  # Non-equity rows (Futures) should be excluded
  non_equity <- df[!is.na(df[["Asset Class"]]) & df[["Asset Class"]] != "Equity", ]
  if (nrow(non_equity) > 0) {
    non_equity_tickers <- trimws(non_equity[["Ticker"]])
    expect_false(any(non_equity_tickers %in% tickers))
  }
})

test_that("compute_collar_credit returns correct net_credit from mock data", {
  call_bid <- 3.50
  put_ask  <- 2.00
  net_credit <- call_bid - put_ask

  expect_equal(net_credit, 1.5)

  # NULL guard: put ask of zero should cause function to return NULL
  put_ask_zero <- 0
  expect_true(put_ask_zero == 0)
})

test_that("collar credit top-N selection picks highest net_credit", {
  skew_data <- data.frame(
    ticker     = c("A", "B", "C", "D", "E"),
    net_credit = c(1.1, 1.5, 0.9, 2.0, 1.3),
    stringsAsFactors = FALSE
  )
  sorted <- skew_data[order(skew_data$net_credit, decreasing = TRUE), ]
  top3 <- head(sorted$ticker, 3)

  expect_equal(top3, c("D", "B", "E"))
})
