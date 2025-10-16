test_that("fetch_options_chain routes to Questrade by default", {
  # Clear any existing option
  options(investR.quote_source = NULL)

  # Mock the Questrade-specific function
  local_mocked_bindings(
    fetch_questrade_options_chain = function(ticker, expiration) {
      list("Mar.21.2025" = list(
        calls = data.frame(Strike = 170, Bid = 8.50, Ask = 8.70, Vol = 100, OI = 500),
        puts = data.frame(Strike = 170, Bid = 5.20, Ask = 5.40, Vol = 80, OI = 400)
      ))
    }
  )

  result <- fetch_options_chain("TEST")

  expect_type(result, "list")
  expect_true("Mar.21.2025" %in% names(result))
  expect_s3_class(result[["Mar.21.2025"]]$calls, "data.frame")
})

test_that("fetch_options_chain routes to Yahoo when configured", {
  options(investR.quote_source = "yahoo")

  # Mock the Yahoo-specific function
  local_mocked_bindings(
    fetch_options_chain_yahoo = function(ticker, expiration) {
      list("Apr.18.2025" = list(
        calls = data.frame(Strike = 180, Bid = 10.00, Ask = 10.20, Vol = 150, OI = 600),
        puts = data.frame(Strike = 180, Bid = 6.00, Ask = 6.20, Vol = 120, OI = 500)
      ))
    }
  )

  result <- fetch_options_chain("TEST")

  expect_type(result, "list")
  expect_true("Apr.18.2025" %in% names(result))

  # Cleanup
  options(investR.quote_source = NULL)
})

test_that("extract_option_ids extracts all call and put IDs", {
  structure <- list(
    list(
      expiryDate = "2025-03-21T00:00:00.000000-05:00",
      chainPerStrikePrice = list(
        list(strikePrice = 170, callSymbolId = 1001, putSymbolId = 2001),
        list(strikePrice = 175, callSymbolId = 1002, putSymbolId = 2002),
        list(strikePrice = 180, callSymbolId = 1003, putSymbolId = 2003)
      )
    ),
    list(
      expiryDate = "2025-04-18T00:00:00.000000-05:00",
      chainPerStrikePrice = list(
        list(strikePrice = 170, callSymbolId = 1011, putSymbolId = 2011)
      )
    )
  )

  ids <- investR:::extract_option_ids(structure)

  expect_equal(length(ids), 8)  # 3 calls + 3 puts + 1 call + 1 put
  expect_true(1001 %in% ids)
  expect_true(2001 %in% ids)
  expect_true(1011 %in% ids)
  expect_true(2011 %in% ids)
})

test_that("extract_option_ids handles missing IDs gracefully", {
  structure <- list(
    list(
      expiryDate = "2025-03-21T00:00:00.000000-05:00",
      chainPerStrikePrice = list(
        list(strikePrice = 170, callSymbolId = 1001),  # No put
        list(strikePrice = 175, putSymbolId = 2002)    # No call
      )
    )
  )

  ids <- investR:::extract_option_ids(structure)

  expect_equal(length(ids), 2)
  expect_true(1001 %in% ids)
  expect_true(2002 %in% ids)
})

test_that("parse_questrade_date converts ISO format correctly", {
  date_str <- "2025-03-21T00:00:00.000000-05:00"
  result <- investR:::parse_questrade_date(date_str)

  expect_s3_class(result, "Date")
  expect_equal(result, as.Date("2025-03-21"))
})

test_that("parse_questrade_date handles NULL input", {
  result <- investR:::parse_questrade_date(NULL)
  expect_null(result)
})

test_that("parse_questrade_date handles malformed input", {
  result <- investR:::parse_questrade_date("invalid-date")
  expect_null(result)
})

test_that("format_yahoo_date formats correctly", {
  date <- as.Date("2025-03-21")
  result <- investR:::format_yahoo_date(date)

  expect_equal(result, "Mar.21.2025")
})

test_that("format_yahoo_date handles NULL input", {
  result <- investR:::format_yahoo_date(NULL)
  expect_null(result)
})

test_that("build_options_dataframe creates correct structure for calls", {
  chain_per_strike <- list(
    list(strikePrice = 170, callSymbolId = 1001, putSymbolId = 2001),
    list(strikePrice = 175, callSymbolId = 1002, putSymbolId = 2002)
  )

  quotes_lookup <- list(
    "1001" = list(
      symbolId = 1001,
      lastTradePrice = 10.50,
      bidPrice = 10.40,
      askPrice = 10.60,
      volume = 100,
      openInterest = 500,
      volatility = 0.25
    ),
    "1002" = list(
      symbolId = 1002,
      lastTradePrice = 8.25,
      bidPrice = 8.20,
      askPrice = 8.30,
      volume = 80,
      openInterest = 400,
      volatility = 0.22
    )
  )

  result <- investR:::build_options_dataframe(chain_per_strike, quotes_lookup, type = "call")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$Strike, c(170, 175))
  expect_equal(result$Bid, c(10.40, 8.20))
  expect_equal(result$Ask, c(10.60, 8.30))
  expect_equal(result$Vol, c(100, 80))
  expect_equal(result$OI, c(500, 400))
})

test_that("build_options_dataframe creates correct structure for puts", {
  chain_per_strike <- list(
    list(strikePrice = 170, callSymbolId = 1001, putSymbolId = 2001)
  )

  quotes_lookup <- list(
    "2001" = list(
      symbolId = 2001,
      lastTradePrice = 5.50,
      bidPrice = 5.40,
      askPrice = 5.60,
      volume = 60,
      openInterest = 300,
      volatility = 0.20
    )
  )

  result <- investR:::build_options_dataframe(chain_per_strike, quotes_lookup, type = "put")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$Strike, 170)
  expect_equal(result$Bid, 5.40)
  expect_equal(result$OI, 300)
})

test_that("build_options_dataframe returns empty dataframe when no data", {
  chain_per_strike <- list()
  quotes_lookup <- list()

  result <- investR:::build_options_dataframe(chain_per_strike, quotes_lookup, type = "call")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("Strike" %in% names(result))
  expect_true("Bid" %in% names(result))
  expect_true("Ask" %in% names(result))
})

test_that("build_options_dataframe handles missing quote data", {
  chain_per_strike <- list(
    list(strikePrice = 170, callSymbolId = 1001),
    list(strikePrice = 175, callSymbolId = 1002)
  )

  quotes_lookup <- list(
    "1001" = list(
      symbolId = 1001,
      bidPrice = 10.40,
      askPrice = 10.60,
      volume = 100,
      openInterest = 500
    )
    # 1002 missing
  )

  result <- investR:::build_options_dataframe(chain_per_strike, quotes_lookup, type = "call")

  expect_equal(nrow(result), 1)
  expect_equal(result$Strike, 170)
})

test_that("convert_options_to_yahoo_format creates complete structure", {
  structure <- list(
    list(
      expiryDate = "2025-03-21T00:00:00.000000-05:00",
      chainPerStrikePrice = list(
        list(strikePrice = 170, callSymbolId = 1001, putSymbolId = 2001)
      )
    )
  )

  quotes <- list(
    list(symbolId = 1001, bidPrice = 10.40, askPrice = 10.60, volume = 100, openInterest = 500),
    list(symbolId = 2001, bidPrice = 5.40, askPrice = 5.60, volume = 60, openInterest = 300)
  )

  result <- investR:::convert_options_to_yahoo_format(structure, quotes, expiration = NULL)

  expect_type(result, "list")
  expect_true("Mar.21.2025" %in% names(result))
  expect_true("calls" %in% names(result[["Mar.21.2025"]]))
  expect_true("puts" %in% names(result[["Mar.21.2025"]]))
  expect_s3_class(result[["Mar.21.2025"]]$calls, "data.frame")
  expect_s3_class(result[["Mar.21.2025"]]$puts, "data.frame")
})

test_that("convert_options_to_yahoo_format filters by expiration when specified", {
  structure <- list(
    list(
      expiryDate = "2025-03-21T00:00:00.000000-05:00",
      chainPerStrikePrice = list(
        list(strikePrice = 170, callSymbolId = 1001, putSymbolId = 2001)
      )
    ),
    list(
      expiryDate = "2025-04-18T00:00:00.000000-05:00",
      chainPerStrikePrice = list(
        list(strikePrice = 180, callSymbolId = 1011, putSymbolId = 2011)
      )
    )
  )

  quotes <- list(
    list(symbolId = 1001, bidPrice = 10.40, askPrice = 10.60, volume = 100, openInterest = 500),
    list(symbolId = 2001, bidPrice = 5.40, askPrice = 5.60, volume = 60, openInterest = 300),
    list(symbolId = 1011, bidPrice = 12.40, askPrice = 12.60, volume = 120, openInterest = 600),
    list(symbolId = 2011, bidPrice = 6.40, askPrice = 6.60, volume = 80, openInterest = 400)
  )

  result <- investR:::convert_options_to_yahoo_format(structure, quotes, expiration = "Mar.21.2025")

  expect_type(result, "list")
  expect_equal(length(result), 1)
  expect_true("Mar.21.2025" %in% names(result))
  expect_false("Apr.18.2025" %in% names(result))
})

test_that("fetch_questrade_options_chain falls back on auth failure", {
  options(investR.quote_source = "questrade")

  local_mocked_bindings(
    get_questrade_auth = function() NULL,
    fetch_options_chain_yahoo = function(ticker, expiration) {
      list("Yahoo.Fallback" = list(calls = data.frame(), puts = data.frame()))
    }
  )

  result <- fetch_questrade_options_chain("TEST")

  expect_type(result, "list")
  expect_true("Yahoo.Fallback" %in% names(result))

  # Cleanup
  options(investR.quote_source = NULL)
})

test_that("fetch_questrade_options_chain falls back on symbol lookup failure", {
  options(investR.quote_source = "questrade")

  local_mocked_bindings(
    get_questrade_auth = function() list(access_token = "test", api_server = "http://test/"),
    search_questrade_symbol = function(ticker, auth) NULL,
    fetch_options_chain_yahoo = function(ticker, expiration) {
      list("Yahoo.Fallback" = list(calls = data.frame(), puts = data.frame()))
    }
  )

  result <- fetch_questrade_options_chain("TEST")

  expect_type(result, "list")
  expect_true("Yahoo.Fallback" %in% names(result))

  # Cleanup
  options(investR.quote_source = NULL)
})

test_that("fetch_questrade_options_chain falls back on structure fetch failure", {
  options(investR.quote_source = "questrade")

  local_mocked_bindings(
    get_questrade_auth = function() list(access_token = "test", api_server = "http://test/"),
    search_questrade_symbol = function(ticker, auth) 8049,
    fetch_questrade_options_structure = function(symbol_id, auth) NULL,
    fetch_options_chain_yahoo = function(ticker, expiration) {
      list("Yahoo.Fallback" = list(calls = data.frame(), puts = data.frame()))
    }
  )

  result <- fetch_questrade_options_chain("TEST")

  expect_type(result, "list")
  expect_true("Yahoo.Fallback" %in% names(result))

  # Cleanup
  options(investR.quote_source = NULL)
})

test_that("fetch_questrade_options_chain falls back when no option IDs found", {
  options(investR.quote_source = "questrade")

  local_mocked_bindings(
    get_questrade_auth = function() list(access_token = "test", api_server = "http://test/"),
    search_questrade_symbol = function(ticker, auth) 8049,
    fetch_questrade_options_structure = function(symbol_id, auth) {
      list(list(expiryDate = "2025-03-21T00:00:00.000000-05:00", chainPerStrikePrice = list()))
    },
    fetch_options_chain_yahoo = function(ticker, expiration) {
      list("Yahoo.Fallback" = list(calls = data.frame(), puts = data.frame()))
    }
  )

  result <- fetch_questrade_options_chain("TEST")

  expect_type(result, "list")
  expect_true("Yahoo.Fallback" %in% names(result))

  # Cleanup
  options(investR.quote_source = NULL)
})

test_that("fetch_questrade_options_chain falls back on pricing fetch failure", {
  options(investR.quote_source = "questrade")

  local_mocked_bindings(
    get_questrade_auth = function() list(access_token = "test", api_server = "http://test/"),
    search_questrade_symbol = function(ticker, auth) 8049,
    fetch_questrade_options_structure = function(symbol_id, auth) {
      list(list(
        expiryDate = "2025-03-21T00:00:00.000000-05:00",
        chainPerStrikePrice = list(
          list(strikePrice = 170, callSymbolId = 1001, putSymbolId = 2001)
        )
      ))
    },
    extract_option_ids = function(structure) c(1001, 2001),
    fetch_questrade_option_quotes = function(option_ids, auth) NULL,
    fetch_options_chain_yahoo = function(ticker, expiration) {
      list("Yahoo.Fallback" = list(calls = data.frame(), puts = data.frame()))
    }
  )

  result <- fetch_questrade_options_chain("TEST")

  expect_type(result, "list")
  expect_true("Yahoo.Fallback" %in% names(result))

  # Cleanup
  options(investR.quote_source = NULL)
})
