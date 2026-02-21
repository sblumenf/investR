# Extracted from test-questrade_options.R:240

# test -------------------------------------------------------------------------
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
