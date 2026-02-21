# Extracted from test-questrade_options.R:270

# test -------------------------------------------------------------------------
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
