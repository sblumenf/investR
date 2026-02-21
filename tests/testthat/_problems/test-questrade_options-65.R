# Extracted from test-questrade_options.R:65

# test -------------------------------------------------------------------------
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
expect_equal(length(ids), 8)
expect_true(1001 %in% ids)
