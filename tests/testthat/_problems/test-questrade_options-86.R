# Extracted from test-questrade_options.R:86

# test -------------------------------------------------------------------------
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
