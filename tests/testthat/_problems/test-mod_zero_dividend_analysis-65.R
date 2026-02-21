# Extracted from test-mod_zero_dividend_analysis.R:65

# test -------------------------------------------------------------------------
testServer(
    mod_zero_dividend_analysis_server,
    {
      # Set slider inputs
      session$setInputs(
        strike_threshold = 85,
        target_days = 1000,
        max_workers = 10
      )

      # Get analysis params
      params <- analysis_params()

      # Strike threshold should be converted to decimal
      expect_equal(params$strike_threshold_pct, 0.85)

      # Target days = 1000 should become NULL
      expect_null(params$target_days)

      # Max workers should pass through
      expect_equal(params$max_workers, 10)
    }
  )
