# Extracted from test-mod_zero_dividend_analysis.R:90

# test -------------------------------------------------------------------------
testServer(
    mod_zero_dividend_analysis_server,
    {
      # Set slider inputs with specific target days
      session$setInputs(
        strike_threshold = 80,
        target_days = 60,
        max_workers = 5
      )

      params <- analysis_params()

      # Strike threshold converted
      expect_equal(params$strike_threshold_pct, 0.80)

      # Target days should NOT be NULL
      expect_equal(params$target_days, 60)

      # Max workers
      expect_equal(params$max_workers, 5)
    }
  )
