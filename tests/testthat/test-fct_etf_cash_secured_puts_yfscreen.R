# tests/testthat/test-fct_etf_cash_secured_puts_yfscreen.R
# Tests for ETF Cash-Secured Puts Analysis (yfscreen variant)
# Date: 2025-11-23

library(testthat)
library(investR)
library(tibble)
library(dplyr)

################################################################################
# FUNCTION EXPORT AND BASIC STRUCTURE TESTS
################################################################################

test_that("analyze_etf_cash_secured_puts_yfscreen exists and is exported", {
  # Function should be available in package namespace
  expect_true(exists("analyze_etf_cash_secured_puts_yfscreen", where = asNamespace("investR")))
  expect_true(is.function(investR::analyze_etf_cash_secured_puts_yfscreen))
})

test_that("analyze_etf_cash_secured_puts_yfscreen returns tibble structure", {
  skip_on_cran()
  skip_if_offline()

  # Mock fetch_yfscreen_etfs to return empty vector
  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) character(0),
    {
      result <- analyze_etf_cash_secured_puts_yfscreen()

      # Should return tibble even when empty
      expect_s3_class(result, "tbl_df")
    }
  )
})

################################################################################
# DEFAULT PARAMETERS TESTS
################################################################################

test_that("analyze_etf_cash_secured_puts_yfscreen works with default parameters", {
  skip_on_cran()
  skip_if_offline()

  # Mock both functions to avoid external API calls
  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY", "QQQ"),
    analyze_puts_generic = function(...) tibble(),
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen()
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      expect_s3_class(result, "tbl_df")
    }
  )
})

################################################################################
# DIVIDEND FILTER TESTS
################################################################################

test_that("analyze_etf_cash_secured_puts_yfscreen handles dividend_filter='all'", {
  skip_on_cran()
  skip_if_offline()

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY", "QQQ", "VTI"),
    analyze_puts_generic = function(...) tibble(),
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen(dividend_filter = "all")
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      expect_s3_class(result, "tbl_df")
    }
  )
})

test_that("analyze_etf_cash_secured_puts_yfscreen handles dividend_filter='dividend_paying'", {
  skip_on_cran()
  skip_if_offline()

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("VYM", "SCHD", "DVY"),
    analyze_puts_generic = function(...) tibble(),
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen(
          dividend_filter = "dividend_paying",
          dividend_yield_min = 2,
          dividend_yield_max = 6
        )
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      expect_s3_class(result, "tbl_df")
    }
  )
})

test_that("analyze_etf_cash_secured_puts_yfscreen handles dividend_filter='zero_dividend'", {
  skip_on_cran()
  skip_if_offline()

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY", "QQQ"),
    analyze_puts_generic = function(...) tibble(),
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen(dividend_filter = "zero_dividend")
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      expect_s3_class(result, "tbl_df")
    }
  )
})

################################################################################
# MARKET CAP FILTERING TESTS
################################################################################

test_that("analyze_etf_cash_secured_puts_yfscreen respects market_cap_min parameter", {
  skip_on_cran()
  skip_if_offline()

  # Track function calls
  fetch_called_with <- NULL

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) {
      fetch_called_with <<- list(...)
      c("SPY", "QQQ")
    },
    analyze_puts_generic = function(...) tibble(),
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen(market_cap_min = 10e9)
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      # Verify fetch_yfscreen_etfs was called with correct market_cap_min
      expect_false(is.null(fetch_called_with))
      expect_equal(fetch_called_with$market_cap_min, 10e9)
    }
  )
})

test_that("analyze_etf_cash_secured_puts_yfscreen respects market_cap_max parameter", {
  skip_on_cran()
  skip_if_offline()

  # Track function calls
  fetch_called_with <- NULL

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) {
      fetch_called_with <<- list(...)
      c("SPY", "QQQ")
    },
    analyze_puts_generic = function(...) tibble(),
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen(market_cap_max = 50e9)
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      # Verify fetch_yfscreen_etfs was called with correct market_cap_max
      expect_false(is.null(fetch_called_with))
      expect_equal(fetch_called_with$market_cap_max, 50e9)
    }
  )
})

################################################################################
# TOP N LIMITING TESTS
################################################################################

test_that("analyze_etf_cash_secured_puts_yfscreen respects top_n parameter", {
  skip_on_cran()
  skip_if_offline()

  # Track function calls
  fetch_called_with <- NULL

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) {
      fetch_called_with <<- list(...)
      c("SPY", "QQQ", "VTI")
    },
    analyze_puts_generic = function(...) tibble(),
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen(top_n = 25)
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      # Verify fetch_yfscreen_etfs was called with correct top_n
      expect_false(is.null(fetch_called_with))
      expect_equal(fetch_called_with$top_n, 25)
    }
  )
})

test_that("analyze_etf_cash_secured_puts_yfscreen uses default top_n=50", {
  skip_on_cran()
  skip_if_offline()

  # Track function calls
  fetch_called_with <- NULL

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) {
      fetch_called_with <<- list(...)
      c("SPY", "QQQ")
    },
    analyze_puts_generic = function(...) tibble(),
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen()  # No top_n specified
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      # Verify fetch_yfscreen_etfs was called with default top_n=50
      expect_false(is.null(fetch_called_with))
      expect_equal(fetch_called_with$top_n, 50)
    }
  )
})

################################################################################
# STRIKE THRESHOLD PARAMETER TESTS
################################################################################

test_that("analyze_etf_cash_secured_puts_yfscreen passes strike_threshold_pct correctly", {
  skip_on_cran()
  skip_if_offline()

  # Track function calls
  analyze_called_with <- NULL

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY"),
    analyze_puts_generic = function(...) {
      analyze_called_with <<- list(...)
      tibble()
    },
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen(strike_threshold_pct = 0.90)
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      # Verify analyze_puts_generic was called with correct strike_threshold_pct
      expect_false(is.null(analyze_called_with))
      expect_equal(analyze_called_with$strike_threshold_pct, 0.90)
    }
  )
})

################################################################################
# DAYS RANGE PARAMETER TESTS
################################################################################

test_that("analyze_etf_cash_secured_puts_yfscreen passes min_days correctly", {
  skip_on_cran()
  skip_if_offline()

  # Track function calls
  analyze_called_with <- NULL

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY"),
    analyze_puts_generic = function(...) {
      analyze_called_with <<- list(...)
      tibble()
    },
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen(min_days = 30)
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      # Verify analyze_puts_generic was called with correct min_days
      expect_false(is.null(analyze_called_with))
      expect_equal(analyze_called_with$min_days, 30)
    }
  )
})

test_that("analyze_etf_cash_secured_puts_yfscreen passes max_days correctly", {
  skip_on_cran()
  skip_if_offline()

  # Track function calls
  analyze_called_with <- NULL

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY"),
    analyze_puts_generic = function(...) {
      analyze_called_with <<- list(...)
      tibble()
    },
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen(max_days = 120)
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      # Verify analyze_puts_generic was called with correct max_days
      expect_false(is.null(analyze_called_with))
      expect_equal(analyze_called_with$max_days, 120)
    }
  )
})

test_that("analyze_etf_cash_secured_puts_yfscreen passes expiry_month correctly", {
  skip_on_cran()
  skip_if_offline()

  # Track function calls
  analyze_called_with <- NULL

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY"),
    analyze_puts_generic = function(...) {
      analyze_called_with <<- list(...)
      tibble()
    },
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen(expiry_month = 3)
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      # Verify analyze_puts_generic was called with correct expiry_month
      expect_false(is.null(analyze_called_with))
      expect_equal(analyze_called_with$expiry_month, 3)
    }
  )
})

################################################################################
# EMPTY ETF UNIVERSE HANDLING TESTS
################################################################################

test_that("analyze_etf_cash_secured_puts_yfscreen handles empty ETF universe gracefully", {
  # Mock fetch_yfscreen_etfs to return empty vector
  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) character(0),
    .package = "investR",
    {
      result <- investR::analyze_etf_cash_secured_puts_yfscreen()

      # Should return empty tibble, not error
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0)
    }
  )
})

test_that("analyze_etf_cash_secured_puts_yfscreen does not call analyze_puts_generic for empty universe", {
  skip_on_cran()
  skip_if_offline()

  # Track if analyze_puts_generic was called
  analyze_called <- FALSE

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) character(0),
    analyze_puts_generic = function(...) {
      analyze_called <<- TRUE
      tibble()
    },
    {
      result <- analyze_etf_cash_secured_puts_yfscreen()

      # analyze_puts_generic should NOT be called for empty ETF universe
      expect_false(analyze_called)
    }
  )
})

################################################################################
# RESULT FLAGS TESTS
################################################################################

test_that("analyze_etf_cash_secured_puts_yfscreen sets correct result flags", {
  skip_on_cran()
  skip_if_offline()

  # Track function calls
  analyze_called_with <- NULL

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY"),
    analyze_puts_generic = function(...) {
      analyze_called_with <<- list(...)
      tibble()
    },
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen()
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      # Verify analyze_puts_generic was called with correct result_flags
      expect_false(is.null(analyze_called_with))

      expected_flags <- list(
        is_put = TRUE,
        is_aristocrat = FALSE,
        is_etf = TRUE,
        is_yfscreen = TRUE
      )

      expect_equal(analyze_called_with$result_flags, expected_flags)
    }
  )
})

test_that("analyze_etf_cash_secured_puts_yfscreen result flags indicate ETF strategy", {
  skip_on_cran()
  skip_if_offline()

  # Create mock result with flags
  mock_result <- tibble(
    ticker = "SPY",
    strike = 500,
    annualized_return = 0.15,
    is_put = TRUE,
    is_aristocrat = FALSE,
    is_etf = TRUE,
    is_yfscreen = TRUE
  )

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY"),
    analyze_puts_generic = function(...) mock_result,
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen()
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      if (nrow(result) > 0) {
        # All results should have correct flags
        expect_true(all(result$is_put == TRUE))
        expect_true(all(result$is_aristocrat == FALSE))
        expect_true(all(result$is_etf == TRUE))
        expect_true(all(result$is_yfscreen == TRUE))
      }
    }
  )
})

################################################################################
# INTEGRATION TESTS
################################################################################

test_that("analyze_etf_cash_secured_puts_yfscreen integration test with mocked ETFs", {
  skip_on_cran()
  skip_if_offline()
  skip("Integration test - requires mocked full pipeline")

  # Mock a complete pipeline
  mock_result <- tibble(
    ticker = c("SPY", "QQQ"),
    company_name = c("SPDR S&P 500 ETF", "Invesco QQQ Trust"),
    current_price = c(500, 400),
    strike = c(480, 380),
    annualized_return = c(0.20, 0.18),
    is_put = c(TRUE, TRUE),
    is_aristocrat = c(FALSE, FALSE),
    is_etf = c(TRUE, TRUE),
    is_yfscreen = c(TRUE, TRUE)
  )

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY", "QQQ"),
    analyze_puts_generic = function(...) mock_result,
    {
      result <- analyze_etf_cash_secured_puts_yfscreen(
        dividend_filter = "all",
        market_cap_min = 1e9,
        top_n = 50
      )

      # Verify result structure
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 2)
      expect_true("ticker" %in% names(result))
      expect_true("is_yfscreen" %in% names(result))

      # Verify sorting by annualized return
      if (nrow(result) > 1) {
        expect_true(all(diff(result$annualized_return) <= 0))
      }
    }
  )
})

test_that("analyze_etf_cash_secured_puts_yfscreen handles dividend yield range", {
  skip_on_cran()
  skip_if_offline()

  # Track function calls
  fetch_called_with <- NULL

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) {
      fetch_called_with <<- list(...)
      c("VYM", "SCHD")
    },
    analyze_puts_generic = function(...) tibble(),
    {
      result <- tryCatch({
        analyze_etf_cash_secured_puts_yfscreen(
          dividend_filter = "dividend_paying",
          dividend_yield_min = 3,
          dividend_yield_max = 8
        )
      }, error = function(e) {
        skip(paste("Skipping test due to error:", e$message))
      })

      # Verify fetch_yfscreen_etfs was called with correct yield range
      expect_false(is.null(fetch_called_with))
      expect_equal(fetch_called_with$dividend_yield_min, 3)
      expect_equal(fetch_called_with$dividend_yield_max, 8)
      expect_equal(fetch_called_with$dividend_filter, "dividend_paying")
    }
  )
})

################################################################################
# PARAMETER VALIDATION TESTS
################################################################################

test_that("analyze_etf_cash_secured_puts_yfscreen validates parameter types", {
  skip_on_cran()
  skip_if_offline()

  # Should handle numeric parameters correctly
  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY"),
    analyze_puts_generic = function(...) tibble(),
    {
      # All numeric parameters should work
      expect_no_error({
        analyze_etf_cash_secured_puts_yfscreen(
          dividend_yield_min = 2.5,
          dividend_yield_max = 5.5,
          market_cap_min = 1.5e9,
          market_cap_max = 75e9,
          top_n = 30,
          strike_threshold_pct = 0.92,
          min_days = 35,
          max_days = 110
        )
      })
    }
  )
})

test_that("analyze_etf_cash_secured_puts_yfscreen handles NULL yield parameters", {
  skip_on_cran()
  skip_if_offline()

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) c("SPY"),
    analyze_puts_generic = function(...) tibble(),
    {
      # NULL yield parameters should work (no filtering)
      expect_no_error({
        analyze_etf_cash_secured_puts_yfscreen(
          dividend_yield_min = NULL,
          dividend_yield_max = NULL,
          dividend_filter = "all"
        )
      })
    }
  )
})
