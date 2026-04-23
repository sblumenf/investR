# tests/testthat/test-fct_monthly_dividend_analysis.R
# Tests for Monthly Dividend Stocks Analysis
# Date: 2026-04-22

library(testthat)
library(investR)
library(tibble)
library(dplyr)

# Shared mock data
mock_stocks <- tibble::tibble(
  ticker         = c("O", "MAIN", "AGNC"),
  company_name   = c("Realty Income", "Main Street Capital", "AGNC Investment"),
  dividend_yield = c("5.13%", "7.99%", "13.20%"),
  market_cap     = c("59B", "4.9B", "12.5B")
)

mock_results <- tibble::tibble(
  ticker              = c("O", "MAIN"),
  company_name        = c("Realty Income", "Main Street Capital"),
  current_price       = c(55.0, 45.0),
  strike              = c(44.0, 36.0),
  annualized_return   = c(0.18, 0.14),
  total_return        = c(0.09, 0.07),
  is_aristocrat       = c(TRUE, TRUE),
  is_monthly_dividend = c(TRUE, TRUE)
)

################################################################################
# EXPORT / CALLABLE TESTS
################################################################################

test_that("analyze_monthly_dividend_stocks is exported and callable", {
  expect_true(
    exists("analyze_monthly_dividend_stocks", where = asNamespace("investR"))
  )
  fn <- base::get("analyze_monthly_dividend_stocks", envir = asNamespace("investR"))
  expect_true(is.function(fn))
})

################################################################################
# RETURNS TIBBLE ON SUCCESS
################################################################################

test_that("analyze_monthly_dividend_stocks returns a tibble on success", {
  with_mocked_bindings(
    fetch_monthly_dividend_stocks = function(...) mock_stocks,
    analyze_covered_calls_generic = function(...) mock_results,
    {
      result <- analyze_monthly_dividend_stocks()
      expect_s3_class(result, "tbl_df")
    }
  )
})

################################################################################
# EMPTY FETCH RETURNS EMPTY TIBBLE
################################################################################

test_that("analyze_monthly_dividend_stocks returns empty tibble when fetch returns 0 rows", {
  with_mocked_bindings(
    fetch_monthly_dividend_stocks = function(...) {
      tibble::tibble(
        ticker         = character(0),
        company_name   = character(0),
        dividend_yield = character(0),
        market_cap     = character(0)
      )
    },
    {
      result <- analyze_monthly_dividend_stocks()
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0)
    }
  )
})

################################################################################
# CORRECT RESULT_FLAGS PASSED
################################################################################

test_that("analyze_monthly_dividend_stocks passes correct result_flags to analyze_covered_calls_generic", {
  captured_flags <- NULL

  with_mocked_bindings(
    fetch_monthly_dividend_stocks = function(...) mock_stocks,
    analyze_covered_calls_generic = function(...) {
      args <- list(...)
      captured_flags <<- args$result_flags
      mock_results
    },
    {
      analyze_monthly_dividend_stocks()

      expect_false(is.null(captured_flags))
      expect_false(isTRUE(captured_flags$is_aristocrat))
      expect_true(isTRUE(captured_flags$is_monthly_dividend))
    }
  )
})

################################################################################
# CORRECT TICKERS PASSED
################################################################################

test_that("analyze_monthly_dividend_stocks passes tickers from $ticker column to analyze_covered_calls_generic", {
  captured_universe <- NULL

  with_mocked_bindings(
    fetch_monthly_dividend_stocks = function(...) mock_stocks,
    analyze_covered_calls_generic = function(...) {
      args <- list(...)
      captured_universe <<- args$stock_universe
      mock_results
    },
    {
      analyze_monthly_dividend_stocks()

      expect_false(is.null(captured_universe))
      expect_true(is.character(captured_universe))
      expect_equal(sort(captured_universe), sort(mock_stocks$ticker))
    }
  )
})

################################################################################
# RESPECTS strike_threshold_pct
################################################################################

test_that("analyze_monthly_dividend_stocks respects strike_threshold_pct parameter", {
  captured_threshold <- NULL

  with_mocked_bindings(
    fetch_monthly_dividend_stocks = function(...) mock_stocks,
    analyze_covered_calls_generic = function(...) {
      args <- list(...)
      captured_threshold <<- args$strike_threshold_pct
      mock_results
    },
    {
      analyze_monthly_dividend_stocks(strike_threshold_pct = 0.75)

      expect_false(is.null(captured_threshold))
      expect_equal(captured_threshold, 0.75)
    }
  )
})

################################################################################
# RESPECTS max_workers
################################################################################

test_that("analyze_monthly_dividend_stocks respects max_workers parameter", {
  captured_workers <- NULL

  with_mocked_bindings(
    fetch_monthly_dividend_stocks = function(...) mock_stocks,
    analyze_covered_calls_generic = function(...) {
      args <- list(...)
      captured_workers <<- args$max_workers
      mock_results
    },
    {
      analyze_monthly_dividend_stocks(max_workers = 3)

      expect_false(is.null(captured_workers))
      expect_equal(captured_workers, 3)
    }
  )
})
