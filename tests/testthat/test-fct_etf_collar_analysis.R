# tests/testthat/test-fct_etf_collar_analysis.R
# Tests for ETF Collar Analysis (yfscreen variant)

library(testthat)
library(investR)
library(tibble)
library(dplyr)

################################################################################
# FUNCTION EXPORT AND BASIC STRUCTURE TESTS
################################################################################

test_that("analyze_etf_collar_yfscreen exists and is exported", {
  expect_true(exists("analyze_etf_collar_yfscreen", where = asNamespace("investR")))
  expect_true(is.function(investR::analyze_etf_collar_yfscreen))
})

test_that("analyze_etf_collar_yfscreen returns empty tibble when no ETFs found", {
  local_mocked_bindings(
    fetch_yfscreen_etfs = function(...) character(0),
    .package = "investR"
  )

  result <- investR::analyze_etf_collar_yfscreen()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

################################################################################
# DIVIDEND FILTER PARAMETER TESTS
################################################################################

test_that("analyze_etf_collar_yfscreen passes dividend_filter to fetch_yfscreen_etfs", {
  skip_on_cran()
  skip_if_offline()

  fetch_called_with <- NULL

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) {
      fetch_called_with <<- list(...)
      character(0)
    },
    {
      analyze_etf_collar_yfscreen(dividend_filter = "zero_dividend")
    }
  )

  expect_equal(fetch_called_with$dividend_filter, "zero_dividend")
})

test_that("analyze_etf_collar_yfscreen rejects invalid dividend_filter", {
  expect_error(
    analyze_etf_collar_yfscreen(dividend_filter = "invalid_filter"),
    "dividend_filter must be one of"
  )
})

test_that("analyze_etf_collar_yfscreen accepts dividend_paying filter", {
  local_mocked_bindings(
    fetch_yfscreen_etfs = function(...) character(0),
    .package = "investR"
  )

  expect_no_error(
    investR::analyze_etf_collar_yfscreen(dividend_filter = "dividend_paying")
  )
})

test_that("analyze_etf_collar_yfscreen accepts zero_dividend filter", {
  local_mocked_bindings(
    fetch_yfscreen_etfs = function(...) character(0),
    .package = "investR"
  )

  expect_no_error(
    investR::analyze_etf_collar_yfscreen(dividend_filter = "zero_dividend")
  )
})

################################################################################
# EMPTY UNIVERSE HANDLING
################################################################################

test_that("analyze_etf_collar_yfscreen does not call analyze_collar_single for empty universe", {
  skip_on_cran()
  skip_if_offline()

  analyze_called <- FALSE

  with_mocked_bindings(
    fetch_yfscreen_etfs = function(...) character(0),
    analyze_collar_single = function(...) {
      analyze_called <<- TRUE
      NULL
    },
    {
      result <- analyze_etf_collar_yfscreen()
      expect_false(analyze_called)
    }
  )
})

################################################################################
# CONFIG VALIDATION TESTS
################################################################################

test_that("ETF_COLLAR_CONFIG exists and has required keys", {
  config <- investR::get_etf_collar_config()

  expect_type(config, "list")
  expect_true("max_workers" %in% names(config))
  expect_true("min_net_credit" %in% names(config))
  expect_true("min_open_interest" %in% names(config))
  expect_true("max_stock_price" %in% names(config))
  expect_true("shares_per_contract" %in% names(config))
  expect_true("sgov_yield_default" %in% names(config))
  expect_true("max_sgov_yield_sanity" %in% names(config))
  expect_true("negative_return_threshold" %in% names(config))
})

test_that("validate_etf_collar_config passes with valid config", {
  expect_true(investR::validate_etf_collar_config())
})

test_that("validate_etf_collar_config rejects invalid max_workers", {
  bad_config <- investR::get_etf_collar_config()
  bad_config$max_workers <- 0
  expect_error(investR::validate_etf_collar_config(bad_config), "max_workers must be positive")
})

test_that("validate_etf_collar_config rejects invalid sgov_yield_default", {
  bad_config <- investR::get_etf_collar_config()
  bad_config$sgov_yield_default <- 1.5
  expect_error(investR::validate_etf_collar_config(bad_config), "sgov_yield_default must be between 0 and 1")
})

test_that("validate_etf_collar_config rejects negative min_net_credit", {
  bad_config <- investR::get_etf_collar_config()
  bad_config$min_net_credit <- -0.1
  expect_error(investR::validate_etf_collar_config(bad_config), "min_net_credit must be non-negative")
})

test_that("get_etf_collar_config returns specific key value", {
  val <- investR::get_etf_collar_config("max_workers")
  expect_type(val, "double")
  expect_gt(val, 0)
})

test_that("get_etf_collar_config errors on unknown key", {
  expect_error(
    investR::get_etf_collar_config("nonexistent_key"),
    "Configuration key 'nonexistent_key' not found"
  )
})
