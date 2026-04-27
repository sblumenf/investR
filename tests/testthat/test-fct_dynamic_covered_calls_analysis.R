# tests/testthat/test-fct_dynamic_covered_calls_analysis.R
# Tests for the post-refactor delegation behaviour of analyze_dynamic_covered_calls()
#
# The refactor moved setup_parallel_processing() out of the fct_ layer into the
# module, and replaced the inline future_map call with a call to
# process_stocks_parallel_generic() via a closure adapter.

library(testthat)
library(investR)
library(withr)

################################################################################
# TEST 1: analyze_dynamic_covered_calls delegates to process_stocks_parallel_generic
################################################################################

test_that("analyze_dynamic_covered_calls calls process_stocks_parallel_generic once", {
  # finalize_results() must also be mocked so the return value of the mocked
  # process_stocks_parallel_generic (an empty list) is handled without error.
  local_mocked_bindings(
    process_stocks_parallel_generic = function(...) list(),
    finalize_results = function(...) tibble::tibble(),
    .package = "investR"
  )

  call_count <- 0L
  local_mocked_bindings(
    process_stocks_parallel_generic = function(...) {
      call_count <<- call_count + 1L
      list()
    },
    finalize_results = function(...) tibble::tibble(),
    .package = "investR"
  )

  result <- analyze_dynamic_covered_calls(tickers = c("AAPL"), limit = 1)

  expect_equal(call_count, 1L)
  expect_s3_class(result, "tbl_df")
})

################################################################################
# TEST 2: The adapter forwards lookback_years and min_strike_pct to
#          analyze_single_stock_dynamic
################################################################################

test_that("adapter passes lookback_years and min_strike_pct to analyze_single_stock_dynamic", {
  captured_args <- list()

  local_mocked_bindings(
    # Intercept analyze_single_stock_dynamic and capture the arguments it
    # receives from the adapter closure.
    analyze_single_stock_dynamic = function(ticker,
                                            lookback_years,
                                            min_strike_pct,
                                            ...) {
      captured_args[[length(captured_args) + 1L]] <<- list(
        ticker         = ticker,
        lookback_years = lookback_years,
        min_strike_pct = min_strike_pct
      )
      NULL  # return NULL so process_stocks_parallel_generic marks it as failed
    },
    # Return 0 for rate_limit_seconds so Sys.sleep(0) is effectively a no-op.
    get_dynamic_config = function(key = NULL, ...) {
      defaults <- list(
        rate_limit_seconds   = 0,
        min_target_days      = 30,
        max_target_days      = 730,
        default_max_price    = 250,
        default_lookback_years = 5,
        default_min_strike_pct = 0.50,
        default_max_strike_pct = 0.85,
        max_workers          = 1
      )
      if (!is.null(key) && key %in% names(defaults)) defaults[[key]] else 0
    },
    finalize_results = function(...) tibble::tibble(),
    .package = "investR"
  )

  analyze_dynamic_covered_calls(
    tickers        = c("MSFT"),
    lookback_years = 3,
    min_strike_pct = 0.6
  )

  # analyze_single_stock_dynamic should have been called at least once
  expect_gte(length(captured_args), 1L)

  first_call <- captured_args[[1]]
  expect_equal(first_call$ticker,         "MSFT")
  expect_equal(first_call$lookback_years, 3)
  expect_equal(first_call$min_strike_pct, 0.6)
})

################################################################################
# TEST 3: analyze_dynamic_covered_calls does NOT call setup_parallel_processing
################################################################################

test_that("analyze_dynamic_covered_calls does not call setup_parallel_processing", {
  spp_called <- FALSE

  local_mocked_bindings(
    setup_parallel_processing = function(...) {
      spp_called <<- TRUE
      future::plan()  # return current plan as a stand-in
    },
    process_stocks_parallel_generic = function(...) list(),
    finalize_results = function(...) tibble::tibble(),
    .package = "investR"
  )

  analyze_dynamic_covered_calls(tickers = c("AAPL"), limit = 1)

  expect_false(
    spp_called,
    info = "setup_parallel_processing must not be called inside analyze_dynamic_covered_calls; it belongs in the module layer"
  )
})
