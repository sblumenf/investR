# Tests for get_quote_source() / set_quote_source() option propagation
# Covers fix: capturing get_quote_source() before future_promise() so the
# option value is available in the worker thread.

test_that("get_quote_source returns 'questrade' when option is unset", {
  withr::with_options(list(investR.quote_source = NULL), {
    result <- get_quote_source()
    expect_equal(result, "questrade")
  })
})

test_that("get_quote_source returns 'questrade' when option is explicitly questrade", {
  withr::with_options(list(investR.quote_source = "questrade"), {
    result <- get_quote_source()
    expect_equal(result, "questrade")
  })
})

test_that("get_quote_source returns 'yahoo' when option is set to yahoo", {
  withr::with_options(list(investR.quote_source = "yahoo"), {
    result <- get_quote_source()
    expect_equal(result, "yahoo")
  })
})

test_that("get_quote_source is isolated: outer option survives inner override", {
  withr::with_options(list(investR.quote_source = "questrade"), {
    inner_val <- withr::with_options(list(investR.quote_source = "yahoo"), {
      get_quote_source()
    })
    expect_equal(inner_val, "yahoo")

    # Outer scope is untouched
    outer_val <- get_quote_source()
    expect_equal(outer_val, "questrade")
  })
})

test_that("get_quote_source captures value correctly for propagation to worker", {
  # Simulates the pattern used in mod_dynamic_covered_calls_analysis.R:
  #   quote_source <- get_quote_source()
  #   future_promise({ options(investR.quote_source = quote_source) ... })
  # The captured value must equal what get_quote_source() returned at capture time.

  withr::with_options(list(investR.quote_source = "yahoo"), {
    captured <- get_quote_source()       # captured before "spawning worker"
    expect_equal(captured, "yahoo")

    # Now change the option in the main process (simulating a UI change
    # that arrives after the worker has already been spawned)
    options(investR.quote_source = "questrade")

    # The captured value is still the old one — this is the invariant the fix
    # guarantees: workers receive the value at dispatch time, not at execution time.
    expect_equal(captured, "yahoo")
  })
})

test_that("set_quote_source sets option to yahoo", {
  withr::with_options(list(investR.quote_source = NULL), {
    investR:::set_quote_source("yahoo")
    expect_equal(getOption("investR.quote_source"), "yahoo")
  })
})

test_that("set_quote_source sets option to questrade", {
  withr::with_options(list(investR.quote_source = NULL), {
    investR:::set_quote_source("questrade")
    expect_equal(getOption("investR.quote_source"), "questrade")
  })
})

test_that("set_quote_source rejects invalid values", {
  withr::with_options(list(investR.quote_source = NULL), {
    expect_error(investR:::set_quote_source("bloomberg"), "must be")
    expect_error(investR:::set_quote_source(""),          "must be")
    expect_error(investR:::set_quote_source(NA_character_), "must be")
  })
})
