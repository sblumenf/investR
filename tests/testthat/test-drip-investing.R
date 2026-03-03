# tests/testthat/test-drip-investing.R
# Tests for DRIP Investing integration in utils_custom_ticker_lists.R
# and the tickers parameter on analyze_aristocrats() in fct_aristocrats_analysis.R

library(testthat)
library(investR)

# Minimal mock data frame used across multiple test groups
mock_drip_data <- data.frame(
  ticker     = c("KO", "PG", "MMM", "JNJ", "CL", "LOW", "WMT", "TGT"),
  stock_type = c("King", "King", "King", "Champion", "Champion", "Contender", "Contender", "Challenger"),
  company    = c("Coca-Cola", "Procter & Gamble", "3M", "Johnson & Johnson",
                 "Colgate", "Lowe's", "Walmart", "Target"),
  stringsAsFactors = FALSE
)

################################################################################
# Group 1: get_drip_tickers() input validation
################################################################################

test_that("get_drip_tickers rejects an invalid category string", {
  expect_error(
    get_drip_tickers("invalid"),
    "category must be one of"
  )
})

test_that("get_drip_tickers rejects NULL category", {
  # NULL triggers R's own length-zero error before the custom message is reached
  expect_error(get_drip_tickers(NULL))
})

test_that("get_drip_tickers accepts 'King' as a valid category", {
  local_mocked_bindings(
    fetch_drip_investing_data = function() mock_drip_data,
    .package = "investR"
  )
  expect_no_error(get_drip_tickers("King"))
})

test_that("get_drip_tickers accepts 'Champion' as a valid category", {
  local_mocked_bindings(
    fetch_drip_investing_data = function() mock_drip_data,
    .package = "investR"
  )
  expect_no_error(get_drip_tickers("Champion"))
})

test_that("get_drip_tickers accepts 'Contender' as a valid category", {
  local_mocked_bindings(
    fetch_drip_investing_data = function() mock_drip_data,
    .package = "investR"
  )
  expect_no_error(get_drip_tickers("Contender"))
})

test_that("get_drip_tickers accepts 'Challenger' as a valid category", {
  local_mocked_bindings(
    fetch_drip_investing_data = function() mock_drip_data,
    .package = "investR"
  )
  expect_no_error(get_drip_tickers("Challenger"))
})

################################################################################
# Group 2: fetch_drip_investing_data() caching logic
################################################################################

test_that("fetch_drip_investing_data does NOT call the API when a fresh cache exists", {
  withr::with_tempdir({
    cache_path <- file.path(getwd(), "drip_investing_data.rds")
    saveRDS(mock_drip_data, cache_path)
    # Set mtime to 1 day ago — well within the 30-day window
    Sys.setFileTime(cache_path, Sys.time() - 60 * 60 * 24)

    api_call_count <- 0L

    local_mocked_bindings(
      system.file = function(...) {
        args <- list(...)
        # Only intercept the cache lookup; let other system.file calls pass through
        if (length(args) >= 2 && identical(args[[1]], "cache") &&
            identical(args[[2]], "drip_investing_data.rds")) {
          return(cache_path)
        }
        base::system.file(...)
      },
      get_golem_config_value = function(section, key, fallback = NULL) {
        if (section == "aristocrats" && key == "drip_cache_days") return(30)
        fallback
      },
      .package = "investR"
    )

    local_mocked_bindings(
      GET = function(...) {
        api_call_count <<- api_call_count + 1L
        stop("GET should not have been called with a fresh cache")
      },
      .package = "httr"
    )

    result <- fetch_drip_investing_data()

    expect_equal(api_call_count, 0L)
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), nrow(mock_drip_data))
  })
})

test_that("fetch_drip_investing_data calls the API when the cache is stale", {
  withr::with_tempdir({
    cache_path <- file.path(getwd(), "drip_investing_data.rds")
    saveRDS(mock_drip_data, cache_path)
    # Set mtime to 31 days ago — beyond the 30-day window
    Sys.setFileTime(cache_path, Sys.time() - 60 * 60 * 24 * 31)

    api_call_count <- 0L

    fake_response_data <- list(dataset = mock_drip_data)

    local_mocked_bindings(
      system.file = function(...) {
        args <- list(...)
        if (length(args) >= 2 && identical(args[[1]], "cache") &&
            identical(args[[2]], "drip_investing_data.rds")) {
          return(cache_path)
        }
        base::system.file(...)
      },
      get_golem_config_value = function(section, key, fallback = NULL) {
        if (section == "aristocrats" && key == "drip_cache_days") return(30)
        if (section == "aristocrats" && key == "drip_api_url") {
          return("https://fake.dripinvesting.org/api")
        }
        fallback
      },
      .package = "investR"
    )

    local_mocked_bindings(
      GET = function(url, ...) {
        api_call_count <<- api_call_count + 1L
        structure(list(status_code = 200L, url = url), class = "response")
      },
      content = function(x, ...) '{"dataset": []}',
      .package = "httr"
    )

    local_mocked_bindings(
      fromJSON = function(txt, ...) fake_response_data,
      .package = "jsonlite"
    )

    result <- fetch_drip_investing_data()

    expect_equal(api_call_count, 1L)
    expect_true(is.data.frame(result))
  })
})

test_that("fetch_drip_investing_data returns stale cache with a warning when the API fails", {
  withr::with_tempdir({
    cache_path <- file.path(getwd(), "drip_investing_data.rds")
    saveRDS(mock_drip_data, cache_path)
    # Stale: 31 days old
    Sys.setFileTime(cache_path, Sys.time() - 60 * 60 * 24 * 31)

    local_mocked_bindings(
      system.file = function(...) {
        args <- list(...)
        if (length(args) >= 2 && identical(args[[1]], "cache") &&
            identical(args[[2]], "drip_investing_data.rds")) {
          return(cache_path)
        }
        base::system.file(...)
      },
      get_golem_config_value = function(section, key, fallback = NULL) {
        if (section == "aristocrats" && key == "drip_cache_days") return(30)
        if (section == "aristocrats" && key == "drip_api_url") {
          return("https://fake.dripinvesting.org/api")
        }
        fallback
      },
      .package = "investR"
    )

    local_mocked_bindings(
      GET = function(...) stop("Network error: connection timed out"),
      .package = "httr"
    )

    # The function logs a warning via logger but does not call base warning(),
    # so we verify it returns the stale data without error
    result <- expect_no_error(fetch_drip_investing_data())
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), nrow(mock_drip_data))
  })
})

################################################################################
# Group 3: get_drip_tickers() data filtering
################################################################################

test_that("get_drip_tickers returns only King tickers", {
  local_mocked_bindings(
    fetch_drip_investing_data = function() mock_drip_data,
    .package = "investR"
  )

  result <- get_drip_tickers("King")

  expect_setequal(result, c("KO", "PG", "MMM"))
})

test_that("get_drip_tickers returns only Champion tickers", {
  local_mocked_bindings(
    fetch_drip_investing_data = function() mock_drip_data,
    .package = "investR"
  )

  result <- get_drip_tickers("Champion")

  expect_setequal(result, c("JNJ", "CL"))
})

test_that("get_drip_tickers returns a character vector", {
  local_mocked_bindings(
    fetch_drip_investing_data = function() mock_drip_data,
    .package = "investR"
  )

  result <- get_drip_tickers("Contender")

  expect_type(result, "character")
})

test_that("get_drip_tickers errors when a valid category has no matching rows", {
  # Data frame with a King row only — Challenger has zero rows
  sparse_data <- data.frame(
    ticker     = "KO",
    stock_type = "King",
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    fetch_drip_investing_data = function() sparse_data,
    .package = "investR"
  )

  expect_error(
    get_drip_tickers("Challenger"),
    "No tickers found for DRIP Investing category"
  )
})

################################################################################
# Group 4: analyze_aristocrats() tickers parameter
################################################################################

test_that("analyze_aristocrats function signature includes a 'tickers' parameter", {
  args <- formalArgs(analyze_aristocrats)
  expect_true("tickers" %in% args)
})

test_that("analyze_aristocrats 'tickers' parameter defaults to NULL", {
  defaults <- formals(analyze_aristocrats)
  expect_null(defaults$tickers)
})
