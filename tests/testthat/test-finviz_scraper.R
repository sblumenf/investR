# tests/testthat/test-finviz_scraper.R
# Tests for Finviz scraper functions in utils_custom_ticker_lists.R

library(testthat)
library(investR)

# Helper: clear the Finviz screened cache between tests to prevent interference
clear_finviz_cache <- function() {
  cache <- investR:::.finviz_screened_cache
  if (length(ls(envir = cache)) > 0) {
    rm(list = ls(envir = cache), envir = cache)
  }
}

################################################################################
# scrape_finviz_page
################################################################################

test_that("scrape_finviz_page returns a character vector of uppercase tickers", {
  # Build a minimal fake HTML document representing a single-page Finviz result
  fake_html_text <- '
    <html><body>
      <span id="screener-total">1-5 / 3</span>
      <table class="screener_table">
        <thead><tr><th>Ticker</th><th>Company</th></tr></thead>
        <tbody>
          <tr><td>aapl</td><td>Apple Inc.</td></tr>
          <tr><td>MSFT</td><td>Microsoft Corp.</td></tr>
          <tr><td>nvda</td><td>NVIDIA Corp.</td></tr>
        </tbody>
      </table>
    </body></html>
  '

  fake_parsed <- xml2::read_html(fake_html_text)

  fake_table <- data.frame(
    Ticker = c("aapl", "MSFT", "nvda"),
    Company = c("Apple Inc.", "Microsoft Corp.", "NVIDIA Corp."),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    read_html = function(url) fake_parsed,
    html_node = function(x, css) {
      if (inherits(css, "character") && css == "#screener-total") {
        rvest::html_node(fake_parsed, "#screener-total")
      } else {
        rvest::html_node(x, css)
      }
    },
    html_text = function(x, ...) "1-5 / 3",
    html_table = function(x, ...) fake_table,
    .package = "rvest"
  )

  result <- investR:::scrape_finviz_page("https://fake.finviz.com/screener.ashx?v=111")

  expect_type(result, "character")
  expect_setequal(result, c("AAPL", "MSFT", "NVDA"))
  # Verify all tickers are uppercase
  expect_true(all(result == toupper(result)))
})

test_that("scrape_finviz_page returns character(0) when total count is 0", {
  fake_html_text <- '
    <html><body>
      <span id="screener-total">0</span>
      <table class="screener_table"></table>
    </body></html>
  '
  fake_parsed <- xml2::read_html(fake_html_text)

  local_mocked_bindings(
    read_html = function(url) fake_parsed,
    html_node = function(x, css) rvest::html_node(fake_parsed, css),
    html_text = function(x, ...) "0",
    html_table = function(x, ...) data.frame(Ticker = character(0)),
    .package = "rvest"
  )

  result <- investR:::scrape_finviz_page("https://fake.finviz.com/screener.ashx?v=111")

  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("scrape_finviz_page returns character(0) when #screener-total is missing", {
  # Page with no screener-total element
  fake_html_text <- '<html><body><p>No results</p></body></html>'
  fake_parsed <- xml2::read_html(fake_html_text)

  local_mocked_bindings(
    read_html = function(url) fake_parsed,
    html_node = function(x, css) rvest::html_node(fake_parsed, css),
    html_text = function(x, ...) NA_character_,
    .package = "rvest"
  )

  result <- investR:::scrape_finviz_page("https://fake.finviz.com/screener.ashx?v=111")

  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("scrape_finviz_page handles errors from read_html gracefully", {
  local_mocked_bindings(
    read_html = function(url) stop("Connection refused"),
    .package = "rvest"
  )

  # The function does not internally catch errors from the first read_html call,
  # so the error should propagate to the caller (fetch_finviz_screened_tickers
  # wraps calls to scrape_finviz_page in tryCatch)
  expect_error(
    investR:::scrape_finviz_page("https://fake.finviz.com/screener.ashx?v=111"),
    "Connection refused"
  )
})

test_that("scrape_finviz_page deduplicates tickers from a single page", {
  fake_table <- data.frame(
    Ticker = c("AAPL", "MSFT", "aapl"),  # duplicate after uppercasing
    stringsAsFactors = FALSE
  )

  fake_html_text <- '<html><body><span id="screener-total">3</span></body></html>'
  fake_parsed <- xml2::read_html(fake_html_text)

  local_mocked_bindings(
    read_html = function(url) fake_parsed,
    html_node = function(x, css) rvest::html_node(fake_parsed, css),
    html_text = function(x, ...) "3",
    html_table = function(x, ...) fake_table,
    .package = "rvest"
  )

  result <- investR:::scrape_finviz_page("https://fake.finviz.com/screener.ashx?v=111")

  expect_type(result, "character")
  # unique() should remove the duplicate AAPL
  expect_equal(length(result), length(unique(toupper(result))))
  expect_true("AAPL" %in% result)
  expect_true("MSFT" %in% result)
})

################################################################################
# fetch_finviz_screened_tickers
################################################################################

test_that("fetch_finviz_screened_tickers deduplicates tickers from multiple URLs", {
  clear_finviz_cache()
  on.exit(clear_finviz_cache())

  local_mocked_bindings(
    get_golem_config_value = function(section, key, fallback = NULL) {
      if (section == "custom_ticker_lists" && key == "finviz_screener_urls") {
        return(list(
          "https://fake.finviz.com/screen1",
          "https://fake.finviz.com/screen2"
        ))
      }
      if (section == "custom_ticker_lists" && key == "ticker_cache_hours") {
        return(24)
      }
      fallback
    },
    scrape_finviz_page = function(url, delay = 0.3) {
      if (grepl("screen1", url)) c("AAPL", "TSLA")
      else if (grepl("screen2", url)) c("TSLA", "NVDA")
      else character(0)
    },
    .package = "investR"
  )

  result <- fetch_finviz_screened_tickers(force_refresh = TRUE)

  expect_type(result, "character")
  # All three unique tickers should be present
  expect_true("AAPL" %in% result)
  expect_true("TSLA" %in% result)
  expect_true("NVDA" %in% result)
  # Should be deduplicated — TSLA appears only once
  expect_equal(sum(result == "TSLA"), 1)
  # Total unique count
  expect_equal(length(result), 3)
})

test_that("fetch_finviz_screened_tickers returns character(0) when no URLs are configured", {
  clear_finviz_cache()
  on.exit(clear_finviz_cache())

  local_mocked_bindings(
    get_golem_config_value = function(section, key, fallback = NULL) {
      if (section == "custom_ticker_lists" && key == "finviz_screener_urls") {
        return(list())
      }
      if (section == "custom_ticker_lists" && key == "ticker_cache_hours") {
        return(24)
      }
      fallback
    },
    .package = "investR"
  )

  result <- fetch_finviz_screened_tickers(force_refresh = TRUE)

  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("fetch_finviz_screened_tickers caches results and skips re-scraping", {
  clear_finviz_cache()
  on.exit(clear_finviz_cache())

  scrape_call_count <- 0L

  local_mocked_bindings(
    get_golem_config_value = function(section, key, fallback = NULL) {
      if (section == "custom_ticker_lists" && key == "finviz_screener_urls") {
        return(list("https://fake.finviz.com/screen1"))
      }
      if (section == "custom_ticker_lists" && key == "ticker_cache_hours") {
        return(24)
      }
      fallback
    },
    scrape_finviz_page = function(url, delay = 0.3) {
      scrape_call_count <<- scrape_call_count + 1L
      c("AAPL", "MSFT")
    },
    .package = "investR"
  )

  # First call — should hit the scraper
  result1 <- fetch_finviz_screened_tickers(force_refresh = TRUE)
  expect_equal(scrape_call_count, 1L)

  # Second call without force_refresh — cache is fresh, scraper should NOT be called again
  result2 <- fetch_finviz_screened_tickers(force_refresh = FALSE)
  expect_equal(scrape_call_count, 1L)

  # Both calls should return the same tickers
  expect_setequal(result1, result2)
})

test_that("fetch_finviz_screened_tickers force_refresh bypasses cache", {
  clear_finviz_cache()
  on.exit(clear_finviz_cache())

  scrape_call_count <- 0L

  local_mocked_bindings(
    get_golem_config_value = function(section, key, fallback = NULL) {
      if (section == "custom_ticker_lists" && key == "finviz_screener_urls") {
        return(list("https://fake.finviz.com/screen1"))
      }
      if (section == "custom_ticker_lists" && key == "ticker_cache_hours") {
        return(24)
      }
      fallback
    },
    scrape_finviz_page = function(url, delay = 0.3) {
      scrape_call_count <<- scrape_call_count + 1L
      c("AAPL", "MSFT")
    },
    .package = "investR"
  )

  fetch_finviz_screened_tickers(force_refresh = TRUE)
  expect_equal(scrape_call_count, 1L)

  # force_refresh = TRUE should ignore the populated cache and scrape again
  fetch_finviz_screened_tickers(force_refresh = TRUE)
  expect_equal(scrape_call_count, 2L)
})

test_that("fetch_finviz_screened_tickers returns character(0) when all scrapers fail", {
  clear_finviz_cache()
  on.exit(clear_finviz_cache())

  local_mocked_bindings(
    get_golem_config_value = function(section, key, fallback = NULL) {
      if (section == "custom_ticker_lists" && key == "finviz_screener_urls") {
        return(list(
          "https://fake.finviz.com/screen1",
          "https://fake.finviz.com/screen2"
        ))
      }
      if (section == "custom_ticker_lists" && key == "ticker_cache_hours") {
        return(24)
      }
      fallback
    },
    scrape_finviz_page = function(url, delay = 0.3) {
      stop("Network error")
    },
    .package = "investR"
  )

  result <- fetch_finviz_screened_tickers(force_refresh = TRUE)

  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("fetch_finviz_screened_tickers returns tickers as uppercase", {
  clear_finviz_cache()
  on.exit(clear_finviz_cache())

  local_mocked_bindings(
    get_golem_config_value = function(section, key, fallback = NULL) {
      if (section == "custom_ticker_lists" && key == "finviz_screener_urls") {
        return(list("https://fake.finviz.com/screen1"))
      }
      if (section == "custom_ticker_lists" && key == "ticker_cache_hours") {
        return(24)
      }
      fallback
    },
    scrape_finviz_page = function(url, delay = 0.3) {
      c("aapl", "msft", "NVDA")
    },
    .package = "investR"
  )

  result <- fetch_finviz_screened_tickers(force_refresh = TRUE)

  expect_type(result, "character")
  expect_true(all(result == toupper(result)))
  expect_true("AAPL" %in% result)
  expect_true("MSFT" %in% result)
  expect_true("NVDA" %in% result)
})
