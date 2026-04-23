# tests/testthat/test-utils_monthly_dividend_scraper.R
# Tests for fetch_monthly_dividend_stocks()

library(testthat)
library(investR)
library(tibble)
library(dplyr)

# Reset the monthly dividend cache before each test so tests are isolated
reset_monthly_cache <- function() {
  ns <- asNamespace("investR")
  cache_env <- base::get(".monthly_dividend_cache", envir = ns, inherits = FALSE)
  if (exists("data",      envir = cache_env)) rm("data",      envir = cache_env)
  if (exists("timestamp", envir = cache_env)) rm("timestamp", envir = cache_env)
}

################################################################################
# EXPORT AND STRUCTURE
################################################################################

test_that("fetch_monthly_dividend_stocks is exported", {
  expect_true(exists("fetch_monthly_dividend_stocks", where = asNamespace("investR")))
  expect_true(is.function(fetch_monthly_dividend_stocks))
})

test_that("fetch_monthly_dividend_stocks returns a tibble on empty scrape", {
  reset_monthly_cache()
  with_mocked_bindings(
    scrape_monthly_dividend_stocks = function() tibble::tibble(),
    {
      result <- fetch_monthly_dividend_stocks(force_refresh = TRUE)
      expect_s3_class(result, "tbl_df")
    }
  )
})

################################################################################
# COLUMN STRUCTURE
################################################################################

test_that("result has expected columns when data is available", {
  reset_monthly_cache()
  mock_data <- tibble::tibble(
    ticker         = c("O", "MAIN"),
    company_name   = c("Realty Income", "Main Street Capital"),
    dividend_yield = c("5.13%", "7.99%"),
    market_cap     = c("59.23B", "4.92B")
  )

  with_mocked_bindings(
    scrape_monthly_dividend_stocks = function() mock_data,
    {
      result <- fetch_monthly_dividend_stocks(force_refresh = TRUE)
      expect_true(all(c("ticker", "company_name", "dividend_yield", "market_cap") %in% names(result)))
    }
  )
})

test_that("result rows match mock data", {
  reset_monthly_cache()
  mock_data <- tibble::tibble(
    ticker         = c("O", "MAIN", "AGNC"),
    company_name   = c("Realty Income", "Main Street Capital", "AGNC Investment"),
    dividend_yield = c("5.13%", "7.99%", "13.20%"),
    market_cap     = c("59.23B", "4.92B", "12.52B")
  )

  with_mocked_bindings(
    scrape_monthly_dividend_stocks = function() mock_data,
    {
      result <- fetch_monthly_dividend_stocks(force_refresh = TRUE)
      expect_equal(nrow(result), 3L)
      expect_true(all(c("O", "MAIN", "AGNC") %in% result$ticker))
    }
  )
})

################################################################################
# SCRAPING LOGIC — parse HTML directly (no network mock needed)
################################################################################

make_html_page <- function(tickers, companies, yields, mktcaps) {
  rows <- paste0(
    "<tr><td>", tickers, "</td><td>", companies, "</td><td>",
    yields, "</td><td>", mktcaps, "</td></tr>",
    collapse = ""
  )
  paste0(
    "<html><body><table>",
    "<thead><tr><th>Symbol</th><th>Company</th><th>Div. Yield</th><th>Market Cap</th></tr></thead>",
    "<tbody>", rows, "</tbody></table></body></html>"
  )
}

test_that("scrape_monthly_dividend_stocks parses HTML table structure correctly", {
  html <- make_html_page(
    tickers   = c("O", "MAIN"),
    companies = c("Realty Income", "Main Street Capital"),
    yields    = c("5.13%", "7.99%"),
    mktcaps   = c("59.23B", "4.92B")
  )
  result <- scrape_monthly_dividend_stocks.__test_parse(html)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ticker", "company_name", "dividend_yield", "market_cap") %in% names(result)))
  expect_equal(nrow(result), 2L)
  expect_true(all(c("O", "MAIN") %in% result$ticker))
})

test_that("scrape_monthly_dividend_stocks uppercases tickers", {
  html <- make_html_page("o", "Realty Income", "5%", "59B")
  result <- scrape_monthly_dividend_stocks.__test_parse(html)
  expect_equal(result$ticker, "O")
})

test_that("scrape_monthly_dividend_stocks filters out oversized ticker strings", {
  html <- make_html_page(
    tickers   = c("O", "TOOLONGTICKER"),
    companies = c("Realty Income", "Bad Entry"),
    yields    = c("5%", "0%"),
    mktcaps   = c("59B", "0")
  )
  result <- scrape_monthly_dividend_stocks.__test_parse(html)
  expect_false("TOOLONGTICKER" %in% result$ticker)
  expect_true("O" %in% result$ticker)
})

test_that("scrape_monthly_dividend_stocks returns empty tibble when no table found", {
  html <- "<html><body><p>No table here</p></body></html>"
  result <- scrape_monthly_dividend_stocks.__test_parse(html)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

################################################################################
# CACHING
################################################################################

test_that("fetch_monthly_dividend_stocks uses cache on second call", {
  reset_monthly_cache()
  mock_data <- tibble::tibble(
    ticker = "O", company_name = "Realty Income",
    dividend_yield = "5.13%", market_cap = "59B"
  )
  call_count <- 0L

  with_mocked_bindings(
    scrape_monthly_dividend_stocks = function() {
      call_count <<- call_count + 1L
      mock_data
    },
    {
      fetch_monthly_dividend_stocks(force_refresh = TRUE)  # populates cache
      fetch_monthly_dividend_stocks()                      # should use cache
      expect_equal(call_count, 1L)
    }
  )
})

test_that("force_refresh bypasses cache", {
  reset_monthly_cache()
  mock_data <- tibble::tibble(
    ticker = "O", company_name = "Realty Income",
    dividend_yield = "5.13%", market_cap = "59B"
  )
  call_count <- 0L

  with_mocked_bindings(
    scrape_monthly_dividend_stocks = function() {
      call_count <<- call_count + 1L
      mock_data
    },
    {
      fetch_monthly_dividend_stocks(force_refresh = TRUE)
      fetch_monthly_dividend_stocks(force_refresh = TRUE)
      expect_equal(call_count, 2L)
    }
  )
})

################################################################################
# ERROR HANDLING
################################################################################

test_that("fetch_monthly_dividend_stocks returns empty tibble on network error", {
  reset_monthly_cache()
  with_mocked_bindings(
    scrape_monthly_dividend_stocks = function() stop("network error"),
    {
      result <- fetch_monthly_dividend_stocks(force_refresh = TRUE)
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0L)
    }
  )
})

test_that("fetch_monthly_dividend_stocks returns empty tibble when scraper returns empty", {
  reset_monthly_cache()
  with_mocked_bindings(
    scrape_monthly_dividend_stocks = function() tibble::tibble(),
    {
      result <- fetch_monthly_dividend_stocks(force_refresh = TRUE)
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0L)
    }
  )
})

################################################################################
# LIVE INTEGRATION (skipped on CI / CRAN)
################################################################################

test_that("fetch_monthly_dividend_stocks returns real data from stockanalysis.com", {
  skip_on_cran()
  skip_if_offline()

  reset_monthly_cache()
  result <- fetch_monthly_dividend_stocks(force_refresh = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 10)
  expect_true(all(c("ticker", "company_name", "dividend_yield", "market_cap") %in% names(result)))
  expect_true("O" %in% result$ticker)
})
