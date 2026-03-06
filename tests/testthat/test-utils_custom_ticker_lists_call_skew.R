test_that("finviz_call_skew_div_urls config has 45 URLs all containing fa_div_pos", {
  urls <- get_golem_config_value("custom_ticker_lists", "finviz_call_skew_div_urls", list())
  expect_equal(length(urls), 45)
  expect_true(all(grepl("fa_div_pos", urls)))
})

test_that("finviz_call_skew_nodiv_urls config has 45 URLs all containing fa_div_none", {
  urls <- get_golem_config_value("custom_ticker_lists", "finviz_call_skew_nodiv_urls", list())
  expect_equal(length(urls), 45)
  expect_true(all(grepl("fa_div_none", urls)))
})

test_that("fetch_finviz_call_skew_div_tickers returns character vector when HTTP mocked", {
  local_mocked_bindings(
    scrape_finviz_page = function(url, delay = 0.3) c("AAPL", "MSFT", "GOOG"),
    .env = asNamespace("investR")
  )

  # Force cache clear
  rm(list = ls(envir = investR:::.finviz_call_skew_div_cache),
     envir = investR:::.finviz_call_skew_div_cache)

  result <- fetch_finviz_call_skew_div_tickers(force_refresh = TRUE)
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("fetch_finviz_call_skew_nodiv_tickers returns character vector when HTTP mocked", {
  local_mocked_bindings(
    scrape_finviz_page = function(url, delay = 0.3) c("TSLA", "NVDA", "AMD"),
    .env = asNamespace("investR")
  )

  rm(list = ls(envir = investR:::.finviz_call_skew_nodiv_cache),
     envir = investR:::.finviz_call_skew_nodiv_cache)

  result <- fetch_finviz_call_skew_nodiv_tickers(force_refresh = TRUE)
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("fetch_finviz_call_skew_div_tickers deduplicates tickers across URLs", {
  local_mocked_bindings(
    scrape_finviz_page = function(url, delay = 0.3) c("AAPL", "MSFT", "AAPL"),
    .env = asNamespace("investR")
  )

  rm(list = ls(envir = investR:::.finviz_call_skew_div_cache),
     envir = investR:::.finviz_call_skew_div_cache)

  result <- fetch_finviz_call_skew_div_tickers(force_refresh = TRUE)
  expect_equal(length(result), length(unique(result)))
})

test_that("fetch_finviz_call_skew_div_tickers uses cache on second call", {
  call_count <- 0L
  local_mocked_bindings(
    scrape_finviz_page = function(url, delay = 0.3) {
      call_count <<- call_count + 1L
      c("AAPL", "MSFT")
    },
    .env = asNamespace("investR")
  )

  rm(list = ls(envir = investR:::.finviz_call_skew_div_cache),
     envir = investR:::.finviz_call_skew_div_cache)

  fetch_finviz_call_skew_div_tickers(force_refresh = TRUE)
  calls_after_first <- call_count

  fetch_finviz_call_skew_div_tickers()
  expect_equal(call_count, calls_after_first)
})

test_that("fetch_finviz_call_skew_nodiv_tickers uses cache on second call", {
  call_count <- 0L
  local_mocked_bindings(
    scrape_finviz_page = function(url, delay = 0.3) {
      call_count <<- call_count + 1L
      c("TSLA", "NVDA")
    },
    .env = asNamespace("investR")
  )

  rm(list = ls(envir = investR:::.finviz_call_skew_nodiv_cache),
     envir = investR:::.finviz_call_skew_nodiv_cache)

  fetch_finviz_call_skew_nodiv_tickers(force_refresh = TRUE)
  calls_after_first <- call_count

  fetch_finviz_call_skew_nodiv_tickers()
  expect_equal(call_count, calls_after_first)
})
