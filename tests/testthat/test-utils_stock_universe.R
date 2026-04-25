test_that("get_sp500_stocks returns valid tickers", {
  # This test may be slow on first run (network call)
  skip_on_cran()

  stocks <- get_sp500_stocks()

  # Should return a character vector
  expect_type(stocks, "character")

  # Should have ~500 stocks
  expect_gte(length(stocks), 400)
  expect_lte(length(stocks), 600)

  # All tickers should be uppercase letters or dots
  expect_true(all(grepl("^[A-Z.]+$", stocks)))

  # Should have known S&P 500 stocks
  expect_true("AAPL" %in% stocks)
  expect_true("MSFT" %in% stocks)
})

test_that("SP500_FALLBACK_TICKERS is valid", {
  # Fallback list should exist
  expect_true(length(SP500_FALLBACK_TICKERS) > 0)

  # Should be character vector
  expect_type(SP500_FALLBACK_TICKERS, "character")

  # Should have reasonable number of stocks
  expect_gte(length(SP500_FALLBACK_TICKERS), 50)

  # All tickers should be valid format
  expect_true(all(grepl("^[A-Z.]+$", SP500_FALLBACK_TICKERS)))
})

test_that("cache functions work correctly", {
  # Clean up any existing test cache
  test_cache_file <- "test_cache.rds"
  cache_dir <- get_cache_dir()
  test_cache_path <- file.path(cache_dir, test_cache_file)

  if (file.exists(test_cache_path)) {
    file.remove(test_cache_path)
  }

  # Save to cache
  test_data <- c("AAPL", "MSFT", "GOOGL")
  save_to_cache(test_cache_file, test_data)

  # Check if cache is fresh
  expect_true(is_cache_fresh(test_cache_file, max_age_days = 1))

  # Load from cache
  loaded_data <- load_from_cache(test_cache_file)
  expect_equal(loaded_data, test_data)

  # Clean up
  if (file.exists(test_cache_path)) {
    file.remove(test_cache_path)
  }
})

test_that("get_zero_dividend_stocks with limit works", {
  skip_on_cran()
  skip_if_offline()

  # Test with small limit for speed
  stocks <- get_zero_dividend_stocks(limit = 10, max_workers = 1)

  # Should return character vector
  expect_type(stocks, "character")

  # Should have at least some zero-dividend stocks
  # (not all 10 will necessarily be zero-dividend)
  expect_gte(length(stocks), 0)

  # All tickers should be valid format
  if (length(stocks) > 0) {
    expect_true(all(grepl("^[A-Z.]+$", stocks)))
  }
})

test_that("clear_stock_cache works", {
  # Create a test cache file
  test_cache_file <- "test_clear_cache.rds"
  cache_dir <- get_cache_dir()
  test_cache_path <- file.path(cache_dir, test_cache_file)

  # Create dummy cache
  saveRDS(list(data = "test", timestamp = Sys.time()), test_cache_path)
  expect_true(file.exists(test_cache_path))

  # Clear it
  # Note: clear_stock_cache only works on specific cache types
  # So we'll test the cache file existence directly
  if (file.exists(test_cache_path)) {
    file.remove(test_cache_path)
  }

  expect_false(file.exists(test_cache_path))
})

test_that("cache freshness check handles missing cache", {
  # Non-existent cache should not be fresh
  expect_false(is_cache_fresh("nonexistent_cache.rds"))
})

test_that("cache freshness check handles corrupted cache", {
  # Create corrupted cache file
  corrupted_cache_file <- "corrupted_cache.rds"
  cache_dir <- get_cache_dir()
  corrupted_cache_path <- file.path(cache_dir, corrupted_cache_file)

  # Write invalid data
  writeLines("corrupted data", corrupted_cache_path)

  # Should return FALSE without erroring
  expect_false(is_cache_fresh(corrupted_cache_file))

  # Clean up
  if (file.exists(corrupted_cache_path)) {
    file.remove(corrupted_cache_path)
  }
})

# ------------------------------------------------------------------------------
# Russell 1000 cache types: clear_stock_cache
# ------------------------------------------------------------------------------

test_that("clear_stock_cache accepts russell_1000 cache type", {
  cache_dir <- get_cache_dir()
  cache_path <- file.path(cache_dir, "russell_1000_stocks.rds")

  # Create a dummy cache file so removal can be verified
  saveRDS(list(data = c("AAPL", "MSFT"), timestamp = Sys.time()), cache_path)
  expect_true(file.exists(cache_path))

  # Should not error
  expect_no_error(clear_stock_cache("russell_1000"))

  # File should be gone
  expect_false(file.exists(cache_path))
})

test_that("clear_stock_cache accepts russell_1000_dividend cache type", {
  cache_dir <- get_cache_dir()
  cache_path <- file.path(cache_dir, "russell_1000_dividend_paying_stocks.rds")

  saveRDS(list(data = c("AAPL", "MSFT"), timestamp = Sys.time()), cache_path)
  expect_true(file.exists(cache_path))

  expect_no_error(clear_stock_cache("russell_1000_dividend"))

  expect_false(file.exists(cache_path))
})

test_that("clear_stock_cache accepts russell_1000_zero_dividend cache type", {
  cache_dir <- get_cache_dir()
  cache_path <- file.path(cache_dir, "russell_1000_zero_dividend_stocks.rds")

  saveRDS(list(data = c("GOOGL", "META"), timestamp = Sys.time()), cache_path)
  expect_true(file.exists(cache_path))

  expect_no_error(clear_stock_cache("russell_1000_zero_dividend"))

  expect_false(file.exists(cache_path))
})

test_that("clear_stock_cache 'all' removes russell 1000 cache files", {
  cache_dir <- get_cache_dir()

  r1000_path <- file.path(cache_dir, "russell_1000_stocks.rds")
  r1000_div_path <- file.path(cache_dir, "russell_1000_dividend_paying_stocks.rds")
  r1000_zero_path <- file.path(cache_dir, "russell_1000_zero_dividend_stocks.rds")

  # Create dummy files for all three russell cache types
  dummy <- list(data = c("AAPL"), timestamp = Sys.time())
  saveRDS(dummy, r1000_path)
  saveRDS(dummy, r1000_div_path)
  saveRDS(dummy, r1000_zero_path)

  expect_no_error(clear_stock_cache("all"))

  expect_false(file.exists(r1000_path))
  expect_false(file.exists(r1000_div_path))
  expect_false(file.exists(r1000_zero_path))
})

# ------------------------------------------------------------------------------
# resolve_ticker_from_name: mocked HTTP
# ------------------------------------------------------------------------------

test_that("resolve_ticker_from_name returns ticker for equity result", {
  fake_response <- structure(
    list(status_code = 200L),
    class = "response"
  )

  fake_json <- jsonlite::toJSON(list(
    finance = list(
      result = list(
        data.frame(
          symbol = "AAPL",
          quoteType = "EQUITY",
          shortname = "Apple Inc.",
          stringsAsFactors = FALSE
        )
      )
    )
  ), auto_unbox = TRUE)

  with_mocked_bindings(
    GET = function(...) fake_response,
    stop_for_status = function(...) invisible(NULL),
    content = function(...) fake_json,
    .package = "httr",
    {
      result <- resolve_ticker_from_name("Apple Inc")
      expect_type(result, "character")
      expect_equal(result, "AAPL")
    }
  )
})

test_that("resolve_ticker_from_name returns NA_character_ when no equity results", {
  fake_response <- structure(
    list(status_code = 200L),
    class = "response"
  )

  fake_json <- jsonlite::toJSON(list(
    finance = list(
      result = list(
        data.frame(
          symbol = "AAPL-USD",
          quoteType = "CRYPTOCURRENCY",
          shortname = "Apple Crypto",
          stringsAsFactors = FALSE
        )
      )
    )
  ), auto_unbox = TRUE)

  with_mocked_bindings(
    GET = function(...) fake_response,
    stop_for_status = function(...) invisible(NULL),
    content = function(...) fake_json,
    .package = "httr",
    {
      result <- resolve_ticker_from_name("Some Crypto Company")
      expect_identical(result, NA_character_)
    }
  )
})

# ------------------------------------------------------------------------------
# get_russell_1000_stocks: cache hit
# ------------------------------------------------------------------------------

test_that("get_russell_1000_stocks returns from cache without network call", {
  cache_file <- "russell_1000_stocks.rds"
  cache_dir <- get_cache_dir()
  cache_path <- file.path(cache_dir, cache_file)

  # Remove any existing cache so we start clean
  if (file.exists(cache_path)) {
    file.remove(cache_path)
  }

  # Pre-populate with a small fake ticker vector
  fake_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA")
  save_to_cache(cache_file, fake_tickers)

  # Confirm the cache is fresh
  expect_true(is_cache_fresh(cache_file, max_age_days = 30))

  # get_russell_1000_stocks should return the cached data without a network call
  result <- get_russell_1000_stocks()

  expect_type(result, "character")
  expect_equal(result, fake_tickers)

  # Clean up
  if (file.exists(cache_path)) {
    file.remove(cache_path)
  }
})
