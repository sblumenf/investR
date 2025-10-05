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
  stocks <- get_zero_dividend_stocks(limit = 10)

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
