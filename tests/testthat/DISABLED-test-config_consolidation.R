test_that("aristocrats config loads shared constants from golem-config.yml", {
  skip("Config consolidation feature not yet implemented")
  config <- get_config()

  # Test shared constant
  expect_equal(config$days_per_year, 365)

  # Test that config is a list
  expect_true(is.list(config))

  # Test that config has expected keys
  expect_true("days_per_year" %in% names(config))
  expect_true("max_workers" %in% names(config))
  expect_true("shares_per_contract" %in% names(config))
})

test_that("weekly dividend config loads shared constants from golem-config.yml", {
  config <- get_dividend_capture_config()

  # Test shared constant
  expect_equal(config$days_per_year, 365)
  expect_equal(config$trading_days_per_year, 252)

  # Test weekly-specific constants
  expect_equal(config$weeks_per_year, 52)
  expect_equal(config$investment_amount, 10000)

  # Test that config is a list
  expect_true(is.list(config))
})

test_that("monthly dividend config loads shared constants from golem-config.yml", {
  config <- get_dividend_capture_monthly_config()

  # Test shared constant
  expect_equal(config$days_per_year, 365)
  expect_equal(config$trading_days_per_year, 252)

  # Test monthly-specific constants
  expect_equal(config$months_per_year, 12)
  expect_equal(config$investment_amount, 10000)

  # Test that config is a list
  expect_true(is.list(config))
})

test_that("all configs have consistent shared values", {
  aristocrats_config <- get_config()
  weekly_config <- get_dividend_capture_config()
  monthly_config <- get_dividend_capture_monthly_config()

  # All should have the same days_per_year (shared constant)
  expect_equal(aristocrats_config$days_per_year, 365)
  expect_equal(weekly_config$days_per_year, 365)
  expect_equal(monthly_config$days_per_year, 365)

  # Weekly and monthly should have same trading_days_per_year
  expect_equal(weekly_config$trading_days_per_year, 252)
  expect_equal(monthly_config$trading_days_per_year, 252)
})

test_that("aristocrats config has strategy-specific values", {
  config <- get_config()

  # Strategy-specific values
  expect_equal(config$strike_threshold_pct, 0.8)
  expect_equal(config$max_workers, 10)  # Aristocrats uses 10, not default 4
  expect_equal(config$shares_per_contract, 100)
  expect_equal(config$history_years, 5)

  # URLs should still be defined (not in golem-config.yml)
  expect_true("urls" %in% names(config))
  expect_true(is.list(config$urls))
  expect_true("stockanalysis" %in% names(config$urls))
})

test_that("weekly config has strategy-specific values", {
  config <- get_dividend_capture_config()

  # Weekly-specific
  expect_equal(config$weeks_per_year, 52)
  expect_equal(config$max_workers, 4)  # Weekly uses 4

  # day_map should still be defined (not in golem-config.yml)
  expect_true("day_map" %in% names(config))
  expect_true(is.list(config$day_map))
  expect_equal(config$day_map$Monday, "Friday")
})

test_that("monthly config has strategy-specific values", {
  config <- get_dividend_capture_monthly_config()

  # Monthly-specific
  expect_equal(config$months_per_year, 12)
  expect_equal(config$max_workers, 4)  # Monthly uses 4

  # schedule_types should still be defined (not in golem-config.yml)
  expect_true("schedule_types" %in% names(config))
  expect_true(is.character(config$schedule_types))

  # days_since_dividend_ranges should still be defined
  expect_true("days_since_dividend_ranges" %in% names(config))
  expect_true(is.character(config$days_since_dividend_ranges))
})

test_that("get_config accessor function works with keys", {
  # Test getting specific key
  days <- get_config("days_per_year")
  expect_equal(days, 365)

  max_workers <- get_config("max_workers")
  expect_equal(max_workers, 10)

  # Test getting entire config
  full_config <- get_config()
  expect_true(is.list(full_config))
  expect_true(length(full_config) > 5)
})

test_that("get_config throws error for invalid key", {
  expect_error(
    get_config("nonexistent_key"),
    "Configuration key 'nonexistent_key' not found"
  )
})

test_that("weekly config accessor function works with keys", {
  # Test getting specific key
  weeks <- get_dividend_capture_config("weeks_per_year")
  expect_equal(weeks, 52)

  # Test getting entire config
  full_config <- get_dividend_capture_config()
  expect_true(is.list(full_config))
})

test_that("monthly config accessor function works with keys", {
  # Test getting specific key
  months <- get_dividend_capture_monthly_config("months_per_year")
  expect_equal(months, 12)

  # Test getting entire config
  full_config <- get_dividend_capture_monthly_config()
  expect_true(is.list(full_config))
})

test_that("config validation still works", {
  # validate_config should run without error for valid config
  expect_true(validate_config())

  # Test with invalid config
  bad_config <- list(
    strike_threshold_pct = 1.5,  # > 1, invalid
    max_workers = 1,
    min_aristocrats = 50,
    history_years = 5
  )

  expect_error(
    validate_config(bad_config),
    "strike_threshold_pct must be between 0 and 1"
  )
})

test_that("config variables are properly defined", {
  # Test that config variables exist and are accessible

  # Risk analysis CONFIG (renamed from CONFIG to RISK_CONFIG)
  expect_true(exists("RISK_CONFIG", envir = asNamespace("investR")))

  # Weekly DIVIDEND_CAPTURE_CONFIG
  expect_true(exists("DIVIDEND_CAPTURE_CONFIG", envir = asNamespace("investR")))

  # Monthly DIVIDEND_CAPTURE_MONTHLY_CONFIG
  expect_true(exists("DIVIDEND_CAPTURE_MONTHLY_CONFIG", envir = asNamespace("investR")))
})
