# tests/testthat/test-utils_cash_secured_puts_config.R
# Configuration tests for cash-secured puts strategy
# Date: 2025-11-23

library(testthat)
library(investR)

test_that("CASH_SECURED_PUTS_CONFIG has all required fields", {
  required_fields <- c(
    "strike_threshold_pct",
    "min_days",
    "max_days",
    "max_workers",
    "min_option_bid",
    "min_open_interest",
    "max_stock_price",
    "shares_per_contract",
    "days_per_year",
    "history_years",
    "short_expiry_warning_days",
    "negative_return_threshold",
    "default_top_n",
    "output_dir"
  )

  for (field in required_fields) {
    expect_true(field %in% names(CASH_SECURED_PUTS_CONFIG),
               info = sprintf("Missing required field: %s", field))
  }
})

test_that("CASH_SECURED_PUTS_CONFIG has sensible default values", {
  expect_equal(CASH_SECURED_PUTS_CONFIG$strike_threshold_pct, 0.95)
  expect_equal(CASH_SECURED_PUTS_CONFIG$min_days, 45)
  expect_equal(CASH_SECURED_PUTS_CONFIG$max_days, 120)
  expect_equal(CASH_SECURED_PUTS_CONFIG$max_workers, 10)
  expect_equal(CASH_SECURED_PUTS_CONFIG$min_option_bid, 0.01)
  expect_equal(CASH_SECURED_PUTS_CONFIG$min_open_interest, 10)
  expect_equal(CASH_SECURED_PUTS_CONFIG$max_stock_price, 250)
  expect_equal(CASH_SECURED_PUTS_CONFIG$shares_per_contract, 100)
  expect_equal(CASH_SECURED_PUTS_CONFIG$days_per_year, 365)
  expect_equal(CASH_SECURED_PUTS_CONFIG$history_years, 5)
  expect_equal(CASH_SECURED_PUTS_CONFIG$short_expiry_warning_days, 14)
  expect_equal(CASH_SECURED_PUTS_CONFIG$negative_return_threshold, 0)
  expect_equal(CASH_SECURED_PUTS_CONFIG$default_top_n, 10)
  expect_equal(CASH_SECURED_PUTS_CONFIG$output_dir, "strategies")
})

test_that("validate_puts_config accepts valid default configuration", {
  expect_true(validate_puts_config(CASH_SECURED_PUTS_CONFIG))
})

test_that("validate_puts_config rejects strike_threshold_pct too high", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$strike_threshold_pct <- 1.5
  expect_error(validate_puts_config(bad_config), "strike_threshold_pct")
})

test_that("validate_puts_config rejects strike_threshold_pct too low", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$strike_threshold_pct <- 0.3
  expect_error(validate_puts_config(bad_config), "strike_threshold_pct")
})

test_that("validate_puts_config rejects negative strike_threshold_pct", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$strike_threshold_pct <- -0.5
  expect_error(validate_puts_config(bad_config), "strike_threshold_pct")
})

test_that("validate_puts_config accepts valid strike_threshold_pct range", {
  # Test boundary values
  config <- CASH_SECURED_PUTS_CONFIG
  config$strike_threshold_pct <- 0.5
  expect_true(validate_puts_config(config))

  config$strike_threshold_pct <- 1.0
  expect_true(validate_puts_config(config))

  config$strike_threshold_pct <- 0.75
  expect_true(validate_puts_config(config))
})

test_that("validate_puts_config rejects negative min_days", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$min_days <- -10
  expect_error(validate_puts_config(bad_config), "min_days")
})

test_that("validate_puts_config rejects max_days less than min_days", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$min_days <- 100
  bad_config$max_days <- 50
  expect_error(validate_puts_config(bad_config), "max_days")
})

test_that("validate_puts_config accepts equal min_days and max_days", {
  config <- CASH_SECURED_PUTS_CONFIG
  config$min_days <- 60
  config$max_days <- 60
  expect_true(validate_puts_config(config))
})

test_that("validate_puts_config rejects too many workers", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$max_workers <- 100
  expect_error(validate_puts_config(bad_config), "max_workers")
})

test_that("validate_puts_config rejects zero or negative workers", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$max_workers <- 0
  expect_error(validate_puts_config(bad_config), "max_workers")

  bad_config$max_workers <- -5
  expect_error(validate_puts_config(bad_config), "max_workers")
})

test_that("validate_puts_config accepts valid worker range", {
  config <- CASH_SECURED_PUTS_CONFIG
  config$max_workers <- 1
  expect_true(validate_puts_config(config))

  config$max_workers <- 50
  expect_true(validate_puts_config(config))

  config$max_workers <- 25
  expect_true(validate_puts_config(config))
})

test_that("validate_puts_config rejects negative min_option_bid", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$min_option_bid <- -0.01
  expect_error(validate_puts_config(bad_config), "min_option_bid")
})

test_that("validate_puts_config accepts zero min_option_bid", {
  config <- CASH_SECURED_PUTS_CONFIG
  config$min_option_bid <- 0
  expect_true(validate_puts_config(config))
})

test_that("validate_puts_config rejects negative min_open_interest", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$min_open_interest <- -10
  expect_error(validate_puts_config(bad_config), "min_open_interest")
})

test_that("validate_puts_config accepts zero min_open_interest", {
  config <- CASH_SECURED_PUTS_CONFIG
  config$min_open_interest <- 0
  expect_true(validate_puts_config(config))
})

test_that("validate_puts_config rejects zero or negative max_stock_price", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$max_stock_price <- 0
  expect_error(validate_puts_config(bad_config), "max_stock_price")

  bad_config$max_stock_price <- -100
  expect_error(validate_puts_config(bad_config), "max_stock_price")
})

test_that("validate_puts_config rejects shares_per_contract not equal to 100", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$shares_per_contract <- 50
  expect_error(validate_puts_config(bad_config), "shares_per_contract")

  bad_config$shares_per_contract <- 200
  expect_error(validate_puts_config(bad_config), "shares_per_contract")
})

test_that("validate_puts_config rejects zero or negative days_per_year", {
  bad_config <- CASH_SECURED_PUTS_CONFIG
  bad_config$days_per_year <- 0
  expect_error(validate_puts_config(bad_config), "days_per_year")

  bad_config$days_per_year <- -365
  expect_error(validate_puts_config(bad_config), "days_per_year")
})

test_that("get_puts_config returns default configuration", {
  config <- get_puts_config()
  expect_equal(config, CASH_SECURED_PUTS_CONFIG)
})

test_that("get_puts_config accepts single override", {
  config <- get_puts_config(strike_threshold_pct = 0.90)
  expect_equal(config$strike_threshold_pct, 0.90)
  # Other values should remain default
  expect_equal(config$min_days, CASH_SECURED_PUTS_CONFIG$min_days)
  expect_equal(config$max_days, CASH_SECURED_PUTS_CONFIG$max_days)
})

test_that("get_puts_config accepts multiple overrides", {
  config <- get_puts_config(
    strike_threshold_pct = 0.90,
    max_workers = 4,
    min_days = 30,
    max_days = 90
  )
  expect_equal(config$strike_threshold_pct, 0.90)
  expect_equal(config$max_workers, 4)
  expect_equal(config$min_days, 30)
  expect_equal(config$max_days, 90)
})

test_that("get_puts_config validates overridden configuration", {
  # Invalid override should trigger validation error
  expect_error(
    get_puts_config(strike_threshold_pct = 1.5),
    "strike_threshold_pct"
  )

  expect_error(
    get_puts_config(max_workers = 100),
    "max_workers"
  )

  expect_error(
    get_puts_config(min_days = 100, max_days = 50),
    "max_days"
  )
})

test_that("get_puts_config warns on unknown parameters", {
  expect_warning(
    get_puts_config(unknown_param = 123),
    "Unknown config parameter: unknown_param"
  )

  expect_warning(
    get_puts_config(invalid_field = "test"),
    "Unknown config parameter: invalid_field"
  )
})

test_that("get_puts_config ignores unknown parameters but applies valid ones", {
  suppressWarnings({
    config <- get_puts_config(
      strike_threshold_pct = 0.90,  # valid
      unknown_param = 123            # invalid
    )
  })

  # Valid override should be applied
  expect_equal(config$strike_threshold_pct, 0.90)
  # Unknown parameter should not be added
  expect_false("unknown_param" %in% names(config))
})
