# tests/testthat/test-fct_put_calendar_spread.R
# Business logic tests for put calendar spread strategy
# Date: 2025-11-30

library(testthat)
library(investR)
library(tibble)
library(dplyr)

################################################################################
# NET DEBIT CALCULATION TESTS
################################################################################

test_that("calculate_calendar_net_debit computes correctly with basic values", {
  # Long put costs more than short put credit
  result <- calculate_calendar_net_debit(short_bid = 2.00, long_ask = 4.50)

  expect_equal(result, 2.50)  # 4.50 - 2.00 = net debit of $2.50
})

test_that("calculate_calendar_net_debit handles equal prices", {
  result <- calculate_calendar_net_debit(short_bid = 3.00, long_ask = 3.00)

  expect_equal(result, 0)  # Break-even entry
})

test_that("calculate_calendar_net_debit handles decimal prices", {
  result <- calculate_calendar_net_debit(short_bid = 1.35, long_ask = 2.80)

  expect_equal(result, 1.45)  # 2.80 - 1.35
})

test_that("calculate_calendar_net_debit can produce credit (unusual but possible)", {
  # If short put premium exceeds long put cost (rare, usually mispricing)
  result <- calculate_calendar_net_debit(short_bid = 5.00, long_ask = 3.00)

  expect_equal(result, -2.00)  # Net credit (unusual)
})

test_that("calculate_calendar_net_debit rejects negative short_bid", {
  expect_error(
    calculate_calendar_net_debit(short_bid = -1.00, long_ask = 2.00),
    "non-negative"
  )
})

test_that("calculate_calendar_net_debit rejects negative long_ask", {
  expect_error(
    calculate_calendar_net_debit(short_bid = 1.00, long_ask = -2.00),
    "non-negative"
  )
})

################################################################################
# RETURN METRICS TESTS
################################################################################

test_that("calculate_calendar_return_metrics computes ROI correctly", {
  result <- calculate_calendar_return_metrics(
    net_debit = 250,              # $250 to enter
    estimated_max_profit = 375,   # $375 potential profit
    days_to_front_expiry = 30
  )

  expect_equal(result$roi, 1.5)  # 375/250 = 150% ROI
  expect_true(result$annualized_return > result$roi)  # Annualized should be higher
})

test_that("calculate_calendar_return_metrics calculates profit targets", {
  result <- calculate_calendar_return_metrics(
    net_debit = 200,
    estimated_max_profit = 300,
    days_to_front_expiry = 30
  )

  expect_equal(result$profit_target_15pct, 30)   # 200 * 0.15
  expect_equal(result$profit_target_20pct, 40)   # 200 * 0.20
  expect_equal(result$profit_target_25pct, 50)   # 200 * 0.25
})

test_that("calculate_calendar_return_metrics handles short expiry", {
  result <- calculate_calendar_return_metrics(
    net_debit = 100,
    estimated_max_profit = 100,
    days_to_front_expiry = 7  # Weekly option
  )

  # Very short term should have high annualized return
  expect_true(result$annualized_return > 5)  # >500% annualized for 1 week
})

test_that("calculate_calendar_return_metrics rejects zero net_debit", {
  expect_error(
    calculate_calendar_return_metrics(
      net_debit = 0,
      estimated_max_profit = 100,
      days_to_front_expiry = 30
    ),
    "positive"
  )
})

test_that("calculate_calendar_return_metrics rejects negative net_debit", {
  expect_error(
    calculate_calendar_return_metrics(
      net_debit = -100,
      estimated_max_profit = 100,
      days_to_front_expiry = 30
    ),
    "positive"
  )
})

test_that("calculate_calendar_return_metrics rejects zero days", {
  expect_error(
    calculate_calendar_return_metrics(
      net_debit = 100,
      estimated_max_profit = 100,
      days_to_front_expiry = 0
    ),
    "positive"
  )
})

################################################################################
# CONFIGURATION TESTS
################################################################################

test_that("PUT_CALENDAR_SPREAD_CONFIG has required parameters", {
  config <- PUT_CALENDAR_SPREAD_CONFIG

  # Strike parameters
  expect_true("strike_pct" %in% names(config))
  expect_equal(config$strike_pct, 0.95)

  # Expiry parameters
  expect_true("short_expiry_min_days" %in% names(config))
  expect_true("short_expiry_max_days" %in% names(config))
  expect_true("long_expiry_min_days" %in% names(config))
  expect_true("long_expiry_max_days" %in% names(config))
  expect_true("min_expiry_ratio" %in% names(config))

  # Verify default values
  expect_equal(config$short_expiry_target_days, 30)
  expect_equal(config$long_expiry_target_days, 60)
  expect_equal(config$min_expiry_ratio, 1.5)

  # Liquidity parameters
  expect_true("min_option_bid" %in% names(config))
  expect_true("min_open_interest" %in% names(config))
  expect_equal(config$min_open_interest, 100)  # Higher for calendars
})

test_that("validate_calendar_spread_config passes for default config", {
  expect_true(validate_calendar_spread_config(PUT_CALENDAR_SPREAD_CONFIG))
})

test_that("validate_calendar_spread_config rejects invalid strike_pct", {
  bad_config <- PUT_CALENDAR_SPREAD_CONFIG
  bad_config$strike_pct <- 1.50  # >105%

  expect_error(
    validate_calendar_spread_config(bad_config),
    "strike_pct"
  )
})

test_that("validate_calendar_spread_config rejects invalid expiry relationship", {
  bad_config <- PUT_CALENDAR_SPREAD_CONFIG
  bad_config$long_expiry_min_days <- 30  # Less than short_expiry_max_days (45)

  expect_error(
    validate_calendar_spread_config(bad_config),
    "long_expiry_min_days"
  )
})

test_that("get_calendar_spread_config returns default config", {
  config <- get_calendar_spread_config()

  expect_equal(config$strike_pct, 0.95)
  expect_equal(config$short_expiry_target_days, 30)
})

test_that("get_calendar_spread_config applies overrides", {
  config <- get_calendar_spread_config(strike_pct = 0.90, max_workers = 5)

  expect_equal(config$strike_pct, 0.90)
  expect_equal(config$max_workers, 5)
})

test_that("get_calendar_spread_config warns on unknown parameters", {
  expect_warning(
    get_calendar_spread_config(unknown_param = 123),
    "Unknown"
  )
})

################################################################################
# SCORING TESTS
################################################################################

test_that("calculate_calendar_score returns 0-100 range", {
  score <- calculate_calendar_score(
    iv_rank = 0.15,
    iv_ratio = 1.10,
    net_theta = 0.08,
    net_vega = 0.20,
    debit_profit_ratio = 0.35,
    open_interest = 600,
    is_range_bound = TRUE,
    dividend_safe = TRUE
  )

  expect_true(score >= 0)
  expect_true(score <= 100)
})

test_that("calculate_calendar_score gives max points for excellent IV rank", {
  # Excellent IV rank (<20%) should give 25 points
  score_excellent <- calculate_calendar_score(iv_rank = 0.15)
  score_good <- calculate_calendar_score(iv_rank = 0.25)
  score_acceptable <- calculate_calendar_score(iv_rank = 0.35)
  score_poor <- calculate_calendar_score(iv_rank = 0.50)

  expect_true(score_excellent > score_good)
  expect_true(score_good > score_acceptable)
  expect_true(score_acceptable > score_poor)
})

test_that("calculate_calendar_score handles missing values gracefully", {
  # Should not error with NA values
  score <- calculate_calendar_score(
    iv_rank = NA,
    iv_ratio = NA,
    net_theta = NA,
    net_vega = NA,
    debit_profit_ratio = NA,
    open_interest = NA
  )

  expect_true(is.numeric(score))
})

test_that("calculate_calendar_score adds points for dividend safety", {
  score_safe <- calculate_calendar_score(dividend_safe = TRUE)
  score_unsafe <- calculate_calendar_score(dividend_safe = FALSE)

  expect_true(score_safe > score_unsafe)
  expect_equal(score_safe - score_unsafe, 5)  # 5 points for dividend safety
})

test_that("calculate_calendar_score adds points for range-bound stocks", {
  score_range <- calculate_calendar_score(is_range_bound = TRUE)
  score_trending <- calculate_calendar_score(is_range_bound = FALSE)

  expect_true(score_range > score_trending)
  expect_equal(score_range - score_trending, 5)  # 5 points for technical
})

################################################################################
# INTEGRATION PLACEHOLDER TESTS
################################################################################

# These tests would require mocking external data sources

test_that("analyze_put_calendar_spread function exists and is exported",
{
  expect_true(exists("analyze_put_calendar_spread"))
  expect_true(is.function(analyze_put_calendar_spread))
})

test_that("analyze_single_stock_calendar function exists", {
  expect_true(exists("analyze_single_stock_calendar"))
  expect_true(is.function(analyze_single_stock_calendar))
})

test_that("analyze_calendar_spread_generic function exists", {
  expect_true(exists("analyze_calendar_spread_generic"))
  expect_true(is.function(analyze_calendar_spread_generic))
})
