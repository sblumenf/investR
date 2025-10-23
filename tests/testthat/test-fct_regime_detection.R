#' Tests for Market Regime Detection
#'
#' Tests the rule-based regime classification and VIX/correlation fetching

test_that("detect_market_regime returns valid structure", {
  skip_on_cran()
  skip_if_offline()

  regime <- detect_market_regime(use_cache = FALSE)

  expect_true(is.list(regime))
  expect_true("name" %in% names(regime))
  expect_true("description" %in% names(regime))
  expect_true("risk_multiplier" %in% names(regime))
  expect_true("jump_frequency_adj" %in% names(regime))
  expect_true("correlation_adj" %in% names(regime))
  expect_true("indicators" %in% names(regime))

  # Regime name should be one of expected values
  expect_true(regime$name %in% c("crisis", "stressed", "correlation_spike", "calm", "normal"))

  # Multipliers should be reasonable
  expect_true(regime$risk_multiplier > 0)
  expect_true(regime$risk_multiplier < 3)
  expect_true(regime$jump_frequency_adj > 0)
  expect_true(regime$correlation_adj > 0)
})

test_that("get_regime_adjusted_parameters returns proper structure", {
  skip_on_cran()
  skip_if_offline()

  params <- get_regime_adjusted_parameters()

  expect_true(is.list(params))
  expect_true("jump_frequency" %in% names(params))
  expect_true("regime_name" %in% names(params))
  expect_true("regime_description" %in% names(params))
  expect_true("risk_multiplier" %in% names(params))
  expect_true("vix_current" %in% names(params))

  # Jump frequency should be adjusted
  expect_true(is.numeric(params$jump_frequency))
  expect_true(params$jump_frequency > 0)
})

test_that("classify_regime handles high VIX correctly", {
  # Mock high VIX scenario
  indicators <- list(
    vix_current = 30,
    vix_20d_avg = 25,
    vix_trend = "rising",
    market_correlation = 0.75,
    timestamp = Sys.time()
  )

  regime <- classify_regime(indicators)

  # High VIX + high correlation = crisis
  expect_equal(regime$name, "crisis")
  expect_true(regime$risk_multiplier > 1)
  expect_true(regime$jump_frequency_adj > 1)
})

test_that("classify_regime handles calm markets correctly", {
  # Mock calm scenario
  indicators <- list(
    vix_current = 12,
    vix_20d_avg = 15,
    vix_trend = "falling",
    market_correlation = 0.4,
    timestamp = Sys.time()
  )

  regime <- classify_regime(indicators)

  # Low VIX = calm
  expect_equal(regime$name, "calm")
  expect_true(regime$risk_multiplier < 1)
  expect_true(regime$jump_frequency_adj < 1)
})

test_that("classify_regime handles normal markets correctly", {
  # Mock normal scenario
  indicators <- list(
    vix_current = 18,
    vix_20d_avg = 17,
    vix_trend = "stable",
    market_correlation = 0.5,
    timestamp = Sys.time()
  )

  regime <- classify_regime(indicators)

  # Normal conditions
  expect_equal(regime$name, "normal")
  expect_equal(regime$risk_multiplier, 1.0)
  expect_equal(regime$jump_frequency_adj, 1.0)
})

test_that("classify_regime handles correlation spike", {
  # Mock correlation spike (VIX normal but correlation high)
  indicators <- list(
    vix_current = 18,
    vix_20d_avg = 17,
    vix_trend = "stable",
    market_correlation = 0.75,  # High correlation
    timestamp = Sys.time()
  )

  regime <- classify_regime(indicators)

  # Should detect correlation spike
  expect_equal(regime$name, "correlation_spike")
  expect_true(regime$correlation_adj > 1)
})

test_that("classify_regime handles stressed markets", {
  # Mock stressed scenario (VIX elevated but not crisis)
  indicators <- list(
    vix_current = 28,
    vix_20d_avg = 20,
    vix_trend = "rising",
    market_correlation = 0.5,  # Normal correlation
    timestamp = Sys.time()
  )

  regime <- classify_regime(indicators)

  # Should detect stressed (not crisis because correlation normal)
  expect_equal(regime$name, "stressed")
  expect_true(regime$risk_multiplier > 1)
  expect_true(regime$risk_multiplier < 1.5)  # Less than crisis
})

test_that("get_vix_data returns valid structure", {
  skip_on_cran()
  skip_if_offline()

  vix_data <- get_vix_data()

  expect_true(is.list(vix_data))
  expect_true("current" %in% names(vix_data))
  expect_true("avg_20d" %in% names(vix_data))
  expect_true("trend" %in% names(vix_data))

  # VIX should be positive (or NA if fetch failed)
  if (!is.na(vix_data$current)) {
    expect_true(vix_data$current > 0)
    expect_true(vix_data$current < 100)  # Reasonable upper bound
  }

  # Trend should be one of expected values
  if (!is.na(vix_data$trend) && vix_data$trend != "unknown") {
    expect_true(vix_data$trend %in% c("rising", "falling", "stable"))
  }
})

test_that("get_market_correlation returns valid value", {
  skip_on_cran()
  skip_if_offline()

  correlation <- get_market_correlation()

  # Should be between -1 and 1 (or NA if fetch failed)
  if (!is.na(correlation)) {
    expect_true(correlation >= -1)
    expect_true(correlation <= 1)

    # Market correlations are typically positive
    expect_true(correlation > 0)
  }
})

test_that("Regime caching works correctly", {
  skip_on_cran()
  skip_if_offline()

  # Clear cache
  clear_regime_cache()

  # First call should fetch fresh
  regime1 <- detect_market_regime(use_cache = TRUE)
  expect_true(!is.null(regime1))

  # Second call should use cache (should be instant)
  start_time <- Sys.time()
  regime2 <- detect_market_regime(use_cache = TRUE)
  elapsed <- as.numeric(Sys.time() - start_time)

  expect_equal(regime1$name, regime2$name)
  expect_true(elapsed < 0.1)  # Cached call should be very fast

  # Clear cache and verify fresh fetch
  clear_regime_cache()
  regime3 <- detect_market_regime(use_cache = TRUE)
  expect_true(!is.null(regime3))
})

test_that("show_current_regime returns data frame", {
  skip_on_cran()
  skip_if_offline()

  result <- show_current_regime()

  expect_true(is.data.frame(result))
  expect_true("Regime" %in% names(result))
  expect_true("Description" %in% names(result))
  expect_true("Risk_Multiplier" %in% names(result))
  expect_true("VIX_Current" %in% names(result))
  expect_true("Market_Correlation" %in% names(result))

  expect_equal(nrow(result), 1)
})

test_that("Regime detection handles API failures gracefully", {
  # This test verifies the module doesn't crash on failures

  # classify_regime should handle NA indicators
  indicators_na <- list(
    vix_current = NA,
    vix_20d_avg = NA,
    vix_trend = NA,
    market_correlation = NA,
    timestamp = Sys.time()
  )

  regime <- classify_regime(indicators_na)

  # Should default to normal when data unavailable
  expect_equal(regime$name, "normal")
  expect_equal(regime$risk_multiplier, 1.0)
})

test_that("Regime adjustments are monotonic with severity", {
  # Create scenarios with increasing severity
  calm <- list(vix_current = 12, vix_20d_avg = 14, vix_trend = "stable", market_correlation = 0.3, timestamp = Sys.time())
  normal <- list(vix_current = 18, vix_20d_avg = 17, vix_trend = "stable", market_correlation = 0.5, timestamp = Sys.time())
  stressed <- list(vix_current = 28, vix_20d_avg = 20, vix_trend = "rising", market_correlation = 0.6, timestamp = Sys.time())
  crisis <- list(vix_current = 35, vix_20d_avg = 30, vix_trend = "rising", market_correlation = 0.8, timestamp = Sys.time())

  calm_regime <- classify_regime(calm)
  normal_regime <- classify_regime(normal)
  stressed_regime <- classify_regime(stressed)
  crisis_regime <- classify_regime(crisis)

  # Risk multipliers should increase with severity
  expect_true(calm_regime$risk_multiplier < normal_regime$risk_multiplier)
  expect_true(normal_regime$risk_multiplier < stressed_regime$risk_multiplier)
  expect_true(stressed_regime$risk_multiplier < crisis_regime$risk_multiplier)

  # Jump frequency adjustments should increase with severity
  expect_true(calm_regime$jump_frequency_adj < normal_regime$jump_frequency_adj)
  expect_true(normal_regime$jump_frequency_adj < stressed_regime$jump_frequency_adj)
  expect_true(stressed_regime$jump_frequency_adj < crisis_regime$jump_frequency_adj)
})

test_that("Null coalescing operator works correctly", {
  # Test the %||% operator used throughout regime detection
  expect_equal(NULL %||% 5, 5)
  expect_equal(10 %||% 5, 10)
  expect_equal(NA %||% 5, NA)  # NA is not NULL
})
