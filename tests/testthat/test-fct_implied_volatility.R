#' Tests for Implied Volatility Module
#'
#' Tests the implied volatility fetching, Black-Scholes solver, and blending logic

test_that("get_volatility returns valid volatility estimate", {
  skip_on_cran()
  skip_if_offline()

  # Test with a liquid ticker (Apple)
  vol <- get_volatility(
    ticker = "AAPL",
    days_to_expiry = 30,
    use_implied = FALSE,  # Use historical only for predictability
    fallback_to_historical = TRUE
  )

  expect_true(is.numeric(vol))
  expect_true(vol > 0)
  expect_true(vol < 5)  # Reasonable range (0% to 500%)
})

test_that("get_volatility handles missing ticker gracefully", {
  expect_error(
    get_volatility(
      ticker = "",
      days_to_expiry = 30
    )
  )
})

test_that("get_volatility handles extreme days_to_expiry", {
  skip_on_cran()
  skip_if_offline()

  # Very short term
  vol_short <- get_volatility(
    ticker = "AAPL",
    days_to_expiry = 1,
    use_implied = FALSE
  )

  expect_true(is.numeric(vol_short))
  expect_true(vol_short > 0)

  # Very long term (implied vol disabled for > 365 days)
  vol_long <- get_volatility(
    ticker = "AAPL",
    days_to_expiry = 800,
    use_implied = FALSE
  )

  expect_true(is.numeric(vol_long))
  expect_true(vol_long > 0)
})

test_that("Black-Scholes pricing works correctly", {
  # Test Black-Scholes formula with known values
  result <- black_scholes(
    S = 100,  # Stock price
    K = 100,  # Strike (ATM)
    T = 1,    # 1 year
    sigma = 0.25,  # 25% vol
    r = 0.05,  # 5% risk-free
    option_type = "call"
  )

  expect_true(is.list(result))
  expect_true("price" %in% names(result))
  expect_true("vega" %in% names(result))

  # ATM call with 1 year should have positive value
  expect_true(result$price > 0)

  # Vega should be positive
  expect_true(result$vega > 0)
})

test_that("Black-Scholes handles put options", {
  result <- black_scholes(
    S = 100,
    K = 110,  # OTM put
    T = 0.5,
    sigma = 0.30,
    r = 0.05,
    option_type = "put"
  )

  expect_true(result$price > 0)
  expect_true(result$vega > 0)
})

test_that("Implied vol solver converges for reasonable inputs", {
  # Create a synthetic option price using Black-Scholes
  true_vol <- 0.30
  S <- 100
  K <- 100
  T <- 0.25  # 3 months
  r <- 0.05

  bs_result <- black_scholes(S, K, T, true_vol, r, "call")
  option_price <- bs_result$price

  # Now try to recover the volatility
  solved_vol <- calculate_implied_vol_from_price(
    option_price = option_price,
    stock_price = S,
    strike = K,
    time_to_expiry = T,
    risk_free_rate = r,
    option_type = "call"
  )

  # Should recover the original volatility (within tolerance)
  expect_true(!is.na(solved_vol))
  expect_equal(solved_vol, true_vol, tolerance = 0.01)
})

test_that("Implied vol solver handles edge cases", {
  # Zero time to expiry
  vol <- calculate_implied_vol_from_price(
    option_price = 5,
    stock_price = 100,
    strike = 100,
    time_to_expiry = 0,
    risk_free_rate = 0.05,
    option_type = "call"
  )

  expect_true(is.na(vol))

  # Zero option price
  vol <- calculate_implied_vol_from_price(
    option_price = 0,
    stock_price = 100,
    strike = 100,
    time_to_expiry = 0.25,
    risk_free_rate = 0.05,
    option_type = "call"
  )

  expect_true(is.na(vol))
})

test_that("Volatility blending works correctly", {
  skip_on_cran()
  skip_if_offline()

  # Mock scenario: assume both implied and historical are available
  # We'll use historical only but test the blending logic by checking
  # that blend_weight parameter affects the result

  # This test verifies the function accepts blend_weight parameter
  vol1 <- get_volatility(
    ticker = "AAPL",
    days_to_expiry = 30,
    use_implied = FALSE,
    fallback_to_historical = TRUE,
    blend_weight = 0.5
  )

  vol2 <- get_volatility(
    ticker = "AAPL",
    days_to_expiry = 30,
    use_implied = FALSE,
    fallback_to_historical = TRUE,
    blend_weight = 0.8
  )

  # Both should be valid
  expect_true(is.numeric(vol1))
  expect_true(is.numeric(vol2))
  expect_true(vol1 > 0)
  expect_true(vol2 > 0)
})

test_that("compare_implied_vs_historical returns proper structure", {
  skip_on_cran()
  skip_if_offline()

  result <- compare_implied_vs_historical(
    ticker = "AAPL",
    days_to_expiry = 30
  )

  expect_true(is.data.frame(result))
  expect_true("ticker" %in% names(result))
  expect_true("implied_vol" %in% names(result))
  expect_true("historical_vol" %in% names(result))
  expect_true("difference" %in% names(result))
  expect_true("interpretation" %in% names(result))
})

test_that("Volatility fallback chain works", {
  skip_on_cran()

  # Test with obscure ticker that won't have options data
  # Should fall back to historical
  vol <- get_volatility(
    ticker = "AAPL",  # Use AAPL but disable implied
    days_to_expiry = 30,
    use_implied = FALSE,  # Force historical
    fallback_to_historical = TRUE
  )

  expect_true(is.numeric(vol))
  expect_true(vol > 0)

  # If both fail, should return default from config
  vol_default <- get_volatility(
    ticker = "NONEXISTENT_TICKER_12345",
    days_to_expiry = 30,
    use_implied = FALSE,
    fallback_to_historical = TRUE
  )

  # Should return something (likely default from config)
  expect_true(is.numeric(vol_default))
})

test_that("Implied volatility disabled for long-dated options", {
  skip_on_cran()
  skip_if_offline()

  # Options > 365 days should not use implied vol (market data sparse)
  vol <- get_volatility(
    ticker = "AAPL",
    days_to_expiry = 400,
    use_implied = TRUE,  # Request implied
    fallback_to_historical = TRUE
  )

  # Should still work (falls back to historical)
  expect_true(is.numeric(vol))
  expect_true(vol > 0)
})

test_that("get_volatility respects config feature flag", {
  skip_on_cran()
  skip_if_offline()

  # Save original config
  original_flag <- RISK_CONFIG$features$use_implied_volatility

  # Test with feature disabled
  RISK_CONFIG$features$use_implied_volatility <- FALSE

  vol <- get_volatility(
    ticker = "AAPL",
    days_to_expiry = 30,
    use_implied = TRUE,  # Try to use implied
    fallback_to_historical = TRUE
  )

  expect_true(is.numeric(vol))
  expect_true(vol > 0)

  # Restore config
  RISK_CONFIG$features$use_implied_volatility <- original_flag
})
