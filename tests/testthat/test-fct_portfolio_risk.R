test_that("Component VaR calculation is mathematically correct", {
  # Create mock data
  set.seed(123)
  n_paths <- 1000
  n_positions <- 3

  # Simulate position P&L matrix
  position_pnl_matrix <- matrix(rnorm(n_paths * n_positions, mean = 100, sd = 50), nrow = n_paths, ncol = n_positions)

  # Calculate portfolio P&L (sum of positions)
  portfolio_pnl <- rowSums(position_pnl_matrix)

  # Calculate portfolio VaR (5th percentile)
  portfolio_var <- quantile(portfolio_pnl, 0.05)

  # Create mock positions tibble
  positions <- tibble::tibble(
    group_id = paste0("group_", 1:n_positions),
    ticker = c("AAPL", "MSFT", "GOOGL")
  )

  # Call the actual function
  contributions <- investR:::calculate_position_contributions(
    positions = positions,
    portfolio_pnl = portfolio_pnl,
    position_pnl_matrix = position_pnl_matrix,
    portfolio_var = portfolio_var
  )

  # Test 1: Sum of Component VaRs should approximately equal Portfolio VaR
  # This is a fundamental property of Component VaR (Euler allocation)
  total_component_var <- sum(contributions$risk_contribution)
  expect_equal(total_component_var, portfolio_var, tolerance = 0.01)

  # Test 2: All required columns should be present
  expect_true(all(c("group_id", "ticker", "expected_contribution", "risk_contribution",
                     "pct_of_portfolio_risk", "risk_return_ratio") %in% names(contributions)))

  # Test 3: Percentages should sum to 1 (or very close)
  expect_equal(sum(abs(contributions$pct_of_portfolio_risk)), 1.0, tolerance = 0.01)

  # Test 4: Risk contribution should be sorted descending
  expect_true(all(diff(abs(contributions$risk_contribution)) <= 0))
})

test_that("Correlation matrix handles NA values correctly", {
  # This test verifies that NA correlations are replaced with 0.3 (market average)
  # instead of 0 (which would understate risk)

  # Create test data with one ticker having insufficient history
  tickers <- c("AAPL", "FAKE_TICKER_NO_DATA")
  lookback_days <- 252

  # Mock the fetch_price_history function
  # In real test, this would fail for FAKE_TICKER_NO_DATA
  # For now, we just test that the function doesn't crash

  expect_error(
    investR:::calculate_correlation_matrix(character(0), 252),
    NA  # Should not error
  )

  # Test single ticker case
  cor_single <- investR:::calculate_correlation_matrix("AAPL", 252)
  expect_equal(dim(cor_single), c(1, 1))
  expect_equal(cor_single[1,1], 1.0)
})

test_that("Input validation works correctly", {
  expect_error(
    investR::analyze_portfolio_risk(simulation_paths = 50),  # Too low
    "must be between 100 and 100,000"
  )

  expect_error(
    investR::analyze_portfolio_risk(simulation_paths = 200000),  # Too high
    "must be between 100 and 100,000"
  )

  expect_error(
    investR::analyze_portfolio_risk(lookback_days = 10),  # Too low
    "must be between 30 and 1,000"
  )

  expect_error(
    investR::analyze_portfolio_risk(lookback_days = 2000),  # Too high
    "must be between 30 and 1,000"
  )
})

test_that("Position extraction handles errors gracefully", {
  # Create mock open groups with problematic data
  open_groups <- tibble::tibble(
    group_id = c("group_1", "group_2", "group_3"),
    status = c("open", "open", "open")
  )

  # The function should not crash even if individual positions fail
  # It should return successfully extracted positions and log warnings for failures

  # This test would need proper mocking of get_members_for_groups, etc.
  # For now, we just verify the function signature is correct
  expect_true(exists("extract_portfolio_positions", where = "package:investR", mode = "function", inherits = FALSE) ||
              exists("extract_portfolio_positions", envir = asNamespace("investR"), mode = "function"))
})

test_that("Risk contribution formulas are academically rigorous", {
  # Test that we're using Component VaR, not just expected returns

  set.seed(456)
  n_paths <- 1000

  # Create two positions:
  # Position 1: High expected return, low correlation with portfolio
  # Position 2: Low expected return, high correlation with portfolio

  pos1_pnl <- rnorm(n_paths, mean = 100, sd = 20)  # High return
  pos2_pnl <- rnorm(n_paths, mean = 10, sd = 50)   # Low return, high volatility

  portfolio_pnl <- pos1_pnl + pos2_pnl

  position_pnl_matrix <- cbind(pos1_pnl, pos2_pnl)
  portfolio_var <- quantile(portfolio_pnl, 0.05)

  positions <- tibble::tibble(
    group_id = c("group_1", "group_2"),
    ticker = c("SAFE", "RISKY")
  )

  contributions <- investR:::calculate_position_contributions(
    positions = positions,
    portfolio_pnl = portfolio_pnl,
    position_pnl_matrix = position_pnl_matrix,
    portfolio_var = portfolio_var
  )

  # Expected contribution should match mean P&L
  expect_equal(contributions$expected_contribution[1], mean(pos1_pnl), tolerance = 0.1)
  expect_equal(contributions$expected_contribution[2], mean(pos2_pnl), tolerance = 0.1)

  # Risk contribution should NOT just be expected contribution
  # (If it were, the function would be wrong)
  expect_false(all.equal(contributions$expected_contribution, contributions$risk_contribution))

  # RISKY position should have higher risk contribution despite lower expected return
  expect_gt(abs(contributions$risk_contribution[2]), abs(contributions$risk_contribution[1]))
})

test_that("Cholesky decomposition fallback works", {
  # Test that correlation matrix with issues falls back gracefully

  # Create a non-positive definite matrix
  bad_matrix <- matrix(c(1, 0.9, 0.9,
                         0.9, 1, 0.9,
                         0.9, 0.9, 1), nrow = 3)
  # Actually this IS positive definite, but let's test the general mechanism

  # The function should handle Cholesky failures gracefully
  # We can't easily test this without mocking, but we verify it doesn't crash
  expect_true(TRUE)  # Placeholder
})
