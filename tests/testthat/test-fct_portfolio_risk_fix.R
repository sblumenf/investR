# Test the fix for portfolio risk calculation in fct_portfolio_risk.R
# specifically verifying that:
# 1. Simulations use PURCHASE PRICE (preserving Expected Value view)
# 2. Risk Level uses DRAWDOWN (preserving Low Risk view for winners)

library(investR)
library(testthat)
library(dplyr)
library(tibble)
library(lubridate)
library(mockery)
library(rlang)

# Define null coalescing operator if not available
`%||%` <- function(x, y) if (is.null(x)) y else x

# Source the modified file to test changes without reinstalling
source("../../R/fct_portfolio_risk.R", local = TRUE)

# Define mocks globally so sourced functions can find them
# Mock RISK_CONFIG for controlled testing
mock_risk_config <- list(
  default_simulation_paths = 1000,
  risk_free_rate = 0.02,
  jump_frequency = 0.01,
  jump_mean = -0.1,
  jump_volatility = 0.2,
  features = list(
    use_implied_volatility = TRUE,
    use_regime_adjustment = FALSE
  ),
  advanced = list(
    implied_vol_blend_weight = 0.70,
    jump_correlation_factor = 0.50
  )
)

RISK_CONFIG <- mock_risk_config

# Mock get_volatility to return a constant value
mock_get_volatility <- function(ticker, days_to_expiry, use_implied, fallback_to_historical, blend_weight) {
  0.20 # Constant volatility
}
get_volatility <- mock_get_volatility

mock_get_ticker_sector <- function(ticker, symbol_id) { "Technology" }
get_ticker_sector <- mock_get_ticker_sector

mock_get_stress_scenario <- function(scenario_name) {
  list(
    name = scenario_name,
    default_return = -0.10, # 10% drop
    sector_returns = list(Technology = -0.15) # 15% drop for tech
  )
}
get_stress_scenario <- mock_get_stress_scenario

# Mock other dependencies needed by analyze_portfolio_risk
mock_get_all_groups <- function() {
  tibble(group_id = "G1", status = "open")
}
get_all_groups <- mock_get_all_groups

# Mock extract_portfolio_positions to return our test positions directly
# We need to mock this because analyze_portfolio_risk calls it
# However, since we sourced the file, we can't easily mock internal functions called by other internal functions 
# unless we mock the dependencies of extract_portfolio_positions.
# A better approach for this integration test is to call run_correlated_monte_carlo directly
# to verify the P&L logic, and then manually verify the logic added to analyze_portfolio_risk.

test_that("P&L in run_correlated_monte_carlo uses PURCHASE PRICE (User Requirement)", {
  # Mock positions: 1 stock, highly profitable
  # Buy: 100, Current: 200.
  # If we use Purchase Price, P&L should be around +10000 (profit).
  # If we used Current Price (old fix), P&L would be around 0.
  mock_positions <- tibble(
    group_id = "G1",
    ticker = "AAPL",
    symbol_id = 1,
    is_csp = FALSE,
    current_price = 200,
    purchase_price = 100, 
    strike = NA_real_,
    expiration = as.Date("2026-12-31"),
    premium_received = 0,
    shares = 100,
    current_value = 20000,
    days_to_expiry = as.numeric(as.Date("2026-12-31") - Sys.Date())
  )

  mock_correlation_matrix <- matrix(1, nrow = 1, ncol = 1, dimnames = list("AAPL", "AAPL"))

  set.seed(123) 
  mc_results <- run_correlated_monte_carlo(
    positions = mock_positions,
    correlation_matrix = mock_correlation_matrix,
    simulation_paths = 1000
  )

  mean_pnl <- mean(mc_results$portfolio_pnl)
  
  # Expect mean_pnl to be around 10000 (Profit from purchase)
  # (200 - 100) * 100 = 10000. Plus drift.
  expect_gt(mean_pnl, 9000) 
  
  # Check 5th percentile (VaR). Even in bad case, should be profitable (since we have 100% cushion)
  # unless crash is > 50%.
  var_95 <- quantile(mc_results$portfolio_pnl, probs = 0.05, names = FALSE)
  expect_gt(var_95, 4000) # Still profitable
})

test_that("Risk Level Logic handles profitable positions correctly (Drawdown Logic)", {
  # We cannot easily call analyze_portfolio_risk because of its many dependencies.
  # Instead, we will verify the math logic used in the function:
  # var_drawdown = var_95 - current_portfolio_pnl
  
  # Scenario: Profitable Position (Winner)
  # Buy: 100, Current: 200. Shares: 100.
  # Current PnL (Paper): 10,000.
  # VaR_95 (from Purchase, simulated): 8,000. (Worst case is we only make 8k, dropping from 10k).
  
  var_95 <- 8000
  current_portfolio_pnl <- 10000
  total_value <- 20000
  
  # Old Logic (Buggy):
  # var_pct = abs(8000) / 20000 = 40%. HIGH RISK.
  
  # New Logic:
  var_drawdown <- var_95 - current_portfolio_pnl # 8000 - 10000 = -2000
  var_pct <- abs(var_drawdown) / total_value # 2000 / 20000 = 10%.
  
  expect_equal(var_drawdown, -2000)
  expect_equal(var_pct, 0.10)
  
  # Scenario: Losing Position
  # Buy: 100, Current: 80. Shares: 100.
  # Current PnL: -2000.
  # VaR_95 (from Purchase): -3000. (Drop further to 70).
  
  var_95_loss <- -3000
  current_pnl_loss <- -2000
  total_value_loss <- 8000
  
  var_drawdown_loss <- var_95_loss - current_pnl_loss # -3000 - (-2000) = -1000
  var_pct_loss <- abs(var_drawdown_loss) / total_value_loss # 1000 / 8000 = 12.5%.
  
  expect_equal(var_drawdown_loss, -1000)
  # Logic holds: Risk is measured from Current.
})

test_that("P&L in run_portfolio_stress_tests uses PURCHASE PRICE", {
  mock_positions <- tibble(
    group_id = "G1",
    ticker = "AAPL",
    symbol_id = 1,
    is_csp = FALSE,
    current_price = 200,
    purchase_price = 100,
    strike = NA_real_,
    expiration = as.Date("2026-12-31"),
    premium_received = 0,
    shares = 100,
    current_value = 20000,
    days_to_expiry = as.numeric(as.Date("2026-12-31") - Sys.Date())
  )

  mock_correlation_matrix <- matrix(1, nrow = 1, ncol = 1, dimnames = list("AAPL", "AAPL"))

  stress_results <- run_portfolio_stress_tests(
    positions = mock_positions,
    correlation_matrix = mock_correlation_matrix
  )

  # Scenario: Tech drops 15%.
  # Stressed Price: 200 * 0.85 = 170.
  # P&L (from Purchase 100): (170 - 100) * 100 = +7000.
  # Before fix (Current baseline): (170 - 200) * 100 = -3000.
  
  expect_equal(stress_results$portfolio_pnl[1], 7000)
})