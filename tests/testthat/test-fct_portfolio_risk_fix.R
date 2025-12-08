
# Test the fix for portfolio risk calculation in fct_portfolio_risk.R
# Specifically, ensure P&L in Monte Carlo simulation and stress tests
# is based on current_price for risk assessment, not purchase_price.

library(investR)
library(testthat)
library(dplyr)
library(tibble)
library(lubridate)
library(mockery)
library(rlang)

# Source the modified file to test changes without reinstalling
# We need to source helper files if they are not exported or if the file depends on them
# Assuming fct_portfolio_risk.R functions are self-contained enough or depend on exported utils
source("../../R/fct_portfolio_risk.R", local = TRUE)

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

# Mock get_volatility to return a constant value
mock_get_volatility <- function(ticker, days_to_expiry, use_implied, fallback_to_historical, blend_weight) {
  0.20 # Constant volatility for predictable results
}


# Define mocks globally so sourced functions can find them
RISK_CONFIG <- mock_risk_config
get_volatility <- mock_get_volatility
get_ticker_sector <- function(ticker, symbol_id) { "Technology" }
get_stress_scenario <- function(scenario_name) {
  list(
    name = scenario_name,
    default_return = -0.10, # 10% drop
    sector_returns = list(Technology = -0.15) # 15% drop for tech
  )
}

test_that("P&L in run_correlated_monte_carlo is calculated from current_price for stocks", {
  # Mock positions: 1 stock, highly profitable
  mock_positions <- tibble(
    group_id = "G1",
    ticker = "AAPL",
    symbol_id = 1,
    is_csp = FALSE,
    current_price = 200,
    purchase_price = 100, # Much lower than current price
    strike = NA_real_,
    expiration = as.Date("2026-12-31"),
    premium_received = 0,
    shares = 100,
    current_value = 20000,
    days_to_expiry = as.numeric(as.Date("2026-12-31") - Sys.Date())
  )

  # Mock correlation matrix (identity for simplicity)
  mock_correlation_matrix <- matrix(1, nrow = 1, ncol = 1, dimnames = list("AAPL", "AAPL"))


  # Define mocks in the local environment where the sourced function runs
  RISK_CONFIG <- mock_risk_config
  get_volatility <- mock_get_volatility

  # Run the modified function
  set.seed(123) # for reproducibility
  mc_results <- run_correlated_monte_carlo(
    positions = mock_positions,
    correlation_matrix = mock_correlation_matrix,
    simulation_paths = mock_risk_config$default_simulation_paths
  )

  # Check that the mean P&L is much closer to 0 than to 10000
  mean_pnl <- mean(mc_results$portfolio_pnl)
  expect_lt(abs(mean_pnl), 1000) # Should be much less than the 10000 profit

  # Check a specific percentile, e.g., 5th percentile (VaR)
  var_95 <- quantile(mc_results$portfolio_pnl, probs = 0.05, names = FALSE)
  expect_lt(var_95, -1000) # Expect a noticeable loss, not a profit, in the worst 5%

  # Ensure the modification also applies to `stock_pnl` logic within `run_correlated_monte_carlo`
  # when strike is not NA, for unassigned covered calls
  mock_positions_cc <- tibble(
    group_id = "G2",
    ticker = "MSFT",
    symbol_id = 2,
    is_csp = FALSE,
    current_price = 300,
    purchase_price = 200, # Highly profitable
    strike = 350, # Out of money, not assigned
    expiration = as.Date("2026-12-31"),
    premium_received = 500, # Example premium
    shares = 100,
    current_value = 30000,
    days_to_expiry = as.numeric(as.Date("2026-12-31") - Sys.Date())
  )

  set.seed(123)
  mc_results_cc <- run_correlated_monte_carlo(
    positions = mock_positions_cc,
    correlation_matrix = matrix(1, nrow = 1, ncol = 1, dimnames = list("MSFT", "MSFT")),
    simulation_paths = mock_risk_config$default_simulation_paths
  )

  mean_pnl_cc <- mean(mc_results_cc$portfolio_pnl)
  # Expect mean_pnl_cc to be around 500 (premium) and not 10000 + 500
  expect_lt(abs(mean_pnl_cc - 500), 1000) 

  var_95_cc <- quantile(mc_results_cc$portfolio_pnl, probs = 0.05, names = FALSE)
  expect_lt(var_95_cc, 0) 
})


test_that("P&L in run_portfolio_stress_tests is calculated from current_price for stocks", {
  # Mock positions: 1 stock, highly profitable
  mock_positions <- tibble(
    group_id = "G1",
    ticker = "AAPL",
    symbol_id = 1,
    is_csp = FALSE,
    current_price = 200,
    purchase_price = 100, # Much lower than current price
    strike = NA_real_,
    expiration = as.Date("2026-12-31"),
    premium_received = 0,
    shares = 100,
    current_value = 20000,
    days_to_expiry = as.numeric(as.Date("2026-12-31") - Sys.Date())
  )

  # Mock correlation matrix
  mock_correlation_matrix <- matrix(1, nrow = 1, ncol = 1, dimnames = list("AAPL", "AAPL"))

  # Mock get_ticker_sector
  mock_get_ticker_sector <- function(ticker, symbol_id) { "Technology" }
  
  # Mock get_stress_scenario to return a predictable scenario
  mock_get_stress_scenario <- function(scenario_name) {
    list(
      name = scenario_name,
      default_return = -0.10, # 10% drop
      sector_returns = list(Technology = -0.15) # 15% drop for tech
    )
  }

  # Define mocks in local environment
  get_ticker_sector <- mock_get_ticker_sector
  get_stress_scenario <- mock_get_stress_scenario

  # Run the modified function
  stress_results <- run_portfolio_stress_tests(
    positions = mock_positions,
    correlation_matrix = mock_correlation_matrix
  )

  # For the mocked scenario (15% drop for tech), stressed_price = 200 * (1 - 0.15) = 170
  # P&L should be (stressed_price - current_price) * shares = (170 - 200) * 100 = -3000
  expect_equal(stress_results$portfolio_pnl[1], -3000)
  expect_equal(stress_results$portfolio_return_pct[1], -3000 / 20000)

  # Test for unassigned covered calls in stress tests
  mock_positions_cc <- tibble(
    group_id = "G2",
    ticker = "MSFT",
    symbol_id = 2,
    is_csp = FALSE,
    current_price = 300,
    purchase_price = 200, # Highly profitable
    strike = 350, # Out of money, not assigned
    expiration = as.Date("2026-12-31"),
    premium_received = 500, # Example premium
    shares = 100,
    current_value = 30000,
    days_to_expiry = as.numeric(as.Date("2026-12-31") - Sys.Date())
  )

  stress_results_cc <- run_portfolio_stress_tests(
    positions = mock_positions_cc,
    correlation_matrix = mock_correlation_matrix
  )

  # For the mocked scenario (15% drop for tech), stressed_price = 300 * (1 - 0.15) = 255
  # Since strike (350) > stressed_price (255), not assigned.
  # stock_pnl = (stressed_price - current_price) * shares = (255 - 300) * 100 = -4500
  # position_pnl = stock_pnl + premium_received = -4500 + 500 = -4000
  expect_equal(stress_results_cc$portfolio_pnl[1], -4000)
  expect_equal(stress_results_cc$portfolio_return_pct[1], -4000 / 30000)
})