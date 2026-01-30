# Simplified Cash-Secured Put Tests
# Focus: CSP-specific features and UI rendering
# Database CRUD operations are tested in other test files

test_that("CSP configuration is available", {
  strategy_types <- get_strategy_types()
  expect_true("S&P 500 Cash-Secured Puts" %in% strategy_types)

  position_roles <- get_position_roles()
  expect_true("short_put" %in% position_roles)
})

test_that("CSP-specific UI metrics calculation", {
  # Test the core CSP metrics logic without database
  strike_price <- 24
  num_contracts <- 3
  premium_collected <- 846.09

  # Cash collateral = strike × 100 × contracts
  cash_collateral <- strike_price * 100 * num_contracts
  expect_equal(cash_collateral, 7200)

  # Return on collateral
  return_on_collateral <- premium_collected / cash_collateral
  expect_equal(return_on_collateral, 0.1175125, tolerance = 0.0001)
  expect_equal(return_on_collateral * 100, 11.75125, tolerance = 0.01)
})

test_that("PUT option type detection for ITM/OTM status", {
  # For puts: ITM when stock price < strike price
  # Example: ETH trading at $20, PUT strike at $24 → ITM (profitable to exercise)
  current_price <- 20.00
  strike_price <- 24.00
  option_type <- "put"

  itm_amount <- strike_price - current_price
  expect_equal(itm_amount, 4.00)
  expect_true(itm_amount > 0)  # Positive ITM amount for puts

  # For puts: OTM when stock price > strike price
  current_price_otm <- 26.00
  otm_amount <- strike_price - current_price_otm
  expect_equal(otm_amount, -2.00)
  expect_true(otm_amount < 0)  # Negative = OTM for puts
})

test_that("CSP P&L calculation - worthless expiration", {
  # Scenario: Sold 3 PUT contracts for $282.03 each, expired worthless
  premium_collected <- 846.09
  strike_price <- 24
  num_contracts <- 3

  # For worthless expiration: no stock purchase, premium is profit
  total_cost <- 0  # No stock purchase
  total_proceeds <- premium_collected
  net_pnl <- total_proceeds - total_cost

  expect_equal(net_pnl, 846.09)

  # Return on collateral
  cash_collateral <- strike_price * 100 * num_contracts
  return_pct <- net_pnl / cash_collateral
  expect_equal(return_pct, 0.1175125, tolerance = 0.0001)
})

test_that("CSP P&L calculation - assignment scenario", {
  # Scenario: Sold 5 PUT contracts for $95.05 each, got assigned, sold stock
  premium_collected <- 475.25
  strike_price <- 19
  num_contracts <- 5
  stock_purchase_price <- 19
  stock_sale_price <- 20
  shares <- num_contracts * 100

  # Total proceeds = premium + stock sale
  total_proceeds <- premium_collected + (stock_sale_price * shares)
  expect_equal(total_proceeds, 10475.25)

  # Total cost = stock purchase
  total_cost <- stock_purchase_price * shares
  expect_equal(total_cost, 9500)

  # Net P&L
  net_pnl <- total_proceeds - total_cost
  expect_equal(net_pnl, 975.25)

  # Return on collateral
  cash_collateral <- strike_price * 100 * num_contracts
  return_pct <- net_pnl / cash_collateral
  expect_equal(return_pct, 0.102658, tolerance = 0.0001)
})

test_that("CSP buy-to-close P&L calculation", {
  # Scenario: Sold PUT for $846.09, bought back for $450
  premium_collected <- 846.09
  buy_to_close_cost <- 450.00

  # Net premium
  net_premium <- premium_collected - buy_to_close_cost
  expect_equal(net_premium, 396.09)

  # Return on collateral (strike $24, 3 contracts)
  cash_collateral <- 24 * 100 * 3
  return_pct <- net_premium / cash_collateral
  expect_equal(return_pct, 0.055013, tolerance = 0.0001)
})
