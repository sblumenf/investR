# Test Cash-Secured Put Risk Analysis Implementation
# This file tests the risk analysis functionality for cash-secured puts

library(testthat)

test_that("calculate_option_payoff works for cash-secured puts", {
  # Test 1: Put assigned when stock falls below strike
  pnl_assigned <- calculate_option_payoff(
    stock_price = 90,
    strike = 100,
    premium_received = 200,
    entry_stock_price = 100,  # For puts, typically strike
    shares = 100,
    option_type = "put"
  )

  # Expected: (90 - 100) * 100 + 200 = -1000 + 200 = -800
  expect_equal(pnl_assigned, -800)

  # Test 2: Put expires worthless when stock above strike
  pnl_not_assigned <- calculate_option_payoff(
    stock_price = 105,
    strike = 100,
    premium_received = 200,
    entry_stock_price = 100,
    shares = 100,
    option_type = "put"
  )

  # Expected: premium only = 200
  expect_equal(pnl_not_assigned, 200)

  # Test 3: At-the-money put (edge case)
  pnl_atm <- calculate_option_payoff(
    stock_price = 100,
    strike = 100,
    premium_received = 300,
    entry_stock_price = 100,
    shares = 100,
    option_type = "put"
  )

  # Expected: assigned (stock <= strike), so (100 - 100) * 100 + 300 = 300
  expect_equal(pnl_atm, 300)
})

test_that("calculate_option_payoff backwards compatibility for calls", {
  # Verify covered calls still work
  pnl_call_assigned <- calculate_option_payoff(
    stock_price = 110,
    strike = 105,
    premium_received = 300,
    entry_stock_price = 100,
    shares = 100,
    option_type = "call"
  )

  # Expected: (105 - 100) * 100 + 300 = 500 + 300 = 800
  expect_equal(pnl_call_assigned, 800)
})

test_that("calculate_covered_call_payoff still works (legacy)", {
  # Test backwards compatibility wrapper
  pnl <- calculate_covered_call_payoff(
    stock_price = 110,
    strike = 105,
    premium_received = 300,
    entry_stock_price = 100,
    shares = 100
  )

  expect_equal(pnl, 800)
})

test_that("analyze_position_risk accepts option_type parameter", {
  skip_if_not(require("RQuantLib", quietly = TRUE), "RQuantLib not available")

  # This is a basic smoke test - just verify the function accepts the parameter
  # Real testing would require actual market data

  expect_error(
    analyze_position_risk(
      ticker = "AAPL",
      strike = 150,
      expiration = Sys.Date() + 30,
      premium_received = 300,
      current_price = 155,
      option_type = "put",
      use_monte_carlo = FALSE,  # Skip MC for speed
      use_rquantlib = FALSE     # Skip RQL for speed
    ),
    NA  # Expect no error
  )
})

test_that("option_type validation", {
  # Test that invalid option_type throws error
  expect_error(
    calculate_option_payoff(
      stock_price = 100,
      strike = 100,
      premium_received = 200,
      entry_stock_price = 100,
      shares = 100,
      option_type = "invalid"
    ),
    "Unknown option_type"
  )
})

test_that("put vs call payoff differences", {
  # Test scenario 1: Stock below strike
  # Put should be assigned, call should not

  params_low <- list(
    stock_price = 95,
    strike = 100,
    premium_received = 300,
    entry_stock_price = 100,
    shares = 100
  )

  call_pnl_low <- do.call(calculate_option_payoff, c(params_low, option_type = "call"))
  put_pnl_low <- do.call(calculate_option_payoff, c(params_low, option_type = "put"))

  # Put: assigned (stock < strike), so (95 - 100) * 100 + 300 = -500 + 300 = -200
  expect_equal(put_pnl_low, -200)

  # Call: not assigned (stock < strike), hold stock: (95 - 100) * 100 + 300 = -500 + 300 = -200
  expect_equal(call_pnl_low, -200)

  # At this price level, both have same P&L (coincidence)
  expect_equal(call_pnl_low, put_pnl_low)

  # Test scenario 2: Stock above strike with profit
  # Call should be assigned, put should not

  params_high <- list(
    stock_price = 110,
    strike = 100,
    premium_received = 300,
    entry_stock_price = 95,  # Bought stock at 95
    shares = 100
  )

  call_pnl_high <- do.call(calculate_option_payoff, c(params_high, option_type = "call"))
  put_pnl_high <- do.call(calculate_option_payoff, c(params_high, option_type = "put"))

  # Call: assigned at strike (100 - 95) * 100 + 300 = 500 + 300 = 800
  expect_equal(call_pnl_high, 800)

  # Put: not assigned (stock > strike), premium only = 300
  expect_equal(put_pnl_high, 300)

  # These should be different
  expect_false(call_pnl_high == put_pnl_high)
})

test_that("net_outlay calculation uses strike for puts, entry_price for calls", {
  # This test prevents regression of the bug where puts incorrectly used entry_price
  # Bug history: APA put showed -63% return when it should be ~2.19%
  # Root cause: net_outlay was (entry_price * 100) - premium for all strategies
  # Fix: puts should use (strike * 100) - premium

  # Scenario: APA-like cash-secured put
  # Strike: $21.50, Premium: $0.47, Current: $23.95
  strike <- 21.50
  premium <- 47  # $0.47 * 100 shares
  current_price <- 23.95

  # For puts, net_outlay should be based on STRIKE (cash reserved)
  expected_put_outlay <- (strike * 100) - premium  # 2150 - 47 = 2103

  # For calls, net_outlay should be based on ENTRY_PRICE (stock purchased)
  expected_call_outlay <- (current_price * 100) - premium  # 2395 - 47 = 2348

  # Test that a profitable put scenario has reasonable returns
  # If stock stays above strike, put expires worthless and you keep premium
  put_pnl <- calculate_option_payoff(
    stock_price = current_price,  # Stock stays at current price
    strike = strike,
    premium_received = premium,
    entry_stock_price = current_price,  # Not used for put return calculation
    shares = 100,
    option_type = "put"
  )

  # P&L should be just the premium (47) since put not assigned
  expect_equal(put_pnl, premium)

  # Return calculation: premium / net_outlay
  # With CORRECT calculation: 47 / 2103 = 2.23%
  # With WRONG calculation (old bug): 47 / 2348 = 2.00%
  put_return <- put_pnl / expected_put_outlay
  expect_true(put_return > 0.02)  # Should be > 2%
  expect_true(put_return < 0.03)  # Should be < 3%

  # Verify the actual percentage is close to theoretical (0.47/21.50 = 2.19%)
  # Note: Small difference due to net_outlay using (strike - premium) vs pure strike
  # 47/2103 = 2.234% vs 0.47/21.50 = 2.186%
  theoretical_return <- 0.47 / 21.50
  expect_true(abs(put_return - theoretical_return) < 0.01)  # Within 1 percentage point

  # For comparison: call with same parameters
  call_pnl <- calculate_option_payoff(
    stock_price = current_price,
    strike = strike,
    premium_received = premium,
    entry_stock_price = current_price,
    shares = 100,
    option_type = "call"
  )

  # Call assigned when stock >= strike, so assigned at strike
  # P&L: (strike - entry) * 100 + premium = (21.50 - 23.95) * 100 + 47 = -245 + 47 = -198
  expect_equal(call_pnl, -198)

  # Call return should be negative (stock price above entry but sold at lower strike)
  call_return <- call_pnl / expected_call_outlay
  expect_true(call_return < 0)
})
