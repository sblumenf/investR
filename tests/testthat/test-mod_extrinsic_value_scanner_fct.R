
library(testthat)
library(dplyr)
library(tibble)

# Source the helper functions
source(test_path("../R/mod_extrinsic_value_scanner_fct.R"))

# Mock formatting functions if they are not globally available during testing
# In a real Golem app, these would be sourced from utils_formatting.R
# For testing purposes, we'll define simple mocks.
if (!exists("format_currency")) {
  format_currency <- function(x) { sprintf("$%.2f", x) }
}
if (!exists("format_percentage")) {
  format_percentage <- function(x) { sprintf("%.2f%%", x * 100) }
}
if (!exists("create_metric_row")) {
  create_metric_row <- function(label, value, is_primary = FALSE) { paste0(label, ": ", value) }
}

test_that("calculate_extrinsic_value works correctly", {
  # ITM Call
  expect_equal(calculate_extrinsic_value(option_premium = 10, stock_price = 100, strike_price = 95, option_type = "call"), 5)
  # OTM Call
  expect_equal(calculate_extrinsic_value(option_premium = 2, stock_price = 100, strike_price = 105, option_type = "call"), 2)
  # ATM Call
  expect_equal(calculate_extrinsic_value(option_premium = 3, stock_price = 100, strike_price = 100, option_type = "call"), 3)

  # ITM Put
  expect_equal(calculate_extrinsic_value(option_premium = 10, stock_price = 95, strike_price = 100, option_type = "put"), 5)
  # OTM Put
  expect_equal(calculate_extrinsic_value(option_premium = 2, stock_price = 105, strike_price = 100, option_type = "put"), 2)
  # ATM Put
  expect_equal(calculate_extrinsic_value(option_premium = 3, stock_price = 100, strike_price = 100, option_type = "put"), 3)

  # Zero premium
  expect_equal(calculate_extrinsic_value(option_premium = 0, stock_price = 100, strike_price = 100, option_type = "call"), 0)
})

test_that("estimate_reverse_collar_margin works correctly", {
  # Basic calculation
  expect_equal(estimate_reverse_collar_margin(stock_price = 100, strike_price = 100, option_type = "put"), 5000)
  expect_equal(estimate_reverse_collar_margin(stock_price = 50, strike_price = 50, option_type = "call"), 2500)
  # Ensure it's always positive
  expect_true(estimate_reverse_collar_margin(stock_price = 1, strike_price = 1, option_type = "put") > 0)
})

test_that("calculate_annualized_return works correctly", {
  # Standard case
  expect_equal(calculate_annualized_return(extrinsic_value = 500, estimated_margin = 5000, days_to_expiry = 45), (500/5000) * (365/45))
  # Edge cases
  expect_equal(calculate_annualized_return(extrinsic_value = 0, estimated_margin = 5000, days_to_expiry = 45), 0)
  expect_equal(calculate_annualized_return(extrinsic_value = 500, estimated_margin = 0, days_to_expiry = 45), 0)
  expect_equal(calculate_annualized_return(extrinsic_value = 500, estimated_margin = 5000, days_to_expiry = 0), 0)
})

test_that("create_scanner_opportunity_card returns a bslib card structure", {
  # Mock data for a single opportunity
  mock_opportunity <- tibble(
    symbol = "TEST",
    current_stock_price = 100,
    optionType = "Put",
    strikePrice = 100,
    expirationDate = as.Date("2025-12-31"),
    days_to_expiry = 45,
    lastPrice = 3.50,
    intrinsic_value = 0,
    extrinsic_value = 3.50,
    estimated_margin = 5000,
    annualized_return_pct = 0.05
  )

  card_output <- create_scanner_opportunity_card(mock_opportunity)

  # Check if it's a bslib card
  expect_s3_class(card_output, "bslib_card")
  # Check for header and body components (basic structure)
  expect_true(length(card_output$children) > 0)
  expect_true(any(sapply(card_output$children, function(x) inherits(x, "bslib_card_header"))))
  expect_true(any(sapply(card_output$children, function(x) inherits(x, "bslib_card_body"))))
})
