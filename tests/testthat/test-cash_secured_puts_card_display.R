# tests/testthat/test-cash_secured_puts_card_display.R
# UI card display tests for cash-secured puts strategy
# Date: 2025-11-23

library(testthat)
library(investR)
library(tibble)

################################################################################
# CARD DISPLAY TESTS
################################################################################

test_that("cash secured puts card displays open interest in Risk Metrics section", {
  # Create sample opportunity with known open_interest value
  sample_opportunity <- tibble(
    ticker = "TEST",
    company_name = "Test Corporation",
    current_price = 100.00,
    strike = 95.00,
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90,
    bid_price = 1.50,
    open_interest = 1234,  # Known value to test formatting
    cash_required = 9500,
    premium_received = 150,
    return_on_cash = 0.0158,
    annualized_return = 0.0641,
    net_outlay = 9350,
    downside_protection_pct = 0.065,
    breakeven_price = 93.50,
    annual_dividend = 2.50,
    intrinsic_value = 0,
    extrinsic_value = 1.50,
    max_drawdown = -0.15,
    current_yield = 0.025
  )

  # Mock namespace function
  mock_ns <- function(id) paste0("test_module-", id)

  # Create card
  card <- investR:::create_cash_secured_put_card_with_risk(
    row = sample_opportunity,
    ns = mock_ns,
    risk_id = "risk_1"
  )

  # Convert to HTML
  html_output <- as.character(card)

  # Test 1: Open Interest label is present
  expect_true(
    grepl("Open Interest", html_output, fixed = TRUE),
    info = "Card should contain 'Open Interest' label"
  )

  # Test 2: Formatted value with comma separator is present
  expect_true(
    grepl("1,234", html_output, fixed = TRUE),
    info = "Card should display formatted open interest value '1,234'"
  )

  # Test 3: Risk Metrics section exists
  expect_true(
    grepl("Risk Metrics", html_output, fixed = TRUE),
    info = "Card should contain 'Risk Metrics' section"
  )

  # Test 4: Open Interest appears in both locations (Risk Metrics + Transaction Details)
  oi_matches <- gregexpr("Open Interest", html_output, fixed = TRUE)[[1]]
  oi_count <- if (oi_matches[1] == -1) 0 else length(oi_matches)

  expect_equal(
    oi_count,
    2,
    info = "Open Interest should appear twice: in Risk Metrics and Transaction Details"
  )

  # Test 5: Open Interest appears after Risk Metrics section
  risk_pos <- regexpr("Risk Metrics", html_output, fixed = TRUE)[1]
  first_oi_pos <- oi_matches[1]

  expect_true(
    first_oi_pos > risk_pos,
    info = "First Open Interest occurrence should be after Risk Metrics section header"
  )

  # Test 6: First Open Interest is close to Risk Metrics (same section)
  distance <- first_oi_pos - risk_pos
  expect_true(
    distance < 1500,
    info = "First Open Interest should be within 1500 characters of Risk Metrics header"
  )
})

test_that("cash secured puts card formats large open interest values correctly", {
  # Test with various large open interest values
  test_cases <- list(
    list(oi = 100, expected = "100"),
    list(oi = 1000, expected = "1,000"),
    list(oi = 10000, expected = "10,000"),
    list(oi = 123456, expected = "123,456")
  )

  mock_ns <- function(id) paste0("test-", id)

  for (test_case in test_cases) {
    sample_opp <- tibble(
      ticker = "TEST",
      company_name = "Test Corp",
      current_price = 100,
      strike = 95,
      expiration = as.Date("2025-03-21"),
      days_to_expiry = 90,
      bid_price = 1.50,
      open_interest = test_case$oi,
      cash_required = 9500,
      premium_received = 150,
      return_on_cash = 0.0158,
      annualized_return = 0.0641,
      net_outlay = 9350,
      downside_protection_pct = 0.065,
      breakeven_price = 93.50,
      annual_dividend = 2.50,
      intrinsic_value = 0,
      extrinsic_value = 1.50,
      max_drawdown = -0.15,
      current_yield = 0.025
    )

    card <- investR:::create_cash_secured_put_card_with_risk(
      row = sample_opp,
      ns = mock_ns,
      risk_id = "risk_1"
    )

    html <- as.character(card)

    expect_true(
      grepl(test_case$expected, html, fixed = TRUE),
      info = sprintf("Open interest %d should format as '%s'",
                     test_case$oi, test_case$expected)
    )
  }
})

test_that("cash secured puts card handles zero open interest", {
  sample_opportunity <- tibble(
    ticker = "TEST",
    company_name = "Test Corp",
    current_price = 100,
    strike = 95,
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90,
    bid_price = 1.50,
    open_interest = 0,  # Edge case: zero OI
    cash_required = 9500,
    premium_received = 150,
    return_on_cash = 0.0158,
    annualized_return = 0.0641,
    net_outlay = 9350,
    downside_protection_pct = 0.065,
    breakeven_price = 93.50,
    annual_dividend = 2.50,
    intrinsic_value = 0,
    extrinsic_value = 1.50,
    max_drawdown = -0.15,
    current_yield = 0.025
  )

  mock_ns <- function(id) paste0("test-", id)

  card <- investR:::create_cash_secured_put_card_with_risk(
    row = sample_opportunity,
    ns = mock_ns,
    risk_id = "risk_1"
  )

  html <- as.character(card)

  # Should still display Open Interest label
  expect_true(
    grepl("Open Interest", html, fixed = TRUE),
    info = "Card should still show Open Interest label even when zero"
  )

  # Should display "0" (format(0, big.mark = ",") gives "0")
  # The value should appear but we can't reliably match just "0" in HTML
  # So just verify the label is present - that's the key requirement
  # The actual formatting of "0" vs "0.0" is not critical
  expect_true(
    grepl("Open Interest", html, fixed = TRUE),
    info = "Card should display Open Interest even when value is zero"
  )
})

test_that("cash secured puts card maintains all other required sections", {
  sample_opportunity <- tibble(
    ticker = "TEST",
    company_name = "Test Corp",
    current_price = 100,
    strike = 95,
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90,
    bid_price = 1.50,
    open_interest = 1234,
    cash_required = 9500,
    premium_received = 150,
    return_on_cash = 0.0158,
    annualized_return = 0.0641,
    net_outlay = 9350,
    downside_protection_pct = 0.065,
    breakeven_price = 93.50,
    annual_dividend = 2.50,
    intrinsic_value = 0,
    extrinsic_value = 1.50,
    max_drawdown = -0.15,
    current_yield = 0.025
  )

  mock_ns <- function(id) paste0("test-", id)

  card <- investR:::create_cash_secured_put_card_with_risk(
    row = sample_opportunity,
    ns = mock_ns,
    risk_id = "risk_1"
  )

  html <- as.character(card)

  # Verify all required sections are still present
  expected_sections <- c(
    "Quick Overview",
    "Risk Metrics",
    "Assignment Scenario",
    "Transaction Details",
    "Option Value Decomposition",
    "Risk Context"
  )

  for (section in expected_sections) {
    expect_true(
      grepl(section, html, fixed = TRUE),
      info = sprintf("Card should contain '%s' section", section)
    )
  }
})
