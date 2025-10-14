test_that("parse_option_details extracts strike and expiry", {
  # Test format: ddMMMYY (e.g., "ALB17Dec27C55.00")
  result1 <- parse_option_details("ALB17Dec27C55.00")
  expect_equal(result1$strike, 55.00)
  expect_equal(result1$expiry, as.Date("2027-12-17"))

  # Test format: ddMMMYY (e.g., "IBM15Jan27C175.00")
  result2 <- parse_option_details("IBM15Jan27C175.00")
  expect_equal(result2$strike, 175.00)
  expect_equal(result2$expiry, as.Date("2027-01-15"))

  # Test format: YYMMDD (e.g., "AAPL240119C150")
  result3 <- parse_option_details("AAPL240119C150")
  expect_equal(result3$strike, 150)
  expect_equal(result3$expiry, as.Date("2024-01-19"))

  # Test invalid input
  result4 <- parse_option_details("INVALID")
  expect_null(result4$strike)
  expect_null(result4$expiry)

  # Test empty input
  result5 <- parse_option_details("")
  expect_null(result5$strike)
  expect_null(result5$expiry)
})

test_that("generate_option_gain_event calculates gain correctly", {
  # Setup: Covered call position
  # Stock: 200 shares @ $45 = $9,000
  # Option: Short 2 calls @ $0.66 premium = $132 received
  # Strike: $55
  # Expiry: 2027-12-17

  stock_cost <- 200 * 45  # $9,000
  premium_received <- 2 * 66  # $132 (premium is in cents, so 0.66 * 100)
  strike_price <- 55
  shares <- 200
  expiry_date <- as.Date("2027-12-17")

  result <- generate_option_gain_event(
    expiry_date = expiry_date,
    strike_price = strike_price,
    shares = shares,
    stock_cost = stock_cost,
    premium_received = premium_received
  )

  # Expected:
  # Exercise proceeds = 55 * 200 = $11,000
  # Net debit = $9,000 - $132 = $8,868
  # Gain = $11,000 - $8,868 = $2,132

  expect_equal(nrow(result), 1)
  expect_equal(result$event_type, "option_gain")
  expect_equal(result$event_date, expiry_date)
  expect_equal(result$amount, 11000 - 8868)  # $2,132
  expect_equal(result$confidence, "high")
})

test_that("generate_option_gain_event handles zero/negative gain", {
  # Setup where gain would be zero or negative
  stock_cost <- 200 * 60  # $12,000 (bought high)
  premium_received <- 100  # $100 premium
  strike_price <- 55
  shares <- 200
  expiry_date <- as.Date("2027-12-17")

  result <- generate_option_gain_event(
    expiry_date = expiry_date,
    strike_price = strike_price,
    shares = shares,
    stock_cost = stock_cost,
    premium_received = premium_received
  )

  # Exercise proceeds = 55 * 200 = $11,000
  # Net debit = $12,000 - $100 = $11,900
  # Gain = $11,000 - $11,900 = -$900 (loss)
  # Should return empty tibble

  expect_equal(nrow(result), 0)
})

test_that("generate_dividend_events returns empty for zero-dividend stocks", {
  skip_on_cran()

  # Use a known zero-dividend ticker (e.g., BRK.B or similar)
  # Note: This test may need to be updated if the ticker starts paying dividends
  result <- generate_dividend_events(
    ticker = "BRK.B",
    shares = 100,
    end_date = Sys.Date() + lubridate::days(365)
  )

  expect_equal(nrow(result), 0)
})
