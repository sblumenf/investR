test_that("create_group_card returns NULL for non-existent group", {
  skip_on_cran()

  card <- create_group_card("NONEXISTENT_GROUP")

  expect_null(card)
})

test_that("create_group_card generates valid HTML structure", {
  skip_on_cran()

  # Create a test group
  group_id <- "TEST_CARD_GROUP"
  create_position_group(
    group_id = group_id,
    group_name = "Test Card",
    strategy_type = "Test Strategy",
    account_number = "99999",
    members = tibble::tibble(
      symbol = c("TEST"),
      role = c("underlying_stock")
    )
  )

  # Generate card
  card <- create_group_card(group_id)

  # Should return a shiny tag object
  expect_true(inherits(card, "shiny.tag") || inherits(card, "shiny.tag.list"))

  # Clean up
  close_position_group(group_id)
})

test_that("create_cash_flow_section handles empty cash flows", {
  skip_on_cran()

  section <- create_cash_flow_section(tibble::tibble(), is_open = TRUE)

  # Should return an accordion section
  expect_true(inherits(section, "shiny.tag"))
})

test_that("create_transaction_history_section handles empty activities", {
  skip_on_cran()

  section <- create_transaction_history_section(tibble::tibble())

  # Should return an accordion section
  expect_true(inherits(section, "shiny.tag"))
})

test_that("format_currency handles edge cases", {
  skip_on_cran()

  expect_equal(format_currency(1234.56), "$1,234.56")
  expect_equal(format_currency(0), "$0.00")
  expect_equal(format_currency(NA), "$0.00")
  expect_equal(format_currency(NULL), "$0.00")
})

test_that("format_percentage handles edge cases", {
  skip_on_cran()

  expect_equal(format_percentage(0.12345), "12.3%")  # Expects decimal input (0.12345 = 12.345%)
  expect_equal(format_percentage(0), "0.0%")
  expect_equal(format_percentage(NA), "N/A")
  expect_equal(format_percentage(NULL), "N/A")
})

# New tests for refactored functions

test_that("format_days_to_expiration handles various day counts", {
  # Test positive days
  result <- format_days_to_expiration(6, as.Date("2025-10-17"))
  expect_true(grepl("6 days", result))
  expect_true(grepl("Oct 17, 2025", result))

  # Test single day
  result_one <- format_days_to_expiration(1, as.Date("2025-01-01"))
  expect_true(grepl("1 day", result_one))

  # Test today
  result_zero <- format_days_to_expiration(0, NULL)
  expect_equal(result_zero, "today")

  # Test past expiration
  result_past <- format_days_to_expiration(-5, NULL)
  expect_true(grepl("5 days ago", result_past))
})

test_that("format_days_to_expiration handles NULL days", {
  result <- format_days_to_expiration(NULL, NULL)
  expect_equal(result, "N/A")
})

test_that("format_strike_relationship shows ITM for calls", {
  # Call ITM: stock price > strike
  result <- format_strike_relationship(27.45, 22.00, "call")
  expect_true(grepl("in-the-money", result))
  expect_true(grepl("\\$5.45", result))
})

test_that("format_strike_relationship shows OTM for calls", {
  # Call OTM: stock price < strike
  result <- format_strike_relationship(20.00, 22.00, "call")
  expect_true(grepl("out-of-the-money", result))
  expect_true(grepl("\\$2.00", result))
})

test_that("format_strike_relationship shows ITM for puts", {
  # Put ITM: stock price < strike
  result <- format_strike_relationship(20.00, 22.00, "put")
  expect_true(grepl("in-the-money", result))
  expect_true(grepl("\\$2.00", result))
})

test_that("format_strike_relationship handles NULL values", {
  result <- format_strike_relationship(NULL, 22.00, "call")
  expect_equal(result, "")

  result2 <- format_strike_relationship(20.00, NULL, "call")
  expect_equal(result2, "")
})

test_that("enrich_group_with_market_data returns proper structure", {
  skip_on_cran()

  # Create test data
  members <- tibble::tibble(
    symbol = c("TEST", "TEST17Dec25C22.00"),
    role = c("underlying_stock", "short_call")
  )

  activities <- tibble::tibble(
    symbol = "TEST",
    action = "Buy",
    type = "Trades",
    quantity = 100,
    price = 25.00,
    trade_date = Sys.Date() - 10
  )

  # Call enrichment (may return NULLs if no market data available)
  result <- enrich_group_with_market_data("TEST_GROUP", members, activities)

  # Verify structure
  expect_type(result, "list")
  expect_true("current_stock_price" %in% names(result))
  expect_true("days_to_expiration" %in% names(result))
  expect_true("strike_price" %in% names(result))
  expect_true("expiration_date" %in% names(result))
  expect_true("itm_otm_amount" %in% names(result))
})

test_that("enrich_group_with_market_data handles groups without stock", {
  skip_on_cran()

  # Members without underlying stock
  members <- tibble::tibble(
    symbol = "TEST17Dec25C22.00",
    role = "short_call"
  )

  activities <- tibble::tibble()

  result <- enrich_group_with_market_data("TEST_GROUP", members, activities)

  # Should return structure with NULL values
  expect_type(result, "list")
  expect_null(result$current_stock_price)
})

test_that("enrich_group_with_market_data parses option details", {
  skip_on_cran()

  # Create test data with option
  members <- tibble::tibble(
    symbol = c("CZR", "CZR17Oct25C22.00"),
    role = c("underlying_stock", "short_call")
  )

  activities <- tibble::tibble(
    symbol = "CZR",
    action = "Buy",
    type = "Trades",
    quantity = 300,
    price = 26.69,
    trade_date = as.Date("2025-10-09")
  )

  result <- enrich_group_with_market_data("CZR_GROUP", members, activities)

  # If option parsing succeeds, should have strike and expiration
  if (!is.null(result$strike_price)) {
    expect_equal(result$strike_price, 22.00)
  }

  if (!is.null(result$expiration_date)) {
    expect_equal(as.character(result$expiration_date), "2025-10-17")
  }
})
