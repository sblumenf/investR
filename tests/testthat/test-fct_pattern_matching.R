test_that("detect_same_day_strategy identifies stock + option on same day", {
  # Create test data: stock purchase and option sale on same day
  activities <- tibble::tibble(
    activity_id = c("ACT001", "ACT002"),
    account_number = c("12345", "12345"),
    symbol = c("AAPL", "AAPL250117C00150000"),
    trade_date = c("2024-01-15", "2024-01-15"),
    action = c("Buy", "Sell"),
    quantity = c(100, -1),
    type = c("Trades", "Trades"),
    net_amount = c(-15000, 250)
  )

  # Run detection
  matches <- detect_same_day_strategy(activities)

  # Assertions
  expect_equal(nrow(matches), 1)
  expect_equal(matches$ticker, "AAPL")
  expect_equal(matches$trade_date, "2024-01-15")
  expect_equal(matches$stock_activity_id, "ACT001")
  expect_equal(matches$option_activity_id, "ACT002")
})

test_that("detect_same_day_strategy returns empty for different days", {
  # Stock and option on different days
  activities <- tibble::tibble(
    activity_id = c("ACT001", "ACT002"),
    account_number = c("12345", "12345"),
    symbol = c("AAPL", "AAPL250117C00150000"),
    trade_date = c("2024-01-15", "2024-01-16"),
    action = c("Buy", "Sell"),
    quantity = c(100, -1),
    type = c("Trades", "Trades"),
    net_amount = c(-15000, 250)
  )

  matches <- detect_same_day_strategy(activities)

  expect_equal(nrow(matches), 0)
})

test_that("detect_same_day_strategy handles multiple same-day positions", {
  # Two separate same-day positions for different tickers
  activities <- tibble::tibble(
    activity_id = c("ACT001", "ACT002", "ACT003", "ACT004"),
    account_number = c("12345", "12345", "12345", "12345"),
    symbol = c("AAPL", "AAPL250117C00150000", "MSFT", "MSFT250117C00400000"),
    trade_date = c("2024-01-15", "2024-01-15", "2024-01-15", "2024-01-15"),
    action = c("Buy", "Sell", "Buy", "Sell"),
    quantity = c(100, -1, 100, -1),
    type = c("Trades", "Trades", "Trades", "Trades"),
    net_amount = c(-15000, 250, -40000, 500)
  )

  matches <- detect_same_day_strategy(activities)

  expect_equal(nrow(matches), 2)
  expect_true("AAPL" %in% matches$ticker)
  expect_true("MSFT" %in% matches$ticker)
})

test_that("detect_dividend_capture identifies overnight stock hold", {
  # Buy on Day 1, sell on Day 2
  activities <- tibble::tibble(
    activity_id = c("ACT001", "ACT002"),
    account_number = c("12345", "12345"),
    symbol = c("JEPI", "JEPI"),
    trade_date = c("2024-01-15", "2024-01-16"),
    action = c("Buy", "Sell"),
    quantity = c(1000, -1000),
    type = c("Trades", "Trades"),
    net_amount = c(-55000, 55200)
  )

  matches <- detect_dividend_capture(activities)

  expect_equal(nrow(matches), 1)
  expect_equal(matches$ticker, "JEPI")
  expect_equal(matches$buy_activity_id, "ACT001")
  expect_equal(matches$sell_activity_id, "ACT002")
  expect_equal(matches$buy_date, "2024-01-15")
  expect_equal(matches$sell_date, "2024-01-16")
})

test_that("detect_dividend_capture rejects holds longer than 2 days", {
  # Buy on Day 1, sell on Day 4 (3 days apart)
  activities <- tibble::tibble(
    activity_id = c("ACT001", "ACT002"),
    account_number = c("12345", "12345"),
    symbol = c("JEPI", "JEPI"),
    trade_date = c("2024-01-15", "2024-01-19"),
    action = c("Buy", "Sell"),
    quantity = c(1000, -1000),
    type = c("Trades", "Trades"),
    net_amount = c(-55000, 55200)
  )

  matches <- detect_dividend_capture(activities)

  expect_equal(nrow(matches), 0)
})

test_that("detect_dividend_capture rejects same-day round trips", {
  # Buy and sell on same day
  activities <- tibble::tibble(
    activity_id = c("ACT001", "ACT002"),
    account_number = c("12345", "12345"),
    symbol = c("JEPI", "JEPI"),
    trade_date = c("2024-01-15", "2024-01-15"),
    action = c("Buy", "Sell"),
    quantity = c(1000, -1000),
    type = c("Trades", "Trades"),
    net_amount = c(-55000, 55200)
  )

  matches <- detect_dividend_capture(activities)

  expect_equal(nrow(matches), 0)
})

test_that("detect_delayed_covered_call identifies option sale on existing position", {
  # Activities: just an option sale
  activities <- tibble::tibble(
    activity_id = "ACT001",
    account_number = "12345",
    symbol = "BAC250117C00045000",
    trade_date = "2024-01-15",
    action = "Sell",
    quantity = -1,
    type = "Trades",
    net_amount = 50
  )

  # Current positions: we own BAC stock
  positions <- tibble::tibble(
    account_number = "12345",
    symbol = "BAC",
    open_quantity = 100,
    average_entry_price = 42.50
  )

  matches <- detect_delayed_covered_call(activities, positions)

  expect_equal(nrow(matches), 1)
  expect_equal(matches$ticker, "BAC")
  expect_equal(matches$option_activity_id, "ACT001")
  expect_equal(matches$position_quantity, 100)
  expect_equal(matches$position_avg_price, 42.50)
})

test_that("detect_delayed_covered_call returns empty when no stock position exists", {
  # Option sale for a stock we don't own
  activities <- tibble::tibble(
    activity_id = "ACT001",
    account_number = "12345",
    symbol = "AAPL250117C00150000",
    trade_date = "2024-01-15",
    action = "Sell",
    quantity = -1,
    type = "Trades",
    net_amount = 250
  )

  # Positions: we don't own AAPL
  positions <- tibble::tibble(
    account_number = "12345",
    symbol = "MSFT",
    open_quantity = 100,
    average_entry_price = 400
  )

  matches <- detect_delayed_covered_call(activities, positions)

  expect_equal(nrow(matches), 0)
})

test_that("detect_late_dividends identifies dividends for closed groups", {
  # Activities: dividend received
  activities <- tibble::tibble(
    activity_id = "ACT001",
    account_number = "12345",
    symbol = "JEPI",
    trade_date = "2024-01-20",
    type = "Dividends",
    net_amount = 450
  )

  # Closed groups: JEPI group closed recently (updated_at within 30 days)
  closed_groups <- tibble::tibble(
    group_id = "GRP001",
    group_name = "JEPI Dividend Capture Jan 15",  # Must start with ticker and contain "Dividend Capture"
    account_number = "12345",
    strategy_type = "Weekly Dividend Capture",
    status = "closed",
    updated_at = as.character(Sys.Date() - 5),  # Closed 5 days ago
    members = list(tibble::tibble(symbol = "JEPI", role = "underlying_stock"))
  )

  matches <- detect_late_dividends(activities, closed_groups)

  expect_equal(nrow(matches), 1)
  expect_equal(matches$ticker, "JEPI")
  expect_equal(matches$dividend_activity_id, "ACT001")
  expect_equal(matches$potential_group_id, "GRP001")
})

test_that("detect_late_dividends ignores dividends for open groups", {
  # Activities: dividend received
  activities <- tibble::tibble(
    activity_id = "ACT001",
    account_number = "12345",
    symbol = "JEPI",
    trade_date = "2024-01-20",
    type = "Dividends",
    net_amount = 450
  )

  # Open groups: JEPI group still open
  open_groups <- tibble::tibble(
    group_id = "GRP001",
    group_name = "JEPI Position",
    account_number = "12345",
    strategy_type = "Dividend Aristocrats",
    status = "open",
    members = list(tibble::tibble(symbol = "JEPI", role = "underlying_stock"))
  )

  matches <- detect_late_dividends(activities, open_groups)

  # Should be empty because we only match closed groups
  expect_equal(nrow(matches), 0)
})

test_that("check_ambiguous_match detects multiple stocks and options same day", {
  # Two stock purchases and two option sales on same day for AAPL
  # This creates 4 possible combinations (2 stocks × 2 options = 4 matches)
  same_day_matches <- tibble::tibble(
    ticker = c("AAPL", "AAPL", "AAPL", "AAPL"),
    trade_date = c("2024-01-15", "2024-01-15", "2024-01-15", "2024-01-15"),
    stock_activity_id = c("ACT001", "ACT002", "ACT001", "ACT002"),
    option_activity_id = c("ACT003", "ACT003", "ACT004", "ACT004")
  )

  ambiguous <- check_ambiguous_match(same_day_matches)

  expect_equal(nrow(ambiguous), 1)
  expect_equal(ambiguous$ticker, "AAPL")
  # Should have all 4 combinations' IDs in the lists
  expect_equal(length(ambiguous$stock_activity_ids[[1]]), 4)
  expect_equal(length(ambiguous$option_activity_ids[[1]]), 4)
  expect_true(ambiguous$is_ambiguous)
})

test_that("check_ambiguous_match returns empty for unambiguous matches", {
  # One stock, one option
  same_day_matches <- tibble::tibble(
    ticker = "AAPL",
    trade_date = "2024-01-15",
    stock_activity_id = "ACT001",
    option_activity_id = "ACT002",
    account_number = "12345"
  )

  ambiguous <- check_ambiguous_match(same_day_matches)

  expect_equal(nrow(ambiguous), 0)
})

test_that("pattern matching handles empty input gracefully", {
  empty_activities <- tibble::tibble(
    activity_id = character(),
    account_number = character(),
    symbol = character(),
    trade_date = character(),
    action = character(),
    quantity = numeric(),
    type = character(),
    net_amount = numeric()
  )

  expect_equal(nrow(detect_same_day_strategy(empty_activities)), 0)
  expect_equal(nrow(detect_dividend_capture(empty_activities)), 0)
  expect_equal(nrow(detect_delayed_covered_call(empty_activities, empty_activities)), 0)
  expect_equal(nrow(detect_late_dividends(empty_activities, empty_activities)), 0)
})

test_that("pattern matching creates all combinations for multiple accounts", {
  # Same ticker, same day, but different accounts
  # Note: Current implementation creates Cartesian product (all combinations)
  # This is expected behavior that will be filtered later by suggestion engine
  activities <- tibble::tibble(
    activity_id = c("ACT001", "ACT002", "ACT003", "ACT004"),
    account_number = c("12345", "12345", "67890", "67890"),
    symbol = c("AAPL", "AAPL250117C00150000", "AAPL", "AAPL250117C00150000"),
    trade_date = c("2024-01-15", "2024-01-15", "2024-01-15", "2024-01-15"),
    action = c("Buy", "Sell", "Buy", "Sell"),
    quantity = c(100, -1, 100, -1),
    type = c("Trades", "Trades", "Trades", "Trades"),
    net_amount = c(-15000, 250, -15000, 250)
  )

  matches <- detect_same_day_strategy(activities)

  # Creates 4 matches: 2 stocks × 2 options
  expect_equal(nrow(matches), 4)
  expect_equal(matches$ticker[1], "AAPL")
})
