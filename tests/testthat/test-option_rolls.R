test_that("detect_option_roll identifies roll pattern", {
  skip_on_cran()

  # Create test activities for a BAC covered call roll
  group_activities <- tibble::tibble(
    symbol = c("BAC", "BAC24Oct25C51.00", "BAC21Nov25C52.00"),
    action = c("Buy", "Buy", "Sell"),
    trade_date = as.POSIXct(c("2024-03-09", "2025-10-23", "2025-10-23")),
    type = c("Trades", "Trades", "Trades")
  )

  # The sell-to-open activity (new option)
  new_sell_activity <- group_activities %>% filter(symbol == "BAC21Nov25C52.00")

  # Detect roll
  result <- detect_option_roll(new_sell_activity, group_activities)

  expect_true(result$is_roll)
  expect_equal(result$old_symbol, "BAC24Oct25C51.00")
  expect_equal(result$new_symbol, "BAC21Nov25C52.00")
})

test_that("detect_option_roll returns FALSE for non-roll sell", {
  skip_on_cran()

  # Activities with just an option sell, no matching buy-to-close
  group_activities <- tibble::tibble(
    symbol = c("BAC", "BAC24Oct25C51.00"),
    action = c("Buy", "Sell"),
    trade_date = as.POSIXct(c("2024-03-09", "2025-04-09")),
    type = c("Trades", "Trades")
  )

  # The option sell activity
  sell_activity <- group_activities %>% filter(symbol == "BAC24Oct25C51.00")

  # Detect roll (should be FALSE - no buy-to-close on same day)
  result <- detect_option_roll(sell_activity, group_activities)

  expect_false(result$is_roll)
  expect_null(result$old_symbol)
  expect_null(result$new_symbol)
})

test_that("detect_option_roll returns FALSE for stock transaction", {
  skip_on_cran()

  # Stock buy activity
  stock_buy <- tibble::tibble(
    symbol = "BAC",
    action = "Buy",
    trade_date = as.POSIXct("2024-03-09"),
    type = "Trades"
  )

  group_activities <- tibble::tibble(
    symbol = c("BAC"),
    action = c("Buy"),
    trade_date = as.POSIXct(c("2024-03-09")),
    type = c("Trades")
  )

  # Detect roll (should be FALSE - not an option sale)
  result <- detect_option_roll(stock_buy, group_activities)

  expect_false(result$is_roll)
})

test_that("detect_option_roll handles different underlying tickers", {
  skip_on_cran()

  # Buy-to-close AAPL option, sell-to-open BAC option (not a roll)
  group_activities <- tibble::tibble(
    symbol = c("AAPL", "AAPL15Nov25C150.00", "BAC21Nov25C52.00"),
    action = c("Buy", "Buy", "Sell"),
    trade_date = as.POSIXct(c("2024-03-09", "2025-10-23", "2025-10-23")),
    type = c("Trades", "Trades", "Trades")
  )

  new_sell_activity <- group_activities %>% filter(symbol == "BAC21Nov25C52.00")

  # Should not detect as roll (different underlying)
  result <- detect_option_roll(new_sell_activity, group_activities)

  expect_false(result$is_roll)
})

test_that("update_group_option_member updates symbol successfully", {
  skip_on_cran()
  local_test_db()

  # Create test group
  group_id <- paste0("TEST_ROLL_", format(Sys.time(), "%Y%m%d%H%M%S"))

  # Create group with old option
  members <- tibble::tibble(
    symbol = c("BAC", "BAC24Oct25C51.00"),
    role = c("underlying_stock", "short_call")
  )

  create_result <- create_position_group(
    group_id = group_id,
    group_name = "Test Roll Group",
    strategy_type = "Covered Call",
    account_number = "TEST123",
    members = members
  )

  skip_if(!create_result, "Failed to create test group")

  # Update the option member (simulate roll)
  update_result <- update_group_option_member(
    group_id = group_id,
    old_symbol = "BAC24Oct25C51.00",
    new_symbol = "BAC21Nov25C52.00"
  )

  expect_true(update_result)

  # Verify member was updated
  updated_members <- get_group_members(group_id)
  expect_equal(nrow(updated_members), 2)

  option_member <- updated_members %>% filter(role == "short_call")
  expect_equal(option_member$symbol, "BAC21Nov25C52.00")
})

test_that("update_group_option_member handles missing old symbol", {
  skip_on_cran()
  local_test_db()

  # Create test group
  group_id <- paste0("TEST_ROLL_MISSING_", format(Sys.time(), "%Y%m%d%H%M%S"))

  # Create group with different option
  members <- tibble::tibble(
    symbol = c("BAC", "BAC15Nov25C50.00"),
    role = c("underlying_stock", "short_call")
  )

  create_result <- create_position_group(
    group_id = group_id,
    group_name = "Test Missing Symbol",
    strategy_type = "Covered Call",
    account_number = "TEST123",
    members = members
  )

  skip_if(!create_result, "Failed to create test group")

  # Try to update with non-existent old symbol
  update_result <- update_group_option_member(
    group_id = group_id,
    old_symbol = "BAC24Oct25C51.00",  # This doesn't exist in members
    new_symbol = "BAC21Nov25C52.00"
  )

  expect_false(update_result)

  # Verify member was NOT changed
  members_after <- get_group_members(group_id)
  option_member <- members_after %>% filter(role == "short_call")
  expect_equal(option_member$symbol, "BAC15Nov25C50.00")  # Still the original
})

test_that("detect_option_roll handles expiration as closing event", {
  skip_on_cran()

  # Create test activities for CZR: option expires worthless, new option sold same day
  group_activities <- tibble::tibble(
    symbol = c("CZR", "CZR17Oct25C22.00", "CZR31Oct25C22.00"),
    action = c("Buy", "EXP", "Sell"),  # EXP instead of Buy
    trade_date = as.POSIXct(c("2025-08-29", "2025-10-20", "2025-10-20")),
    type = c("Trades", "Other", "Trades")  # EXP has type "Other"
  )

  # The sell-to-open activity (new option)
  new_sell_activity <- group_activities %>% filter(symbol == "CZR31Oct25C22.00")

  # Detect roll
  result <- detect_option_roll(new_sell_activity, group_activities)

  expect_true(result$is_roll)
  expect_equal(result$old_symbol, "CZR17Oct25C22.00")
  expect_equal(result$new_symbol, "CZR31Oct25C22.00")
})

test_that("Zero-Dividend Stocks: initial premium is NOT a cash flow, roll premium IS", {
  skip_on_cran()
  local_test_db()

  # Create test group
  group_id <- paste0("TEST_ZERODIV_CF_", format(Sys.time(), "%Y%m%d%H%M%S"))

  # Create Zero-Dividend Stocks group
  members <- tibble::tibble(
    symbol = c("CZR", "CZR17Oct25C22.00"),
    role = c("underlying_stock", "short_call")
  )

  create_result <- create_position_group(
    group_id = group_id,
    group_name = "Test Zero-Div Group",
    strategy_type = "Zero-Dividend Stocks",
    account_number = "TEST123",
    members = members
  )

  skip_if(!create_result, "Failed to create test group")

  # Create stock buy and initial option sell (same day)
  conn <- get_portfolio_db_connection()

  stock_buy <- tibble::tibble(
    activity_id = paste0("TEST_STOCK_", format(Sys.time(), "%Y%m%d%H%M%S")),
    account_number = "TEST123",
    account_type = "LIRA",
    trade_date = as.POSIXct("2025-08-29"),
    transaction_date = as.POSIXct("2025-08-29"),
    settlement_date = as.POSIXct("2025-09-02"),
    action = "Buy",
    symbol = "CZR",
    symbol_id = NA_integer_,
    description = "Test stock buy",
    currency = "USD",
    quantity = 300,
    price = 26.69,
    gross_amount = 8007.00,
    commission = -1.98,
    net_amount = -8008.98,
    type = "Trades",
    group_id = group_id,
    is_processed = TRUE,
    ignore_for_grouping = FALSE,
    fetched_at = Sys.time()
  )

  initial_option <- tibble::tibble(
    activity_id = paste0("TEST_OPT1_", format(Sys.time(), "%Y%m%d%H%M%S")),
    account_number = "TEST123",
    account_type = "LIRA",
    trade_date = as.POSIXct("2025-08-29"),  # Same day as stock buy
    transaction_date = as.POSIXct("2025-08-29"),
    settlement_date = as.POSIXct("2025-09-02"),
    action = "Sell",
    symbol = "CZR17Oct25C22.00",
    symbol_id = NA_integer_,
    description = "Test initial option sell",
    currency = "USD",
    quantity = -3,
    price = 5.11,
    gross_amount = 1533.00,
    commission = -2.97,
    net_amount = 1530.03,
    type = "Trades",
    group_id = NA_character_,
    is_processed = TRUE,
    ignore_for_grouping = FALSE,
    fetched_at = Sys.time()
  )

  roll_option <- tibble::tibble(
    activity_id = paste0("TEST_OPT2_", format(Sys.time(), "%Y%m%d%H%M%S")),
    account_number = "TEST123",
    account_type = "LIRA",
    trade_date = as.POSIXct("2025-10-20"),  # Different day (roll)
    transaction_date = as.POSIXct("2025-10-20"),
    settlement_date = as.POSIXct("2025-10-22"),
    action = "Sell",
    symbol = "CZR31Oct25C22.00",
    symbol_id = NA_integer_,
    description = "Test roll option sell",
    currency = "USD",
    quantity = -3,
    price = 1.19,
    gross_amount = 357.00,
    commission = -0.97,
    net_amount = 356.03,
    type = "Trades",
    group_id = NA_character_,
    is_processed = TRUE,
    ignore_for_grouping = FALSE,
    fetched_at = Sys.time()
  )

  DBI::dbWriteTable(conn, "account_activities", stock_buy, append = TRUE)
  DBI::dbWriteTable(conn, "account_activities", initial_option, append = TRUE)
  DBI::dbWriteTable(conn, "account_activities", roll_option, append = TRUE)
  DBI::dbDisconnect(conn, shutdown = TRUE)

  # Link initial option (should NOT create cash flow)
  link_result1 <- link_activities_to_group(
    activity_ids = initial_option$activity_id,
    group_id = group_id
  )
  expect_true(link_result1)

  cash_flows_after_initial <- get_group_cash_flows(group_id)
  initial_premiums <- cash_flows_after_initial %>% filter(event_type == "option_premium", status == "actual")
  expect_equal(nrow(initial_premiums), 0)  # Should be ZERO (initial premium is cost reduction)

  # Link roll option (should CREATE cash flow)
  link_result2 <- link_activities_to_group(
    activity_ids = roll_option$activity_id,
    group_id = group_id
  )
  expect_true(link_result2)

  cash_flows_after_roll <- get_group_cash_flows(group_id)
  roll_premiums <- cash_flows_after_roll %>% filter(event_type == "option_premium", status == "actual")
  expect_equal(nrow(roll_premiums), 1)  # Should be ONE (roll premium is income)
  expect_equal(roll_premiums$amount[1], 356.03)
})
