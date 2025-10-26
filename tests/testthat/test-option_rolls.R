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

  # Create test group
  group_id <- paste0("TEST_ROLL_", format(Sys.time(), "%Y%m%d%H%M%S"))

  # Clean up first
  tryCatch({
    conn <- get_portfolio_db_connection()
    DBI::dbExecute(conn, "DELETE FROM position_group_members WHERE group_id = ?", params = list(group_id))
    DBI::dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
    DBI::dbDisconnect(conn, shutdown = TRUE)
  }, error = function(e) NULL)

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

  # Clean up
  tryCatch({
    conn <- get_portfolio_db_connection()
    DBI::dbExecute(conn, "DELETE FROM position_group_members WHERE group_id = ?", params = list(group_id))
    DBI::dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
    DBI::dbDisconnect(conn, shutdown = TRUE)
  }, error = function(e) NULL)
})

test_that("update_group_option_member handles missing old symbol", {
  skip_on_cran()

  # Create test group
  group_id <- paste0("TEST_ROLL_MISSING_", format(Sys.time(), "%Y%m%d%H%M%S"))

  # Clean up first
  tryCatch({
    conn <- get_portfolio_db_connection()
    DBI::dbExecute(conn, "DELETE FROM position_group_members WHERE group_id = ?", params = list(group_id))
    DBI::dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
    DBI::dbDisconnect(conn, shutdown = TRUE)
  }, error = function(e) NULL)

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

  # Clean up
  tryCatch({
    conn <- get_portfolio_db_connection()
    DBI::dbExecute(conn, "DELETE FROM position_group_members WHERE group_id = ?", params = list(group_id))
    DBI::dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
    DBI::dbDisconnect(conn, shutdown = TRUE)
  }, error = function(e) NULL)
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

test_that("cash flows created for Zero-Dividend Stocks strategy", {
  skip_on_cran()

  # Create test group
  group_id <- paste0("TEST_ZERODIV_CF_", format(Sys.time(), "%Y%m%d%H%M%S"))

  # Clean up first
  tryCatch({
    conn <- get_portfolio_db_connection()
    DBI::dbExecute(conn, "DELETE FROM account_activities WHERE group_id = ?", params = list(group_id))
    DBI::dbExecute(conn, "DELETE FROM position_group_cash_flows WHERE group_id = ?", params = list(group_id))
    DBI::dbExecute(conn, "DELETE FROM position_group_members WHERE group_id = ?", params = list(group_id))
    DBI::dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
    DBI::dbDisconnect(conn, shutdown = TRUE)
  }, error = function(e) NULL)

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

  # Create mock option sell activity
  conn <- get_portfolio_db_connection()
  activity_df <- tibble::tibble(
    activity_id = paste0("TEST_ACT_", format(Sys.time(), "%Y%m%d%H%M%S")),
    account_number = "TEST123",
    account_type = "LIRA",
    trade_date = as.POSIXct("2025-08-29"),
    transaction_date = as.POSIXct("2025-08-29"),
    settlement_date = as.POSIXct("2025-09-02"),
    action = "Sell",
    symbol = "CZR17Oct25C22.00",
    symbol_id = NA_integer_,
    description = "Test option sell",
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

  DBI::dbWriteTable(conn, "account_activities", activity_df, append = TRUE)
  DBI::dbDisconnect(conn, shutdown = TRUE)

  # Link activity to group
  link_result <- link_activities_to_group(
    activity_ids = activity_df$activity_id,
    group_id = group_id
  )

  expect_true(link_result)

  # Verify cash flow was created
  cash_flows <- get_group_cash_flows(group_id)
  option_premiums <- cash_flows %>% filter(event_type == "option_premium", status == "actual")

  expect_gt(nrow(option_premiums), 0)
  expect_equal(option_premiums$amount[1], 1530.03)

  # Clean up
  tryCatch({
    conn <- get_portfolio_db_connection()
    DBI::dbExecute(conn, "DELETE FROM account_activities WHERE group_id = ?", params = list(group_id))
    DBI::dbExecute(conn, "DELETE FROM position_group_cash_flows WHERE group_id = ?", params = list(group_id))
    DBI::dbExecute(conn, "DELETE FROM position_group_members WHERE group_id = ?", params = list(group_id))
    DBI::dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
    DBI::dbDisconnect(conn, shutdown = TRUE)
  }, error = function(e) NULL)
})
