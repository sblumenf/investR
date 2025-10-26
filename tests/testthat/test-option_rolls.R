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
