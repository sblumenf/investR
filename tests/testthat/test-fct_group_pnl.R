test_that("calculate_group_pnl computes correct total and annualized returns", {
  # Setup: Create test group and activities in database
  skip_on_ci()
  skip_on_cran()

  # Create test group
  group_id <- paste0("TEST_PNL_", format(Sys.time(), "%Y%m%d%H%M%S"))
  create_position_group(
    group_id = group_id,
    group_name = "Test P&L Group",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = c("AAPL", "AAPL250117C00150000"),
      role = c("underlying_stock", "short_call")
    )
  )

  # Create activities for 100-day hold
  start_date <- Sys.Date() - 100
  end_date <- Sys.Date()

  activities <- tibble::tibble(
    account_number = rep("TEST123", 4),
    symbol = c("AAPL", "AAPL250117C00150000", "AAPL", "AAPL"),
    trade_date = c(start_date, start_date, start_date + 50, end_date),
    transaction_date = c(start_date, start_date, start_date + 50, end_date),
    settlement_date = c(start_date + 2, start_date + 2, start_date + 52, end_date + 2),
    action = c("Buy", "Sell", NA, "Sell"),
    quantity = c(100, -1, NA, -100),
    price = c(150, 2.50, NA, 155),
    gross_amount = c(-15000, 250, 0, 15500),
    commission = c(0, 0, 0, 0),
    net_amount = c(-15000, 250, 100, 15500),
    type = c("Trades", "Trades", "Dividends", "Trades"),
    description = c("Buy AAPL", "Sell Call", "Dividend", "Sell AAPL"),
    currency = rep("USD", 4),
    symbol_id = c(1, 2, 1, 1)
  )

  # Save activities
  save_result <- save_activities_batch(activities)
  expect_true(save_result$inserted_count > 0)

  # Link activities to group
  saved_activities <- get_activities() %>%
    filter(account_number == "TEST123", symbol %in% c("AAPL", "AAPL250117C00150000"))

  for (act_id in saved_activities$activity_id) {
    link_activity_to_group(act_id, group_id)
  }

  # Calculate P&L
  pnl <- calculate_group_pnl(group_id)

  # Assertions
  expect_equal(nrow(pnl), 1)

  # With new accounting for covered call strategies (non-"Other"):
  # - Option net premiums reduce cost basis (net debit accounting)
  # - Stock purchases tracked separately for display
  expect_equal(pnl$stock_purchases, 15000)
  expect_equal(pnl$option_sales, 250)
  expect_equal(pnl$option_purchases, 0)
  expect_equal(pnl$option_net_premium, 250)

  # Total cost = stock purchase - option premium = 15000 - 250 = 14750
  expect_equal(pnl$total_cost, 14750)

  # Total proceeds = stock sale + dividend (no option premiums)
  expect_equal(pnl$stock_sales, 15500)
  expect_equal(pnl$total_dividends, 100)
  expect_equal(pnl$total_proceeds, 15600)

  # Net P&L = proceeds - cost = 15600 - 14750 = 850
  expect_equal(pnl$net_pnl, 850)

  # Total return = (850 / 14750) * 100 = 5.76% (higher % due to lower cost basis)
  expect_equal(pnl$total_return_pct, (850 / 14750) * 100, tolerance = 0.01)

  # Annualized return for 100-day hold
  total_return_ratio <- 1 + (850 / 14750)  # 1.0576
  annualized <- ((total_return_ratio ^ (365 / 100)) - 1) * 100
  expect_equal(pnl$annualized_return_pct, annualized, tolerance = 0.01)

  expect_equal(pnl$hold_days, 100)

  # Cleanup
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
  dbExecute(conn, "DELETE FROM account_activities WHERE account_number = 'TEST123'")
})

test_that("calculate_group_pnl includes commissions in total cost", {
  skip_on_ci()
  skip_on_cran()

  # Create test group
  group_id <- paste0("TEST_PNL_COMM_", format(Sys.time(), "%Y%m%d%H%M%S"))
  create_position_group(
    group_id = group_id,
    group_name = "Test Commission Group",
    strategy_type = "Weekly Dividend Capture",
    account_number = "TEST456",
    members = tibble::tibble(
      symbol = "JEPI",
      role = "underlying_stock"
    )
  )

  # Activities with commissions
  activities <- tibble::tibble(
    account_number = rep("TEST456", 2),
    symbol = rep("JEPI", 2),
    trade_date = c("2024-01-01", "2024-01-02"),
    transaction_date = c("2024-01-01", "2024-01-02"),
    settlement_date = c("2024-01-03", "2024-01-04"),
    action = c("Buy", "Sell"),
    quantity = c(1000, -1000),
    price = c(55, 55.10),
    gross_amount = c(-55000, 55100),
    commission = c(-5, -5),  # $10 total commissions
    net_amount = c(-55005, 55095),
    type = rep("Trades", 2),
    description = c("Buy JEPI", "Sell JEPI"),
    currency = rep("USD", 2),
    symbol_id = rep(3, 2)
  )

  save_result <- save_activities_batch(activities)
  expect_true(save_result$inserted_count > 0)

  saved_activities <- get_activities() %>%
    filter(account_number == "TEST456")

  for (act_id in saved_activities$activity_id) {
    link_activity_to_group(act_id, group_id)
  }

  pnl <- calculate_group_pnl(group_id)

  # Total cost includes purchase + commissions (no option premiums in this test)
  expect_equal(pnl$stock_purchases, 55000)
  expect_equal(pnl$total_commissions, 10)
  expect_equal(pnl$option_sales, 0)  # No options in this test
  expect_equal(pnl$option_purchases, 0)
  expect_equal(pnl$option_net_premium, 0)
  expect_equal(pnl$total_cost, 55010)   # 55000 + 10 - 0

  # Proceeds (no option premiums for Weekly Dividend Capture without options)
  expect_equal(pnl$stock_sales, 55100)
  expect_equal(pnl$total_proceeds, 55100)

  # Net P&L = 55100 - 55010 = 90
  expect_equal(pnl$net_pnl, 90)

  # Cleanup
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
  dbExecute(conn, "DELETE FROM account_activities WHERE account_number = 'TEST456'")
})

test_that("close_position_group marks group closed and calculates final P&L", {
  skip_on_ci()
  skip_on_cran()

  # Create test group
  group_id <- paste0("TEST_CLOSE_", format(Sys.time(), "%Y%m%d%H%M%S"))
  create_position_group(
    group_id = group_id,
    group_name = "Test Close Group",
    strategy_type = "Dividend Aristocrats",
    account_number = "TEST789",
    members = tibble::tibble(
      symbol = c("BAC", "BAC250117C00045000"),
      role = c("underlying_stock", "short_call")
    )
  )

  # Create activities
  activities <- tibble::tibble(
    account_number = rep("TEST789", 3),
    symbol = c("BAC", "BAC250117C00045000", "BAC"),
    trade_date = c("2024-01-01", "2024-01-01", "2024-02-01"),
    transaction_date = c("2024-01-01", "2024-01-01", "2024-02-01"),
    settlement_date = c("2024-01-03", "2024-01-03", "2024-02-03"),
    action = c("Buy", "Sell", "Sell"),
    quantity = c(100, -1, -100),
    price = c(40, 0.50, 42),
    gross_amount = c(-4000, 50, 4200),
    commission = c(0, 0, 0),
    net_amount = c(-4000, 50, 4200),
    type = rep("Trades", 3),
    description = c("Buy BAC", "Sell Call", "Sell BAC"),
    currency = rep("USD", 3),
    symbol_id = c(4, 5, 4)
  )

  save_result <- save_activities_batch(activities)
  saved_activities <- get_activities() %>%
    filter(account_number == "TEST789")

  for (act_id in saved_activities$activity_id) {
    link_activity_to_group(act_id, group_id)
  }

  # Create projected cash flow (to verify deletion)
  save_cash_flow_event(
    group_id = group_id,
    event_date = Sys.Date() + 30,
    event_type = "dividend",
    amount = 50,
    status = "projected",
    confidence = "high"
  )

  # Verify cash flow exists
  cash_flows_before <- get_group_cash_flows(group_id)
  expect_gt(nrow(cash_flows_before %>% filter(status == "projected")), 0)

  # Close the group
  pnl <- close_position_group(group_id)

  # Assertions
  expect_equal(nrow(pnl), 1)
  # With net debit accounting: cost = 4000 - 50 = 3950, proceeds = 4200
  # Net P&L = 4200 - 3950 = 250 (same total gain, but from lower cost basis)
  expect_equal(pnl$net_pnl, 250)

  # Verify group is marked closed
  group <- get_group_by_id(group_id)
  expect_equal(group$status, "closed")
  expect_equal(group$total_return_amount, 250)
  # Return % = (250 / 3950) * 100 = 6.33% (higher % due to lower cost basis)
  expect_equal(group$total_return_pct, (250 / 3950) * 100, tolerance = 0.01)

  # Verify projected cash flows are deleted
  cash_flows_after <- get_group_cash_flows(group_id)
  expect_equal(nrow(cash_flows_after %>% filter(status == "projected")), 0)

  # Cleanup
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
  dbExecute(conn, "DELETE FROM account_activities WHERE account_number = 'TEST789'")
  dbExecute(conn, "DELETE FROM position_group_cash_flows WHERE group_id = ?", params = list(group_id))
})

test_that("calculate_group_pnl handles groups with only dividends correctly", {
  skip_on_ci()
  skip_on_cran()

  # Create test group
  group_id <- paste0("TEST_DIV_ONLY_", format(Sys.time(), "%Y%m%d%H%M%S"))
  create_position_group(
    group_id = group_id,
    group_name = "Test Dividend Only",
    strategy_type = "Dividend Aristocrats",
    account_number = "TEST999",
    members = tibble::tibble(
      symbol = "PFE",
      role = "underlying_stock"
    )
  )

  # Activities: only dividend (no trades yet)
  activities <- tibble::tibble(
    account_number = "TEST999",
    symbol = "PFE",
    trade_date = Sys.Date(),
    transaction_date = Sys.Date(),
    settlement_date = Sys.Date() + 2,
    action = NA_character_,
    quantity = 0,
    price = 0,
    gross_amount = 50,
    commission = 0,
    net_amount = 50,
    type = "Dividends",
    description = "Quarterly dividend",
    currency = "USD",
    symbol_id = 6
  )

  save_result <- save_activities_batch(activities)
  saved_activities <- get_activities() %>%
    filter(account_number == "TEST999")

  for (act_id in saved_activities$activity_id) {
    link_activity_to_group(act_id, group_id)
  }

  pnl <- calculate_group_pnl(group_id)

  # Should handle zero cost gracefully
  expect_equal(pnl$total_cost, 0)
  expect_equal(pnl$total_proceeds, 50)
  expect_equal(pnl$total_dividends, 50)
  expect_equal(pnl$total_return_pct, 0)  # Avoid division by zero

  # Cleanup
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
  dbExecute(conn, "DELETE FROM account_activities WHERE account_number = 'TEST999'")
})

test_that("calculate_group_pnl uses legacy accounting for Other strategy", {
  skip_on_ci()
  skip_on_cran()

  # Test that "Other" strategy keeps legacy behavior: premiums as income
  group_id <- paste0("TEST_OTHER_", format(Sys.time(), "%Y%m%d%H%M%S"))
  create_position_group(
    group_id = group_id,
    group_name = "Test Other Strategy",
    strategy_type = "Other",
    account_number = "TEST111",
    members = tibble::tibble(
      symbol = c("XYZ", "XYZ250117C00100000"),
      role = c("underlying_stock", "short_call")
    )
  )

  # Same activities as first test but with "Other" strategy
  activities <- tibble::tibble(
    account_number = rep("TEST111", 3),
    symbol = c("XYZ", "XYZ250117C00100000", "XYZ"),
    trade_date = c("2024-01-01", "2024-01-01", "2024-02-01"),
    transaction_date = c("2024-01-01", "2024-01-01", "2024-02-01"),
    settlement_date = c("2024-01-03", "2024-01-03", "2024-02-03"),
    action = c("Buy", "Sell", "Sell"),
    quantity = c(100, -1, -100),
    price = c(150, 2.50, 155),
    gross_amount = c(-15000, 250, 15500),
    commission = c(0, 0, 0),
    net_amount = c(-15000, 250, 15500),
    type = rep("Trades", 3),
    description = c("Buy XYZ", "Sell Call", "Sell XYZ"),
    currency = rep("USD", 3),
    symbol_id = c(7, 8, 7)
  )

  save_result <- save_activities_batch(activities)
  saved_activities <- get_activities() %>%
    filter(account_number == "TEST111")

  for (act_id in saved_activities$activity_id) {
    link_activity_to_group(act_id, group_id)
  }

  pnl <- calculate_group_pnl(group_id)

  # With legacy accounting for "Other" strategy:
  # Total cost = stock purchase only = 15000
  expect_equal(pnl$stock_purchases, 15000)
  expect_equal(pnl$option_sales, 250)
  expect_equal(pnl$option_purchases, 0)
  expect_equal(pnl$option_net_premium, 250)
  expect_equal(pnl$total_cost, 15000)  # No premium offset

  # Total proceeds = stock sale + option premium
  expect_equal(pnl$stock_sales, 15500)
  expect_equal(pnl$total_proceeds, 15750)  # 15500 + 250

  # Net P&L = 15750 - 15000 = 750
  expect_equal(pnl$net_pnl, 750)

  # Return % = (750 / 15000) * 100 = 5%
  expect_equal(pnl$total_return_pct, (750 / 15000) * 100, tolerance = 0.01)

  # Cleanup
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
  dbExecute(conn, "DELETE FROM account_activities WHERE account_number = 'TEST111'")
})

test_that("calculate_group_pnl handles buy-to-close option transactions correctly", {
  # Test the user's reported bug scenario:
  # - Sold call for $7,153.01
  # - Bought call back for $13,882.99
  # - Net option loss: -$6,729.98
  # - Stock gain: $7,275.00
  # - Net P&L should be: $545.02

  # Create test group
  group_id <- paste0("TEST_BUY_TO_CLOSE_", format(Sys.time(), "%Y%m%d%H%M%S"))
  create_position_group(
    group_id = group_id,
    group_name = "Test Buy-to-Close",
    strategy_type = "DIVIDEND_ARISTOCRATS",
    account_number = "TEST222",
    members = tibble::tibble(
      symbol = c("ABC", "ABC250117C00050000"),
      role = c("underlying_stock", "short_call")
    )
  )

  # Create activities matching user's scenario
  activities <- tibble::tibble(
    activity_id = c(
      paste0("BTC_BUY_STOCK_", group_id),
      paste0("BTC_SELL_CALL_", group_id),
      paste0("BTC_BUY_CALL_", group_id),
      paste0("BTC_SELL_STOCK_", group_id)
    ),
    account_number = rep("TEST222", 4),
    account_type = rep("TFSA", 4),
    trade_date = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 10:01:00",
      "2024-03-31 14:00:00",  # Buy to close
      "2024-03-31 14:01:00"
    )),
    transaction_date = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 10:01:00",
      "2024-03-31 14:00:00",
      "2024-03-31 14:01:00"
    )),
    settlement_date = as.POSIXct(c(
      "2024-01-03 10:00:00",
      "2024-01-03 10:01:00",
      "2024-04-02 14:00:00",
      "2024-04-02 14:01:00"
    )),
    action = c("Buy", "Sell", "Buy", "Sell"),
    symbol = c("ABC", "ABC250117C00050000", "ABC250117C00050000", "ABC"),
    symbol_id = c(12345, 67890, 67890, 12345),
    description = c(
      "Buy 100 ABC @ $236.45",
      "Sell 1 ABC Jan17'25 $50 Call",
      "Buy 1 ABC Jan17'25 $50 Call",
      "Sell 100 ABC @ $309.20"
    ),
    currency = rep("USD", 4),
    quantity = c(100, 1, -1, -100),
    price = c(236.45, 71.5301, 138.8299, 309.20),
    gross_amount = c(-23645, 7153.01, -13882.99, 30920),
    commission = c(0, 0, 0, 0),
    net_amount = c(-23645, 7153.01, -13882.99, 30920),
    type = rep("Trades", 4),
    group_id = rep(NA_character_, 4),
    is_processed = rep(FALSE, 4),
    fetched_at = rep(Sys.time(), 4)
  )

  save_result <- save_activities_batch(activities)
  saved_activities <- get_activities() %>%
    filter(account_number == "TEST222")

  for (act_id in saved_activities$activity_id) {
    link_activity_to_group(act_id, group_id)
  }

  pnl <- calculate_group_pnl(group_id)

  # Verify option breakdown
  expect_equal(pnl$option_sales, 7153.01)
  expect_equal(pnl$option_purchases, 13882.99)
  expect_equal(pnl$option_net_premium, 7153.01 - 13882.99)  # -6729.98

  # For DIVIDEND_ARISTOCRATS strategy (net debit accounting):
  # total_cost = stock_purchases + commissions - option_net_premium
  # total_cost = 23645 + 0 - (-6729.98) = 30374.98
  expect_equal(pnl$stock_purchases, 23645)
  expect_equal(pnl$total_commissions, 0)
  expect_equal(pnl$total_cost, 23645 + 0 - (7153.01 - 13882.99), tolerance = 0.01)

  # total_proceeds = stock_sales + dividends
  expect_equal(pnl$stock_sales, 30920)
  expect_equal(pnl$total_dividends, 0)
  expect_equal(pnl$total_proceeds, 30920)

  # Net P&L = proceeds - cost = 30920 - 30374.98 = 545.02
  expect_equal(pnl$net_pnl, 545.02, tolerance = 0.01)

  # Cleanup
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?", params = list(group_id))
  dbExecute(conn, "DELETE FROM account_activities WHERE account_number = 'TEST222'")
})

test_that("annualized return calculation is correct for various hold periods", {
  # Test annualized return formula: ((1 + return)^(365/days) - 1) * 100

  # 10% return over 1 year (365 days) = 10% annualized
  total_return <- 0.10
  hold_days <- 365
  annualized <- ((1 + total_return) ^ (365 / hold_days) - 1) * 100
  expect_equal(annualized, 10, tolerance = 0.01)

  # 5% return over 180 days ≈ 10.40% annualized
  total_return <- 0.05
  hold_days <- 180
  annualized <- ((1 + total_return) ^ (365 / hold_days) - 1) * 100
  expect_equal(annualized, 10.40, tolerance = 0.01)

  # 1% return over 30 days ≈ 12.87% annualized
  total_return <- 0.01
  hold_days <- 30
  annualized <- ((1 + total_return) ^ (365 / hold_days) - 1) * 100
  expect_equal(annualized, 12.87, tolerance = 0.01)

  # 20% return over 730 days (2 years) ≈ 9.54% annualized
  total_return <- 0.20
  hold_days <- 730
  annualized <- ((1 + total_return) ^ (365 / hold_days) - 1) * 100
  expect_equal(annualized, 9.54, tolerance = 0.01)
})
