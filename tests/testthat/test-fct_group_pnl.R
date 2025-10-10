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

  # Total cost = stock purchase
  expect_equal(pnl$stock_purchases, 15000)
  expect_equal(pnl$total_cost, 15000)

  # Total proceeds = stock sale + option premium + dividend
  expect_equal(pnl$stock_sales, 15500)
  expect_equal(pnl$option_premiums, 250)
  expect_equal(pnl$total_dividends, 100)
  expect_equal(pnl$total_proceeds, 15850)

  # Net P&L = proceeds - cost = 15850 - 15000 = 850
  expect_equal(pnl$net_pnl, 850)

  # Total return = (850 / 15000) * 100 = 5.67%
  expect_equal(pnl$total_return_pct, (850 / 15000) * 100, tolerance = 0.01)

  # Annualized return for 100-day hold
  total_return_ratio <- 1 + (850 / 15000)  # 1.0567
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

  # Total cost includes purchase + commissions
  expect_equal(pnl$stock_purchases, 55000)
  expect_equal(pnl$total_commissions, 10)
  expect_equal(pnl$total_cost, 55010)

  # Proceeds
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
  expect_equal(pnl$net_pnl, 250)  # 4200 + 50 - 4000

  # Verify group is marked closed
  group <- get_group_by_id(group_id)
  expect_equal(group$status, "closed")
  expect_equal(group$total_return_amount, 250)
  expect_equal(group$total_return_pct, (250 / 4000) * 100, tolerance = 0.01)

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
