test_that("linking single dividend activity deletes projected dividend", {
  skip_on_cran()
  local_test_db()

  # Setup: Create test group with projected dividend
  test_group_id <- paste0("TEST_RECON_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Test Group for Single Link",
    strategy_type = "Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "TEST",
      role = "underlying_stock"
    )
  )

  # Add projected dividend for March
  projected_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  # Verify projected dividend exists
  cash_flows_before <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_before), 1)
  expect_equal(cash_flows_before$status[1], "projected")

  # Create a dividend activity and link it
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  # Use unique timestamp to avoid constraint violations across tests
  unique_time <- Sys.time()
  activity_data <- tibble::tibble(
    activity_id = paste0("ACT_DIV_", format(unique_time, "%Y%m%d%H%M%S%OS3")),
    account_number = "TEST123",
    account_type = "Margin",
    trade_date = lubridate::as_datetime("2025-03-20 10:00:00") + as.numeric(unique_time) %% 3600,
    transaction_date = lubridate::as_datetime("2025-03-20 10:00:00") + as.numeric(unique_time) %% 3600,
    settlement_date = lubridate::as_datetime("2025-03-22 10:00:00"),
    action = "DIV",
    symbol = "TEST",
    symbol_id = 12345L,
    description = "Dividend payment",
    currency = "USD",
    quantity = 0,
    price = 0,
    gross_amount = 105,
    commission = 0,
    net_amount = 105,
    type = "Dividends",
    group_id = NA_character_,
    is_processed = FALSE,
    fetched_at = unique_time
  )

  dbWriteTable(conn, "account_activities", activity_data, append = TRUE)

  # Link the dividend activity - this should trigger reconciliation
  result <- link_activity_to_group(
    activity_id = activity_data$activity_id,
    group_id = test_group_id
  )

  expect_true(result)

  # Verify projected dividend was deleted
  cash_flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_after), 0)
})

test_that("linking batch of dividend activities deletes multiple months", {
  skip_on_cran()
  local_test_db()

  # Setup: Create test group with projected dividends for multiple months
  test_group_id <- paste0("TEST_BATCH_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Test Group for Batch Link",
    strategy_type = "Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "TEST",
      role = "underlying_stock"
    )
  )

  # Add projected dividends for March, April, May
  march_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  april_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-04-20"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  may_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-05-10"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  # Verify all three projected dividends exist
  cash_flows_before <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_before), 3)

  # Create actual dividend activities for March and April (not May)
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  unique_time <- Sys.time()
  march_activity <- tibble::tibble(
    activity_id = paste0("ACT_MAR_", format(unique_time, "%Y%m%d%H%M%S%OS3")),
    account_number = "TEST123",
    account_type = "Margin",
    trade_date = lubridate::as_datetime("2025-03-18 10:00:00") + as.numeric(unique_time) %% 3600,
    transaction_date = lubridate::as_datetime("2025-03-18 10:00:00") + as.numeric(unique_time) %% 3600,
    settlement_date = lubridate::as_datetime("2025-03-20 10:00:00"),
    action = "DIV",
    symbol = "TEST",
    symbol_id = 12345L,
    description = "Dividend payment",
    currency = "USD",
    quantity = 0,
    price = 0,
    gross_amount = 105,
    commission = 0,
    net_amount = 105,
    type = "Dividends",
    group_id = NA_character_,
    is_processed = FALSE,
    fetched_at = unique_time
  )

  april_activity <- tibble::tibble(
    activity_id = paste0("ACT_APR_", format(unique_time, "%Y%m%d%H%M%S%OS3")),
    account_number = "TEST123",
    account_type = "Margin",
    trade_date = lubridate::as_datetime("2025-04-15 10:00:00") + as.numeric(unique_time) %% 3600,
    transaction_date = lubridate::as_datetime("2025-04-15 10:00:00") + as.numeric(unique_time) %% 3600,
    settlement_date = lubridate::as_datetime("2025-04-17 10:00:00"),
    action = "DIV",
    symbol = "TEST",
    symbol_id = 12345L,
    description = "Dividend payment",
    currency = "USD",
    quantity = 0,
    price = 0,
    gross_amount = 102,
    commission = 0,
    net_amount = 102,
    type = "Dividends",
    group_id = NA_character_,
    is_processed = FALSE,
    fetched_at = unique_time
  )

  all_activities <- bind_rows(march_activity, april_activity)
  dbWriteTable(conn, "account_activities", all_activities, append = TRUE)

  # Link activities in batch - this should reconcile March and April
  result <- link_activities_to_group(
    activity_ids = all_activities$activity_id,
    group_id = test_group_id
  )

  expect_true(result)

  # Verify March and April projected dividends deleted, May remains
  cash_flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_after), 1)
  expect_equal(lubridate::month(cash_flows_after$event_date[1]), 5)  # May
})

test_that("linking non-dividend activities does not trigger reconciliation", {
  skip_on_cran()
  local_test_db()

  # Setup: Create test group with projected dividend
  test_group_id <- paste0("TEST_NONDIV_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Test Group for Non-Dividend",
    strategy_type = "Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "TEST",
      role = "underlying_stock"
    )
  )

  # Add projected dividend
  projected_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  # Verify projected dividend exists
  cash_flows_before <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_before), 1)

  # Create a TRADE activity (not dividend) and link it
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  unique_time <- Sys.time()
  trade_activity <- tibble::tibble(
    activity_id = paste0("ACT_TRADE_", format(unique_time, "%Y%m%d%H%M%S%OS3")),
    account_number = "TEST123",
    account_type = "Margin",
    trade_date = lubridate::as_datetime("2025-03-20 10:00:00") + as.numeric(unique_time) %% 3600,
    transaction_date = lubridate::as_datetime("2025-03-20 10:00:00") + as.numeric(unique_time) %% 3600,
    settlement_date = lubridate::as_datetime("2025-03-22 10:00:00"),
    action = "Buy",
    symbol = "TEST",
    symbol_id = 12345L,
    description = "Stock purchase",
    currency = "USD",
    quantity = 100,
    price = 50,
    gross_amount = -5000,
    commission = -5,
    net_amount = -5005,
    type = "Trades",
    group_id = NA_character_,
    is_processed = FALSE,
    fetched_at = unique_time
  )

  dbWriteTable(conn, "account_activities", trade_activity, append = TRUE)

  # Link the trade activity - should NOT trigger reconciliation
  result <- link_activity_to_group(
    activity_id = trade_activity$activity_id,
    group_id = test_group_id
  )

  expect_true(result)

  # Verify projected dividend still exists
  cash_flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_after), 1)
  expect_equal(cash_flows_after$status[1], "projected")
})

test_that("linking dividend when no projection exists does not error", {
  skip_on_cran()
  local_test_db()

  # Setup: Create test group with NO projected dividends
  test_group_id <- paste0("TEST_NOPROJ_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Test Group No Projection",
    strategy_type = "Other",  # Other strategies don't get projections
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "TEST",
      role = "underlying_stock"
    )
  )

  # Verify no projected dividends
  cash_flows_before <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_before), 0)

  # Create a dividend activity and link it
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  unique_time <- Sys.time()
  dividend_activity <- tibble::tibble(
    activity_id = paste0("ACT_ORPHAN_", format(unique_time, "%Y%m%d%H%M%S%OS3")),
    account_number = "TEST123",
    account_type = "Margin",
    trade_date = lubridate::as_datetime("2025-03-20 10:00:00") + as.numeric(unique_time) %% 3600,
    transaction_date = lubridate::as_datetime("2025-03-20 10:00:00") + as.numeric(unique_time) %% 3600,
    settlement_date = lubridate::as_datetime("2025-03-22 10:00:00"),
    action = "DIV",
    symbol = "TEST",
    symbol_id = 12345L,
    description = "Dividend payment",
    currency = "USD",
    quantity = 0,
    price = 0,
    gross_amount = 50,
    commission = 0,
    net_amount = 50,
    type = "Dividends",
    group_id = NA_character_,
    is_processed = FALSE,
    fetched_at = unique_time
  )

  dbWriteTable(conn, "account_activities", dividend_activity, append = TRUE)

  # Link the dividend - should succeed silently with no projection to delete
  result <- link_activity_to_group(
    activity_id = dividend_activity$activity_id,
    group_id = test_group_id
  )

  expect_true(result)

  # Verify still no projected dividends
  cash_flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_after), 0)
})
