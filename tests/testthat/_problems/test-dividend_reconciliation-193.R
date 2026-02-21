# Extracted from test-dividend_reconciliation.R:193

# test -------------------------------------------------------------------------
skip_on_cran()
local_test_db()
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
cash_flows_before <- get_group_cash_flows(test_group_id)
expect_equal(nrow(cash_flows_before), 3)
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
result <- link_activities_to_group(
    activity_ids = all_activities$activity_id,
    group_id = test_group_id
  )
expect_true(result)
cash_flows_after <- get_group_cash_flows(test_group_id)
expect_equal(nrow(cash_flows_after), 1)
expect_equal(lubridate::month(cash_flows_after$event_date[1]), 5)
