# Extracted from test-fct_convert_to_legacy.R:643

# test -------------------------------------------------------------------------
skip_on_cran()
local_test_db()
test_group_id <- paste0("TEST_E2E_WORKFLOW_", format(Sys.time(), "%Y%m%d%H%M%S"))
create_position_group(
    group_id = test_group_id,
    group_name = "Complete Workflow Test",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = c("CSCO", "CSCO250620C50"),
      role = c("underlying_stock", "short_call")
    )
  )
for (quarter in 0:3) {
    save_cash_flow_event(
      group_id = test_group_id,
      event_date = as.Date("2025-03-15") + (quarter * 90),
      event_type = "dividend",
      amount = 40,
      status = "projected",
      confidence = "high"
    )
  }
save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-06-20"),
    event_type = "option_gain",
    amount = 300,
    status = "projected",
    confidence = "medium"
  )
save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-02-01"),
    event_type = "dividend",
    amount = 40,
    status = "actual",
    confidence = NA_character_
  )
save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-01-15"),
    event_type = "option_premium",
    amount = 150,
    status = "actual",
    confidence = NA_character_
  )
conn <- get_portfolio_db_connection()
on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
initial_flows <- get_group_cash_flows(test_group_id)
expect_equal(nrow(initial_flows), 7)
validation <- can_convert_to_legacy(conn, test_group_id)
expect_true(validation$valid)
expect_equal(validation$projected_count, 5)
expect_equal(validation$projected_amount, 460)
preview <- preview_legacy_conversion(conn, test_group_id)
expect_true(preview$has_events)
expect_equal(preview$total_count, 5)
expect_equal(preview$by_type$dividend$count, 4)
expect_equal(preview$by_type$option_gain$count, 1)
success <- convert_to_legacy_covered_call(conn, test_group_id)
expect_true(success)
group_after <- get_group_by_id(test_group_id)
expect_equal(group_after$strategy_type, "Legacy Covered Call")
flows_after <- get_group_cash_flows(test_group_id)
expect_equal(nrow(flows_after), 2)
expect_true(all(flows_after$status == "actual"))
actual_types <- sort(flows_after$event_type)
expect_equal(actual_types, c("dividend", "option_premium"))
