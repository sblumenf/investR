# Extracted from test-fct_convert_to_legacy.R:337

# test -------------------------------------------------------------------------
skip_on_cran()
local_test_db()
test_group_id <- paste0("TEST_CONV_SUCCESS_", format(Sys.time(), "%Y%m%d%H%M%S"))
create_position_group(
    group_id = test_group_id,
    group_name = "Conversion Test Group",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = c("META", "META250117C350"),
      role = c("underlying_stock", "short_call")
    )
  )
save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )
save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-04-15"),
    event_type = "option_gain",
    amount = 250,
    status = "projected",
    confidence = "medium"
  )
save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-02-01"),
    event_type = "dividend",
    amount = 100,
    status = "actual",
    confidence = NA_character_
  )
cash_flows_before <- get_group_cash_flows(test_group_id)
expect_equal(nrow(cash_flows_before), 3)
projected_before <- sum(cash_flows_before$status == "projected")
expect_equal(projected_before, 2)
result <- convert_to_legacy_covered_call(group_id = test_group_id)
expect_true(result)
group <- get_group_by_id(test_group_id)
expect_equal(group$strategy_type, "Legacy Covered Call")
cash_flows_after <- get_group_cash_flows(test_group_id)
expect_equal(nrow(cash_flows_after), 1)
