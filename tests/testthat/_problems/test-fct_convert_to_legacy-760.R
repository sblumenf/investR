# Extracted from test-fct_convert_to_legacy.R:760

# test -------------------------------------------------------------------------
skip_on_cran()
local_test_db()
test_group_id <- paste0("TEST_EDGE_MIXED_", format(Sys.time(), "%Y%m%d%H%M%S"))
create_position_group(
    group_id = test_group_id,
    group_name = "Mixed Events Test",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "CVX",
      role = "underlying_stock"
    )
  )
save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-01-15"),
    event_type = "dividend",
    amount = 100,
    status = "actual",
    confidence = NA_character_
  )
save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-02-15"),
    event_type = "dividend",
    amount = 100,
    status = "actual",
    confidence = NA_character_
  )
save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-05-15"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )
save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-08-15"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )
result <- convert_to_legacy_covered_call(group_id = test_group_id)
expect_true(result)
flows_after <- get_group_cash_flows(test_group_id)
expect_equal(nrow(flows_after), 2)
