test_that("delete_projected_cash_flows_by_month deletes correct events", {
  skip_on_cran()
  local_test_db()

  # Setup: Create a test group with projected dividend events
  test_group_id <- paste0("TEST_GROUP_", format(Sys.time(), "%Y%m%d%H%M%S"))

  # Create group
  create_position_group(
    group_id = test_group_id,
    group_name = "Test Group for Reconciliation",
    strategy_type = "Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "TEST",
      role = "underlying_stock"
    )
  )

  # Add projected dividends for different months
  march_event_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  april_event_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-04-20"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  may_event_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-05-10"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  # Verify all three events exist
  cash_flows_before <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_before), 3)

  # Delete projected dividends for April only
  rows_deleted <- delete_projected_cash_flows_by_month(
    group_id = test_group_id,
    event_type = "dividend",
    event_date = as.Date("2025-04-15")  # Different day in April
  )

  expect_equal(rows_deleted, 1)

  # Verify only April event was deleted
  cash_flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_after), 2)

  remaining_months <- lubridate::month(cash_flows_after$event_date)
  expect_true(3 %in% remaining_months)  # March still there
  expect_false(4 %in% remaining_months) # April deleted
  expect_true(5 %in% remaining_months)  # May still there
})

test_that("delete_projected_cash_flows_by_month only deletes projected status", {
  skip_on_cran()
  local_test_db()

  test_group_id <- paste0("TEST_GROUP_", format(Sys.time(), "%Y%m%d%H%M%S"))

  # Create group
  create_position_group(
    group_id = test_group_id,
    group_name = "Test Group for Status Filter",
    strategy_type = "Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "TEST",
      role = "underlying_stock"
    )
  )

  # Add both projected and actual events for March
  projected_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  actual_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-20"),
    event_type = "dividend",
    amount = 105,
    status = "actual",
    confidence = "high"
  )

  # Verify both exist
  cash_flows_before <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_before), 2)

  # Delete projected dividends for March
  rows_deleted <- delete_projected_cash_flows_by_month(
    group_id = test_group_id,
    event_type = "dividend",
    event_date = as.Date("2025-03-10")
  )

  expect_equal(rows_deleted, 1)

  # Verify only projected was deleted, actual remains
  cash_flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_after), 1)
  expect_equal(cash_flows_after$status[1], "actual")
  expect_equal(cash_flows_after$amount[1], 105)
})

test_that("delete_projected_cash_flows_by_month deletes all projected events in month", {
  skip_on_cran()
  local_test_db()

  test_group_id <- paste0("TEST_GROUP_", format(Sys.time(), "%Y%m%d%H%M%S"))

  # Create group
  create_position_group(
    group_id = test_group_id,
    group_name = "Test Group for Multiple Events",
    strategy_type = "Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "TEST",
      role = "underlying_stock"
    )
  )

  # Add multiple projected events in the same month (shouldn't normally happen, but test it)
  event1 <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-05"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  event2 <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  event3 <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-25"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  # Verify all three exist
  cash_flows_before <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_before), 3)

  # Delete projected dividends for March - should delete ALL three
  rows_deleted <- delete_projected_cash_flows_by_month(
    group_id = test_group_id,
    event_type = "dividend",
    event_date = as.Date("2025-03-10")
  )

  expect_equal(rows_deleted, 3)

  # Verify all were deleted
  cash_flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_after), 0)
})

test_that("delete_projected_cash_flows_by_month handles non-existent group/month gracefully", {
  skip_on_cran()
  local_test_db()

  # Try to delete from non-existent group
  rows_deleted <- delete_projected_cash_flows_by_month(
    group_id = "NONEXISTENT_GROUP",
    event_type = "dividend",
    event_date = as.Date("2025-03-15")
  )

  # Should return 0, not error
  expect_equal(rows_deleted, 0)

  # Create a group but don't add any cash flows
  test_group_id <- paste0("TEST_GROUP_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Test Group Empty",
    strategy_type = "Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "TEST",
      role = "underlying_stock"
    )
  )

  # Try to delete from month with no events
  rows_deleted <- delete_projected_cash_flows_by_month(
    group_id = test_group_id,
    event_type = "dividend",
    event_date = as.Date("2025-03-15")
  )

  # Should return 0, not error
  expect_equal(rows_deleted, 0)
})

test_that("delete_projected_cash_flows_by_month only deletes matching event_type", {
  skip_on_cran()
  local_test_db()

  test_group_id <- paste0("TEST_GROUP_", format(Sys.time(), "%Y%m%d%H%M%S"))

  # Create group
  create_position_group(
    group_id = test_group_id,
    group_name = "Test Group for Event Type",
    strategy_type = "Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "TEST",
      role = "underlying_stock"
    )
  )

  # Add both dividend and option_gain for the same month
  dividend_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  option_id <- save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-20"),
    event_type = "option_gain",
    amount = 500,
    status = "projected",
    confidence = "high"
  )

  # Verify both exist
  cash_flows_before <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_before), 2)

  # Delete only dividend type for March
  rows_deleted <- delete_projected_cash_flows_by_month(
    group_id = test_group_id,
    event_type = "dividend",
    event_date = as.Date("2025-03-10")
  )

  expect_equal(rows_deleted, 1)

  # Verify only dividend was deleted, option_gain remains
  cash_flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_after), 1)
  expect_equal(cash_flows_after$event_type[1], "option_gain")
  expect_equal(cash_flows_after$amount[1], 500)
})
