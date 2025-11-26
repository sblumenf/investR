# Test Suite: Convert to Legacy Covered Call Feature
#
# Tests database functions for converting Dynamic Covered Calls positions
# to Legacy Covered Call strategy by removing projected cash flows.
#
# Functions tested:
# - can_convert_to_legacy()
# - preview_legacy_conversion()
# - convert_to_legacy_covered_call()

################################################################################
# Test: can_convert_to_legacy() - Validation Logic
################################################################################

test_that("can_convert_to_legacy returns valid=TRUE for Dynamic Covered Calls with projections", {
  skip_on_cran()

  # Setup: Create Dynamic Covered Calls group with projected cash flows
  test_group_id <- paste0("TEST_CONV_VALID_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Valid Dynamic Position",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = c("AAPL", "AAPL250117C150"),
      role = c("underlying_stock", "short_call")
    )
  )

  # Add projected cash flows
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

  # Test validation
  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  result <- can_convert_to_legacy(conn, test_group_id)

  expect_true(result$valid)
  expect_equal(result$projected_count, 2)
  expect_equal(result$projected_amount, 350)

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

test_that("can_convert_to_legacy accepts any strategy type with projected cash flows", {
  skip_on_cran()

  # Setup: Create Zero-Dividend Stocks group with projected cash flows
  test_group_id <- paste0("TEST_CONV_ANY_STRATEGY_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Zero-Dividend Test Position",
    strategy_type = "Zero-Dividend Stocks",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = c("MSFT", "MSFT250117C300"),
      role = c("underlying_stock", "short_call")
    )
  )

  # Add projected cash flows
  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "option_gain",
    amount = 250,
    status = "projected",
    confidence = "high"
  )

  # Test validation - should be valid for any strategy type
  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  result <- can_convert_to_legacy(conn, test_group_id)

  expect_true(result$valid)
  expect_equal(result$projected_count, 1)
  expect_equal(result$projected_amount, 250)

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

test_that("can_convert_to_legacy returns valid=FALSE for closed groups", {
  skip_on_cran()

  # Setup: Create Dynamic Covered Calls group
  test_group_id <- paste0("TEST_CONV_CLOSED_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Closed Dynamic Position",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = c("GOOGL", "GOOGL250117C140"),
      role = c("underlying_stock", "short_call")
    )
  )

  # Manually mark group as closed (bypassing P&L requirements)
  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  DBI::dbExecute(conn, "
    UPDATE position_groups
    SET status = 'closed'
    WHERE group_id = ?
  ", params = list(test_group_id))

  # Test validation
  result <- can_convert_to_legacy(conn, test_group_id)

  expect_false(result$valid)
  expect_match(result$reason, "not open")
})

test_that("can_convert_to_legacy returns valid=FALSE when no projected cash flows exist", {
  skip_on_cran()

  # Setup: Create Dynamic Covered Calls group WITHOUT projected cash flows
  test_group_id <- paste0("TEST_CONV_NOPROJ_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Dynamic Without Projections",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = c("TSLA", "TSLA250117C200"),
      role = c("underlying_stock", "short_call")
    )
  )

  # Don't add any projected cash flows

  # Test validation
  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  result <- can_convert_to_legacy(conn, test_group_id)

  expect_false(result$valid)
  expect_match(result$reason, "No projected cash flows")
})

test_that("can_convert_to_legacy returns valid=FALSE for non-existent group_id", {
  skip_on_cran()

  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  result <- can_convert_to_legacy(conn, "NONEXISTENT_GROUP_12345")

  expect_false(result$valid)
  expect_match(result$reason, "not found")
})

################################################################################
# Test: preview_legacy_conversion() - Event Preview
################################################################################

test_that("preview_legacy_conversion returns correct breakdown of projected events", {
  skip_on_cran()

  # Setup: Create group with multiple projected events
  test_group_id <- paste0("TEST_PREV_DETAIL_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Preview Test Group",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = c("NVDA", "NVDA250117C500"),
      role = c("underlying_stock", "short_call")
    )
  )

  # Add 3 dividend events
  for (i in 1:3) {
    save_cash_flow_event(
      group_id = test_group_id,
      event_date = as.Date("2025-03-15") + (i * 90),
      event_type = "dividend",
      amount = 100,
      status = "projected",
      confidence = "high"
    )
  }

  # Add 1 option_gain event
  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-06-20"),
    event_type = "option_gain",
    amount = 500,
    status = "projected",
    confidence = "medium"
  )

  # Test preview
  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  preview <- preview_legacy_conversion(conn, test_group_id)

  expect_true(preview$has_events)
  expect_equal(preview$total_count, 4)
  expect_equal(preview$total_amount, 800)  # 3 * 100 + 500

  # Check breakdown by type
  expect_equal(preview$by_type$dividend$count, 3)
  expect_equal(preview$by_type$dividend$amount, 300)
  expect_equal(preview$by_type$option_gain$count, 1)
  expect_equal(preview$by_type$option_gain$amount, 500)

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

test_that("preview_legacy_conversion handles groups with no projections gracefully", {
  skip_on_cran()

  # Setup: Create group without projected events
  test_group_id <- paste0("TEST_PREV_EMPTY_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Empty Preview Group",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "AMD",
      role = "underlying_stock"
    )
  )

  # Test preview
  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  preview <- preview_legacy_conversion(conn, test_group_id)

  expect_false(preview$has_events)
  expect_match(preview$message, "No projected events")
})

################################################################################
# Test: convert_to_legacy_covered_call() - Happy Path
################################################################################

test_that("convert_to_legacy_covered_call successfully converts Dynamic to Legacy", {
  skip_on_cran()

  # Setup: Create Dynamic Covered Calls group with projected and actual events
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

  # Add projected events
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

  # Add actual event (should be preserved)
  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-02-01"),
    event_type = "dividend",
    amount = 100,
    status = "actual",
    confidence = NA_character_
  )

  # Verify setup
  cash_flows_before <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_before), 3)
  projected_before <- sum(cash_flows_before$status == "projected")
  expect_equal(projected_before, 2)

  # Execute conversion
  result <- convert_to_legacy_covered_call(group_id = test_group_id)

  expect_true(result)

  # Verify strategy_type changed
  group <- get_group_by_id(test_group_id)
  expect_equal(group$strategy_type, "Legacy Covered Call")

  # Verify projected events deleted
  cash_flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(cash_flows_after), 1)
  expect_equal(cash_flows_after$status, "actual")

  # Verify actual events preserved
  expect_equal(cash_flows_after$event_type, "dividend")
  expect_equal(cash_flows_after$amount, 100)

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

test_that("convert_to_legacy_covered_call updates updated_at timestamp", {
  skip_on_cran()

  # Setup
  test_group_id <- paste0("TEST_CONV_TIMESTAMP_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Timestamp Test",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "INTC",
      role = "underlying_stock"
    )
  )

  # Add projected event
  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 50,
    status = "projected",
    confidence = "high"
  )

  # Get original timestamp
  group_before <- get_group_by_id(test_group_id)
  original_timestamp <- group_before$updated_at

  # Wait a moment to ensure timestamp difference
  Sys.sleep(1)

  # Execute conversion
  convert_to_legacy_covered_call(group_id = test_group_id)

  # Verify timestamp updated
  group_after <- get_group_by_id(test_group_id)
  new_timestamp <- group_after$updated_at

  expect_true(new_timestamp > original_timestamp)

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

test_that("convert_to_legacy_covered_call creates projection_recalculations log entry", {
  skip_on_cran()

  # Setup
  test_group_id <- paste0("TEST_CONV_LOG_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Log Test Group",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "AMZN",
      role = "underlying_stock"
    )
  )

  # Add projected events
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
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  # Execute conversion
  convert_to_legacy_covered_call(group_id = test_group_id)

  # Verify log entry created
  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  log_entries <- DBI::dbGetQuery(conn, "
    SELECT * FROM projection_recalculations
    WHERE group_id = ? AND reason = 'converted_to_legacy'
    ORDER BY recalc_date DESC
    LIMIT 1
  ", params = list(test_group_id))

  expect_equal(nrow(log_entries), 1)
  expect_equal(log_entries$old_projection_count, 2)
  expect_equal(log_entries$new_projection_count, 0)
  expect_true(!is.na(log_entries$recalc_id))

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

################################################################################
# Test: convert_to_legacy_covered_call() - Failure Cases
################################################################################

test_that("convert_to_legacy_covered_call rolls back transaction on validation failure", {
  skip_on_cran()

  # Setup: Create group without projected cash flows (invalid for conversion)
  test_group_id <- paste0("TEST_CONV_ROLLBACK1_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "No Projections Group",
    strategy_type = "Other",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "IBM",
      role = "underlying_stock"
    )
  )

  # Don't add any projected cash flows - this makes conversion invalid

  # Attempt conversion (should fail validation - no projected flows)
  result <- convert_to_legacy_covered_call(group_id = test_group_id)

  expect_false(result)

  # Verify group unchanged
  group <- get_group_by_id(test_group_id)
  expect_equal(group$strategy_type, "Other")  # Strategy type unchanged
})

test_that("convert_to_legacy_covered_call handles non-existent group gracefully", {
  skip_on_cran()

  # Attempt conversion on non-existent group
  result <- convert_to_legacy_covered_call(group_id = "NONEXISTENT_GROUP_99999")

  expect_false(result)
})

test_that("convert_to_legacy_covered_call handles connection management correctly", {
  skip_on_cran()

  # Setup
  test_group_id <- paste0("TEST_CONV_CONN_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Connection Test",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "NFLX",
      role = "underlying_stock"
    )
  )

  # Add projected event
  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 75,
    status = "projected",
    confidence = "high"
  )

  # Test 1: Function creates its own connection when none provided
  result1 <- convert_to_legacy_covered_call(conn = NULL, group_id = test_group_id)
  expect_true(result1)

  # Reset for second test
  update_position_group(test_group_id, strategy_type = "Dynamic Covered Calls")
  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-04-15"),
    event_type = "dividend",
    amount = 75,
    status = "projected",
    confidence = "high"
  )

  # Test 2: Function uses provided connection
  conn <- get_portfolio_db_connection()
  result2 <- convert_to_legacy_covered_call(conn = conn, group_id = test_group_id)
  expect_true(result2)

  # Connection should still be valid (not closed by function)
  expect_true(DBI::dbIsValid(conn))
  DBI::dbDisconnect(conn, shutdown = TRUE)

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

################################################################################
# Test: End-to-End Integration
################################################################################

test_that("End-to-End: Complete conversion workflow", {
  skip_on_cran()

  # Setup: Create realistic Dynamic Covered Calls position
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

  # Add projected dividends (quarterly)
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

  # Add projected option_gain (assumes assignment)
  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-06-20"),
    event_type = "option_gain",
    amount = 300,
    status = "projected",
    confidence = "medium"
  )

  # Add actual dividend already received
  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-02-01"),
    event_type = "dividend",
    amount = 40,
    status = "actual",
    confidence = NA_character_
  )

  # Add actual option premium from initial sale
  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-01-15"),
    event_type = "option_premium",
    amount = 150,
    status = "actual",
    confidence = NA_character_
  )

  # Verify initial state
  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  initial_flows <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(initial_flows), 7)  # 4 div + 1 opt_gain + 2 actual

  # Step 1: Check if conversion is valid
  validation <- can_convert_to_legacy(conn, test_group_id)
  expect_true(validation$valid)
  expect_equal(validation$projected_count, 5)
  expect_equal(validation$projected_amount, 460)  # 4*40 + 300

  # Step 2: Preview conversion
  preview <- preview_legacy_conversion(conn, test_group_id)
  expect_true(preview$has_events)
  expect_equal(preview$total_count, 5)
  expect_equal(preview$by_type$dividend$count, 4)
  expect_equal(preview$by_type$option_gain$count, 1)

  # Step 3: Execute conversion
  success <- convert_to_legacy_covered_call(conn, test_group_id)
  expect_true(success)

  # Step 4: Verify post-conversion state
  group_after <- get_group_by_id(test_group_id)
  expect_equal(group_after$strategy_type, "Legacy Covered Call")

  # Step 5: Verify projected events deleted
  flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(flows_after), 2)  # Only actual events remain
  expect_true(all(flows_after$status == "actual"))

  # Step 6: Verify actual events preserved
  actual_types <- sort(flows_after$event_type)
  expect_equal(actual_types, c("dividend", "option_premium"))

  # Step 7: Verify cannot convert again (no projected flows remaining)
  validation2 <- can_convert_to_legacy(conn, test_group_id)
  expect_false(validation2$valid)
  expect_match(validation2$reason, "No projected cash flows")

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

################################################################################
# Test: Edge Cases
################################################################################

test_that("Edge Case: Multiple projected events of same type", {
  skip_on_cran()

  # Setup: Create group with many projected dividends
  test_group_id <- paste0("TEST_EDGE_MULTI_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Multiple Events Test",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "PFE",
      role = "underlying_stock"
    )
  )

  # Add 12 monthly dividend projections
  for (month in 1:12) {
    save_cash_flow_event(
      group_id = test_group_id,
      event_date = as.Date("2025-01-15") + (month * 30),
      event_type = "dividend",
      amount = 25,
      status = "projected",
      confidence = "high"
    )
  }

  # Verify setup
  flows_before <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(flows_before), 12)

  # Execute conversion
  result <- convert_to_legacy_covered_call(group_id = test_group_id)
  expect_true(result)

  # Verify all deleted
  flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(flows_after), 0)

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

test_that("Edge Case: Mixed projected and actual events of same type", {
  skip_on_cran()

  # Setup
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

  # Add past actual dividends
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

  # Add future projected dividends
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

  # Execute conversion
  result <- convert_to_legacy_covered_call(group_id = test_group_id)
  expect_true(result)

  # Verify only projected deleted, actual preserved
  flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(flows_after), 2)
  expect_true(all(flows_after$status == "actual"))
  expect_true(all(flows_after$event_date < as.Date("2025-03-01")))

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

test_that("Edge Case: Large amounts (precision testing)", {
  skip_on_cran()

  # Setup
  test_group_id <- paste0("TEST_EDGE_PRECISION_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Precision Test",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "BRK.B",
      role = "underlying_stock"
    )
  )

  # Add projected event with large precise amount
  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 12345.67,
    status = "projected",
    confidence = "high"
  )

  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-06-15"),
    event_type = "option_gain",
    amount = 98765.43,
    status = "projected",
    confidence = "medium"
  )

  # Verify precision in validation
  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  validation <- can_convert_to_legacy(conn, test_group_id)
  expect_true(validation$valid)
  expect_equal(validation$projected_amount, 111111.10, tolerance = 0.01)

  # Execute conversion
  result <- convert_to_legacy_covered_call(conn, test_group_id)
  expect_true(result)

  # Verify deletion successful
  flows_after <- get_group_cash_flows(test_group_id)
  expect_equal(nrow(flows_after), 0)

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

################################################################################
# Test: Data Integrity and Transaction Safety
################################################################################

test_that("Transaction Safety: No orphaned records after failed conversion", {
  skip_on_cran()

  # Setup: Create group that will fail validation (closed)
  test_group_id <- paste0("TEST_SAFETY_ORPHAN_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Transaction Safety Test",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "DIS",
      role = "underlying_stock"
    )
  )

  # Add projected events
  save_cash_flow_event(
    group_id = test_group_id,
    event_date = as.Date("2025-03-15"),
    event_type = "dividend",
    amount = 100,
    status = "projected",
    confidence = "high"
  )

  # Manually close group to make conversion invalid (bypassing P&L requirements)
  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  DBI::dbExecute(conn, "
    UPDATE position_groups
    SET status = 'closed'
    WHERE group_id = ?
  ", params = list(test_group_id))

  # Get baseline counts
  flows_before_count <- nrow(get_group_cash_flows(test_group_id))
  group_before <- get_group_by_id(test_group_id)

  # Attempt conversion (should fail)
  result <- convert_to_legacy_covered_call(conn, test_group_id)
  expect_false(result)

  # Verify database unchanged
  flows_after_count <- nrow(get_group_cash_flows(test_group_id))
  group_after <- get_group_by_id(test_group_id)

  expect_equal(flows_before_count, flows_after_count)
  expect_equal(group_before$strategy_type, group_after$strategy_type)
  expect_equal(group_before$status, group_after$status)

  # Cleanup
  delete_group_cash_flows(test_group_id)
})

test_that("Audit Trail: projection_recalculations entry contains correct metadata", {
  skip_on_cran()

  # Setup
  test_group_id <- paste0("TEST_AUDIT_META_", format(Sys.time(), "%Y%m%d%H%M%S"))

  create_position_group(
    group_id = test_group_id,
    group_name = "Audit Metadata Test",
    strategy_type = "Dynamic Covered Calls",
    account_number = "TEST123",
    members = tibble::tibble(
      symbol = "WMT",
      role = "underlying_stock"
    )
  )

  # Add exactly 3 projected events
  for (i in 1:3) {
    save_cash_flow_event(
      group_id = test_group_id,
      event_date = as.Date("2025-03-15") + (i * 30),
      event_type = "dividend",
      amount = 50,
      status = "projected",
      confidence = "high"
    )
  }

  # Record time before conversion
  time_before <- Sys.time()

  # Execute conversion
  result <- convert_to_legacy_covered_call(group_id = test_group_id)
  expect_true(result)

  # Record time after conversion
  time_after <- Sys.time()

  # Verify audit log entry
  conn <- get_portfolio_db_connection()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  log_entry <- DBI::dbGetQuery(conn, "
    SELECT * FROM projection_recalculations
    WHERE group_id = ? AND reason = 'converted_to_legacy'
  ", params = list(test_group_id))

  expect_equal(nrow(log_entry), 1)
  expect_equal(log_entry$old_projection_count, 3)
  expect_equal(log_entry$new_projection_count, 0)
  expect_true(log_entry$recalc_date >= time_before)
  expect_true(log_entry$recalc_date <= time_after)
  expect_match(log_entry$recalc_id, test_group_id)

  # Cleanup
  delete_group_cash_flows(test_group_id)
})
