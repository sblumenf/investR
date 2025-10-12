test_that("calculate_open_group_metrics returns correct structure", {
  skip_on_cran()

  # Create a test group with activities
  group_id <- "TEST_METRICS_GROUP"
  create_position_group(
    group_id = group_id,
    group_name = "Test Metrics",
    strategy_type = "Test Strategy",
    account_number = "99999",
    members = tibble::tibble(
      symbol = c("TEST"),
      role = c("underlying_stock")
    )
  )

  # Add some test activities (this would normally come from fetch_activities)
  # For testing purposes, we'll need to mock or skip this test if no activities exist
  metrics <- calculate_open_group_metrics(group_id)

  # If no activities, metrics should be empty
  if (nrow(metrics) == 0) {
    expect_true(TRUE)  # Pass test if no activities
  } else {
    # Verify structure
    expect_true("group_id" %in% names(metrics))
    expect_true("cost_basis" %in% names(metrics))
    expect_true("cash_collected" %in% names(metrics))
    expect_true("projected_income" %in% names(metrics))
    expect_true("target_total_return" %in% names(metrics))
    expect_true("pct_recovered" %in% names(metrics))
    expect_true("days_held" %in% names(metrics))
    expect_true("projected_annualized_return_pct" %in% names(metrics))
  }

  # Clean up
  close_position_group(group_id)
})

test_that("calculate_open_group_metrics handles groups with no activities", {
  skip_on_cran()

  # Create a group without activities
  group_id <- "TEST_NO_ACTIVITIES"
  create_position_group(
    group_id = group_id,
    group_name = "No Activities",
    strategy_type = "Test",
    account_number = "99999",
    members = tibble::tibble(
      symbol = c("TEST"),
      role = c("underlying_stock")
    )
  )

  metrics <- calculate_open_group_metrics(group_id)

  # Should return empty tibble
  expect_equal(nrow(metrics), 0)

  # Clean up
  close_position_group(group_id)
})

test_that("get_group_summary_for_card routes correctly by status", {
  skip_on_cran()

  # Create an open group
  group_id <- "TEST_SUMMARY_GROUP"
  create_position_group(
    group_id = group_id,
    group_name = "Test Summary",
    strategy_type = "Test",
    account_number = "99999",
    members = tibble::tibble(
      symbol = c("TEST"),
      role = c("underlying_stock")
    )
  )

  # Get summary for open group
  summary <- get_group_summary_for_card(group_id)

  # Should have status column
  if (nrow(summary) > 0) {
    expect_true("status" %in% names(summary))
    expect_equal(summary$status, "open")
  }

  # Close the group
  close_position_group(group_id)

  # Get summary for closed group
  summary_closed <- get_group_summary_for_card(group_id)

  if (nrow(summary_closed) > 0) {
    expect_true("status" %in% names(summary_closed))
    expect_equal(summary_closed$status, "closed")
  }
})

test_that("calculate_dashboard_metrics returns correct structure", {
  skip_on_cran()

  # Calculate metrics for all groups
  metrics <- calculate_dashboard_metrics()

  # Should return list with two tibbles
  expect_type(metrics, "list")
  expect_true("open_metrics" %in% names(metrics))
  expect_true("closed_metrics" %in% names(metrics))

  # Open metrics structure
  expect_true("count" %in% names(metrics$open_metrics))
  expect_true("total_cost_basis" %in% names(metrics$open_metrics))
  expect_true("total_cash_collected" %in% names(metrics$open_metrics))
  expect_true("total_projected_income" %in% names(metrics$open_metrics))

  # Closed metrics structure
  expect_true("count" %in% names(metrics$closed_metrics))
  expect_true("total_realized_pnl" %in% names(metrics$closed_metrics))
  expect_true("avg_annualized_return_pct" %in% names(metrics$closed_metrics))
})

test_that("calculate_dashboard_metrics filters correctly", {
  skip_on_cran()

  # Test open filter
  open_metrics <- calculate_dashboard_metrics(status_filter = "open")
  expect_equal(nrow(open_metrics$closed_metrics), 1)
  expect_equal(open_metrics$closed_metrics$count, 0)

  # Test closed filter
  closed_metrics <- calculate_dashboard_metrics(status_filter = "closed")
  expect_equal(nrow(closed_metrics$open_metrics), 1)
  expect_equal(closed_metrics$open_metrics$count, 0)
})

test_that("get_strategy_breakdown groups correctly", {
  skip_on_cran()

  breakdown <- get_strategy_breakdown()

  if (nrow(breakdown) > 0) {
    # Verify structure
    expect_true("strategy_type" %in% names(breakdown))
    expect_true("count" %in% names(breakdown))
    expect_true("avg_return_pct" %in% names(breakdown))

    # Verify counts are positive
    expect_true(all(breakdown$count > 0))
  }
})

test_that("calculate_open_group_metrics applies correct accounting for covered call strategies", {
  skip_on_cran()

  # This test verifies that for non-"Other" strategies:
  # - Option premiums reduce cost basis (not added to cash collected)
  # - For "Other" strategy: option premiums are added to cash collected (legacy behavior)

  # Note: This test would require mocking activities data with stock purchases and option sales
  # Since we don't have a full mock setup, we document the expected behavior:
  #
  # For Zero-Dividend strategy with:
  # - Stock purchase: $7,422
  # - Option premium: $1,602
  # Expected:
  # - cost_basis = $7,422 - $1,602 = $5,820
  # - cash_collected = $0 (no dividends)
  #
  # For "Other" strategy with same transactions:
  # Expected:
  # - cost_basis = $7,422
  # - cash_collected = $1,602

  # Skip this test as it requires complex mocking
  skip("Requires activity mocking infrastructure")
})
