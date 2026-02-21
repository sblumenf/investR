#!/usr/bin/env Rscript
#
# DRY RUN: Show what test data would be deleted
# This script shows all position groups and members that would be removed
# Run this first before executing the actual cleanup
#

library(DBI)
library(duckdb)
library(dplyr)

# Connect to database (read-only for dry run)
db_path <- "inst/database/portfolio.duckdb"
conn <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

# Fake account numbers used in tests
fake_accounts_sql <- "('12345', '99999', '88888', 'TEST123', 'TEST456', 'TEST789', 'TEST222', 'TEST111', 'TEST999')"

cat("\n=================================================================\n")
cat("DRY RUN: Test Data Cleanup Analysis\n")
cat("=================================================================\n\n")

# Find all test position groups (by fake account number or TEST% group_id)
test_groups <- dbGetQuery(conn, sprintf("
  SELECT
    group_id,
    group_name,
    strategy_type,
    status,
    account_number,
    created_at
  FROM position_groups
  WHERE account_number IN %s
     OR group_id LIKE 'TEST%%'
  ORDER BY created_at DESC
", fake_accounts_sql))

cat("POSITION GROUPS TO DELETE:\n")
cat("---------------------------\n")
if (nrow(test_groups) > 0) {
  print(test_groups, row.names = FALSE)
  cat(sprintf("\nTotal groups to delete: %d\n", nrow(test_groups)))

  # Show breakdown by status
  status_summary <- test_groups %>%
    group_by(status) %>%
    summarise(count = n(), .groups = "drop")
  cat("\nBreakdown by status:\n")
  print(status_summary, row.names = FALSE)

  # Show breakdown by strategy
  strategy_summary <- test_groups %>%
    group_by(strategy_type) %>%
    summarise(count = n(), .groups = "drop")
  cat("\nBreakdown by strategy:\n")
  print(strategy_summary, row.names = FALSE)

  # Show creation timestamps to identify test runs
  cat("\nCreation timestamps (to identify test runs):\n")
  timestamps <- test_groups %>%
    mutate(run_batch = as.POSIXct(created_at)) %>%
    group_by(run_batch) %>%
    summarise(groups_in_batch = n(), .groups = "drop") %>%
    arrange(run_batch)
  print(timestamps, row.names = FALSE)

} else {
  cat("No test groups found in database.\n")
}

cat("\n=================================================================\n\n")

# Find all test position group members (by fake account number or TEST% group_id)
test_members <- dbGetQuery(conn, sprintf("
  SELECT
    group_id,
    symbol,
    role,
    account_number,
    added_at
  FROM position_group_members
  WHERE account_number IN %s
     OR group_id LIKE 'TEST%%'
  ORDER BY added_at DESC
", fake_accounts_sql))

cat("POSITION GROUP MEMBERS TO DELETE:\n")
cat("----------------------------------\n")
if (nrow(test_members) > 0) {
  print(test_members, row.names = FALSE)
  cat(sprintf("\nTotal members to delete: %d\n", nrow(test_members)))

  # Show unique symbols
  unique_symbols <- test_members %>%
    filter(role == "stock") %>%
    distinct(symbol) %>%
    arrange(symbol)
  cat(sprintf("\nUnique underlying stocks: %d\n", nrow(unique_symbols)))
  cat("Symbols: ", paste(unique_symbols$symbol, collapse = ", "), "\n")

} else {
  cat("No members found for test groups.\n")
}

cat("\n=================================================================\n\n")

# Check projection_recalculations for TEST% group_ids
test_projections <- dbGetQuery(conn, "
  SELECT
    group_id,
    reason,
    recalc_date
  FROM projection_recalculations
  WHERE group_id LIKE 'TEST%'
  ORDER BY recalc_date DESC
")

cat("PROJECTION RECALCULATIONS TO DELETE:\n")
cat("-------------------------------------\n")
if (nrow(test_projections) > 0) {
  print(test_projections, row.names = FALSE)
  cat(sprintf("\nTotal projection recalculations to delete: %d\n", nrow(test_projections)))
} else {
  cat("No test projection recalculations found.\n")
}

cat("\n=================================================================\n\n")

# Check position_group_cash_flows for TEST% group_ids
test_cash_flows <- dbGetQuery(conn, "
  SELECT
    group_id,
    event_date,
    amount,
    event_type
  FROM position_group_cash_flows
  WHERE group_id LIKE 'TEST%'
  ORDER BY event_date DESC
")

cat("POSITION GROUP CASH FLOWS TO DELETE:\n")
cat("-------------------------------------\n")
if (nrow(test_cash_flows) > 0) {
  print(test_cash_flows, row.names = FALSE)
  cat(sprintf("\nTotal cash flows to delete: %d\n", nrow(test_cash_flows)))
} else {
  cat("No test cash flows found.\n")
}

cat("\n=================================================================\n\n")

# Check for test account activities by fake account number
test_activities <- dbGetQuery(conn, sprintf("
  SELECT
    activity_id,
    account_number,
    symbol,
    transaction_date,
    type,
    quantity,
    price
  FROM account_activities
  WHERE account_number IN %s
  ORDER BY transaction_date DESC
  LIMIT 20
", fake_accounts_sql))

cat("TEST TRANSACTIONS IN ACCOUNT_ACTIVITIES:\n")
cat("-----------------------------------------\n")
if (nrow(test_activities) > 0) {
  cat("WARNING: Found test transactions in account_activities table:\n\n")
  print(test_activities, row.names = FALSE)
  cat(sprintf("\nTotal test activities to delete: %d\n", nrow(test_activities)))
} else {
  cat("No test transactions found in account_activities (this is good).\n")
}

cat("\n=================================================================\n\n")

# Summary
cat("SUMMARY:\n")
cat("--------\n")
cat(sprintf("Position groups to delete:               %d\n", nrow(test_groups)))
cat(sprintf("Position group members to delete:        %d\n", nrow(test_members)))
cat(sprintf("Projection recalculations to delete:     %d\n", nrow(test_projections)))
cat(sprintf("Position group cash flows to delete:     %d\n", nrow(test_cash_flows)))
cat(sprintf("Account activities to delete:            %d\n", nrow(test_activities)))

total <- nrow(test_groups) + nrow(test_members) + nrow(test_projections) + nrow(test_cash_flows) + nrow(test_activities)
if (total > 0) {
  cat("\n")
  cat("NEXT STEPS:\n")
  cat("-----------\n")
  cat("1. Review the data above carefully\n")
  cat("2. If this looks correct, run: Rscript scripts/cleanup_test_data_execute.R\n")
  cat("3. The execute script will perform the actual deletion\n")
} else {
  cat("\nNo test data found - database is clean!\n")
}

cat("\n=================================================================\n")

# Disconnect
dbDisconnect(conn, shutdown = TRUE)
