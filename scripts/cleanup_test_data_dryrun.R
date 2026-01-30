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

cat("\n=================================================================\n")
cat("DRY RUN: Test Data Cleanup Analysis\n")
cat("=================================================================\n\n")

# Find all TEST_ position groups
test_groups <- dbGetQuery(conn, "
  SELECT
    group_id,
    group_name,
    strategy_type,
    status,
    account_number,
    created_at
  FROM position_groups
  WHERE group_id LIKE 'TEST_%'
  ORDER BY created_at DESC
")

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
  cat("No TEST_ groups found in database.\n")
}

cat("\n=================================================================\n\n")

# Find all members of TEST_ groups
test_members <- dbGetQuery(conn, "
  SELECT
    pgm.group_id,
    pgm.symbol,
    pgm.role,
    pgm.added_at,
    pg.group_name
  FROM position_group_members pgm
  JOIN position_groups pg ON pgm.group_id = pg.group_id
  WHERE pg.group_id LIKE 'TEST_%'
  ORDER BY pgm.added_at DESC
")

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
  cat("No members found for TEST_ groups.\n")
}

cat("\n=================================================================\n\n")

# Check for any TEST_ symbols in account_activities
test_activities <- dbGetQuery(conn, "
  SELECT
    activity_id,
    account_number,
    symbol,
    transaction_date,
    type,
    quantity,
    price
  FROM account_activities
  WHERE symbol LIKE 'TEST%' OR symbol IN ('ABC', 'XYZ')
  ORDER BY transaction_date DESC
  LIMIT 20
")

cat("TEST TRANSACTIONS IN ACCOUNT_ACTIVITIES:\n")
cat("-----------------------------------------\n")
if (nrow(test_activities) > 0) {
  cat("WARNING: Found test transactions in account_activities table:\n\n")
  print(test_activities, row.names = FALSE)
  cat("\nNote: These are in account_activities and won't be deleted by the cleanup script.\n")
  cat("They should be removed separately if needed.\n")
} else {
  cat("No test transactions found in account_activities (this is good).\n")
}

cat("\n=================================================================\n\n")

# Summary
cat("SUMMARY:\n")
cat("--------\n")
cat(sprintf("Position groups to delete: %d\n", nrow(test_groups)))
cat(sprintf("Position group members to delete: %d\n", nrow(test_members)))
cat(sprintf("Test transactions in account_activities: %d\n", nrow(test_activities)))

if (nrow(test_groups) > 0) {
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
