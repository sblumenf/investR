#!/usr/bin/env Rscript
#
# EXECUTE: Delete test data from database
# This script permanently removes test position groups, members, projections,
# cash flows, and account activities identified by fake account numbers or TEST% group IDs
# Make sure you've reviewed the dry run output first!
#

library(DBI)
library(duckdb)

# Fake account numbers used in tests
fake_accounts_sql <- "('12345', '99999', '88888', 'TEST123', 'TEST456', 'TEST789', 'TEST222', 'TEST111', 'TEST999')"

# Connect to database (read-write mode)
db_path <- "inst/database/portfolio.duckdb"
conn <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)

cat("\n=================================================================\n")
cat("EXECUTING: Test Data Cleanup\n")
cat("=================================================================\n\n")

cat("Press Ctrl+C within 5 seconds to cancel...\n")
Sys.sleep(5)
cat("Proceeding with cleanup...\n\n")

# Record before counts
before_projections <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM projection_recalculations WHERE group_id LIKE 'TEST%'")$count
before_cash_flows  <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM position_group_cash_flows WHERE group_id LIKE 'TEST%'")$count
before_members     <- dbGetQuery(conn, sprintf("SELECT COUNT(*) as count FROM position_group_members WHERE account_number IN %s OR group_id LIKE 'TEST%%'", fake_accounts_sql))$count
before_groups      <- dbGetQuery(conn, sprintf("SELECT COUNT(*) as count FROM position_groups WHERE account_number IN %s OR group_id LIKE 'TEST%%'", fake_accounts_sql))$count
before_activities  <- dbGetQuery(conn, sprintf("SELECT COUNT(*) as count FROM account_activities WHERE account_number IN %s", fake_accounts_sql))$count

cat("Before counts:\n")
cat(sprintf("  projection_recalculations: %d\n", before_projections))
cat(sprintf("  position_group_cash_flows: %d\n", before_cash_flows))
cat(sprintf("  position_group_members:    %d\n", before_members))
cat(sprintf("  position_groups:           %d\n", before_groups))
cat(sprintf("  account_activities:        %d\n", before_activities))
cat("\n")

# Start transaction for safety
dbBegin(conn)

tryCatch({

  # Step 1: Delete projection_recalculations (references group_id)
  cat("Step 1: Deleting projection recalculations...\n")
  projections_deleted <- dbExecute(conn, "
    DELETE FROM projection_recalculations
    WHERE group_id LIKE 'TEST%'
  ")
  cat(sprintf("  Deleted %d projection recalculations\n\n", projections_deleted))

  # Step 2: Delete position_group_cash_flows (references group_id)
  cat("Step 2: Deleting position group cash flows...\n")
  cash_flows_deleted <- dbExecute(conn, "
    DELETE FROM position_group_cash_flows
    WHERE group_id LIKE 'TEST%'
  ")
  cat(sprintf("  Deleted %d position group cash flows\n\n", cash_flows_deleted))

  # Step 3: Delete position group members (direct match catches orphans too)
  cat("Step 3: Deleting position group members...\n")
  members_deleted <- dbExecute(conn, sprintf("
    DELETE FROM position_group_members
    WHERE account_number IN %s
       OR group_id LIKE 'TEST%%'
  ", fake_accounts_sql))
  cat(sprintf("  Deleted %d position group members\n\n", members_deleted))

  # Step 4: Delete position groups
  cat("Step 4: Deleting position groups...\n")
  groups_deleted <- dbExecute(conn, sprintf("
    DELETE FROM position_groups
    WHERE account_number IN %s
       OR group_id LIKE 'TEST%%'
  ", fake_accounts_sql))
  cat(sprintf("  Deleted %d position groups\n\n", groups_deleted))

  # Step 5: Delete test account activities
  cat("Step 5: Deleting test account activities...\n")
  activities_deleted <- dbExecute(conn, sprintf("
    DELETE FROM account_activities
    WHERE account_number IN %s
  ", fake_accounts_sql))
  cat(sprintf("  Deleted %d test account activities\n\n", activities_deleted))

  # Commit transaction
  cat("Step 6: Committing changes...\n")
  dbCommit(conn)
  cat("  Transaction committed successfully\n\n")

  cat("=================================================================\n")
  cat("SUCCESS: Test data cleanup completed\n")
  cat("=================================================================\n\n")

  cat("Summary:\n")
  cat(sprintf("  - Projection recalculations removed: %d\n", projections_deleted))
  cat(sprintf("  - Position group cash flows removed: %d\n", cash_flows_deleted))
  cat(sprintf("  - Position group members removed:    %d\n", members_deleted))
  cat(sprintf("  - Position groups removed:           %d\n", groups_deleted))
  cat(sprintf("  - Account activities removed:        %d\n", activities_deleted))
  cat("\n")

  # Verify cleanup
  cat("Verification: Checking for remaining test data...\n")

  remaining_projections <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM projection_recalculations WHERE group_id LIKE 'TEST%'")$count
  remaining_cash_flows  <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM position_group_cash_flows WHERE group_id LIKE 'TEST%'")$count
  remaining_members     <- dbGetQuery(conn, sprintf("SELECT COUNT(*) as count FROM position_group_members WHERE account_number IN %s OR group_id LIKE 'TEST%%'", fake_accounts_sql))$count
  remaining_groups      <- dbGetQuery(conn, sprintf("SELECT COUNT(*) as count FROM position_groups WHERE account_number IN %s OR group_id LIKE 'TEST%%'", fake_accounts_sql))$count
  remaining_activities  <- dbGetQuery(conn, sprintf("SELECT COUNT(*) as count FROM account_activities WHERE account_number IN %s", fake_accounts_sql))$count

  total_remaining <- remaining_projections + remaining_cash_flows + remaining_members + remaining_groups + remaining_activities

  cat(sprintf("  projection_recalculations remaining: %d\n", remaining_projections))
  cat(sprintf("  position_group_cash_flows remaining: %d\n", remaining_cash_flows))
  cat(sprintf("  position_group_members remaining:    %d\n", remaining_members))
  cat(sprintf("  position_groups remaining:           %d\n", remaining_groups))
  cat(sprintf("  account_activities remaining:        %d\n", remaining_activities))

  if (total_remaining == 0) {
    cat("\n  All test data successfully removed\n")
  } else {
    cat(sprintf("\n  WARNING: %d test rows still remain - manual review required\n", total_remaining))
  }

  cat("\n=================================================================\n")
  cat("\nNext steps:\n")
  cat("1. Your Portfolio Risk Analysis should no longer try to fetch quotes for test symbols\n")
  cat("2. Consider fixing the test suite to use a separate test database\n")
  cat("\n")

}, error = function(e) {
  # Rollback on error
  cat("\nERROR occurred during cleanup:\n")
  cat(sprintf("   %s\n\n", e$message))
  cat("Rolling back transaction...\n")
  dbRollback(conn)
  cat("  Transaction rolled back - no changes made\n\n")
  cat("=================================================================\n")
  stop(e)
})

# Disconnect
dbDisconnect(conn, shutdown = TRUE)
cat("Database connection closed.\n\n")
