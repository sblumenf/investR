#!/usr/bin/env Rscript
#
# EXECUTE: Delete test data from database
# This script permanently removes TEST_ position groups and members
# Make sure you've reviewed the dry run output first!
#

library(DBI)
library(duckdb)

# Connect to database (read-write mode)
db_path <- "inst/database/portfolio.duckdb"
conn <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)

cat("\n=================================================================\n")
cat("EXECUTING: Test Data Cleanup\n")
cat("=================================================================\n\n")

# Start transaction for safety
dbBegin(conn)

tryCatch({

  # Step 1: Delete position group members (direct match catches orphans too)
  cat("Step 1: Deleting position group members...\n")
  members_deleted <- dbExecute(conn, "
    DELETE FROM position_group_members
    WHERE group_id LIKE 'TEST_%'
  ")
  cat(sprintf("  ✓ Deleted %d position group members\n\n", members_deleted))

  # Step 2: Delete position groups
  cat("Step 2: Deleting position groups...\n")
  groups_deleted <- dbExecute(conn, "
    DELETE FROM position_groups
    WHERE group_id LIKE 'TEST_%'
  ")
  cat(sprintf("  ✓ Deleted %d position groups\n\n", groups_deleted))

  # Step 3: Delete test account activities
  cat("Step 3: Deleting test account activities...\n")
  activities_deleted <- dbExecute(conn, "
    DELETE FROM account_activities
    WHERE account_number = 'TEST123'
  ")
  cat(sprintf("  ✓ Deleted %d test account activities\n\n", activities_deleted))

  # Commit transaction
  cat("Step 4: Committing changes...\n")
  dbCommit(conn)
  cat("  ✓ Transaction committed successfully\n\n")

  cat("=================================================================\n")
  cat("SUCCESS: Test data cleanup completed\n")
  cat("=================================================================\n\n")

  cat("Summary:\n")
  cat(sprintf("  - Position groups removed: %d\n", groups_deleted))
  cat(sprintf("  - Position group members removed: %d\n", members_deleted))
  cat(sprintf("  - Account activities removed: %d\n", activities_deleted))
  cat("\n")

  # Verify cleanup
  cat("Verification: Checking for remaining TEST_ groups...\n")
  remaining <- dbGetQuery(conn, "
    SELECT COUNT(*) as count
    FROM position_groups
    WHERE group_id LIKE 'TEST_%'
  ")

  if (remaining$count == 0) {
    cat("  ✓ All TEST_ groups successfully removed\n")
  } else {
    cat(sprintf("  ⚠ WARNING: %d TEST_ groups still remain\n", remaining$count))
  }

  cat("\n=================================================================\n")
  cat("\nNext steps:\n")
  cat("1. Your Portfolio Risk Analysis should no longer try to fetch quotes for test symbols\n")
  cat("2. Consider fixing the test suite to use a separate test database\n")
  cat("\n")

}, error = function(e) {
  # Rollback on error
  cat("\n❌ ERROR occurred during cleanup:\n")
  cat(sprintf("   %s\n\n", e$message))
  cat("Rolling back transaction...\n")
  dbRollback(conn)
  cat("  ✓ Transaction rolled back - no changes made\n\n")
  cat("=================================================================\n")
  stop(e)
})

# Disconnect
dbDisconnect(conn, shutdown = TRUE)
cat("Database connection closed.\n\n")
