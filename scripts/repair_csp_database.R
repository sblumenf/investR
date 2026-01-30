#!/usr/bin/env Rscript
#
# Database Repair Script: Fix Corrupted XYZ Cash-Secured Put Position
#
# Problem: XYZ CSP position was saved with corrupted data:
#   - member_symbol = "" (empty)
#   - member_role = "underlying_stock" (should be "short_put")
#   - group_name = "S&P 500 Cash-Secured Puts - " (incomplete, missing ticker)
#
# Solution:
#   1. Find the corrupted group (empty symbol + incomplete group name)
#   2. Extract correct symbol from related activity description
#   3. Update member record with correct symbol and role
#   4. Regenerate group name to include ticker
#   5. Verify fix was applied correctly
#
# Usage:
#   Rscript scripts/repair_csp_database.R
#
# Author: Backend Architect Agent
# Date: 2025-12-10

library(DBI)
library(duckdb)
library(dplyr)
library(stringr)

# Color output helpers
cat_success <- function(...) cat("\033[32m", ..., "\033[0m\n", sep = "")
cat_error <- function(...) cat("\033[31m", ..., "\033[0m\n", sep = "")
cat_info <- function(...) cat("\033[34m", ..., "\033[0m\n", sep = "")
cat_warn <- function(...) cat("\033[33m", ..., "\033[0m\n", sep = "")

# Database connection
DB_PATH <- "inst/database/portfolio.duckdb"

cat_info("=================================================")
cat_info("CSP Database Repair Script")
cat_info("=================================================\n")

# Connect to database
cat_info("Connecting to database: ", DB_PATH)
conn <- dbConnect(duckdb::duckdb(), DB_PATH)

tryCatch({

  # ============================================================================
  # STEP 1: Find the corrupted group
  # ============================================================================

  cat_info("\n[STEP 1] Searching for corrupted CSP group...")

  corrupted_members <- dbGetQuery(conn, "
    SELECT
      m.group_id,
      m.symbol,
      m.role,
      m.account_number,
      g.group_name,
      g.strategy_type,
      g.status
    FROM position_group_members m
    JOIN position_groups g ON m.group_id = g.group_id
    WHERE m.symbol = ''
      AND m.role = 'underlying_stock'
      AND g.group_name LIKE '% - '
      AND g.strategy_type = 'S&P 500 Cash-Secured Puts'
  ")

  if (nrow(corrupted_members) == 0) {
    cat_warn("No corrupted CSP group found. Either:")
    cat_warn("  - The issue has already been fixed")
    cat_warn("  - The corruption pattern has changed")
    cat_info("\nExiting without making changes.")
    dbDisconnect(conn, shutdown = TRUE)
    quit(status = 0)
  }

  if (nrow(corrupted_members) > 1) {
    cat_error("Multiple corrupted groups found! Expected only 1.")
    cat_error("Please investigate manually before running this script.")
    print(corrupted_members)
    dbDisconnect(conn, shutdown = TRUE)
    quit(status = 1)
  }

  # Extract group details
  group_id <- corrupted_members$group_id
  account_number <- corrupted_members$account_number
  old_group_name <- corrupted_members$group_name

  cat_success("Found corrupted group:")
  cat_info("  Group ID: ", group_id)
  cat_info("  Account: ", account_number)
  cat_info("  Current Group Name: '", old_group_name, "'")
  cat_info("  Current Member Symbol: '", corrupted_members$symbol, "' (empty)")
  cat_info("  Current Member Role: '", corrupted_members$role, "'")

  # ============================================================================
  # STEP 2: Extract correct symbol from activity description
  # ============================================================================

  cat_info("\n[STEP 2] Looking up correct symbol from related activities...")

  activities <- dbGetQuery(conn, "
    SELECT
      activity_id,
      symbol,
      description,
      action,
      quantity,
      price,
      trade_date
    FROM account_activities
    WHERE group_id = ?
    ORDER BY trade_date
  ", params = list(group_id))

  if (nrow(activities) == 0) {
    cat_error("No activities found for group ", group_id)
    cat_error("Cannot determine correct symbol. Manual intervention required.")
    dbDisconnect(conn, shutdown = TRUE)
    quit(status = 1)
  }

  cat_info("Found ", nrow(activities), " related activity/activities")

  # Extract symbol from description (format: "PUT XYZ 01/23/26 50 BLOCK INC CL A...")
  # Pattern: "PUT <SYMBOL> <DATE> <STRIKE>"
  first_activity <- activities[1, ]
  description <- first_activity$description

  cat_info("  Activity Description: ", description)

  # Extract symbol using regex: Match word after "PUT " and before next space
  symbol_match <- str_match(description, "PUT\\s+([A-Z]+)\\s+")

  if (is.na(symbol_match[1, 2])) {
    cat_error("Could not extract symbol from description: ", description)
    cat_error("Expected format: 'PUT <SYMBOL> <DATE> <STRIKE>'")
    dbDisconnect(conn, shutdown = TRUE)
    quit(status = 1)
  }

  correct_symbol <- symbol_match[1, 2]
  cat_success("Extracted symbol: ", correct_symbol)

  # ============================================================================
  # STEP 3: Verify this is the expected fix
  # ============================================================================

  cat_info("\n[STEP 3] Preparing database updates...")

  new_group_name <- paste0("S&P 500 Cash-Secured Puts - ", correct_symbol)
  new_role <- "short_put"

  cat_info("Planned changes:")
  cat_info("  Member Symbol: '' → '", correct_symbol, "'")
  cat_info("  Member Role: 'underlying_stock' → '", new_role, "'")
  cat_info("  Group Name: '", old_group_name, "' → '", new_group_name, "'")

  # ============================================================================
  # STEP 4: Apply the fix in a transaction
  # ============================================================================

  cat_info("\n[STEP 4] Applying database updates (transactional)...")

  dbExecute(conn, "BEGIN TRANSACTION")

  tryCatch({
    # Update member record: symbol and role
    rows_updated_member <- dbExecute(conn, "
      UPDATE position_group_members
      SET
        symbol = ?,
        role = ?,
        added_at = ?
      WHERE group_id = ?
        AND symbol = ''
        AND role = 'underlying_stock'
    ", params = list(correct_symbol, new_role, Sys.time(), group_id))

    if (rows_updated_member != 1) {
      stop("Expected to update 1 member record, but updated ", rows_updated_member)
    }

    cat_success("  Updated member record (1 row)")

    # Update group record: group_name and updated_at
    rows_updated_group <- dbExecute(conn, "
      UPDATE position_groups
      SET
        group_name = ?,
        updated_at = ?
      WHERE group_id = ?
    ", params = list(new_group_name, Sys.time(), group_id))

    if (rows_updated_group != 1) {
      stop("Expected to update 1 group record, but updated ", rows_updated_group)
    }

    cat_success("  Updated group record (1 row)")

    # Commit transaction
    dbExecute(conn, "COMMIT")
    cat_success("  Transaction committed successfully")

  }, error = function(e) {
    dbExecute(conn, "ROLLBACK")
    cat_error("Transaction rolled back due to error:")
    cat_error("  ", conditionMessage(e))
    stop(e)
  })

  # ============================================================================
  # STEP 5: Verify the fix
  # ============================================================================

  cat_info("\n[STEP 5] Verifying repair...")

  # Check member record
  fixed_member <- dbGetQuery(conn, "
    SELECT symbol, role
    FROM position_group_members
    WHERE group_id = ?
  ", params = list(group_id))

  # Check group record
  fixed_group <- dbGetQuery(conn, "
    SELECT group_name
    FROM position_groups
    WHERE group_id = ?
  ", params = list(group_id))

  # Verify expectations
  verification_passed <- TRUE

  if (nrow(fixed_member) != 1) {
    cat_error("Verification failed: Expected 1 member, found ", nrow(fixed_member))
    verification_passed <- FALSE
  } else if (fixed_member$symbol != correct_symbol) {
    cat_error("Verification failed: Member symbol = '", fixed_member$symbol, "', expected '", correct_symbol, "'")
    verification_passed <- FALSE
  } else if (fixed_member$role != new_role) {
    cat_error("Verification failed: Member role = '", fixed_member$role, "', expected '", new_role, "'")
    verification_passed <- FALSE
  } else {
    cat_success("  Member record verified: symbol = '", fixed_member$symbol, "', role = '", fixed_member$role, "'")
  }

  if (nrow(fixed_group) != 1) {
    cat_error("Verification failed: Expected 1 group, found ", nrow(fixed_group))
    verification_passed <- FALSE
  } else if (fixed_group$group_name != new_group_name) {
    cat_error("Verification failed: Group name = '", fixed_group$group_name, "', expected '", new_group_name, "'")
    verification_passed <- FALSE
  } else {
    cat_success("  Group record verified: group_name = '", fixed_group$group_name, "'")
  }

  # Final status
  if (verification_passed) {
    cat_success("\n=================================================")
    cat_success("REPAIR COMPLETED SUCCESSFULLY")
    cat_success("=================================================")
    cat_info("\nSummary:")
    cat_info("  - Fixed symbol: ", correct_symbol)
    cat_info("  - Fixed role: ", new_role)
    cat_info("  - Updated group name: ", new_group_name)
    cat_info("\nThe XYZ CSP position has been repaired and is now properly configured.")
  } else {
    cat_error("\n=================================================")
    cat_error("VERIFICATION FAILED")
    cat_error("=================================================")
    cat_error("The updates were applied but verification checks failed.")
    cat_error("Manual investigation required.")
    quit(status = 1)
  }

}, error = function(e) {
  cat_error("\n=================================================")
  cat_error("REPAIR FAILED")
  cat_error("=================================================")
  cat_error("Error: ", conditionMessage(e))
  cat_error("\nNo changes were made to the database.")
}, finally = {
  dbDisconnect(conn, shutdown = TRUE)
  cat_info("\nDatabase connection closed.")
})
