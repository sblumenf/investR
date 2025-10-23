#!/usr/bin/env Rscript
#
# Duplicate Transaction Cleanup - EXECUTION
#
# Purpose: Actually DELETE duplicate transactions identified by dry-run script.
#
# WARNING: This script permanently deletes data. Run dry-run first and review!
#
# Strategy: Keep linked transactions (manually curated), prefer populated symbols
#           Delete unlinked duplicates with blank/inferior data
#

library(DBI)
library(duckdb)
library(dplyr)
library(logger)

log_threshold(INFO)

log_info("=== DUPLICATE CLEANUP EXECUTION ===")
log_warn("⚠️  WARNING: This will PERMANENTLY DELETE duplicate transactions!")
log_info("Press Ctrl+C within 5 seconds to cancel...")
Sys.sleep(5)
log_info("Proceeding with cleanup...")

# Connect to database (READ-WRITE mode)
# Use inst/database directly since we're running from source, not installed package
db_path <- file.path(getwd(), "inst", "database", "portfolio.duckdb")

if (!file.exists(db_path)) {
  stop("Database file not found: ", db_path)
}

conn <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

# Get all activities
log_info("Fetching all transactions...")
all_activities <- dbGetQuery(conn, "
  SELECT
    activity_id,
    account_number,
    symbol,
    symbol_id,
    description,
    trade_date,
    action,
    quantity,
    net_amount,
    group_id,
    fetched_at
  FROM account_activities
  ORDER BY trade_date DESC, fetched_at
") %>% as_tibble()

log_info("Total transactions in database: {nrow(all_activities)}")

# Find duplicate sets
log_info("Identifying duplicate sets...")

duplicate_sets <- all_activities %>%
  group_by(account_number, description, trade_date, action, quantity, net_amount) %>%
  filter(n() > 1) %>%
  arrange(account_number, trade_date, description) %>%
  ungroup()

if (nrow(duplicate_sets) == 0) {
  log_info("No duplicates found! Database is clean.")
  quit(status = 0)
}

log_info("Found {nrow(duplicate_sets)} duplicate transactions")

# Analyze duplicates to determine keep vs delete
log_info("Analyzing duplicates...")

duplicate_analysis <- duplicate_sets %>%
  group_by(account_number, description, trade_date, action, quantity, net_amount) %>%
  mutate(
    set_id = cur_group_id(),
    set_size = n(),
    has_group_id = !is.na(group_id) & group_id != "",
    has_symbol = !is.na(symbol) & symbol != "" & symbol != "NOSYMBOL",
    has_valid_symbol_id = !is.na(symbol_id) & symbol_id > 0,
    priority_score = (has_group_id * 1000) + (has_symbol * 100) + (has_valid_symbol_id * 10) + row_number(),
    action_to_take = if_else(
      priority_score == max(priority_score),
      "KEEP",
      "DELETE"
    ),
    decision_reason = case_when(
      action_to_take == "KEEP" & has_group_id ~ "Linked to group",
      action_to_take == "KEEP" & has_symbol ~ "Has populated symbol",
      action_to_take == "KEEP" ~ "Best available",
      action_to_take == "DELETE" & !has_group_id ~ "Unlinked duplicate",
      TRUE ~ "Inferior duplicate"
    )
  ) %>%
  ungroup()

# Get list of activity_ids to delete
to_delete <- duplicate_analysis %>%
  filter(action_to_take == "DELETE") %>%
  pull(activity_id)

to_keep <- duplicate_analysis %>%
  filter(action_to_take == "KEEP") %>%
  pull(activity_id)

log_info("")
log_info("=== EXECUTION PLAN ===")
log_info("Transactions to KEEP: {length(to_keep)}")
log_info("Transactions to DELETE: {length(to_delete)}")
log_info("")

if (length(to_delete) == 0) {
  log_info("Nothing to delete!")
  quit(status = 0)
}

# Create deletion log file
log_file <- file.path(getwd(), "deleted_duplicates_log.csv")
duplicate_analysis %>%
  filter(action_to_take == "DELETE") %>%
  select(
    activity_id,
    account_number,
    symbol,
    description,
    trade_date,
    net_amount,
    group_id,
    decision_reason,
    fetched_at
  ) %>%
  write.csv(log_file, row.names = FALSE)

log_info("Deletion log will be saved to: {log_file}")

# Execute deletions
log_info("")
log_info("Executing deletions...")

deletion_count <- 0
for (i in seq_along(to_delete)) {
  activity_id <- to_delete[i]

  tryCatch({
    rows_affected <- dbExecute(conn, "
      DELETE FROM account_activities
      WHERE activity_id = ?
    ", params = list(activity_id))

    if (rows_affected > 0) {
      deletion_count <- deletion_count + 1
      if (i %% 10 == 0) {
        log_info("Progress: {i}/{length(to_delete)} deleted...")
      }
    } else {
      log_warn("Activity {activity_id} not found (may have been deleted already)")
    }
  }, error = function(e) {
    log_error("Failed to delete {activity_id}: {e$message}")
  })
}

# Verify results
final_count <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM account_activities")$count

log_info("")
log_info("=== EXECUTION COMPLETE ===")
log_info("Transactions deleted: {deletion_count}")
log_info("Transactions remaining in database: {final_count}")
log_info("Deletion log saved to: {log_file}")
log_info("")

# Verify no duplicates remain
remaining_duplicates <- dbGetQuery(conn, "
  SELECT
    account_number,
    description,
    trade_date,
    action,
    quantity,
    net_amount,
    COUNT(*) as count
  FROM account_activities
  GROUP BY account_number, description, trade_date, action, quantity, net_amount
  HAVING COUNT(*) > 1
")

if (nrow(remaining_duplicates) > 0) {
  log_warn("⚠️  Warning: {nrow(remaining_duplicates)} duplicate sets still remain!")
  log_warn("This may indicate linked duplicates or edge cases.")
  log_warn("Review manually if needed.")
} else {
  log_info("✅ SUCCESS: No duplicates remain in database!")
}

log_info("")
log_info("=== NEXT STEPS ===")
log_info("1. Review the deletion log: {log_file}")
log_info("2. Verify your linked transactions are intact")
log_info("3. Proceed with updating the unique index (Phase 4)")
