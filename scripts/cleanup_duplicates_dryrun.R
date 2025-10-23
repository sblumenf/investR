#!/usr/bin/env Rscript
#
# Duplicate Transaction Cleanup - DRY RUN
#
# Purpose: Identify duplicate transactions created by Questrade API inconsistency
#          and report which would be kept vs deleted, WITHOUT actually deleting.
#
# Background: Questrade API sometimes returns same transaction with different
#             symbol fields (blank vs populated) across multiple API calls,
#             bypassing deduplication that matches on symbol field.
#
# Strategy: Keep linked transactions (manually curated), prefer populated symbols
#           Delete unlinked duplicates with blank/inferior data
#

library(DBI)
library(duckdb)
library(dplyr)
library(logger)

log_threshold(INFO)

log_info("=== DUPLICATE CLEANUP DRY-RUN ===")
log_info("This will identify duplicates but NOT delete anything")

# Connect to database
# Use inst/database directly since we're running from source, not installed package
db_path <- file.path(getwd(), "inst", "database", "portfolio.duckdb")

if (!file.exists(db_path)) {
  stop("Database file not found: ", db_path)
}

conn <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
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
# Duplicates = same (account_number, description, trade_date, action, quantity, net_amount)
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

log_info("Found {nrow(duplicate_sets)} duplicate transactions in {n_distinct(paste(duplicate_sets$account_number, duplicate_sets$description, duplicate_sets$trade_date, duplicate_sets$quantity, duplicate_sets$net_amount))} duplicate sets")

# For each duplicate set, determine which to KEEP and which to DELETE
log_info("Analyzing each duplicate set to determine keep vs delete...")

duplicate_analysis <- duplicate_sets %>%
  group_by(account_number, description, trade_date, action, quantity, net_amount) %>%
  mutate(
    set_id = cur_group_id(),
    set_size = n(),
    # Priority scoring
    has_group_id = !is.na(group_id) & group_id != "",
    has_symbol = !is.na(symbol) & symbol != "" & symbol != "NOSYMBOL",
    has_valid_symbol_id = !is.na(symbol_id) & symbol_id > 0,
    # Priority: linked > populated symbol > has valid symbol_id > newest
    priority_score = (has_group_id * 1000) + (has_symbol * 100) + (has_valid_symbol_id * 10) + row_number(),
    # Determine action
    action_to_take = if_else(
      priority_score == max(priority_score),
      "KEEP",
      "DELETE"
    ),
    # Reason for decision
    decision_reason = case_when(
      action_to_take == "KEEP" & has_group_id ~ "Linked to group (manually curated)",
      action_to_take == "KEEP" & has_symbol ~ "Has populated symbol field",
      action_to_take == "KEEP" ~ "Best available version",
      action_to_take == "DELETE" & !has_group_id ~ "Unlinked duplicate",
      TRUE ~ "Inferior duplicate"
    )
  ) %>%
  ungroup()

# Summary statistics
total_duplicates <- nrow(duplicate_analysis)
to_keep <- sum(duplicate_analysis$action_to_take == "KEEP")
to_delete <- sum(duplicate_analysis$action_to_take == "DELETE")

log_info("")
log_info("=== DRY-RUN SUMMARY ===")
log_info("Total duplicate transactions: {total_duplicates}")
log_info("Transactions to KEEP: {to_keep}")
log_info("Transactions to DELETE: {to_delete}")
log_info("")

# Generate detailed report
cat("\n=== DETAILED DUPLICATE SETS ===\n\n")

for (set_num in unique(duplicate_analysis$set_id)) {
  set_data <- duplicate_analysis %>% filter(set_id == set_num)

  cat("--- Duplicate Set #", set_num, " (", nrow(set_data), " transactions) ---\n", sep = "")
  cat("Description: ", set_data$description[1], "\n", sep = "")
  cat("Trade Date: ", as.character(set_data$trade_date[1]), "\n", sep = "")
  cat("Amount: $", sprintf("%.2f", set_data$net_amount[1]), "\n\n", sep = "")

  for (i in 1:nrow(set_data)) {
    row <- set_data[i, ]
    cat(sprintf("  [%s] %s\n", row$action_to_take, row$activity_id))
    cat(sprintf("      Symbol: '%s' | Symbol ID: %s | Group: %s\n",
                ifelse(is.na(row$symbol) | row$symbol == "", "(blank)", row$symbol),
                ifelse(is.na(row$symbol_id) | row$symbol_id == 0, "(none)", row$symbol_id),
                ifelse(is.na(row$group_id) | row$group_id == "", "(unlinked)", substr(row$group_id, 1, 30))))
    cat(sprintf("      Reason: %s\n", row$decision_reason))
    cat(sprintf("      Imported: %s\n\n", row$fetched_at))
  }

  cat("\n")
}

# Create CSV report
report_file <- file.path(getwd(), "duplicate_cleanup_report.csv")
duplicate_analysis %>%
  select(
    set_id,
    action_to_take,
    decision_reason,
    activity_id,
    symbol,
    symbol_id,
    description,
    trade_date,
    net_amount,
    group_id,
    fetched_at
  ) %>%
  write.csv(report_file, row.names = FALSE)

log_info("Detailed report saved to: {report_file}")
log_info("")
log_info("=== NEXT STEPS ===")
log_info("1. Review the duplicate sets above")
log_info("2. Check the CSV file: {report_file}")
log_info("3. If everything looks correct, run: scripts/cleanup_duplicates_execute.R")
log_info("4. That script will actually DELETE the duplicates marked above")
log_info("")
log_info("DRY-RUN COMPLETE - No data was modified")
