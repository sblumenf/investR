#!/usr/bin/env Rscript
#
# Fix Rolled Option Groups - Migration Script
#
# Purpose: Update position_group_members to reflect rolled options
#
# Background: When covered call options are rolled (buy-to-close old option,
#             sell-to-open new option), the activities are linked to the group
#             but the members table shows the old expired option symbol.
#
# Strategy: For each open group with an expired option member:
#           1. Look in linked activities for the most recent sell-to-open option
#           2. Update the member symbol from old to new option
#           3. Generate audit report of all changes
#
# Usage:
#   Rscript scripts/fix_rolled_option_groups.R           # Dry-run (preview)
#   Rscript scripts/fix_rolled_option_groups.R --execute  # Apply changes
#

library(DBI)
library(RSQLite)
library(dplyr)
library(logger)
library(devtools)

# Load investR functions
suppressPackageStartupMessages(load_all())

log_threshold(INFO)

# Parse command line args
args <- commandArgs(trailingOnly = TRUE)
dry_run <- !("--execute" %in% args)

if (dry_run) {
  log_info("=== DRY RUN MODE - No changes will be made ===")
  log_info("Run with --execute to apply changes")
} else {
  log_info("=== EXECUTE MODE - Changes will be applied ===")
}

# Connect to database
db_path <- file.path(getwd(), "inst", "database", "portfolio.sqlite")

if (!file.exists(db_path)) {
  stop("Database file not found: ", db_path)
}

log_info("Connecting to database: {db_path}")

conn <- if (dry_run) {
  dbConnect(RSQLite::SQLite(), db_path, flags = RSQLite::SQLITE_RO)
} else {
  dbConnect(RSQLite::SQLite(), db_path)
}
on.exit(dbDisconnect(conn), add = TRUE)

# Get all open groups
log_info("Fetching open position groups...")
open_groups <- dbGetQuery(conn, "
  SELECT group_id, group_name, strategy_type
  FROM position_groups
  WHERE status = 'open'
  ORDER BY created_at
") %>% as_tibble()

log_info("Found {nrow(open_groups)} open groups")

# Track changes for report
changes <- list()
total_rolls_found <- 0

# Process each group
for (i in seq_len(nrow(open_groups))) {
  group_id <- open_groups$group_id[i]
  group_name <- open_groups$group_name[i]

  # Get members
  members <- dbGetQuery(conn, "
    SELECT * FROM position_group_members WHERE group_id = ?
  ", params = list(group_id)) %>% as_tibble()

  # Find option members
  option_members <- members %>% filter(role == "short_call")

  if (nrow(option_members) == 0) {
    next  # No options in this group
  }

  # Check each option for expiration
  for (j in seq_len(nrow(option_members))) {
    old_symbol <- option_members$symbol[j]

    # Parse option details
    option_details <- parse_option_details(old_symbol)

    if (is.null(option_details$expiry)) {
      next  # Can't determine expiry
    }

    days_to_expiry <- as.numeric(difftime(option_details$expiry, Sys.Date(), units = "days"))

    # If option is expired, look for a roll
    if (days_to_expiry < 0) {
      log_info("Group {group_id} ({group_name}): Option {old_symbol} expired {abs(days_to_expiry)} days ago")

      # Get all activities for this group
      activities <- dbGetQuery(conn, "
        SELECT symbol, action, trade_date, type
        FROM account_activities
        WHERE group_id = ?
        ORDER BY trade_date DESC
      ", params = list(group_id)) %>% as_tibble()

      # Find most recent sell-to-open option transaction
      option_sells <- activities %>%
        filter(
          !is.na(action),
          action == "Sell",
          !is.na(symbol),
          is_option_symbol(symbol)
        )

      if (nrow(option_sells) > 0) {
        new_symbol <- option_sells$symbol[1]  # Most recent

        # Verify this is different from old symbol
        if (new_symbol != old_symbol) {
          # Verify it's for the same underlying
          old_underlying <- parse_option_symbol(old_symbol)
          new_underlying <- parse_option_symbol(new_symbol)

          if (!is.na(old_underlying) && !is.na(new_underlying) && old_underlying == new_underlying) {
            total_rolls_found <- total_rolls_found + 1

            log_info("  → Found roll: {old_symbol} → {new_symbol}")

            # Record change
            changes[[length(changes) + 1]] <- list(
              group_id = group_id,
              group_name = group_name,
              old_symbol = old_symbol,
              new_symbol = new_symbol,
              days_expired = abs(days_to_expiry)
            )

            # Apply change if not dry-run
            if (!dry_run) {
              rows_affected <- dbExecute(conn, "
                UPDATE position_group_members
                SET symbol = ?, added_at = ?
                WHERE group_id = ? AND symbol = ? AND role = 'short_call'
              ", params = list(new_symbol, Sys.time(), group_id, old_symbol))

              if (rows_affected > 0) {
                log_info("  ✓ Updated member in database")
              } else {
                log_warn("  ✗ Failed to update member")
              }
            }
          }
        }
      } else {
        log_warn("  No option sell transactions found in activities")
      }
    }
  }
}

# Generate report
log_info("\n=== MIGRATION SUMMARY ===")
log_info("Total rolls found: {total_rolls_found}")

if (length(changes) > 0) {
  # Convert to data frame
  report <- bind_rows(changes)

  # Save to CSV
  report_file <- if (dry_run) {
    "fix_rolled_options_preview.csv"
  } else {
    "fix_rolled_options_applied.csv"
  }

  write.csv(report, report_file, row.names = FALSE)
  log_info("Report saved to: {report_file}")

  # Print summary table
  cat("\n")
  print(report %>% select(group_name, old_symbol, new_symbol, days_expired))
  cat("\n")
}

if (dry_run) {
  log_info("\nTo apply these changes, run:")
  log_info("  Rscript scripts/fix_rolled_option_groups.R --execute")
} else {
  log_info("\n✓ Migration complete!")
  log_info("  {total_rolls_found} group(s) updated")
}
