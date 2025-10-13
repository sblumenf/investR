#!/usr/bin/env Rscript

# Audit script to check all position groups for missing activities
# Identifies groups that have members but no linked activities

library(dplyr)
library(DBI)
library(duckdb)

# Connect to database
db_path <- "inst/database/portfolio.duckdb"
conn <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)

cat("\n=================================================================\n")
cat("AUDIT: Position Groups Missing Activities\n")
cat("=================================================================\n\n")

# Get all open groups
all_groups <- dbGetQuery(conn, "
  SELECT group_id, group_name, strategy_type, created_at
  FROM position_groups
  WHERE status = 'open'
  ORDER BY created_at DESC
") %>% as_tibble()

cat(sprintf("Total open groups: %d\n\n", nrow(all_groups)))

# Initialize results
groups_with_issues <- list()

# Check each group
for (i in 1:nrow(all_groups)) {
  group_id <- all_groups$group_id[i]
  group_name <- all_groups$group_name[i]
  strategy_type <- all_groups$strategy_type[i]
  created_at <- all_groups$created_at[i]

  # Get members count
  members <- dbGetQuery(conn, "
    SELECT COUNT(*) as count
    FROM position_group_members
    WHERE group_id = ?
  ", params = list(group_id))
  member_count <- members$count[1]

  # Get activities count (excluding dividends, focusing on Trades)
  trade_activities <- dbGetQuery(conn, "
    SELECT COUNT(*) as count
    FROM account_activities
    WHERE group_id = ?
      AND type = 'Trades'
  ", params = list(group_id))
  trade_count <- trade_activities$count[1]

  # Get total activities count
  all_activities <- dbGetQuery(conn, "
    SELECT COUNT(*) as count
    FROM account_activities
    WHERE group_id = ?
  ", params = list(group_id))
  total_activity_count <- all_activities$count[1]

  # Flag if group has members but no trade activities
  if (member_count > 0 && trade_count == 0) {
    groups_with_issues[[length(groups_with_issues) + 1]] <- list(
      group_id = group_id,
      group_name = group_name,
      strategy_type = strategy_type,
      created_at = created_at,
      member_count = member_count,
      trade_count = trade_count,
      total_activity_count = total_activity_count
    )
  }
}

# Display results
if (length(groups_with_issues) > 0) {
  cat(sprintf("FOUND %d GROUPS WITH MISSING TRADE ACTIVITIES:\n", length(groups_with_issues)))
  cat("=================================================================\n\n")

  for (issue in groups_with_issues) {
    cat(sprintf("Group: %s\n", issue$group_name))
    cat(sprintf("  ID: %s\n", issue$group_id))
    cat(sprintf("  Strategy: %s\n", issue$strategy_type))
    cat(sprintf("  Created: %s\n", issue$created_at))
    cat(sprintf("  Members: %d\n", issue$member_count))
    cat(sprintf("  Trade Activities: %d\n", issue$trade_count))
    cat(sprintf("  Total Activities: %d (includes dividends, etc.)\n", issue$total_activity_count))

    # Get member details
    members <- dbGetQuery(conn, "
      SELECT symbol, role
      FROM position_group_members
      WHERE group_id = ?
    ", params = list(issue$group_id))

    cat("  Members:\n")
    for (j in 1:nrow(members)) {
      cat(sprintf("    - %s (%s)\n", members$symbol[j], members$role[j]))
    }

    # Check if positions exist
    cat("  Position Data:\n")
    for (j in 1:nrow(members)) {
      symbol <- members$symbol[j]
      pos <- dbGetQuery(conn, "
        SELECT open_quantity, average_entry_price, total_cost
        FROM positions_history
        WHERE symbol = ?
        ORDER BY snapshot_timestamp DESC
        LIMIT 1
      ", params = list(symbol))

      if (nrow(pos) > 0 && pos$open_quantity[1] != 0) {
        cat(sprintf("    - %s: %.0f @ $%.4f = $%.2f\n",
                    symbol, pos$open_quantity[1], pos$average_entry_price[1], pos$total_cost[1]))
      } else {
        cat(sprintf("    - %s: NO POSITION DATA\n", symbol))
      }
    }

    cat("\n")
  }

  cat("\n=================================================================\n")
  cat("RECOMMENDATION:\n")
  cat("=================================================================\n\n")
  cat("These groups need bootstrap activities created. Add their group IDs\n")
  cat("to the bootstrap script and run it:\n\n")

  for (issue in groups_with_issues) {
    cat(sprintf('  "%s",  # %s\n', issue$group_id, issue$group_name))
  }

} else {
  cat("No issues found! All open groups with members have trade activities.\n")
}

cat("\n=================================================================\n")

# Close connection
dbDisconnect(conn, shutdown = TRUE)
