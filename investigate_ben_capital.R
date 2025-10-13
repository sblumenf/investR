#!/usr/bin/env Rscript
# Investigation script for BEN group capital deployed calculation
# Group ID: OTHER_53238853_20251008235947_2223

library(DBI)
library(dplyr)
library(duckdb)
library(tidyr)
library(purrr)
library(logger)

# Simple override for the database path function
get_portfolio_db_path <- function() {
  db_path <- "inst/database/portfolio.duckdb"
  if (!file.exists(db_path)) {
    stop("Database not found at: ", db_path)
  }
  return(db_path)
}

# Get connection
get_portfolio_db_connection <- function() {
  db_path <- get_portfolio_db_path()
  conn <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  return(conn)
}

# Source required functions
source("R/utils_transaction_helpers.R")
source("R/fct_portfolio_groups_database.R")
source("R/fct_activities_database.R")
source("R/fct_income_projection_database.R")
source("R/fct_group_metrics.R")

group_id <- "OTHER_53238853_20251008235947_2223"

cat("=================================================================\n")
cat("INVESTIGATION: Capital Deployed for Position Group BEN\n")
cat("=================================================================\n\n")

cat("Group ID:", group_id, "\n\n")

# 1. Get group information
cat("--- 1. GROUP INFORMATION ---\n")
group_info <- get_group_by_id(group_id)
print(group_info)
cat("\n")

# 2. Get group members
cat("--- 2. GROUP MEMBERS ---\n")
members <- get_group_members(group_id)
print(members)
cat("\n")

# 3. Get activities linked to this group
cat("--- 3. ACTIVITIES LINKED TO GROUP ---\n")
activities <- get_activities_by_group(group_id)
cat("Total activities:", nrow(activities), "\n\n")

if (nrow(activities) > 0) {
  cat("Activity summary:\n")
  activity_summary <- activities %>%
    group_by(type, action) %>%
    summarise(
      count = n(),
      total_quantity = sum(abs(quantity), na.rm = TRUE),
      total_gross_amount = sum(abs(gross_amount), na.rm = TRUE),
      total_net_amount = sum(abs(net_amount), na.rm = TRUE),
      .groups = "drop"
    )
  print(activity_summary)
  cat("\n")

  cat("Detailed activities:\n")
  activities_detail <- activities %>%
    select(trade_date, type, action, symbol, quantity, price,
           gross_amount, commission, net_amount) %>%
    arrange(trade_date)
  print(activities_detail, n = 50)
  cat("\n")
} else {
  cat("WARNING: No activities found for this group!\n\n")
}

# 4. Get current positions from positions_history
cat("--- 4. CURRENT POSITIONS FROM POSITIONS_HISTORY ---\n")
conn <- get_portfolio_db_connection()

# Get the latest snapshot
latest_snapshot <- dbGetQuery(conn, "
  SELECT MAX(snapshot_timestamp) as latest_time
  FROM positions_history
")
cat("Latest snapshot:", as.character(latest_snapshot$latest_time), "\n\n")

# Get current positions for BEN
current_positions <- dbGetQuery(conn, "
  SELECT *
  FROM positions_history
  WHERE symbol = 'BEN'
    AND snapshot_timestamp = ?
  ORDER BY account_number
", params = list(latest_snapshot$latest_time)) %>%
  as_tibble()

cat("Current positions for BEN:\n")
print(current_positions %>%
  select(account_number, symbol, open_quantity, average_entry_price,
         total_cost, current_market_value, open_pnl))
cat("\n")

dbDisconnect(conn, shutdown = TRUE)

# 5. Get cash flows
cat("--- 5. CASH FLOWS FOR GROUP ---\n")
conn <- get_portfolio_db_connection()
cash_flows <- dbGetQuery(conn, "
  SELECT *
  FROM position_group_cash_flows
  WHERE group_id = ?
  ORDER BY event_date
", params = list(group_id)) %>%
  as_tibble()

if (nrow(cash_flows) > 0) {
  print(cash_flows)
} else {
  cat("No cash flows found for this group.\n")
}
cat("\n")
dbDisconnect(conn, shutdown = TRUE)

# 6. Calculate metrics using the function
cat("--- 6. CALCULATED METRICS FROM calculate_open_group_metrics() ---\n")
metrics <- calculate_open_group_metrics(group_id)
if (nrow(metrics) > 0) {
  print(metrics)
  cat("\n")
  cat("Key metric - Cost Basis (Capital Deployed):", metrics$cost_basis, "\n")
  cat("Cash Collected:", metrics$cash_collected, "\n")
  cat("Projected Income:", metrics$projected_income, "\n")
} else {
  cat("ERROR: Could not calculate metrics!\n")
}
cat("\n")

# 7. Compare and analyze
cat("--- 7. ANALYSIS & COMPARISON ---\n\n")

if (nrow(activities) > 0 && nrow(current_positions) > 0) {
  # Calculate what activities say
  stock_purchases_from_activities <- activities %>%
    filter(type == "Trades", action == "Buy") %>%
    summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
    pull(total)

  commissions_from_activities <- activities %>%
    summarise(total = sum(abs(commission), na.rm = TRUE)) %>%
    pull(total)

  # Calculate what positions say
  total_cost_from_positions <- sum(current_positions$total_cost, na.rm = TRUE)
  avg_entry_price <- sum(current_positions$average_entry_price *
                         current_positions$open_quantity, na.rm = TRUE) /
                     sum(current_positions$open_quantity, na.rm = TRUE)
  total_quantity <- sum(current_positions$open_quantity, na.rm = TRUE)

  cat("FROM ACTIVITIES:\n")
  cat("  Stock purchases (gross):", stock_purchases_from_activities, "\n")
  cat("  Commissions:", commissions_from_activities, "\n")
  cat("  Total cost basis:", stock_purchases_from_activities + commissions_from_activities, "\n\n")

  cat("FROM POSITIONS_HISTORY:\n")
  cat("  Total cost (from DB):", total_cost_from_positions, "\n")
  cat("  Average entry price:", avg_entry_price, "\n")
  cat("  Total quantity:", total_quantity, "\n")
  cat("  Calculated cost (qty * avg_price):", total_quantity * avg_entry_price, "\n\n")

  cat("FROM CALCULATED METRICS:\n")
  if (nrow(metrics) > 0) {
    cat("  Cost basis:", metrics$cost_basis, "\n\n")
  }

  # Check for discrepancies
  discrepancy <- abs(total_cost_from_positions -
                     (stock_purchases_from_activities + commissions_from_activities))

  cat("DISCREPANCY ANALYSIS:\n")
  cat("  Difference between positions and activities: $",
      round(discrepancy, 2), "\n")

  if (discrepancy > 1) {
    cat("  STATUS: DISCREPANCY DETECTED!\n\n")
    cat("  Possible causes:\n")
    cat("  1. Missing BUY activities that aren't linked to this group\n")
    cat("  2. Bootstrap data issue - synthetic activities don't match actual positions\n")
    cat("  3. Activities linked to wrong group\n")
    cat("  4. Position was transferred from another account\n")
  } else {
    cat("  STATUS: Values match - calculations appear correct\n")
  }

} else {
  cat("ERROR: Missing data for comparison\n")
  if (nrow(activities) == 0) {
    cat("  - No activities found\n")
  }
  if (nrow(current_positions) == 0) {
    cat("  - No current positions found\n")
  }
}

cat("\n")

# 8. Check for unlinked activities
cat("--- 8. UNLINKED BEN ACTIVITIES ---\n")
if (nrow(current_positions) > 0) {
  account_num <- current_positions$account_number[1]
  unlinked <- get_unlinked_activities_for_ticker("BEN", account_num)

  if (nrow(unlinked) > 0) {
    cat("Found", nrow(unlinked), "unlinked activities for BEN:\n")
    print(unlinked %>%
      select(trade_date, action, quantity, price, gross_amount, net_amount))
  } else {
    cat("No unlinked activities found for BEN in this account.\n")
  }
} else {
  cat("Cannot check - no position data available\n")
}

cat("\n=================================================================\n")
cat("END OF INVESTIGATION\n")
cat("=================================================================\n")
