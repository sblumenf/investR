#!/usr/bin/env Rscript
# Detailed investigation for BEN group capital deployed
# Examining all BEN activities and positions

library(DBI)
library(dplyr)
library(RSQLite)
library(tidyr)

# Database connection
db_path <- "inst/database/portfolio.sqlite"
conn <- dbConnect(RSQLite::SQLite(), db_path)

group_id <- "OTHER_53238853_20251008235947_2223"

cat("=================================================================\n")
cat("DETAILED INVESTIGATION: BEN Group Capital Deployed\n")
cat("=================================================================\n\n")

# 1. All BEN stock buy activities across all groups
cat("--- 1. ALL BEN STOCK BUY ACTIVITIES IN DATABASE ---\n")
all_ben_buys <- dbGetQuery(conn, "
  SELECT
    activity_id,
    trade_date,
    account_number,
    symbol,
    action,
    quantity,
    price,
    gross_amount,
    commission,
    net_amount,
    group_id
  FROM account_activities
  WHERE symbol = 'BEN'
    AND type = 'Trades'
    AND action = 'Buy'
  ORDER BY trade_date, account_number
") %>% as_tibble()

if (nrow(all_ben_buys) > 0) {
  print(all_ben_buys)
  cat("\n")

  cat("Summary by account:\n")
  buy_summary <- all_ben_buys %>%
    group_by(account_number, group_id) %>%
    summarise(
      buy_count = n(),
      total_quantity = sum(quantity, na.rm = TRUE),
      total_cost = sum(abs(gross_amount), na.rm = TRUE),
      total_commissions = sum(abs(commission), na.rm = TRUE),
      .groups = "drop"
    )
  print(buy_summary)
  cat("\n")
} else {
  cat("No BEN buy activities found!\n\n")
}

# 2. All BEN activities for the specific group
cat("--- 2. ACTIVITIES LINKED TO GROUP", group_id, "---\n")
group_activities <- dbGetQuery(conn, "
  SELECT *
  FROM account_activities
  WHERE group_id = ?
  ORDER BY trade_date
", params = list(group_id)) %>% as_tibble()

if (nrow(group_activities) > 0) {
  print(group_activities %>%
    select(trade_date, type, action, symbol, quantity, price,
           gross_amount, commission, net_amount))
} else {
  cat("No activities linked to this group!\n")
}
cat("\n")

# 3. Current positions from positions_history for BOTH accounts
cat("--- 3. ALL CURRENT BEN POSITIONS (ALL ACCOUNTS) ---\n")
latest_snapshot <- dbGetQuery(conn, "
  SELECT MAX(snapshot_timestamp) as latest_time
  FROM positions_history
")$latest_time

all_ben_positions <- dbGetQuery(conn, "
  SELECT
    account_number,
    symbol,
    open_quantity,
    average_entry_price,
    total_cost,
    current_market_value,
    open_pnl
  FROM positions_history
  WHERE symbol = 'BEN'
    AND snapshot_timestamp = ?
  ORDER BY account_number
", params = list(latest_snapshot)) %>% as_tibble()

if (nrow(all_ben_positions) > 0) {
  print(all_ben_positions)
  cat("\n")

  total_ben_cost <- sum(all_ben_positions$total_cost, na.rm = TRUE)
  total_ben_quantity <- sum(all_ben_positions$open_quantity, na.rm = TRUE)

  cat("TOTAL across all accounts:\n")
  cat("  Total quantity:", total_ben_quantity, "shares\n")
  cat("  Total cost:", total_ben_cost, "\n")
  cat("  Weighted avg price:", total_ben_cost / total_ben_quantity, "\n\n")
} else {
  cat("No current BEN positions found!\n\n")
}

# 4. Check which account the group belongs to
cat("--- 4. GROUP ACCOUNT ASSIGNMENT ---\n")
group_info <- dbGetQuery(conn, "
  SELECT group_id, group_name, account_number, strategy_type
  FROM position_groups
  WHERE group_id = ?
", params = list(group_id)) %>% as_tibble()

print(group_info)
cat("\n")

# 5. Members of this group
cat("--- 5. GROUP MEMBERS ---\n")
members <- dbGetQuery(conn, "
  SELECT *
  FROM position_group_members
  WHERE group_id = ?
", params = list(group_id)) %>% as_tibble()

print(members)
cat("\n")

# 6. Analysis
cat("--- 6. ROOT CAUSE ANALYSIS ---\n\n")

group_account <- group_info$account_number[1]
cat("Group is assigned to account:", group_account, "\n")

position_for_this_account <- all_ben_positions %>%
  filter(account_number == group_account)

if (nrow(position_for_this_account) > 0) {
  cat("Current BEN position in this account:", position_for_this_account$open_quantity[1],
      "shares\n")
  cat("Position cost basis:", position_for_this_account$total_cost[1], "\n\n")

  # Check activities
  buy_activities_for_account <- all_ben_buys %>%
    filter(account_number == group_account)

  if (nrow(buy_activities_for_account) > 0) {
    cat("BEN buy activities in this account:\n")
    print(buy_activities_for_account %>%
      select(trade_date, quantity, price, gross_amount, group_id))
    cat("\n")

    # Check if linked to this group
    linked_to_group <- buy_activities_for_account %>%
      filter(group_id == !!group_id)

    not_linked <- buy_activities_for_account %>%
      filter(is.na(group_id) | group_id != !!group_id)

    cat("Activities linked to THIS group:", nrow(linked_to_group), "\n")
    cat("Activities NOT linked to this group:", nrow(not_linked), "\n\n")

    if (nrow(not_linked) > 0) {
      cat("UNLINKED ACTIVITIES FOUND:\n")
      print(not_linked %>%
        select(activity_id, trade_date, quantity, price, gross_amount, group_id))
      cat("\n")

      unlinked_cost <- sum(abs(not_linked$gross_amount), na.rm = TRUE) +
                       sum(abs(not_linked$commission), na.rm = TRUE)

      cat("Cost basis from unlinked activities:", unlinked_cost, "\n")
      cat("\nCONCLUSION: The discrepancy is caused by BUY activities that\n")
      cat("exist in the database but are NOT linked to this group!\n")
    }
  } else {
    cat("NO BUY ACTIVITIES FOUND for account", group_account, "\n")
    cat("\nCONCLUSION: Position exists but no buy activities were recorded.\n")
    cat("This suggests the position existed before activity tracking began,\n")
    cat("or activities need to be bootstrapped from historical data.\n")
  }
}

cat("\n=================================================================\n")
cat("END OF DETAILED INVESTIGATION\n")
cat("=================================================================\n")

dbDisconnect(conn)
