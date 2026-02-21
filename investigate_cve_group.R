#!/usr/bin/env Rscript

# Investigation script for CVE group capital deployed calculation
# Group ID: OTHER_53238334_20251009000042_2872

library(dplyr)
library(DBI)
library(RSQLite)
library(logger)

# Set log level
log_threshold(INFO)

# Connect directly to database
db_path <- "inst/database/portfolio.sqlite"
conn <- dbConnect(RSQLite::SQLite(), db_path)

cat("\n=================================================================\n")
cat("INVESTIGATION: CVE Group Capital Deployed Calculation\n")
cat("=================================================================\n\n")

group_id <- "OTHER_53238334_20251009000042_2872"

# 1. Get group information
cat("1. GROUP INFORMATION\n")
cat("--------------------\n")
group_info <- dbGetQuery(conn, "
  SELECT * FROM position_groups WHERE group_id = ?
", params = list(group_id)) %>% as_tibble()
print(group_info)
cat("\n")

# 2. Get group members
cat("2. GROUP MEMBERS\n")
cat("----------------\n")
members <- dbGetQuery(conn, "
  SELECT * FROM position_group_members WHERE group_id = ?
  ORDER BY added_at
", params = list(group_id)) %>% as_tibble()
print(members)
cat("\n")

# 3. Get activities linked to this group
cat("3. ACTIVITIES LINKED TO THIS GROUP\n")
cat("-----------------------------------\n")
activities <- dbGetQuery(conn, "
  SELECT *
  FROM account_activities
  WHERE group_id = ?
  ORDER BY trade_date ASC
", params = list(group_id)) %>% as_tibble()
if (nrow(activities) > 0) {
  activities_summary <- activities %>%
    select(activity_id, trade_date, type, action, symbol, quantity,
           price, gross_amount, commission, net_amount) %>%
    arrange(trade_date)
  print(activities_summary, n = 100)

  cat("\n\nACTIVITIES SUMMARY BY TYPE:\n")
  activities %>%
    group_by(type, action) %>%
    summarise(
      count = n(),
      total_quantity = sum(quantity, na.rm = TRUE),
      total_gross = sum(abs(gross_amount), na.rm = TRUE),
      total_commission = sum(abs(commission), na.rm = TRUE),
      total_net = sum(abs(net_amount), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    print()
} else {
  cat("No activities linked to this group!\n")
}
cat("\n")

# 4. Get current positions for CVE from positions_history
cat("4. CURRENT POSITION DATA FROM POSITIONS_HISTORY\n")
cat("------------------------------------------------\n")

# Get account number from group
account_num <- group_info$account_number[1]

# Get latest snapshot for CVE
latest_cve_position <- dbGetQuery(conn, "
  SELECT *
  FROM positions_history
  WHERE account_number = ?
    AND symbol = 'CVE'
  ORDER BY snapshot_timestamp DESC
  LIMIT 1
", params = list(account_num)) %>% as_tibble()

if (nrow(latest_cve_position) > 0) {
  print(as_tibble(latest_cve_position))

  cat("\n\nKEY POSITION METRICS:\n")
  cat(sprintf("  Open Quantity: %.2f shares\n", latest_cve_position$open_quantity))
  cat(sprintf("  Average Entry Price: $%.4f\n", latest_cve_position$average_entry_price))
  cat(sprintf("  Total Cost: $%.2f\n", latest_cve_position$total_cost))
  cat(sprintf("  Current Market Value: $%.2f\n", latest_cve_position$current_market_value))
  cat(sprintf("  Current Price: $%.4f\n", latest_cve_position$current_price))
} else {
  cat("No position data found for CVE in positions_history!\n")
}
cat("\n")

# 5. Manual calculation of metrics (simulating calculate_open_group_metrics)
cat("5. MANUAL METRICS CALCULATION\n")
cat("------------------------------\n")

if (nrow(activities) > 0) {
  # Calculate stock purchases (excluding commissions for gross amount)
  stock_purchases <- activities %>%
    filter(type == "Trades", action == "Buy", symbol == "CVE") %>%
    summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
    pull(total)
  stock_purchases <- if (length(stock_purchases) == 0) 0 else stock_purchases

  # Calculate total commissions
  total_commissions <- activities %>%
    summarise(total = sum(abs(commission), na.rm = TRUE)) %>%
    pull(total)
  total_commissions <- if (length(total_commissions) == 0) 0 else total_commissions

  # Get strategy type
  strategy_type <- group_info$strategy_type[1]

  # For "Other" strategy: cost_basis = stock_purchases + total_commissions
  cost_basis <- stock_purchases + total_commissions

  cat(sprintf("Strategy Type: %s\n", strategy_type))
  cat(sprintf("Stock Purchases (gross): $%.2f\n", stock_purchases))
  cat(sprintf("Total Commissions: $%.2f\n", total_commissions))
  cat(sprintf("Calculated Cost Basis: $%.2f\n", cost_basis))
} else {
  cat("No activities to calculate metrics from!\n")
}
cat("\n")

# 6. Check for unlinked activities for CVE
cat("6. UNLINKED ACTIVITIES FOR CVE (NOT IN THIS GROUP)\n")
cat("---------------------------------------------------\n")
unlinked <- dbGetQuery(conn, "
  SELECT *
  FROM account_activities
  WHERE symbol = ?
    AND account_number = ?
    AND type = 'Trades'
    AND (group_id IS NULL OR group_id = '')
  ORDER BY trade_date ASC
", params = list("CVE", account_num)) %>% as_tibble()
if (nrow(unlinked) > 0) {
  unlinked_summary <- unlinked %>%
    select(activity_id, trade_date, type, action, symbol, quantity,
           price, gross_amount, commission, net_amount) %>%
    arrange(trade_date)
  print(unlinked_summary, n = 100)
} else {
  cat("No unlinked activities found for CVE.\n")
}
cat("\n")

# 7. Get all CVE activities (linked and unlinked)
cat("7. ALL CVE ACTIVITIES IN DATABASE\n")
cat("----------------------------------\n")
all_cve_activities <- dbGetQuery(conn, "
  SELECT *
  FROM account_activities
  WHERE symbol = 'CVE'
    AND account_number = ?
  ORDER BY trade_date ASC
", params = list(account_num)) %>% as_tibble()

if (nrow(all_cve_activities) > 0) {
  all_cve_summary <- as_tibble(all_cve_activities) %>%
    select(activity_id, trade_date, type, action, quantity,
           price, gross_amount, commission, net_amount, group_id) %>%
    arrange(trade_date)
  print(all_cve_summary, n = 100)

  cat("\n\nALL CVE ACTIVITIES SUMMARY:\n")
  as_tibble(all_cve_activities) %>%
    group_by(type, action, group_id) %>%
    summarise(
      count = n(),
      total_quantity = sum(quantity, na.rm = TRUE),
      total_gross = sum(abs(gross_amount), na.rm = TRUE),
      total_commission = sum(abs(commission), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    print()
} else {
  cat("No CVE activities found in database at all!\n")
}
cat("\n")

# 8. Analysis and comparison
cat("8. ANALYSIS AND COMPARISON\n")
cat("---------------------------\n")

if (nrow(activities) > 0 && nrow(latest_cve_position) > 0) {

  # Calculate expected cost basis from activities
  stock_purchases <- activities %>%
    filter(type == "Trades", action == "Buy", symbol == "CVE") %>%
    summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
    pull(total)
  stock_purchases <- if (length(stock_purchases) == 0) 0 else stock_purchases

  total_commissions <- activities %>%
    summarise(total = sum(abs(commission), na.rm = TRUE)) %>%
    pull(total)
  total_commissions <- if (length(total_commissions) == 0) 0 else total_commissions

  expected_cost_from_activities <- stock_purchases + total_commissions

  # Get strategy type
  strategy_type <- group_info$strategy_type[1]

  cat(sprintf("Strategy Type: %s\n\n", strategy_type))

  cat("COMPARISON:\n")
  cat(sprintf("  A. Cost Basis from Activities:     $%.2f\n", expected_cost_from_activities))
  cat(sprintf("     - Stock Purchases (gross):      $%.2f\n", stock_purchases))
  cat(sprintf("     - Total Commissions:            $%.2f\n", total_commissions))
  cat(sprintf("\n"))
  cat(sprintf("  B. Total Cost from Position:       $%.2f\n", latest_cve_position$total_cost))
  cat(sprintf("\n"))
  cat(sprintf("  DISCREPANCY (A vs B):              $%.2f\n",
              expected_cost_from_activities - latest_cve_position$total_cost))

  cat("\n")

  # Check if position quantity matches activities
  total_shares_bought <- activities %>%
    filter(type == "Trades", action == "Buy", symbol == "CVE") %>%
    summarise(total = sum(abs(quantity), na.rm = TRUE)) %>%
    pull(total)
  total_shares_bought <- if (length(total_shares_bought) == 0) 0 else total_shares_bought

  total_shares_sold <- activities %>%
    filter(type == "Trades", action == "Sell", symbol == "CVE") %>%
    summarise(total = sum(abs(quantity), na.rm = TRUE)) %>%
    pull(total)
  total_shares_sold <- if (length(total_shares_sold) == 0) 0 else total_shares_sold

  expected_shares <- total_shares_bought - total_shares_sold

  cat("SHARE QUANTITY CHECK:\n")
  cat(sprintf("  Shares Bought (from activities):   %.0f\n", total_shares_bought))
  cat(sprintf("  Shares Sold (from activities):     %.0f\n", total_shares_sold))
  cat(sprintf("  Expected Current Shares:           %.0f\n", expected_shares))
  cat(sprintf("  Actual Current Shares (position):  %.0f\n", latest_cve_position$open_quantity))
  cat(sprintf("  DISCREPANCY:                       %.0f shares\n",
              expected_shares - latest_cve_position$open_quantity))
}

cat("\n=================================================================\n")
cat("END OF INVESTIGATION\n")
cat("=================================================================\n")

# Close database connection
dbDisconnect(conn)
