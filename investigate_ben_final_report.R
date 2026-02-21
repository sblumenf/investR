#!/usr/bin/env Rscript
# Final report on BEN group capital deployed investigation

library(DBI)
library(dplyr)
library(RSQLite)

db_path <- "inst/database/portfolio.sqlite"
conn <- dbConnect(RSQLite::SQLite(), db_path)

group_id <- "OTHER_53238853_20251008235947_2223"

cat("=================================================================\n")
cat("FINAL INVESTIGATION REPORT: BEN Group Capital Deployed\n")
cat("=================================================================\n\n")

# Get group info
group_info <- dbGetQuery(conn, "
  SELECT *
  FROM position_groups
  WHERE group_id = ?
", params = list(group_id)) %>% as_tibble()

group_account <- group_info$account_number[1]
strategy_type <- group_info$strategy_type[1]

cat("GROUP INFORMATION:\n")
cat("  Group ID:", group_id, "\n")
cat("  Group Name:", group_info$group_name[1], "\n")
cat("  Account:", group_account, "\n")
cat("  Strategy Type:", strategy_type, "\n\n")

# Get activities for this group
activities <- dbGetQuery(conn, "
  SELECT *
  FROM account_activities
  WHERE group_id = ?
  ORDER BY trade_date
", params = list(group_id)) %>% as_tibble()

cat("ACTIVITIES LINKED TO GROUP:\n")
cat("  Total activities:", nrow(activities), "\n\n")

if (nrow(activities) > 0) {
  # Stock purchases
  stock_buys <- activities %>%
    filter(type == "Trades", action == "Buy", !grepl("\\d{2}[A-Z][a-z]{2}\\d{2}[CP]", symbol))

  if (nrow(stock_buys) > 0) {
    cat("  Stock Purchases:\n")
    for (i in seq_len(nrow(stock_buys))) {
      cat(sprintf("    - %s: %d shares @ $%.2f = $%.2f\n",
                  as.character(stock_buys$trade_date[i]),
                  stock_buys$quantity[i],
                  stock_buys$price[i],
                  abs(stock_buys$gross_amount[i])))
    }
    total_stock_purchases <- sum(abs(stock_buys$gross_amount), na.rm = TRUE)
    cat(sprintf("    TOTAL STOCK PURCHASES: $%.2f\n", total_stock_purchases))
  }

  # Commissions
  total_commissions <- sum(abs(activities$commission), na.rm = TRUE)
  cat(sprintf("    TOTAL COMMISSIONS: $%.2f\n", total_commissions))

  # Option sales
  option_sales <- activities %>%
    filter(type == "Trades", action == "Sell", grepl("\\d{2}[A-Z][a-z]{2}\\d{2}[CP]", symbol))

  if (nrow(option_sales) > 0) {
    cat("\n  Option Sales (Premiums):\n")
    for (i in seq_len(nrow(option_sales))) {
      cat(sprintf("    - %s: %s, qty %d @ $%.2f = $%.2f (net: $%.2f)\n",
                  as.character(option_sales$trade_date[i]),
                  option_sales$symbol[i],
                  abs(option_sales$quantity[i]),
                  option_sales$price[i],
                  abs(option_sales$gross_amount[i]),
                  abs(option_sales$net_amount[i])))
    }
    total_option_premiums_gross <- sum(abs(option_sales$gross_amount), na.rm = TRUE)
    total_option_premiums_net <- sum(abs(option_sales$net_amount), na.rm = TRUE)
    cat(sprintf("    TOTAL OPTION PREMIUMS (gross): $%.2f\n", total_option_premiums_gross))
    cat(sprintf("    TOTAL OPTION PREMIUMS (net): $%.2f\n", total_option_premiums_net))
  }

  cat("\n  CALCULATED COST BASIS (by strategy type):\n")
  if (exists("total_stock_purchases") && exists("total_commissions")) {
    if (strategy_type != "Other") {
      # For covered call strategies: premiums reduce cost basis
      cost_basis <- total_stock_purchases + total_commissions - total_option_premiums_gross
      cat(sprintf("    Stock purchases: $%.2f\n", total_stock_purchases))
      cat(sprintf("    + Commissions: $%.2f\n", total_commissions))
      if (exists("total_option_premiums_gross")) {
        cat(sprintf("    - Option premiums: $%.2f\n", total_option_premiums_gross))
      }
      cat(sprintf("    = COST BASIS: $%.2f\n", cost_basis))
      cat("    (Covered call strategy: premiums reduce cost basis)\n")
    } else {
      # For "Other" strategy: premiums are income
      cost_basis <- total_stock_purchases + total_commissions
      cat(sprintf("    Stock purchases: $%.2f\n", total_stock_purchases))
      cat(sprintf("    + Commissions: $%.2f\n", total_commissions))
      cat(sprintf("    = COST BASIS: $%.2f\n", cost_basis))
      cat("    (Other strategy: premiums are cash collected, not cost reduction)\n")
    }
  }
}

cat("\n")

# Get current position
cat("CURRENT POSITION FROM POSITIONS_HISTORY:\n")
latest_snapshot <- dbGetQuery(conn, "
  SELECT MAX(snapshot_timestamp) as latest_time
  FROM positions_history
")$latest_time

current_position <- dbGetQuery(conn, "
  SELECT *
  FROM positions_history
  WHERE symbol = 'BEN'
    AND account_number = ?
    AND snapshot_timestamp = ?
", params = list(group_account, latest_snapshot)) %>% as_tibble()

if (nrow(current_position) > 0) {
  cat(sprintf("  Symbol: %s\n", current_position$symbol[1]))
  cat(sprintf("  Quantity: %d shares\n", current_position$open_quantity[1]))
  cat(sprintf("  Average Entry Price: $%.2f\n", current_position$average_entry_price[1]))
  cat(sprintf("  Total Cost: $%.2f\n", current_position$total_cost[1]))
  cat(sprintf("  Current Market Value: $%.2f\n", current_position$current_market_value[1]))
  cat(sprintf("  Open P&L: $%.2f\n", current_position$open_pnl[1]))
} else {
  cat("  No position found for BEN in account", group_account, "\n")
}

cat("\n")

# Comparison
cat("COMPARISON AND ANALYSIS:\n")

if (exists("cost_basis") && nrow(current_position) > 0) {
  position_cost <- current_position$total_cost[1]
  discrepancy <- abs(position_cost - cost_basis)

  cat(sprintf("  Cost basis from activities: $%.2f\n", cost_basis))
  cat(sprintf("  Cost from positions_history: $%.2f\n", position_cost))
  cat(sprintf("  Discrepancy: $%.2f\n", discrepancy))

  if (discrepancy < 1) {
    cat("\n  ✓ VALUES MATCH - Calculation is correct!\n")
  } else {
    cat("\n  ✗ DISCREPANCY DETECTED\n")
  }
}

cat("\n")

# Check for any missing activities
cat("CHECK FOR MISSING/UNLINKED ACTIVITIES:\n")

# All BEN buys for this account
all_buys_this_account <- dbGetQuery(conn, "
  SELECT *
  FROM account_activities
  WHERE symbol = 'BEN'
    AND account_number = ?
    AND type = 'Trades'
    AND action = 'Buy'
  ORDER BY trade_date
", params = list(group_account)) %>% as_tibble()

if (nrow(all_buys_this_account) > 0) {
  cat("  Total BEN buy activities for account", group_account, ":", nrow(all_buys_this_account), "\n")

  linked_to_this_group <- all_buys_this_account %>%
    filter(group_id == !!group_id)

  not_linked <- all_buys_this_account %>%
    filter(is.na(group_id) | group_id != !!group_id)

  cat("  Linked to this group:", nrow(linked_to_this_group), "\n")
  cat("  Not linked to this group:", nrow(not_linked), "\n")

  if (nrow(not_linked) > 0) {
    cat("\n  UNLINKED ACTIVITIES FOUND:\n")
    for (i in seq_len(nrow(not_linked))) {
      cat(sprintf("    - %s: %d shares @ $%.2f (group_id: %s)\n",
                  as.character(not_linked$trade_date[i]),
                  not_linked$quantity[i],
                  not_linked$price[i],
                  ifelse(is.na(not_linked$group_id[i]), "NONE", not_linked$group_id[i])))
    }
  }
} else {
  cat("  No BEN buy activities found for this account\n")
}

# Check position quantity vs activity quantity
cat("\n")
cat("QUANTITY RECONCILIATION:\n")
if (nrow(activities) > 0 && nrow(current_position) > 0) {
  buy_quantity <- sum(stock_buys$quantity, na.rm = TRUE)
  position_quantity <- current_position$open_quantity[1]

  cat(sprintf("  Shares bought (from activities): %d\n", buy_quantity))
  cat(sprintf("  Shares held (from position): %d\n", position_quantity))

  if (buy_quantity == position_quantity) {
    cat("  ✓ Quantities match!\n")
  } else {
    cat(sprintf("  ✗ Quantity mismatch: %d shares difference\n",
                abs(position_quantity - buy_quantity)))
  }
}

cat("\n=================================================================\n")
cat("CONCLUSION:\n")
cat("=================================================================\n\n")

if (exists("cost_basis") && nrow(current_position) > 0) {
  if (discrepancy < 1 && buy_quantity == position_quantity) {
    cat("STATUS: ✓ WORKING CORRECTLY\n\n")
    cat("The capital deployed calculation is accurate. The cost basis from\n")
    cat("activities matches the position cost in positions_history.\n\n")
    cat("For strategy type '", strategy_type, "':\n", sep = "")
    if (strategy_type != "Other") {
      cat("- Option premiums REDUCE the cost basis (covered call accounting)\n")
    } else {
      cat("- Option premiums are treated as CASH COLLECTED (income)\n")
      cat("- They do NOT reduce the cost basis\n")
    }
  } else {
    cat("STATUS: ✗ DISCREPANCY FOUND\n\n")

    if (buy_quantity != position_quantity) {
      cat("Issue: Quantity mismatch suggests missing BUY activities\n")
      cat("Solution: Need to bootstrap additional activities or check for\n")
      cat("          position transfers from another account\n\n")
    }

    if (discrepancy >= 1) {
      cat("Issue: Cost basis doesn't match position cost\n")
      cat("Possible causes:\n")
      cat("1. Missing buy activities not linked to this group\n")
      cat("2. Historical position existed before activity tracking\n")
      cat("3. Position transferred from another account\n")
      cat("4. Bootstrap data needs to be regenerated\n\n")

      cat("Recommended action:\n")
      cat("- Review bootstrap_position_activities.R script\n")
      cat("- Check if synthetic activities match actual position data\n")
      cat("- Consider running bootstrap for this specific position\n")
    }
  }
}

dbDisconnect(conn)
