#!/usr/bin/env Rscript
#
# Bootstrap Activities for Position Groups - One-Time Setup Script
#
# Purpose: Create synthetic activities from position data for groups that were
#          set up before Questrade activity download integration. This allows
#          the metrics calculation system to work properly.
#
# What this does:
# - For each group without activities, creates stock buy and option sell activities
# - Uses actual position data (quantities, prices, costs) from positions_history
# - Marks all activities with "BOOTSTRAP_MANUAL_SETUP" for traceability
# - Links activities to correct group_id
#
# This is a ONE-TIME setup script. Future groups will have activities from Questrade API.
#

library(DBI)
library(dplyr)
library(logger)

# Load the investR package
devtools::load_all()

log_info("=== STARTING BOOTSTRAP ACTIVITIES SETUP ===")

# Connect to database
conn <- get_portfolio_db_connection()

# Define the 17 groups that need bootstrap activities
groups_to_bootstrap <- c(
  # Dividend Aristocrats
  "DIVIDEND_ARISTOCRATS_53238334_20251008230605_4905",  # ALB
  "DIVIDEND_ARISTOCRATS_53238334_20251008230627_2524",  # BEN
  "DIVIDEND_ARISTOCRATS_53238334_20251008230658_2599",  # CAG
  "DIVIDEND_ARISTOCRATS_53238334_20251008231605_9313",  # CLX
  "DIVIDEND_ARISTOCRATS_53238334_20251008231649_3933",  # CVX
  "DIVIDEND_ARISTOCRATS_53238334_20251008231736_7745",  # IBM
  "DIVIDEND_ARISTOCRATS_53238334_20251008231756_9478",  # LOW
  "DIVIDEND_ARISTOCRATS_53238334_20251008231818_2643",  # MMM
  "DIVIDEND_ARISTOCRATS_53238334_20251008231835_3587",  # NUE
  "DIVIDEND_ARISTOCRATS_53238334_20251008231914_1112",  # TGT
  "DIVIDEND_ARISTOCRATS_53238334_20251008231955_8190",  # WMT
  # Zero-Dividend Stocks
  "ZERODIVIDEND_STOCKS_53238334_20251008231631_4718",   # CNC
  "ZERODIVIDEND_STOCKS_53238334_20251008231714_8711",   # CZR
  "ZERODIVIDEND_STOCKS_53238334_20251008231935_7563",   # WBD
  "ZERODIVIDEND_STOCKS_53238334_20251008233014_6411",   # SMCI
  "ZERODIVIDEND_STOCKS_53238334_20251009205730_9841",   # ENPH Oct
  "ZERODIVIDEND_STOCKS_53238334_20251009205732_3744"    # ENPH Nov
)

# Function to create bootstrap activities for a group
create_bootstrap_activities <- function(group_id, conn) {
  log_info("Processing group: {group_id}")

  # Check if bootstrap activities already exist for this group
  existing_bootstrap <- dbGetQuery(conn, "
    SELECT COUNT(*) as count
    FROM account_activities
    WHERE group_id = ?
      AND description LIKE '%BOOTSTRAP_MANUAL_SETUP%'
  ", params = list(group_id))

  if (existing_bootstrap$count[1] > 0) {
    log_info("  SKIPPED: Group already has {existing_bootstrap$count[1]} bootstrap activities")
    return(TRUE)  # Return TRUE as this is not an error, just already processed
  }

  # Get group info
  group <- dbGetQuery(conn, "
    SELECT group_id, group_name, account_number, created_at
    FROM position_groups
    WHERE group_id = ?
  ", params = list(group_id))

  if (nrow(group) == 0) {
    log_error("Group {group_id} not found!")
    return(FALSE)
  }

  group_name <- group$group_name[1]
  account_number <- group$account_number[1]
  created_at <- as.POSIXct(group$created_at[1])

  # Use group creation date as trade date
  trade_date <- as.Date(created_at)
  settlement_date <- trade_date + 2  # T+2 settlement

  log_info("  Group: {group_name}, Account: {account_number}, Trade Date: {trade_date}")

  # Get members
  members <- dbGetQuery(conn, "
    SELECT symbol, role
    FROM position_group_members
    WHERE group_id = ?
  ", params = list(group_id))

  if (nrow(members) == 0) {
    log_error("  No members found for group {group_id}")
    return(FALSE)
  }

  activities_created <- 0

  # Process each member
  for (i in 1:nrow(members)) {
    symbol <- members$symbol[i]
    role <- members$role[i]

    # Get latest position data
    pos <- dbGetQuery(conn, "
      SELECT symbol, open_quantity, average_entry_price, total_cost
      FROM positions_history
      WHERE symbol = ?
      ORDER BY snapshot_timestamp DESC
      LIMIT 1
    ", params = list(symbol))

    if (nrow(pos) == 0) {
      log_warn("  No position data found for {symbol}, skipping")
      next
    }

    quantity <- pos$open_quantity[1]
    price <- pos$average_entry_price[1]
    total_cost <- pos$total_cost[1]

    # Determine action and amounts based on role
    if (role == "underlying_stock") {
      action <- "Buy"
      # For stock purchase: quantity is positive, amounts are negative (cash out)
      gross_amount <- -abs(total_cost)
      net_amount <- gross_amount
      description <- sprintf("%s - BOOTSTRAP_MANUAL_SETUP (stock purchase)", symbol)

    } else if (role == "short_call") {
      action <- "Sell"
      # For option sale: quantity is absolute value, amounts are positive (cash in)
      quantity <- abs(quantity) * 100  # Convert contracts to shares
      gross_amount <- abs(total_cost)
      net_amount <- gross_amount
      description <- sprintf("%s - BOOTSTRAP_MANUAL_SETUP (option sale)", symbol)

    } else {
      log_warn("  Unknown role {role} for {symbol}, skipping")
      next
    }

    # Generate unique activity ID
    timestamp_str <- format(Sys.time(), "%Y%m%d%H%M%S")
    random_suffix <- sample(1000:9999, 1)
    activity_id <- sprintf("ACT_%s_%s_BOOTSTRAP_%s_%d",
                          account_number, symbol, timestamp_str, random_suffix)

    # Insert activity
    tryCatch({
      dbExecute(conn, "
        INSERT INTO account_activities (
          activity_id, account_number, account_type, trade_date,
          transaction_date, settlement_date, action, symbol, description,
          currency, quantity, price, gross_amount, commission, net_amount,
          type, group_id, is_processed, fetched_at, ignore_for_grouping
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(
        activity_id,
        account_number,
        "LIRA",  # Default account type
        as.character(trade_date),
        as.character(trade_date),
        as.character(settlement_date),
        action,
        symbol,
        description,
        "USD",
        quantity,
        price,
        gross_amount,
        0,  # commission
        net_amount,
        "Trades",
        group_id,
        TRUE,  # is_processed
        as.character(Sys.time()),
        FALSE  # ignore_for_grouping
      ))

      log_info("    Created {action} activity for {symbol}: qty={quantity}, price=${price}, total=${total_cost}")
      activities_created <- activities_created + 1

    }, error = function(e) {
      log_error("    Failed to create activity for {symbol}: {e$message}")
    })
  }

  log_info("  Completed: {activities_created} activities created for {group_name}")
  return(TRUE)
}

# Process all groups
log_info("\nProcessing {length(groups_to_bootstrap)} groups...")

success_count <- 0
fail_count <- 0

for (group_id in groups_to_bootstrap) {
  result <- create_bootstrap_activities(group_id, conn)
  if (result) {
    success_count <- success_count + 1
  } else {
    fail_count <- fail_count + 1
  }
  cat("\n")
}

# Disconnect
dbDisconnect(conn, shutdown = TRUE)

# Summary
log_info("=== BOOTSTRAP COMPLETE ===")
log_info("Successfully processed: {success_count} groups")
log_info("Failed: {fail_count} groups")
log_info("\nAll activities are marked with 'BOOTSTRAP_MANUAL_SETUP' for traceability.")
log_info("The metrics calculation system should now work for all groups.")
