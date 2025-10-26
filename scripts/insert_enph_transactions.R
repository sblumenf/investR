#!/usr/bin/env Rscript
#' Manual Activity Insertion Script - ENPH Transactions
#'
#' Inserting historical ENPH transactions from August 2025
#' that fall outside the Questrade API 30-day lookup window.
#'
#' USAGE:
#'   Rscript scripts/insert_enph_transactions.R --dry-run    # Preview only
#'   Rscript scripts/insert_enph_transactions.R --execute    # Actually insert
#'

library(DBI)
library(duckdb)
library(dplyr)
library(lubridate)
library(logger)

# Source required functions
source("R/utils_portfolio_config.R")
source("R/fct_portfolio_database.R")
source("R/fct_activities_database.R")

# For standalone scripts, define simplified db path function
get_portfolio_db_connection <- function() {
  db_path <- "inst/database/portfolio.duckdb"
  if (!file.exists(db_path)) {
    stop("Database not found at: ", db_path)
  }
  conn <- dbConnect(duckdb(), dbdir = db_path, read_only = FALSE)
  return(conn)
}

################################################################################
# CONFIGURATION
################################################################################

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
DRY_RUN <- if (length(args) == 0 || "--dry-run" %in% args) TRUE else FALSE

if (DRY_RUN) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("                    🔍 DRY RUN MODE - NO WRITES                     \n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("\n")
} else {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("                   ⚠️  EXECUTE MODE - WRITING TO DB                 \n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("\n")
}

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Generate activity ID matching the format used by fetch_all_activities()
generate_activity_id <- function(account_number, symbol, transaction_date) {
  symbol_clean <- if (is.na(symbol) || symbol == "") {
    "NOSYMBOL"
  } else {
    gsub("[^A-Za-z0-9]", "", symbol)
  }

  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  random_suffix <- sample(1000:9999, 1)

  sprintf("ACT_%s_%s_%s_%s", account_number, symbol_clean, timestamp, random_suffix)
}

#' Parse date from DD/MM/YYYY format to POSIXct
parse_transaction_date <- function(date_str) {
  # Handle DD/MM/YYYY format (European/Questrade format)
  parts <- strsplit(date_str, "/")[[1]]
  day <- parts[1]
  month <- parts[2]
  year <- parts[3]

  date_str_iso <- sprintf("%s-%s-%s", year, month, day)
  as.POSIXct(paste(date_str_iso, "00:00:00"), tz = "UTC")
}

#' Check if transaction already exists in database
check_duplicate <- function(conn, account_number, description, trade_date, action, quantity, net_amount) {
  result <- dbGetQuery(conn, "
    SELECT COUNT(*) as count
    FROM account_activities
    WHERE account_number = ?
      AND description = ?
      AND trade_date = ?
      AND action = ?
      AND quantity = ?
      AND net_amount = ?
  ", params = list(account_number, description, trade_date, action, quantity, net_amount))

  return(result$count > 0)
}

#' Preview a transaction (dry run display)
preview_transaction <- function(tx, idx) {
  cat(sprintf("\n────────────────────── Transaction %d ──────────────────────\n", idx))
  cat(sprintf("Activity ID:     %s\n", tx$activity_id))
  cat(sprintf("Account:         %s (%s)\n", tx$account_number, tx$account_type))
  cat(sprintf("Trade Date:      %s\n", tx$trade_date))
  cat(sprintf("Settlement:      %s\n", tx$settlement_date))
  cat(sprintf("Action:          %s\n", tx$action))
  cat(sprintf("Symbol:          %s\n", tx$symbol))
  cat(sprintf("Quantity:        %s\n", format(tx$quantity, big.mark = ",")))
  cat(sprintf("Price:           $%.2f\n", tx$price))
  cat(sprintf("Gross Amount:    $%s\n", format(tx$gross_amount, big.mark = ",")))
  cat(sprintf("Commission:      $%.2f\n", tx$commission))
  cat(sprintf("Net Amount:      $%s\n", format(tx$net_amount, big.mark = ",")))
  cat(sprintf("Type:            %s\n", tx$type))
  cat(sprintf("Description:     %s\n", tx$description))
  cat("────────────────────────────────────────────────────────────\n")
}

################################################################################
# TRANSACTION DEFINITIONS
################################################################################

#' ENPH Transactions from August 2025
#'
#' Transaction 1: Bought 100 shares of ENPH at $38.04
#' Transaction 2: Sold ENPH Nov 21 2025 $30 Call (1 contract)
#' Transaction 3: Bought 300 shares of ENPH at $37.71
#' Transaction 4: Sold ENPH Oct 17 2025 $30 Call (3 contracts)
#'
#' These form a covered call position (400 total shares with 4 calls sold).

transactions <- list(
  # Transaction 1: ENPH Stock Purchase (100 shares)
  list(
    account_number = "53238334",
    account_type = "LIRA",  # Adjust if needed
    trans_date = "25/08/2025",
    settlement_date = "26/08/2025",
    action = "Buy",
    symbol = "ENPH",
    description = "ENPHASE ENERGY INC WE ACTED AS AGENT",
    quantity = 100.0,  # Positive for buy
    price = 38.04,
    gross = -3804.00,
    commission = 0.00,
    net = -3804.00,
    currency = "USD",
    type = "Trades"
  ),

  # Transaction 2: ENPH Call Option Sale (Nov 21)
  list(
    account_number = "53238334",
    account_type = "LIRA",  # Adjust if needed
    trans_date = "25/08/2025",
    settlement_date = "26/08/2025",
    action = "Sell",
    symbol = "ENPH21Nov25C30.00",
    description = "CALL ENPH 11/21/25 30 ENPHASE ENERGY INC WE ACTED AS AGENT",
    quantity = -1.0,  # Negative for sell
    price = 10.20,
    gross = 1020.00,
    commission = -0.99,
    net = 1019.01,
    currency = "USD",
    type = "Trades"
  ),

  # Transaction 3: ENPH Stock Purchase (300 shares)
  list(
    account_number = "53238334",
    account_type = "LIRA",  # Adjust if needed
    trans_date = "27/08/2025",
    settlement_date = "28/08/2025",
    action = "Buy",
    symbol = "ENPH",
    description = "ENPHASE ENERGY INC WE ACTED AS AGENT AVG PRICE - ASK US FOR DETAILS",
    quantity = 300.0,  # Positive for buy
    price = 37.71,
    gross = -11313.00,
    commission = 0.00,
    net = -11313.00,
    currency = "USD",
    type = "Trades"
  ),

  # Transaction 4: ENPH Call Option Sale (Oct 17 expiry - 3 contracts)
  list(
    account_number = "53238334",
    account_type = "LIRA",  # Adjust if needed
    trans_date = "27/08/2025",
    settlement_date = "28/08/2025",
    action = "Sell",
    symbol = "ENPH17Oct25C30.00",  # Converted from broker code 9NWZZH7
    description = "CALL ENPH 10/17/25 30 ENPHASE ENERGY INC WE ACTED AS AGENT AVG PRICE - ASK US FOR DETAILS",
    quantity = -3.0,  # Negative for sell
    price = 8.71,
    gross = 2613.00,
    commission = -2.97,
    net = 2610.03,
    currency = "USD",
    type = "Trades"
  )
)

################################################################################
# MAIN PROCESSING
################################################################################

cat(sprintf("Processing %d ENPH transactions...\n\n", length(transactions)))

# Connect to database
conn <- get_portfolio_db_connection()
on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

# Ensure schema exists
initialize_activities_schema(conn)

# Process each transaction
processed_transactions <- list()
duplicate_count <- 0
insert_count <- 0

for (i in seq_along(transactions)) {
  tx <- transactions[[i]]

  # Parse dates
  trade_date <- parse_transaction_date(tx$trans_date)
  trans_date <- parse_transaction_date(tx$trans_date)
  settle_date <- parse_transaction_date(tx$settlement_date)

  # Symbol is already in correct format
  symbol_final <- tx$symbol

  # Generate activity ID
  activity_id <- generate_activity_id(tx$account_number, symbol_final, trans_date)

  # Check for duplicate
  is_duplicate <- check_duplicate(
    conn,
    tx$account_number,
    tx$description,
    trade_date,
    tx$action,
    tx$quantity,
    tx$net
  )

  # Build transaction record
  tx_record <- list(
    activity_id = activity_id,
    account_number = tx$account_number,
    account_type = tx$account_type,
    trade_date = trade_date,
    transaction_date = trans_date,
    settlement_date = settle_date,
    action = tx$action,
    symbol = symbol_final,
    symbol_id = NA_integer_,
    description = tx$description,
    currency = tx$currency,
    quantity = tx$quantity,
    price = tx$price,
    gross_amount = tx$gross,
    commission = tx$commission,
    net_amount = tx$net,
    type = tx$type,
    group_id = NA_character_,
    is_processed = TRUE,  # Mark as processed since these are historical
    ignore_for_grouping = FALSE,
    fetched_at = Sys.time()
  )

  # Display transaction
  if (is_duplicate) {
    cat(sprintf("⚠️  DUPLICATE DETECTED - Transaction %d will be SKIPPED\n", i))
    duplicate_count <- duplicate_count + 1
  } else {
    cat(sprintf("✅ NEW TRANSACTION - Transaction %d will be INSERTED\n", i))
    insert_count <- insert_count + 1
  }

  preview_transaction(tx_record, i)

  # Store for insertion (if not duplicate)
  if (!is_duplicate) {
    processed_transactions[[length(processed_transactions) + 1]] <- tx_record
  }
}

################################################################################
# SUMMARY & EXECUTION
################################################################################

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("                            SUMMARY                                 \n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("Total Transactions:    %d\n", length(transactions)))
cat(sprintf("New to Insert:         %d\n", insert_count))
cat(sprintf("Duplicates Skipped:    %d\n", duplicate_count))
cat("═══════════════════════════════════════════════════════════════════\n")
cat("\n")

if (DRY_RUN) {
  cat("🔍 DRY RUN COMPLETE - No changes made to database\n")
  cat("\n")
  cat("To execute these insertions, run:\n")
  cat("  Rscript scripts/insert_enph_transactions.R --execute\n")
  cat("\n")
} else {
  if (insert_count == 0) {
    cat("✅ No new transactions to insert (all were duplicates)\n\n")
  } else {
    cat(sprintf("⚠️  EXECUTING: Inserting %d transactions...\n\n", insert_count))

    # Convert to data frame and insert
    df <- bind_rows(processed_transactions)

    tryCatch({
      dbWriteTable(conn, "account_activities", df, append = TRUE)
      cat(sprintf("✅ SUCCESS: Inserted %d transactions into database\n\n", insert_count))

      # Display inserted activity IDs for verification
      cat("Inserted Activity IDs:\n")
      for (tx in processed_transactions) {
        cat(sprintf("  - %s (%s %s)\n", tx$activity_id, tx$action, tx$symbol))
      }
      cat("\n")

    }, error = function(e) {
      cat(sprintf("❌ ERROR: Failed to insert transactions\n"))
      cat(sprintf("Error message: %s\n\n", e$message))
    })
  }
}

cat("═══════════════════════════════════════════════════════════════════\n")
cat("                          SCRIPT COMPLETE                           \n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("\n")
