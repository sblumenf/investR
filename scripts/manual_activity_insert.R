#!/usr/bin/env Rscript
#' Manual Activity Insertion Script
#'
#' Bootstrap script for manually inserting historical transactions that fall
#' outside the Questrade API 30-day lookup window. This script mimics the
#' format and structure of API-fetched activities.
#'
#' USAGE:
#'   Rscript scripts/manual_activity_insert.R --dry-run    # Preview only
#'   Rscript scripts/manual_activity_insert.R --execute    # Actually insert
#'
#' IMPORTANT: This script is only needed during initial database seeding.
#' Once all historical transactions are loaded, regular API fetching will maintain the database.

library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(logger)

# Source required functions
source("R/utils_portfolio_config.R")
source("R/fct_portfolio_database.R")
source("R/fct_activities_database.R")

# For standalone scripts, define simplified db path function
get_portfolio_db_connection <- function() {
  db_path <- "inst/database/portfolio.sqlite"
  if (!file.exists(db_path)) {
    stop("Database not found at: ", db_path)
  }
  conn <- dbConnect(RSQLite::SQLite(), db_path)
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

#' Convert broker internal option codes to standard format
#' Example: 9LTFSC4 + description "CALL WBD 10/17/25 9" -> WBD17Oct25C9.00
parse_option_symbol <- function(internal_code, description) {
  # Try to extract: underlying, date, strike from description
  # Pattern: "CALL UNDERLYING MM/DD/YY STRIKE ..."

  pattern <- "CALL\\s+([A-Z]+)\\s+(\\d{1,2})/(\\d{1,2})/(\\d{2})\\s+(\\d+(?:\\.\\d+)?)"
  match <- regexec(pattern, description)

  if (match[[1]][1] == -1) {
    # Fallback: use internal code if parsing fails
    return(internal_code)
  }

  matches <- regmatches(description, match)[[1]]
  underlying <- matches[2]
  month <- sprintf("%02d", as.integer(matches[3]))
  day <- sprintf("%02d", as.integer(matches[4]))
  year <- paste0("20", matches[5])  # Assume 20xx
  strike <- as.numeric(matches[6])

  # Convert to month abbreviation
  month_abbr <- month.abb[as.integer(month)]

  # Format: SYMBOL17Oct25C9.00
  sprintf("%s%s%s%sC%.2f", underlying, day, month_abbr, substring(year, 3), strike)
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

#' Define transactions to insert
#'
#' Add your transactions here in the same format. Each transaction should match
#' the broker statement exactly.
#'
#' DATES: Use DD/MM/YYYY format (as shown on Questrade statements)
#' QUANTITIES: Use positive for buys/receiving, negative for sells
#' AMOUNTS: Use positive for credits, negative for debits
#' SYMBOLS: For options, use broker internal code - script will convert to standard format

transactions <- list(
  # Transaction 1: WMT Stock Purchase
  list(
    account_number = "53238334",
    account_type = "TFSA",  # Adjust if different
    trans_date = "13/08/2025",
    settlement_date = "14/08/2025",
    action = "Buy",
    symbol = "WMT",
    description = "WALMART INC COMMON STOCK WE ACTED AS AGENT",
    quantity = 200.0,
    price = 101.73,
    gross = -20346.00,
    commission = 0.00,
    net = -20346.00,
    currency = "USD",
    type = "Trades"
  ),

  # Transaction 2: WMT Call Option Sale
  list(
    account_number = "53238334",
    account_type = "TFSA",  # Adjust if different
    trans_date = "13/08/2025",
    settlement_date = "14/08/2025",
    action = "Sell",
    symbol = "WMT17Jun27C80.00",
    description = "CALL WMT 06/17/27 80 WALMART INC WE ACTED AS AGENT",
    quantity = -2.0,
    price = 29.02,
    gross = 5804.00,
    commission = -1.98,
    net = 5802.02,
    currency = "USD",
    type = "Trades"
  )
)

################################################################################
# MAIN PROCESSING
################################################################################

cat(sprintf("Processing %d transactions...\n\n", length(transactions)))

# Connect to database
conn <- get_portfolio_db_connection()
on.exit(dbDisconnect(conn), add = TRUE)

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

  # Parse symbol (convert option codes to standard format)
  symbol_final <- if (grepl("^[0-9]", tx$symbol) || nchar(tx$symbol) == 7) {
    # Looks like an internal option code
    parse_option_symbol(tx$symbol, tx$description)
  } else {
    # Regular stock symbol
    tx$symbol
  }

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
  cat("  Rscript scripts/manual_activity_insert.R --execute\n")
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
