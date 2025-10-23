#!/usr/bin/env Rscript
#
# Test Framework for Duplicate Detection Logic
#
# Purpose: Verify that duplicate detection correctly identifies which transactions
#          to keep vs delete, using fake test data.
#
# Process:
#   1. Insert fake duplicate transactions (safe test data)
#   2. Run duplicate detection logic
#   3. Verify correct keep/delete decisions
#   4. Clean up test data
#

library(DBI)
library(duckdb)
library(dplyr)
library(logger)

log_threshold(INFO)

log_info("=== DUPLICATE DETECTION TEST FRAMEWORK ===")

# Connect to database
devtools::load_all()
conn <- get_portfolio_db_connection()
on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

# Test data constants
TEST_ACCOUNT <- "99999999"  # Fake account number that won't conflict with real data
TEST_GROUP_ID <- "TEST_GROUP_DEDUP_12345"
TEST_DATE <- as.POSIXct("2025-01-01 10:00:00")
TEST_DESCRIPTION <- "TEST CALL XYZ 01/15/25 50 TEST COMPANY INC WE ACTED AS AGENT"

log_info("Creating test duplicate set...")

# Test Case 1: Linked transaction with blank symbol (should be KEPT)
test_tx_1 <- tibble(
  activity_id = "TEST_ACT_1_BLANK_LINKED",
  account_number = TEST_ACCOUNT,
  account_type = "TEST",
  trade_date = TEST_DATE,
  transaction_date = TEST_DATE,
  settlement_date = TEST_DATE + (2 * 24 * 60 * 60),
  action = "Sell",
  symbol = "",  # BLANK
  symbol_id = 0,
  description = TEST_DESCRIPTION,
  currency = "USD",
  quantity = -2,
  price = 0.50,
  gross_amount = 100.00,
  commission = 0,
  net_amount = 100.00,
  type = "Trades",
  group_id = TEST_GROUP_ID,  # LINKED
  is_processed = FALSE,
  fetched_at = Sys.time() - (2 * 60 * 60)  # 2 hours ago
)

# Test Case 2: Unlinked transaction with populated symbol (should be DELETED)
test_tx_2 <- tibble(
  activity_id = "TEST_ACT_2_POPULATED_UNLINKED",
  account_number = TEST_ACCOUNT,
  account_type = "TEST",
  trade_date = TEST_DATE,
  transaction_date = TEST_DATE,
  settlement_date = TEST_DATE + (2 * 24 * 60 * 60),
  action = "Sell",
  symbol = "XYZ15Jan25C50.00",  # POPULATED
  symbol_id = 12345678,
  description = TEST_DESCRIPTION,
  currency = "USD",
  quantity = -2,
  price = 0.50,
  gross_amount = 100.00,
  commission = 0,
  net_amount = 100.00,
  type = "Trades",
  group_id = NA_character_,  # UNLINKED
  is_processed = FALSE,
  fetched_at = Sys.time() - (1 * 60 * 60)  # 1 hour ago
)

# Insert test transactions
log_info("Inserting test transactions...")
tryCatch({
  dbWriteTable(conn, "account_activities", test_tx_1, append = TRUE)
  dbWriteTable(conn, "account_activities", test_tx_2, append = TRUE)
  log_info("✅ Test transactions inserted successfully")
}, error = function(e) {
  log_error("Failed to insert test data: {e$message}")
  stop("Test setup failed")
})

# Verify insertion
test_count <- dbGetQuery(conn, "
  SELECT COUNT(*) as count
  FROM account_activities
  WHERE account_number = ?
", params = list(TEST_ACCOUNT))

log_info("Test transactions in database: {test_count$count}")

# Run duplicate detection logic (same as dry-run script)
log_info("")
log_info("Running duplicate detection logic on test data...")

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
  WHERE account_number = ?
", params = list(TEST_ACCOUNT)) %>% as_tibble()

duplicate_sets <- all_activities %>%
  group_by(account_number, description, trade_date, action, quantity, net_amount) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(duplicate_sets) == 0) {
  log_error("❌ TEST FAILED: No duplicates detected!")
  stop("Duplicate detection logic failed")
}

log_info("✅ Duplicates detected: {nrow(duplicate_sets)} transactions")

# Analyze duplicates
duplicate_analysis <- duplicate_sets %>%
  group_by(account_number, description, trade_date, action, quantity, net_amount) %>%
  mutate(
    has_group_id = !is.na(group_id) & group_id != "",
    has_symbol = !is.na(symbol) & symbol != "" & symbol != "NOSYMBOL",
    has_valid_symbol_id = !is.na(symbol_id) & symbol_id > 0,
    priority_score = (has_group_id * 1000) + (has_symbol * 100) + (has_valid_symbol_id * 10) + row_number(),
    action_to_take = if_else(
      priority_score == max(priority_score),
      "KEEP",
      "DELETE"
    )
  ) %>%
  ungroup()

# Verify results
log_info("")
log_info("Verifying test results...")

kept_tx <- duplicate_analysis %>% filter(action_to_take == "KEEP")
deleted_tx <- duplicate_analysis %>% filter(action_to_take == "DELETE")

# Expected: Keep the linked one (test_tx_1), delete the unlinked one (test_tx_2)
if (nrow(kept_tx) == 1 && kept_tx$activity_id[1] == "TEST_ACT_1_BLANK_LINKED") {
  log_info("✅ CORRECT: Keeping linked transaction with blank symbol")
} else {
  log_error("❌ FAILED: Wrong transaction marked to keep!")
  log_error("Expected: TEST_ACT_1_BLANK_LINKED")
  log_error("Got: {kept_tx$activity_id[1]}")
}

if (nrow(deleted_tx) == 1 && deleted_tx$activity_id[1] == "TEST_ACT_2_POPULATED_UNLINKED") {
  log_info("✅ CORRECT: Deleting unlinked duplicate with populated symbol")
} else {
  log_error("❌ FAILED: Wrong transaction marked to delete!")
  log_error("Expected: TEST_ACT_2_POPULATED_UNLINKED")
  log_error("Got: {deleted_tx$activity_id[1]}")
}

# Clean up test data
log_info("")
log_info("Cleaning up test data...")
dbExecute(conn, "
  DELETE FROM account_activities
  WHERE account_number = ?
", params = list(TEST_ACCOUNT))

verify_cleanup <- dbGetQuery(conn, "
  SELECT COUNT(*) as count
  FROM account_activities
  WHERE account_number = ?
", params = list(TEST_ACCOUNT))

if (verify_cleanup$count == 0) {
  log_info("✅ Test data cleaned up successfully")
} else {
  log_warn("⚠️ Warning: {verify_cleanup$count} test transactions remain in database")
}

log_info("")
log_info("=== TEST RESULTS ===")
if (nrow(kept_tx) == 1 && kept_tx$activity_id[1] == "TEST_ACT_1_BLANK_LINKED" &&
    nrow(deleted_tx) == 1 && deleted_tx$activity_id[1] == "TEST_ACT_2_POPULATED_UNLINKED" &&
    verify_cleanup$count == 0) {
  log_info("✅ ALL TESTS PASSED")
  log_info("Duplicate detection logic is working correctly!")
  log_info("")
  log_info("Safe to proceed with real duplicate cleanup.")
} else {
  log_error("❌ TESTS FAILED")
  log_error("Review the errors above before proceeding.")
  quit(status = 1)
}
