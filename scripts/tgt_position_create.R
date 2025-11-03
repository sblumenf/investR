#!/usr/bin/env Rscript
#
# TGT Position Manual Creation - EXECUTION SCRIPT
#
# Creates a consolidated Dividend Aristocrats position for TGT
# by creating synthetic activities and marking originals as ignored.
#

library(dplyr)
library(tibble)
library(DBI)
library(duckdb)

cat("========================================\n")
cat("TGT POSITION CREATION - EXECUTION\n")
cat("========================================\n\n")

# Connect to database (read-write mode)
conn <- dbConnect(duckdb::duckdb(), 'inst/database/portfolio.duckdb')

# Wrap everything in a transaction for safety
tryCatch({
  dbBegin(conn)

  # =============================================================================
  # STEP 1: Create Position Group
  # =============================================================================
  cat("STEP 1: Creating position group...\n")

  account_number <- "53238334"
  group_id <- "DIVIDEND_ARISTOCRATS_TGT_MANUAL_20250820"
  group_name <- "Dividend Aristocrats - TGT - Jan 2027 @ $75"
  strategy_type <- "Dividend Aristocrats"
  timestamp <- Sys.time()

  dbExecute(conn, "
    INSERT INTO position_groups (
      group_id, group_name, strategy_type, status, account_number, created_at, updated_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    group_id,
    group_name,
    strategy_type,
    "open",
    account_number,
    timestamp,
    timestamp
  ))

  cat("  ✓ Position group created: ", group_id, "\n\n")

  # =============================================================================
  # STEP 2: Create Position Group Members
  # =============================================================================
  cat("STEP 2: Creating position group members...\n")

  dbExecute(conn, "
    INSERT INTO position_group_members (group_id, symbol, role)
    VALUES (?, ?, ?)
  ", params = list(group_id, "TGT", "underlying_stock"))

  dbExecute(conn, "
    INSERT INTO position_group_members (group_id, symbol, role)
    VALUES (?, ?, ?)
  ", params = list(group_id, "TGT15Jan27C75.00", "short_call"))

  cat("  ✓ Added 2 members: TGT (underlying_stock), TGT15Jan27C75.00 (short_call)\n\n")

  # =============================================================================
  # STEP 3: Create Synthetic Activities
  # =============================================================================
  cat("STEP 3: Creating synthetic activities...\n")

  # Activity 1: Stock purchase
  activity_id_1 <- "ACT_53238334_TGT_SYNTHETIC_STOCK_20250820"
  dbExecute(conn, "
    INSERT INTO account_activities (
      activity_id, account_number, transaction_date, trade_date, settlement_date,
      action, type, symbol, description, currency,
      quantity, price, gross_amount, commission, net_amount,
      group_id, is_processed, ignore_for_grouping, fetched_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    activity_id_1,
    account_number,
    "2025-08-20",
    "2025-08-20",
    "2025-08-22",  # T+2 settlement
    "Buy",
    "Trades",
    "TGT",
    "TARGET CORP - SYNTHETIC CONSOLIDATED POSITION (400 shares avg $101.18)",
    "USD",
    400,
    101.18,
    -40472.00,
    0.00,
    -40472.00,
    group_id,
    TRUE,
    FALSE,
    timestamp
  ))

  cat("  ✓ Created synthetic stock purchase: 400 shares @ $101.18\n")

  # Activity 2: Option sale
  activity_id_2 <- "ACT_53238334_TGT15Jan27C7500_SYNTHETIC_OPTION_20250820"
  dbExecute(conn, "
    INSERT INTO account_activities (
      activity_id, account_number, transaction_date, trade_date, settlement_date,
      action, type, symbol, description, currency,
      quantity, price, gross_amount, commission, net_amount,
      group_id, is_processed, ignore_for_grouping, fetched_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    activity_id_2,
    account_number,
    "2025-08-20",
    "2025-08-20",
    "2025-08-22",  # T+2 settlement
    "Sell",
    "Trades",
    "TGT15Jan27C75.00",
    "CALL TGT 01/15/27 75 - SYNTHETIC CONSOLIDATED POSITION (net of roll: $6,072 initial + $6,104 roll)",
    "USD",
    -4,
    30.44,
    12176.08,
    0.00,
    12176.08,
    group_id,
    TRUE,
    FALSE,
    timestamp
  ))

  cat("  ✓ Created synthetic option sale: 4 contracts @ $30.44 ($12,176.08 total)\n")

  # Activity 3: Dividend payment (already occurred)
  activity_id_3 <- "ACT_53238334_TGT_DIV_20250901"
  dbExecute(conn, "
    INSERT INTO account_activities (
      activity_id, account_number, transaction_date, trade_date, settlement_date,
      action, type, symbol, description, currency,
      quantity, price, gross_amount, commission, net_amount,
      group_id, is_processed, ignore_for_grouping, fetched_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    activity_id_3,
    account_number,
    "2025-09-01",
    "2025-09-01",
    "2025-09-01",
    NA_character_,
    "Dividends",
    "TGT",
    "TARGET CORP CASH DIV ON 200 SHS REC 08/13/25 PAY 09/01/25",
    "USD",
    200,
    1.14,
    228.00,
    0.00,
    228.00,
    group_id,
    TRUE,
    FALSE,
    timestamp
  ))

  cat("  ✓ Created dividend activity: $228.00 (200 shares @ $1.14)\n\n")

  # =============================================================================
  # STEP 4: Create Projected Cash Flows
  # =============================================================================
  cat("STEP 4: Creating projected cash flows...\n")

  # Actual dividend (already occurred)
  dbExecute(conn, "
    INSERT INTO position_group_cash_flows (
      event_id, group_id, event_date, event_type, amount, status, confidence,
      created_at, updated_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    "EVENT_TGT_DIV_ACTUAL_20250901",
    group_id,
    "2025-09-01",
    "dividend",
    228.00,
    "actual",
    "high",
    timestamp,
    timestamp
  ))

  cat("  ✓ Recorded actual dividend: $228.00 (Sep 2025)\n")

  # Projected dividends (5 payments)
  dividend_dates <- c("2025-12-01", "2026-03-01", "2026-06-01", "2026-09-01", "2026-12-01")
  dividend_amount <- 456.00

  for (i in seq_along(dividend_dates)) {
    event_id <- sprintf("EVENT_TGT_DIV_%s", gsub("-", "", dividend_dates[i]))
    dbExecute(conn, "
      INSERT INTO position_group_cash_flows (
        event_id, group_id, event_date, event_type, amount, status, confidence,
        created_at, updated_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      event_id,
      group_id,
      dividend_dates[i],
      "dividend",
      dividend_amount,
      "projected",
      "high",
      timestamp,
      timestamp
    ))
  }

  cat("  ✓ Created 5 projected dividends: $456.00 each\n")

  # Projected option gain
  dbExecute(conn, "
    INSERT INTO position_group_cash_flows (
      event_id, group_id, event_date, event_type, amount, status, confidence,
      created_at, updated_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    "EVENT_TGT_OPTIONGAIN_20270115",
    group_id,
    "2027-01-15",
    "option_gain",
    1704.08,
    "projected",
    "high",
    timestamp,
    timestamp
  ))

  cat("  ✓ Created projected option gain: $1,704.08 (Jan 2027)\n\n")

  # =============================================================================
  # STEP 5: Mark Original Activities as Ignored
  # =============================================================================
  cat("STEP 5: Marking original activities as ignored...\n")

  original_activity_ids <- c(
    "ACT_53238334_TGT15Jan27C8000_20251102190117_2398",
    "ACT_53238334_TGT_20251102190117_6634",
    "ACT_53238334_TGT15Jan27C7500_20251102190117_7405",
    "ACT_53238334_TGT15Jan27C8000_20251102190117_3961",
    "ACT_53238334_TGT_20251102190117_4813"
  )

  for (activity_id in original_activity_ids) {
    dbExecute(conn, "
      UPDATE account_activities
      SET ignore_for_grouping = TRUE,
          description = description || ' [SUPERSEDED BY CONSOLIDATED POSITION - SEE DIVIDEND_ARISTOCRATS_TGT_MANUAL_20250820]'
      WHERE activity_id = ?
    ", params = list(activity_id))
  }

  cat("  ✓ Marked 5 original activities as ignored\n\n")

  # =============================================================================
  # STEP 6: Commit Transaction
  # =============================================================================
  dbCommit(conn)

  cat("========================================\n")
  cat("SUCCESS: TGT Position Created\n")
  cat("========================================\n\n")

  # =============================================================================
  # STEP 7: Verification
  # =============================================================================
  cat("VERIFICATION:\n")
  cat("-------------\n\n")

  # Check position group
  group_check <- dbGetQuery(conn, "
    SELECT group_id, group_name, strategy_type, status
    FROM position_groups
    WHERE group_id = ?
  ", params = list(group_id))

  cat("Position Group:\n")
  print(group_check)
  cat("\n")

  # Check members
  members_check <- dbGetQuery(conn, "
    SELECT symbol, role
    FROM position_group_members
    WHERE group_id = ?
  ", params = list(group_id))

  cat("Position Members:\n")
  print(members_check)
  cat("\n")

  # Check linked activities
  activities_check <- dbGetQuery(conn, "
    SELECT trade_date, action, type, symbol, quantity, net_amount
    FROM account_activities
    WHERE group_id = ?
    ORDER BY trade_date
  ", params = list(group_id))

  cat("Linked Activities:\n")
  print(activities_check)
  cat("\n")

  # Check cash flows
  cash_flows_check <- dbGetQuery(conn, "
    SELECT event_date, event_type, amount, status
    FROM position_group_cash_flows
    WHERE group_id = ?
    ORDER BY event_date
  ", params = list(group_id))

  cat("Cash Flows:\n")
  print(cash_flows_check)
  cat("\n")

  # Check ignored activities
  ignored_check <- dbGetQuery(conn, "
    SELECT activity_id, symbol, ignore_for_grouping
    FROM account_activities
    WHERE symbol LIKE '%TGT%' AND activity_id LIKE 'ACT_53238334_TGT%20251102%'
  ")

  cat("Ignored Original Activities:\n")
  print(ignored_check)
  cat("\n")

  cat("========================================\n")
  cat("Position Summary:\n")
  cat("  Capital Deployed: $28,295.92\n")
  cat("  Expected Profit: $3,984.08\n")
  cat("  Total Return: 14.08%\n")
  cat("  Annualized Return: ~10.43%\n")
  cat("========================================\n\n")

  cat("Next steps:\n")
  cat("1. Refresh the UI and view the TGT position\n")
  cat("2. Verify it displays correctly without option premium cash flows\n")
  cat("3. Check that original activities don't appear in unlinked lists\n")

}, error = function(e) {
  cat("\n\n========================================\n")
  cat("ERROR: Transaction failed\n")
  cat("========================================\n")
  cat("Rolling back all changes...\n\n")
  dbRollback(conn)
  cat("Error message: ", e$message, "\n")
  cat("\nNo changes were made to the database.\n")
}, finally = {
  dbDisconnect(conn, shutdown = TRUE)
})
