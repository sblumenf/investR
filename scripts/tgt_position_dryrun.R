#!/usr/bin/env Rscript
#
# TGT Position Manual Creation - DRY RUN
#
# This script shows what WOULD be created for the TGT Dividend Aristocrats position
# without actually writing to the database.
#

library(dplyr)
library(tibble)
library(DBI)
library(RSQLite)

cat("========================================\n")
cat("TGT POSITION CREATION - DRY RUN\n")
cat("========================================\n\n")

# Connect to database (read-only for verification)
conn <- dbConnect(RSQLite::SQLite(), 'inst/database/portfolio.sqlite', flags = RSQLite::SQLITE_RO)

# =============================================================================
# STEP 1: Show Current State
# =============================================================================
cat("STEP 1: CURRENT STATE\n")
cat("---------------------\n\n")

cat("Original TGT activities (will be marked as ignored):\n")
original_activities <- dbGetQuery(conn, "
  SELECT
    activity_id,
    trade_date,
    action,
    type,
    symbol,
    quantity,
    price,
    net_amount
  FROM account_activities
  WHERE symbol LIKE '%TGT%'
  ORDER BY trade_date, activity_id
")
print(original_activities)

cat("\n\nCost Basis Calculation:\n")
cat("Stock purchases: $40,472.00\n")
cat("  - 200 shares @ $104.70 = $20,940.00\n")
cat("  - 200 shares @ $97.66  = $19,532.00\n")
cat("\nOption premiums (net):\n")
cat("  - Initial $80 call premium: +$6,072.02\n")
cat("  - Buyback $80 calls:        -$5,131.98\n")
cat("  - Roll $75 call premium:    +$11,236.04\n")
cat("  - Net option credit:         $12,176.08\n")
cat("\nNet Capital Deployed: $40,472.00 - $12,176.08 = $28,295.92\n")
cat("Average Stock Price: $40,472.00 / 400 = $101.18\n")

# =============================================================================
# STEP 2: Synthetic Activities to be Created
# =============================================================================
cat("\n\n========================================\n")
cat("STEP 2: SYNTHETIC ACTIVITIES TO CREATE\n")
cat("========================================\n\n")

synthetic_activities <- tribble(
  ~activity_id, ~trade_date, ~action, ~type, ~symbol, ~quantity, ~price, ~gross_amount, ~net_amount, ~commission, ~description,

  "ACT_53238334_TGT_SYNTHETIC_STOCK_20250820",
  "2025-08-20",
  "Buy",
  "Trades",
  "TGT",
  400,
  101.18,
  -40472.00,
  -40472.00,
  0.00,
  "TARGET CORP - SYNTHETIC CONSOLIDATED POSITION (400 shares avg $101.18)",

  "ACT_53238334_TGT15Jan27C7500_SYNTHETIC_OPTION_20250820",
  "2025-08-20",
  "Sell",
  "Trades",
  "TGT15Jan27C75.00",
  -4,
  30.44,
  12176.08,
  12176.08,
  0.00,
  "CALL TGT 01/15/27 75 - SYNTHETIC CONSOLIDATED POSITION (net of roll: $6,072 initial + $6,104 roll)",

  "ACT_53238334_TGT_DIV_20250901",
  "2025-09-01",
  NA_character_,
  "Dividends",
  "TGT",
  200,
  1.14,
  228.00,
  228.00,
  0.00,
  "TARGET CORP CASH DIV ON 200 SHS REC 08/13/25 PAY 09/01/25"
)

print(synthetic_activities %>% select(activity_id, trade_date, action, type, symbol, quantity, net_amount))

cat("\n\nNote: These activities will have:\n")
cat("  - account_number: 53238334\n")
cat("  - group_id: DIVIDEND_ARISTOCRATS_TGT_MANUAL_20250820\n")
cat("  - ignore_for_grouping: FALSE\n")
cat("  - created_at: [timestamp]\n")
cat("  - updated_at: [timestamp]\n")

# =============================================================================
# STEP 3: Position Group to be Created
# =============================================================================
cat("\n\n========================================\n")
cat("STEP 3: POSITION GROUP TO CREATE\n")
cat("========================================\n\n")

position_group <- tribble(
  ~group_id, ~group_name, ~strategy_type, ~status, ~notes,
  "DIVIDEND_ARISTOCRATS_TGT_MANUAL_20250820",
  "Dividend Aristocrats - TGT - Jan 2027 @ $75",
  "Dividend Aristocrats",
  "open",
  "Manual consolidated position - legged in and rolled, see original activities for full history"
)

print(position_group)

# =============================================================================
# STEP 4: Position Group Members to be Created
# =============================================================================
cat("\n\n========================================\n")
cat("STEP 4: POSITION GROUP MEMBERS TO CREATE\n")
cat("========================================\n\n")

group_members <- tribble(
  ~group_id, ~symbol, ~role,
  "DIVIDEND_ARISTOCRATS_TGT_MANUAL_20250820", "TGT", "underlying_stock",
  "DIVIDEND_ARISTOCRATS_TGT_MANUAL_20250820", "TGT15Jan27C75.00", "short_call"
)

print(group_members)

# =============================================================================
# STEP 5: Projected Cash Flows to be Created
# =============================================================================
cat("\n\n========================================\n")
cat("STEP 5: PROJECTED CASH FLOWS TO CREATE\n")
cat("========================================\n\n")

cat("Dividend Projections (400 shares @ $1.14/share = $456.00):\n\n")

projected_cash_flows <- tribble(
  ~event_id, ~event_date, ~event_type, ~amount, ~status, ~confidence,

  "EVENT_TGT_DIV_ACTUAL_20250901", "2025-09-01", "dividend", 228.00, "actual", "high",
  "EVENT_TGT_DIV_20251201", "2025-12-01", "dividend", 456.00, "projected", "high",
  "EVENT_TGT_DIV_20260301", "2026-03-01", "dividend", 456.00, "projected", "high",
  "EVENT_TGT_DIV_20260601", "2026-06-01", "dividend", 456.00, "projected", "high",
  "EVENT_TGT_DIV_20260901", "2026-09-01", "dividend", 456.00, "projected", "high",
  "EVENT_TGT_DIV_20261201", "2026-12-01", "dividend", 456.00, "projected", "high",
  "EVENT_TGT_OPTIONGAIN_20270115", "2027-01-15", "option_gain", 1704.08, "projected", "high"
)

print(projected_cash_flows %>% select(event_date, event_type, amount, status))

cat("\n\nOption Gain Calculation:\n")
cat("  Exercise proceeds: 400 shares × $75 = $30,000.00\n")
cat("  Net capital deployed:                 $28,295.92\n")
cat("  Option gain:                           $1,704.08\n")

cat("\n\nTotal Projected Income:\n")
cat("  Dividends: 5 payments × $456 = $2,280.00\n")
cat("  Option gain:                   $1,704.08\n")
cat("  Total:                         $3,984.08\n")
cat("  Return: $3,984.08 / $28,295.92 = 14.08%\n")
cat("  Annualized (439 days):         ~10.43%\n")

# =============================================================================
# STEP 6: Original Activities Update Preview
# =============================================================================
cat("\n\n========================================\n")
cat("STEP 6: ORIGINAL ACTIVITIES TO MARK AS IGNORED\n")
cat("========================================\n\n")

cat("These 5 activities will have ignore_for_grouping set to TRUE:\n")
print(original_activities %>% select(activity_id, trade_date, symbol, net_amount))

cat("\nTheir descriptions will be appended with:\n")
cat("[SUPERSEDED BY CONSOLIDATED POSITION - SEE DIVIDEND_ARISTOCRATS_TGT_MANUAL_20250820]\n")

# =============================================================================
# STEP 7: Verification Queries
# =============================================================================
cat("\n\n========================================\n")
cat("STEP 7: POST-CREATION VERIFICATION QUERIES\n")
cat("========================================\n\n")

cat("After creation, these queries will verify the position:\n\n")

cat("1. Check position group:\n")
cat("   SELECT * FROM position_groups WHERE group_id = 'DIVIDEND_ARISTOCRATS_TGT_MANUAL_20250820';\n\n")

cat("2. Check members:\n")
cat("   SELECT * FROM position_group_members WHERE group_id = 'DIVIDEND_ARISTOCRATS_TGT_MANUAL_20250820';\n\n")

cat("3. Check linked activities:\n")
cat("   SELECT trade_date, action, type, symbol, quantity, net_amount\n")
cat("   FROM account_activities\n")
cat("   WHERE group_id = 'DIVIDEND_ARISTOCRATS_TGT_MANUAL_20250820'\n")
cat("   ORDER BY trade_date;\n\n")

cat("4. Check cash flows:\n")
cat("   SELECT event_date, event_type, amount, status\n")
cat("   FROM position_group_cash_flows\n")
cat("   WHERE group_id = 'DIVIDEND_ARISTOCRATS_TGT_MANUAL_20250820'\n")
cat("   ORDER BY event_date;\n\n")

cat("5. Verify original activities are ignored:\n")
cat("   SELECT activity_id, symbol, ignore_for_grouping\n")
cat("   FROM account_activities\n")
cat("   WHERE symbol LIKE '%TGT%' AND activity_id LIKE 'ACT_53238334_TGT%20251102%'\n")
cat("   ORDER BY trade_date;\n\n")

# =============================================================================
# STEP 8: Expected Position Card Display
# =============================================================================
cat("\n\n========================================\n")
cat("STEP 8: EXPECTED POSITION CARD DISPLAY\n")
cat("========================================\n\n")

cat("TGT $[current_price] OPEN\n")
cat("Dividend Aristocrats | 439 days (Jan 15, 2027) to expiration | ~10.43% annualized\n\n")

cat("Position Status\n")
cat("---------------\n")
cat("Current Stock Price: $[current_price]\n")
cat("Strike Price: $75.00 (in-the-money by $[ITM_amount])\n")
cat("Days to Expiration: 439 days (Jan 15, 2027)\n\n")

cat("Capital & Returns\n")
cat("-----------------\n")
cat("Capital Deployed: $28,295.92 (net after $12,176.08 premium collected)\n")
cat("Expected Profit: $3,984.08 (14.08% return | 10.43% annualized)\n\n")

cat("Income Tracking\n")
cat("---------------\n")
cat("Dividends Collected: $228.00 collected\n\n")

cat("Projected Events (6 upcoming) $3,984.08\n")
cat("2025-12-01 | Dividend Payment: $456.00\n")
cat("2026-03-01 | Dividend Payment: $456.00\n")
cat("2026-06-01 | Dividend Payment: $456.00\n")
cat("2026-09-01 | Dividend Payment: $456.00\n")
cat("2026-12-01 | Dividend Payment: $456.00\n")
cat("2027-01-15 | Profit at Expiration: $1,704.08\n\n")

cat("Actual Events (1 realized) $228.00\n")
cat("2025-09-01 | Dividend Payment: $228.00\n\n")

cat("Transaction History\n")
cat("-------------------\n")
cat("2025-08-20 | Buy Trades | TGT | 400 @ $101.18: $-40,472.00\n")
cat("2025-08-20 | Sell Trades | TGT15Jan27C75.00 | 4 @ $30.44: $12,176.08\n")
cat("2025-09-01 | Dividends | TGT | 200 @ $1.14: $228.00\n")

# =============================================================================
# Summary
# =============================================================================
cat("\n\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n\n")

cat("This dry run shows:\n")
cat("✓ 3 synthetic activities to be created\n")
cat("✓ 1 position group to be created\n")
cat("✓ 2 position group members to be created\n")
cat("✓ 7 cash flow events to be created (1 actual, 6 projected)\n")
cat("✓ 5 original activities to be marked as ignored\n\n")

cat("NO DATA HAS BEEN WRITTEN TO THE DATABASE.\n\n")

cat("If this looks correct, I will create the actual execution script.\n")

dbDisconnect(conn)
