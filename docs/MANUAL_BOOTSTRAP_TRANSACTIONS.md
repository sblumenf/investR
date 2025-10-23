# Manual Bootstrap Transactions Guide

## Purpose

This guide helps you manually create missing opening transactions for positions where:
- Opening trades occurred before your Questrade import window (>30 days ago)
- Questrade API didn't return the transactions
- You have closing transactions but no opening transactions (incomplete P&L)

**Use Case Example:**
- You have a covered call position showing a stock sale and assignment
- But missing the original stock purchase and option sale
- P&L shows incorrect profit because opening cost is $0

---

## When to Use This

✅ **Use manual bootstrap when:**
- You know the position details (quantities, prices, dates)
- Opening transactions are permanently unavailable from Questrade
- You want accurate P&L calculations for closed positions

❌ **Don't use this if:**
- Transactions might still be in Questrade (try extending import window first)
- You're unsure of the exact quantities/prices
- The position is already complete

---

## Step-by-Step Process

### Step 1: Gather Required Information

Before creating bootstrap transactions, collect:

1. **Group ID**: The position group you're fixing
2. **Account Number**: Your Questrade account number
3. **Trade Date**: Approximate date you opened the position
4. **Stock Details** (if applicable):
   - Symbol
   - Quantity
   - Purchase price per share
5. **Option Details** (if applicable):
   - Option symbol (e.g., `MRNA17Oct25C20.00`)
   - Number of contracts
   - Premium per contract

### Step 2: Calculate Expected Amounts

**For Covered Call Positions:**
```
Stock Cost = Quantity × Purchase Price
Option Premium = Contracts × Premium per Contract × 100

Total Cost = Stock Cost - Option Premium
```

**Example (MRNA Oct 17 @ $20):**
```
Stock: 300 shares @ $20 = $6,000
Option: 3 contracts @ $0.60 = $180
Net Cost: $6,000 - $180 = $5,820
```

---

## R Script Template

Save this as a standalone script (e.g., `scripts/bootstrap_missing_transactions.R`):

```r
#!/usr/bin/env Rscript
#
# Bootstrap Missing Transactions
#
# Purpose: Manually create opening transactions for positions where Questrade
#          didn't return the original trades (too old or missing from API)
#

library(DBI)
library(duckdb)

# ============================================================================
# CONFIGURATION - EDIT THESE VALUES FOR YOUR POSITION
# ============================================================================

# Position details
GROUP_ID <- "DIVIDEND_ARISTOCRATS_53238334_20251009233712_7370"
ACCOUNT_NUMBER <- "53238334"
TRADE_DATE <- "2025-10-01"  # Approximate opening date

# Stock purchase (if needed)
CREATE_STOCK_PURCHASE <- TRUE
STOCK_SYMBOL <- "MRNA"
STOCK_QUANTITY <- 300
STOCK_PRICE <- 20.00

# Option sale (if needed)
CREATE_OPTION_SALE <- TRUE
OPTION_SYMBOL <- "MRNA17Oct25C20.00"
OPTION_CONTRACTS <- 3
OPTION_PREMIUM_PER_CONTRACT <- 0.60

# ============================================================================
# SCRIPT EXECUTION - DO NOT EDIT BELOW THIS LINE
# ============================================================================

cat("=== BOOTSTRAP MISSING TRANSACTIONS ===\n\n")

# Connect to database
db_path <- file.path(getwd(), "inst", "database", "portfolio.duckdb")
if (!file.exists(db_path)) {
  stop("Database not found: ", db_path)
}

conn <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

# Verify group exists
group_check <- dbGetQuery(conn, "
  SELECT group_id, group_name, account_number
  FROM position_groups
  WHERE group_id = ?
", params = list(GROUP_ID))

if (nrow(group_check) == 0) {
  stop("Group ID not found: ", GROUP_ID)
}

cat("Target Group:\n")
cat("  ID:", GROUP_ID, "\n")
cat("  Name:", group_check$group_name, "\n")
cat("  Account:", group_check$account_number, "\n\n")

# Verify account number matches
if (group_check$account_number != ACCOUNT_NUMBER) {
  stop("Account number mismatch! Group belongs to: ", group_check$account_number)
}

transactions_created <- 0

# ============================================================================
# CREATE STOCK PURCHASE TRANSACTION
# ============================================================================

if (CREATE_STOCK_PURCHASE) {
  cat("Creating stock purchase transaction...\n")

  stock_gross_amount <- -(STOCK_QUANTITY * STOCK_PRICE)
  stock_activity_id <- sprintf(
    "ACT_%s_%s_MANUAL_BOOTSTRAP_%s_%d",
    ACCOUNT_NUMBER,
    gsub("[^A-Za-z0-9]", "", STOCK_SYMBOL),
    format(Sys.time(), "%Y%m%d%H%M%S"),
    sample(1000:9999, 1)
  )

  dbExecute(conn, "
    INSERT INTO account_activities (
      activity_id, account_number, account_type, trade_date,
      transaction_date, settlement_date, action, symbol, description,
      currency, quantity, price, gross_amount, commission, net_amount,
      type, group_id, is_processed, fetched_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    stock_activity_id,
    ACCOUNT_NUMBER,
    "LIRA",
    TRADE_DATE,
    TRADE_DATE,
    as.character(as.Date(TRADE_DATE) + 2),  # T+2 settlement
    "Buy",
    STOCK_SYMBOL,
    sprintf("%s - MANUAL_BOOTSTRAP (stock purchase)", STOCK_SYMBOL),
    "USD",
    STOCK_QUANTITY,
    STOCK_PRICE,
    stock_gross_amount,
    0,  # commission
    stock_gross_amount,
    "Trades",
    GROUP_ID,
    TRUE,  # is_processed
    as.character(Sys.time())
  ))

  cat("  ✅ Stock purchase created:", stock_activity_id, "\n")
  cat("     ", STOCK_QUANTITY, STOCK_SYMBOL, "@", STOCK_PRICE, "=",
      sprintf("$%.2f", stock_gross_amount), "\n\n")

  transactions_created <- transactions_created + 1
}

# ============================================================================
# CREATE OPTION SALE TRANSACTION
# ============================================================================

if (CREATE_OPTION_SALE) {
  cat("Creating option sale transaction...\n")

  option_gross_amount <- OPTION_CONTRACTS * OPTION_PREMIUM_PER_CONTRACT * 100
  option_activity_id <- sprintf(
    "ACT_%s_%s_MANUAL_BOOTSTRAP_%s_%d",
    ACCOUNT_NUMBER,
    gsub("[^A-Za-z0-9]", "", OPTION_SYMBOL),
    format(Sys.time(), "%Y%m%d%H%M%S"),
    sample(1000:9999, 1)
  )

  # Negative quantity for option SALE
  option_quantity <- -OPTION_CONTRACTS

  dbExecute(conn, "
    INSERT INTO account_activities (
      activity_id, account_number, account_type, trade_date,
      transaction_date, settlement_date, action, symbol, description,
      currency, quantity, price, gross_amount, commission, net_amount,
      type, group_id, is_processed, fetched_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    option_activity_id,
    ACCOUNT_NUMBER,
    "LIRA",
    TRADE_DATE,
    TRADE_DATE,
    as.character(as.Date(TRADE_DATE) + 2),
    "Sell",
    OPTION_SYMBOL,
    sprintf("%s - MANUAL_BOOTSTRAP (option sale)", OPTION_SYMBOL),
    "USD",
    option_quantity,
    OPTION_PREMIUM_PER_CONTRACT,
    option_gross_amount,
    0,  # commission
    option_gross_amount,
    "Trades",
    GROUP_ID,
    TRUE,
    as.character(Sys.time())
  ))

  cat("  ✅ Option sale created:", option_activity_id, "\n")
  cat("     ", OPTION_CONTRACTS, "contracts @", OPTION_PREMIUM_PER_CONTRACT, "=",
      sprintf("$%.2f", option_gross_amount), "\n\n")

  transactions_created <- transactions_created + 1
}

# ============================================================================
# VERIFY RESULTS
# ============================================================================

cat("=== VERIFICATION ===\n\n")

all_txns <- dbGetQuery(conn, "
  SELECT trade_date, action, symbol, quantity, price, net_amount
  FROM account_activities
  WHERE group_id = ?
  ORDER BY trade_date, action
", params = list(GROUP_ID))

cat("All transactions in group:\n")
print(all_txns)

# Calculate expected P&L
stock_cost <- sum(all_txns$net_amount[all_txns$action == "Buy" & !grepl("C", all_txns$symbol, fixed = TRUE)], na.rm = TRUE)
stock_proceeds <- sum(all_txns$net_amount[all_txns$action == "Sell" & !grepl("C", all_txns$symbol, fixed = TRUE)], na.rm = TRUE)
option_premium <- sum(all_txns$net_amount[all_txns$action == "Sell" & grepl("C", all_txns$symbol, fixed = TRUE)], na.rm = TRUE)

total_cost <- stock_cost - option_premium  # Premium reduces cost
total_proceeds <- stock_proceeds
net_pnl <- total_proceeds - abs(total_cost)

cat("\n=== P&L CALCULATION ===\n")
cat("Stock Cost:      ", sprintf("$%.2f", stock_cost), "\n")
cat("Option Premium:  ", sprintf("$%.2f", option_premium), "\n")
cat("Net Cost:        ", sprintf("$%.2f", abs(total_cost)), "\n")
cat("Stock Proceeds:  ", sprintf("$%.2f", stock_proceeds), "\n")
cat("Net P&L:         ", sprintf("$%.2f", net_pnl), "\n\n")

cat("=== BOOTSTRAP COMPLETE ===\n")
cat("Created", transactions_created, "transaction(s)\n")
cat("Position is now ready to close with accurate P&L\n")
```

---

## Usage Instructions

### Quick Start

1. **Copy the template script above** to `scripts/bootstrap_missing_transactions.R`

2. **Edit the CONFIGURATION section** with your position details:
   ```r
   GROUP_ID <- "YOUR_GROUP_ID_HERE"
   ACCOUNT_NUMBER <- "YOUR_ACCOUNT_NUMBER"
   TRADE_DATE <- "2025-10-01"  # When you opened the position

   # Stock details
   STOCK_SYMBOL <- "MRNA"
   STOCK_QUANTITY <- 300
   STOCK_PRICE <- 20.00

   # Option details
   OPTION_SYMBOL <- "MRNA17Oct25C20.00"
   OPTION_CONTRACTS <- 3
   OPTION_PREMIUM_PER_CONTRACT <- 0.60
   ```

3. **Run the script**:
   ```bash
   Rscript scripts/bootstrap_missing_transactions.R
   ```

4. **Verify the output** shows correct P&L calculations

5. **Close your position** - the P&L will now be accurate

---

## Finding Required Information

### How to Find Group ID

**Option 1 - From Database:**
```r
library(DBI)
library(duckdb)
conn <- dbConnect(duckdb::duckdb(), "inst/database/portfolio.duckdb")

# List all groups
dbGetQuery(conn, "
  SELECT group_id, group_name, account_number
  FROM position_groups
  WHERE status = 'open'
  ORDER BY created_at DESC
")

dbDisconnect(conn, shutdown = TRUE)
```

**Option 2 - From your UI:**
- The group ID is often shown in the position card details
- Look for a long string like `DIVIDEND_ARISTOCRATS_53238334_...`

### How to Calculate Option Premium

If you don't know the exact premium you received:

```
Expected Profit = (Known values from your records)
Stock Sale Price = (What you sold for at assignment)
Stock Purchase Price = (What you paid for stock)

Option Premium = Expected Profit - (Stock Sale Price - Stock Purchase Price)
```

**Example:**
```
Expected Profit: $180
Stock bought at: $20 × 300 = $6,000
Stock sold at: $20 × 300 = $6,000
Stock P&L: $0

Therefore: Option Premium = $180
Per contract: $180 / 3 contracts / 100 shares = $0.60
```

---

## Common Scenarios

### Scenario 1: Covered Call (Stock + Short Call)

**What you're missing:**
- Stock purchase
- Option sale

**Set these flags:**
```r
CREATE_STOCK_PURCHASE <- TRUE
CREATE_OPTION_SALE <- TRUE
```

### Scenario 2: Stock Only (No Options)

**What you're missing:**
- Stock purchase only

**Set these flags:**
```r
CREATE_STOCK_PURCHASE <- TRUE
CREATE_OPTION_SALE <- FALSE
```

### Scenario 3: Options Only (Naked Call)

**What you're missing:**
- Option sale only (no stock)

**Set these flags:**
```r
CREATE_STOCK_PURCHASE <- FALSE
CREATE_OPTION_SALE <- TRUE
```

---

## Verification Checklist

After running the bootstrap script, verify:

- ✅ Transactions appear in your position group
- ✅ Capital Deployed shows correct amount (not $0)
- ✅ P&L calculation matches your expectations
- ✅ Activity IDs contain "MANUAL_BOOTSTRAP" for audit trail
- ✅ Group can now be closed with accurate P&L

---

## Troubleshooting

### "Group ID not found"

Make sure you're using the full group ID string:
```r
GROUP_ID <- "DIVIDEND_ARISTOCRATS_53238334_20251009233712_7370"  # ✅ Full ID
GROUP_ID <- "DIVIDEND_ARISTOCRATS"  # ❌ Partial ID won't work
```

### "Account number mismatch"

The GROUP_ID you specified belongs to a different account. Verify:
```r
# Check which account owns the group
dbGetQuery(conn, "
  SELECT account_number, group_name
  FROM position_groups
  WHERE group_id = 'YOUR_GROUP_ID'
")
```

### P&L still shows $0 after bootstrap

- Check that transactions were linked to the correct group
- Verify `group_id` field matches in the database
- Refresh your UI/app to reload data

### Duplicate key error

You ran the script twice. The activity_id already exists. Either:
- Delete the previous bootstrap transactions first
- Modify the script to generate different activity_ids

---

## Cleaning Up Bootstrap Transactions

If you need to remove manually created transactions:

```r
library(DBI)
library(duckdb)
conn <- dbConnect(duckdb::duckdb(), "inst/database/portfolio.duckdb", read_only = FALSE)

# Delete all manual bootstrap transactions for a specific group
dbExecute(conn, "
  DELETE FROM account_activities
  WHERE group_id = ?
    AND description LIKE '%MANUAL_BOOTSTRAP%'
", params = list("YOUR_GROUP_ID_HERE"))

dbDisconnect(conn, shutdown = TRUE)
```

---

## Notes

- **Audit Trail**: All bootstrap transactions are marked with "MANUAL_BOOTSTRAP" in description
- **Timestamps**: Uses current system time for `fetched_at` (not the trade date)
- **Commissions**: Set to $0 by default (adjust if you know actual commission)
- **Settlement Date**: Automatically set to trade_date + 2 business days (T+2)
- **Currency**: Hardcoded to USD (change if needed)

---

## Example: MRNA Oct 17 @ $20 Position

```r
# Configuration for MRNA covered call
GROUP_ID <- "DIVIDEND_ARISTOCRATS_53238334_20251009233712_7370"
ACCOUNT_NUMBER <- "53238334"
TRADE_DATE <- "2025-10-01"

CREATE_STOCK_PURCHASE <- TRUE
STOCK_SYMBOL <- "MRNA"
STOCK_QUANTITY <- 300
STOCK_PRICE <- 20.00

CREATE_OPTION_SALE <- TRUE
OPTION_SYMBOL <- "MRNA17Oct25C20.00"
OPTION_CONTRACTS <- 3
OPTION_PREMIUM_PER_CONTRACT <- 0.60
```

**Expected Result:**
- Stock Cost: -$6,000
- Option Premium: $180
- Net Cost: $5,820
- P&L (after assignment): $180

---

## FAQ

**Q: How do I know if I need to bootstrap?**

A: Check if your position shows "Capital Deployed: $0.00" but has closing transactions (sales, assignments). If yes, you're missing opening transactions.

**Q: What if I don't remember the exact prices?**

A: Use your expected profit to back-calculate. See "How to Calculate Option Premium" section above.

**Q: Will this affect my taxes?**

A: The bootstrap creates database records for your app's P&L tracking. Your official tax records should come from Questrade's reports, not this app.

**Q: Can I bootstrap dividends?**

A: This guide is for trades only. Dividends are handled separately in the cash flow system.

**Q: What's the difference between bootstrap and manual transaction linking?**

A:
- **Linking**: Connects existing Questrade transactions to groups
- **Bootstrap**: Creates NEW synthetic transactions when Questrade data doesn't exist

---

## See Also

- `scripts/bootstrap_position_activities.R` - Original bootstrap for initial setup
- `R/fct_group_pnl.R` - P&L calculation logic
- `R/fct_activities_database.R` - Transaction storage functions
