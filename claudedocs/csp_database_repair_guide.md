# CSP Database Repair Guide

## Problem Summary

The XYZ Cash-Secured Put (CSP) position was saved to the database with corrupted data:

- **Member Symbol**: Empty string `""` instead of `"XYZ"`
- **Member Role**: `"underlying_stock"` instead of `"short_put"`
- **Group Name**: `"S&P 500 Cash-Secured Puts - "` (incomplete, missing ticker)

This corruption occurred during the CSP position creation process and prevents the position from being properly displayed and analyzed.

## Root Cause

The corruption happened in the position creation workflow where:
1. The symbol extraction logic failed to populate the member symbol
2. The role assignment logic incorrectly set the role to `"underlying_stock"` for a put option
3. The group name generation completed before the symbol was available

## Solution Overview

The repair script `/home/sergeblumenfeld/investR/scripts/repair_csp_database.R` performs the following operations:

1. **Locate**: Find the corrupted group (empty symbol + incomplete group name)
2. **Extract**: Parse the correct symbol from the related activity description
3. **Update**: Fix the member record (symbol + role) and group name
4. **Verify**: Confirm all changes were applied correctly

## Running the Repair

### Prerequisites

- R environment with packages: `DBI`, `duckdb`, `dplyr`, `stringr`
- Access to `inst/database/portfolio.duckdb`
- No active R sessions with the database open

### Execution

```bash
# From the investR project root directory
Rscript scripts/repair_csp_database.R
```

### Expected Output

```
=================================================
CSP Database Repair Script
=================================================

Connecting to database: inst/database/portfolio.duckdb

[STEP 1] Searching for corrupted CSP group...
Found corrupted group:
  Group ID: SP_500_CASHSECURED_PUTS_53254220_20251210223622_5490
  Account: 53254220
  Current Group Name: 'S&P 500 Cash-Secured Puts - '
  Current Member Symbol: '' (empty)
  Current Member Role: 'underlying_stock'

[STEP 2] Looking up correct symbol from related activities...
Found 1 related activity/activities
  Activity Description: PUT  XYZ    01/23/26    50  BLOCK INC CL A  WE ACTED AS AGENT
Extracted symbol: XYZ

[STEP 3] Preparing database updates...
Planned changes:
  Member Symbol: '' → 'XYZ'
  Member Role: 'underlying_stock' → 'short_put'
  Group Name: 'S&P 500 Cash-Secured Puts - ' → 'S&P 500 Cash-Secured Puts - XYZ'

[STEP 4] Applying database updates (transactional)...
  Updated member record (1 row)
  Updated group record (1 row)
  Transaction committed successfully

[STEP 5] Verifying repair...
  Member record verified: symbol = 'XYZ', role = 'short_put'
  Group record verified: group_name = 'S&P 500 Cash-Secured Puts - XYZ'

=================================================
REPAIR COMPLETED SUCCESSFULLY
=================================================

Summary:
  - Fixed symbol: XYZ
  - Fixed role: short_put
  - Updated group name: S&P 500 Cash-Secured Puts - XYZ

The XYZ CSP position has been repaired and is now properly configured.

Database connection closed.
```

## Safety Features

### 1. Pre-Flight Checks

The script verifies the corruption pattern before making changes:
- Ensures exactly 1 corrupted group exists
- Validates the group matches expected corruption criteria
- Extracts symbol from activity description using regex pattern matching

### 2. Transactional Updates

All database modifications occur within a transaction:
- If any update fails, all changes are rolled back
- Database remains consistent even if errors occur
- No partial updates possible

### 3. Post-Repair Verification

After updates complete, the script:
- Queries the updated records
- Verifies symbol, role, and group name match expectations
- Reports verification status with detailed output

### 4. Idempotent Execution

The script can be run multiple times safely:
- If the corruption is already fixed, exits cleanly with status 0
- No changes made if the target record is not found
- Clear messaging about why no action was taken

## Database Schema

### Tables Modified

**position_group_members**
```sql
UPDATE position_group_members
SET
  symbol = 'XYZ',              -- Fixed from ''
  role = 'short_put',          -- Fixed from 'underlying_stock'
  added_at = CURRENT_TIMESTAMP -- Updated timestamp
WHERE group_id = '<corrupted_group_id>'
  AND symbol = ''
  AND role = 'underlying_stock'
```

**position_groups**
```sql
UPDATE position_groups
SET
  group_name = 'S&P 500 Cash-Secured Puts - XYZ',  -- Fixed from incomplete name
  updated_at = CURRENT_TIMESTAMP                    -- Updated timestamp
WHERE group_id = '<corrupted_group_id>'
```

## Verification Queries

After running the repair, you can manually verify the fix:

```r
library(DBI)
library(duckdb)

conn <- dbConnect(duckdb::duckdb(), "inst/database/portfolio.duckdb")

# Check the repaired member
dbGetQuery(conn, "
  SELECT m.symbol, m.role, g.group_name
  FROM position_group_members m
  JOIN position_groups g ON m.group_id = g.group_id
  WHERE g.strategy_type = 'S&P 500 Cash-Secured Puts'
    AND g.group_name LIKE '%XYZ%'
")

# Expected output:
# symbol      role       group_name
# XYZ         short_put  S&P 500 Cash-Secured Puts - XYZ

dbDisconnect(conn, shutdown = TRUE)
```

## Troubleshooting

### Script Reports "No corrupted CSP group found"

This means:
- The corruption has already been fixed, OR
- The corruption pattern doesn't match expectations

Check manually:
```r
# Look for any CSP groups with empty symbols
dbGetQuery(conn, "
  SELECT m.*, g.group_name
  FROM position_group_members m
  JOIN position_groups g ON m.group_id = g.group_id
  WHERE g.strategy_type LIKE '%Cash-Secured Puts%'
    AND m.symbol = ''
")
```

### Script Reports "Multiple corrupted groups found"

This indicates more widespread corruption than expected. Do NOT run the script.
Instead:
1. Review the output showing all corrupted groups
2. Investigate why multiple groups are corrupted
3. Consider whether a more comprehensive repair is needed

### Verification Failed

If the script reports verification failure:
1. Check the error message for details
2. Query the database manually to inspect the state
3. Review the transaction log for any unusual behavior
4. Contact the development team if the issue persists

## Related Files

- **Repair Script**: `scripts/repair_csp_database.R`
- **Database Schema**: `R/fct_portfolio_groups_database.R`
- **Position Creation Logic**: (Being fixed by other agents in the 4-fix plan)
- **Design Documentation**: `claudedocs/cash_secured_puts_database_integration_design.md`

## Part of 4-Fix Plan

This repair script is **Fix #1** in the comprehensive 4-fix plan:

1. **Database Repair** (This Script): Fix the existing corrupted XYZ record
2. **Symbol Extraction Fix**: Fix the code that extracts symbols from descriptions
3. **Role Assignment Fix**: Fix the logic that determines member roles
4. **Group Name Generation**: Ensure group names are generated after symbol extraction

This script addresses the immediate data corruption. The other fixes prevent future occurrences.

## Maintenance Notes

### When to Run This Script

- After discovering corrupted CSP positions in the database
- As part of data integrity audits
- Before generating portfolio risk reports (if CSP data is suspect)

### When NOT to Run This Script

- If the database is actively being written to
- If you're unsure about the corruption pattern
- If multiple corrupted groups exist (investigate first)
- During production trading hours (run during maintenance windows)

## Success Criteria

The repair is successful when:

1. Exit status is 0
2. Output shows "REPAIR COMPLETED SUCCESSFULLY"
3. All verification checks pass
4. Manual query confirms:
   - Member symbol = "XYZ"
   - Member role = "short_put"
   - Group name = "S&P 500 Cash-Secured Puts - XYZ"
5. XYZ CSP position appears correctly in the Portfolio Risk Dashboard

---

**Author**: Backend Architect Agent
**Date**: 2025-12-10
**Version**: 1.0
