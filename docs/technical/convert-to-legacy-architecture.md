# Convert to Legacy Covered Call - Technical Architecture

## Overview

This document describes the technical implementation of the "Convert to Legacy Covered Call" feature, which allows users to convert Dynamic Covered Calls positions to Legacy mode by removing projected cash flows.

**Implementation Date:** January 2025
**Components:** Database functions, UI layer, test suite
**Total Code:** ~800 lines (functions + tests + UI)

## Architecture Overview

### Layer Structure

```
┌─────────────────────────────────────────┐
│         UI Layer (Shiny)                │
│  - Group card button rendering          │
│  - Click event handlers                 │
│  - Confirmation modal                   │
│  - Success/error notifications          │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│      Backend Functions (R)              │
│  - can_convert_to_legacy()              │
│  - preview_legacy_conversion()          │
│  - convert_to_legacy_covered_call()     │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│      Database Layer (DuckDB)            │
│  - position_groups table                │
│  - position_group_cash_flows table      │
│  - projection_recalculations table      │
└─────────────────────────────────────────┘
```

## Database Schema

### Tables Used

#### position_groups
Stores position group metadata.

**Columns:**
- `group_id` (TEXT, PRIMARY KEY) - Unique identifier
- `strategy_type` (TEXT) - "Dynamic Covered Calls" or "Legacy Covered Call"
- `status` (TEXT) - "open" or "closed"
- `group_name` (TEXT) - User-friendly name
- `account_number` (TEXT) - Associated brokerage account
- `created_at` (TIMESTAMP) - Creation timestamp
- `updated_at` (TIMESTAMP) - Last modification timestamp

**Modified by conversion:**
- `strategy_type`: "Dynamic Covered Calls" → "Legacy Covered Call"
- `updated_at`: Set to current timestamp

#### position_group_cash_flows
Stores projected and actual cash flow events.

**Columns:**
- `flow_id` (TEXT, PRIMARY KEY) - Unique identifier
- `group_id` (TEXT, FOREIGN KEY) - References position_groups
- `event_date` (DATE) - When cash flow occurs/occurred
- `event_type` (TEXT) - "dividend", "option_gain", "option_premium"
- `amount` (NUMERIC) - Dollar amount
- `status` (TEXT) - "projected" or "actual"
- `confidence` (TEXT) - "high", "medium", "low" (projected only)

**Modified by conversion:**
- All rows with `status = 'projected'` are **deleted**
- Rows with `status = 'actual'` are **preserved**

#### projection_recalculations
Audit log for projection changes.

**Columns:**
- `recalc_id` (TEXT, PRIMARY KEY) - Unique identifier
- `group_id` (TEXT) - References position_groups
- `recalc_date` (TIMESTAMP) - When recalculation occurred
- `reason` (TEXT) - Why projections changed
- `old_projection_count` (INTEGER) - Count before change
- `new_projection_count` (INTEGER) - Count after change

**Created by conversion:**
- New row with `reason = 'converted_to_legacy'`
- `old_projection_count` = number of deleted projections
- `new_projection_count` = 0 (all removed)

## Backend Functions

### File Location
`R/fct_portfolio_groups_database.R` (lines 820-1020)

### Function: can_convert_to_legacy()

**Purpose:** Validates whether a group is eligible for conversion.

**Signature:**
```r
can_convert_to_legacy(conn, group_id)
```

**Parameters:**
- `conn` (DBI connection) - Database connection object
- `group_id` (character) - Group identifier

**Returns:**
List with:
- `valid` (logical) - TRUE if conversion allowed
- `reason` (character) - Explanation if invalid
- `projected_count` (integer) - Number of projected events
- `projected_amount` (numeric) - Total dollar amount of projections

**Validation Rules:**

| Check | SQL Query | Invalid Reason |
|-------|-----------|----------------|
| Group exists | `SELECT * FROM position_groups WHERE group_id = ?` | "Group not found" |
| Strategy type | `strategy_type = 'Dynamic Covered Calls'` | "Not a Dynamic Covered Calls strategy" |
| Status is open | `status = 'open'` | "Group is not open" |
| Has projections | `COUNT(*) WHERE status = 'projected'` | "No projected cash flows to remove" |

**Example Usage:**
```r
conn <- get_portfolio_db_connection()
result <- can_convert_to_legacy(conn, "CC_AAPL_20250117")

if (result$valid) {
  # Proceed with conversion
} else {
  warning(result$reason)
}
```

### Function: preview_legacy_conversion()

**Purpose:** Shows what will be deleted during conversion (for confirmation UI).

**Signature:**
```r
preview_legacy_conversion(conn, group_id)
```

**Parameters:**
- `conn` (DBI connection) - Database connection object
- `group_id` (character) - Group identifier

**Returns:**
List with:
- `has_events` (logical) - TRUE if projected events exist
- `total_count` (integer) - Total number of events to delete
- `total_amount` (numeric) - Total dollar amount to delete
- `by_type` (list) - Breakdown by event_type with count and amount
- `message` (character) - User-friendly summary

**Example Output:**
```r
$has_events
[1] TRUE

$total_count
[1] 5

$total_amount
[1] 660

$by_type
$by_type$dividend
$by_type$dividend$count
[1] 4

$by_type$dividend$amount
[1] 160


$by_type$option_gain
$by_type$option_gain$count
[1] 1

$by_type$option_gain$amount
[1] 500
```

**Example Usage:**
```r
preview <- preview_legacy_conversion(conn, "CC_AAPL_20250117")

# Display in UI confirmation modal
sprintf("Will delete %d events totaling %s",
        preview$total_count,
        format_currency(preview$total_amount))
```

### Function: convert_to_legacy_covered_call()

**Purpose:** Executes the conversion transaction atomically.

**Signature:**
```r
convert_to_legacy_covered_call(conn = NULL, group_id)
```

**Parameters:**
- `conn` (DBI connection, optional) - Database connection (creates if NULL)
- `group_id` (character) - Group identifier

**Returns:**
- `TRUE` on success
- `FALSE` on failure (validation failed or error occurred)

**Transaction Steps:**

1. **BEGIN TRANSACTION** - Start atomic operation
2. **Validate** - Call `can_convert_to_legacy()`, rollback if invalid
3. **Delete projections** - `DELETE FROM position_group_cash_flows WHERE group_id = ? AND status = 'projected'`
4. **Update strategy** - `UPDATE position_groups SET strategy_type = 'Legacy Covered Call', updated_at = ?`
5. **Log conversion** - `INSERT INTO projection_recalculations ...`
6. **COMMIT** - Finalize all changes

**Error Handling:**
- Any error triggers **ROLLBACK** - no partial changes
- Errors logged with `log_error()`
- Returns FALSE to caller

**Example Usage:**
```r
# Automatic connection management
success <- convert_to_legacy_covered_call(group_id = "CC_AAPL_20250117")

# Manual connection management (for batch operations)
conn <- get_portfolio_db_connection()
success1 <- convert_to_legacy_covered_call(conn, "CC_AAPL_20250117")
success2 <- convert_to_legacy_covered_call(conn, "CC_MSFT_20250221")
dbDisconnect(conn, shutdown = TRUE)
```

## UI Layer

### File Locations
- Card rendering: `R/utils_group_cards.R` (lines 900-950)
- Event handlers: `R/mod_portfolio_groups_cards.R` (lines 320-420)

### Button Rendering Logic

**Location:** `create_open_group_card()` in `utils_group_cards.R`

**Visibility Condition:**
```r
can_convert_to_legacy <-
  group_data$strategy_type == "Dynamic Covered Calls" &&
  nrow(cash_flows %>% filter(status == "projected")) > 0
```

**Button HTML:**
```r
if (can_convert_to_legacy) {
  tags$button(
    id = sprintf("convert_legacy_%s", group_data$group_id),
    type = "button",
    class = "btn btn-sm btn-info",
    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                      ns("convert_legacy_clicked"),
                      group_data$group_id),
    icon("archive"),
    " Convert to Legacy"
  )
}
```

### Event Handler Flow

**Location:** `mod_group_cards_server()` in `mod_portfolio_groups_cards.R`

#### Step 1: Button Click
```r
observeEvent(input$convert_legacy_clicked, {
  group_id <- input$convert_legacy_clicked

  # Get preview data
  conn <- get_portfolio_db_connection()
  preview <- preview_legacy_conversion(conn, group_id)

  # Show confirmation modal with preview details
  showModal(...)
})
```

#### Step 2: Confirmation Modal
Modal displays:
- What will happen (bullet points)
- Event deletion breakdown (by type with counts and amounts)
- Total projected amount
- Warning about permanence
- Explanation of Legacy mode

User actions:
- **Cancel** - Close modal, no changes
- **Convert to Legacy** - Proceed to step 3

#### Step 3: Confirmed Conversion
```r
observeEvent(input$confirm_convert_legacy, {
  group_id <- input$confirm_convert_legacy

  # Execute conversion
  conn <- get_portfolio_db_connection()
  result <- convert_to_legacy_covered_call(conn, group_id)

  if (result) {
    showNotification("Successfully converted to Legacy", type = "message")
    card_version(card_version() + 1)  # Force re-render
  } else {
    showNotification("Conversion failed", type = "error")
  }

  removeModal()
})
```

### UI Reactivity

**Card Re-rendering:**
- `card_version` reactive value incremented on conversion
- Triggers `output$cards_container` re-render
- New card shows "Legacy Covered Call" badge
- "Convert to Legacy" button no longer displayed
- Projected events removed from Cash Flows section

## Test Suite

### File Location
`tests/testthat/test-fct_convert_to_legacy.R`

### Test Coverage

**Total Tests:** 19
**Status:** All passing ✓
**Coverage Areas:** Validation, preview, conversion, edge cases, transaction safety

### Test Categories

#### Validation Tests (5 tests)
- `can_convert_to_legacy()` returns TRUE for valid groups
- Returns FALSE for Legacy groups (already converted)
- Returns FALSE for closed groups
- Returns FALSE when no projected events exist
- Returns FALSE for non-existent group_id

#### Preview Tests (2 tests)
- Returns correct breakdown by event type
- Handles groups with no projections gracefully

#### Conversion Tests (3 tests)
- Successfully converts Dynamic to Legacy
- Updates `updated_at` timestamp
- Creates `projection_recalculations` log entry

#### Failure Tests (3 tests)
- Rolls back transaction on validation failure
- Handles non-existent group gracefully
- Connection management works correctly (with/without provided conn)

#### Integration Test (1 test)
- End-to-end workflow: validate → preview → convert → verify

#### Edge Cases (3 tests)
- Multiple projected events of same type
- Mixed projected and actual events (preserves actual)
- Large amounts (precision testing)

#### Transaction Safety (2 tests)
- No orphaned records after failed conversion
- Audit log contains correct metadata

### Running Tests

```r
# Run all tests
testthat::test_file("tests/testthat/test-fct_convert_to_legacy.R")

# Run specific test
testthat::test_that("can_convert_to_legacy returns valid=TRUE for Dynamic Covered Calls with projections", {
  # ...
})
```

## Performance Considerations

### Database Operations

**Typical Conversion:**
- 1 validation query (~10ms)
- 1 preview query (~10ms)
- 3 transaction queries: DELETE + UPDATE + INSERT (~50ms)
- **Total: ~70ms** for typical group with 5-10 projected events

**Optimization:**
- Indexed queries on `group_id` and `status`
- Transaction batching for atomic operations
- Connection reuse for batch conversions

### UI Performance

**Card Rendering:**
- Button visibility check: O(1) - single attribute check
- Re-render triggered only on conversion success
- No additional database queries during render

## Error Handling

### Database Errors

| Error Type | Handling Strategy | User Impact |
|------------|------------------|-------------|
| Connection failure | Log error, return FALSE | Error notification shown |
| Transaction timeout | Automatic rollback | No changes applied |
| Validation failure | Return FALSE with reason | Cannot convert (expected) |
| Constraint violation | Rollback, log error | Error notification shown |

### UI Errors

| Error Type | Handling Strategy | User Impact |
|------------|------------------|-------------|
| Modal load failure | Show error notification | Cannot see preview |
| Button click failure | Log to console | Button unresponsive |
| Network timeout | Shiny reconnection | Session reconnects |

## Logging

### Log Levels

**DEBUG:**
- Preview generation details
- Projection counts and amounts

**INFO:**
- Conversion initiated
- Conversion completed successfully
- Each transaction step

**WARN:**
- Validation failures
- Button hidden due to conditions

**ERROR:**
- Database errors
- Transaction failures
- Unexpected exceptions

### Log Format

```r
log_info("Convert to Legacy: Starting conversion for group {group_id}")
log_debug("Convert to Legacy: Preview generated - {count} events, ${amount}")
log_error("Convert to Legacy: Conversion failed - {error_message}")
```

## Security Considerations

### SQL Injection Protection
- All queries use parameterized statements
- User input (`group_id`) passed as parameters, not concatenated

**Safe:**
```r
dbExecute(conn, "DELETE FROM ... WHERE group_id = ?", params = list(group_id))
```

**Unsafe (NOT used):**
```r
# NEVER DO THIS
dbExecute(conn, paste0("DELETE FROM ... WHERE group_id = '", group_id, "'"))
```

### Transaction Integrity
- ACID properties enforced by DuckDB
- Atomic operations prevent partial updates
- Rollback on any error ensures consistency

### Authorization
- No explicit authorization checks (single-user application)
- Future enhancement: Add user permission checks

## Future Enhancements

### Potential Improvements

1. **Undo Functionality**
   - Store deleted projections in archive table
   - Allow restoration within 24 hours
   - Implementation: `projection_archive` table + `undo_legacy_conversion()`

2. **Batch Conversion**
   - Convert multiple groups simultaneously
   - UI: Checkbox selection + bulk action button
   - Implementation: Loop with single transaction

3. **Conversion Reasons**
   - Add optional reason field to audit log
   - UI: Text input in confirmation modal
   - Implementation: Add `user_reason` column to `projection_recalculations`

4. **Notification System**
   - Email notification on conversion
   - Implementation: Hook into notification service

5. **Conversion History Report**
   - Dashboard showing all conversions
   - Query `projection_recalculations` table
   - Implementation: New report module

## Related Documentation

- [User Guide](../user-guide/convert-to-legacy-covered-call.md)
- [PDCA Documentation](../pdca/convert-to-legacy/)
- [Pattern: Dynamic to Legacy Conversion](../patterns/dynamic-to-legacy-conversion.md)
- [Portfolio Groups Database Schema](./portfolio-groups-schema.md)
