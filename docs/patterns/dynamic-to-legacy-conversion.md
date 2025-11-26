# Pattern: Dynamic to Legacy Conversion

## Pattern Overview

**Pattern Name:** Dynamic to Legacy Conversion Pattern

**Category:** Data State Transition

**Problem:** Users need to transition automated projection systems to manual tracking when automatic projections become invalid or unwanted.

**Solution:** Provide explicit conversion mechanism with validation, preview, and atomic execution that removes automatic projections while preserving historical data.

**Applicability:** Any feature where:
- System generates automatic projections/estimates
- Projections may become invalid due to external events
- Users need option to switch to manual tracking
- Historical data must be preserved during transition

---

## Context

### When This Pattern Applies

**Use this pattern when:**

1. **Projection Invalidation**
   - System projections based on assumptions
   - Assumptions can become invalid (e.g., option expires worthless)
   - User needs to clear invalid projections

2. **Mode Switching**
   - System has "automatic" vs "manual" modes
   - User wants to switch between modes
   - Mode change affects data (projections exist in automatic mode)

3. **Data Preservation Required**
   - Historical actual data must be preserved
   - Only projected/future data should be removed
   - Audit trail of transition needed

4. **Irreversible Actions**
   - Transition is permanent (or expensive to reverse)
   - User must understand consequences
   - Confirmation required before proceeding

### When NOT to Use This Pattern

**Avoid this pattern if:**

- Transition is trivial (simple field update)
- No data deletion involved
- Easily reversible (can just toggle back)
- No user confirmation needed
- Automatic transition (not user-initiated)

---

## Structure

### Components

```
┌─────────────────────────────────────┐
│     Validation Function             │
│  - Check eligibility                │
│  - Return structured result         │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│     Preview Function                │
│  - Show what will change            │
│  - Breakdown by category            │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│     Conversion Function             │
│  - Transaction wrapper              │
│  - Validate → Execute → Log         │
│  - Return success/failure           │
└─────────────────────────────────────┘
```

### Database Schema Requirements

**State Tracking Table** (e.g., `position_groups`):
- State field (e.g., `strategy_type`)
- Status field (e.g., `status`)
- Timestamp fields (`created_at`, `updated_at`)

**Data Table** (e.g., `position_group_cash_flows`):
- State indicator field (e.g., `status = 'projected'` vs `'actual'`)
- Foreign key to state tracking table
- Sufficient fields to support preview breakdown

**Audit Table** (e.g., `projection_recalculations`):
- Action identifier
- Timestamp
- Before/after counts or states
- Reason code
- Metadata

---

## Implementation

### Step 1: Validation Function

**Purpose:** Check if entity is eligible for conversion

**Signature:**
```r
can_convert_to_mode(conn, entity_id)
```

**Returns:**
```r
list(
  valid = TRUE/FALSE,
  reason = "explanation if invalid",
  metadata = list(
    current_state = "...",
    data_count = N,
    data_amount = $X
  )
)
```

**Validation Checks:**
1. Entity exists
2. Current state is correct (e.g., "Dynamic")
3. Target state is different (e.g., not already "Legacy")
4. Entity status allows conversion (e.g., "open" not "closed")
5. Data to remove exists (e.g., has projected events)

**Example:**
```r
can_convert_to_legacy <- function(conn, group_id) {
  # Check 1: Entity exists
  entity <- dbGetQuery(conn, "
    SELECT * FROM entities WHERE id = ?
  ", params = list(group_id))

  if (nrow(entity) == 0) {
    return(list(valid = FALSE, reason = "Entity not found"))
  }

  # Check 2: Current state
  if (entity$mode != "dynamic") {
    return(list(valid = FALSE, reason = "Not in dynamic mode"))
  }

  # Check 3: Status allows
  if (entity$status == "closed") {
    return(list(valid = FALSE, reason = "Entity is closed"))
  }

  # Check 4: Has data to remove
  projected <- dbGetQuery(conn, "
    SELECT COUNT(*) as count, SUM(amount) as total
    FROM data_items
    WHERE entity_id = ? AND type = 'projected'
  ", params = list(group_id))

  if (projected$count == 0) {
    return(list(valid = FALSE, reason = "No projected data to remove"))
  }

  # All valid
  return(list(
    valid = TRUE,
    metadata = list(
      projected_count = projected$count,
      projected_amount = projected$total
    )
  ))
}
```

---

### Step 2: Preview Function

**Purpose:** Show user exactly what will be removed

**Signature:**
```r
preview_conversion(conn, entity_id)
```

**Returns:**
```r
list(
  has_data = TRUE/FALSE,
  total_count = N,
  total_amount = $X,
  by_category = list(
    category1 = list(count = N1, amount = $X1),
    category2 = list(count = N2, amount = $X2)
  ),
  message = "user-friendly summary"
)
```

**Example:**
```r
preview_legacy_conversion <- function(conn, entity_id) {
  # Query projected data grouped by category
  data <- dbGetQuery(conn, "
    SELECT
      category,
      COUNT(*) as count,
      SUM(amount) as total
    FROM data_items
    WHERE entity_id = ? AND type = 'projected'
    GROUP BY category
  ", params = list(entity_id))

  if (nrow(data) == 0) {
    return(list(
      has_data = FALSE,
      message = "No projected data found"
    ))
  }

  # Build preview structure
  preview <- list(
    has_data = TRUE,
    total_count = sum(data$count),
    total_amount = sum(data$total),
    by_category = list()
  )

  # Breakdown by category
  for (i in seq_len(nrow(data))) {
    category <- data$category[i]
    preview$by_category[[category]] <- list(
      count = data$count[i],
      amount = data$total[i]
    )
  }

  return(preview)
}
```

---

### Step 3: Conversion Function

**Purpose:** Execute atomic state transition

**Signature:**
```r
convert_to_mode(conn = NULL, entity_id)
```

**Returns:** `TRUE` on success, `FALSE` on failure

**Transaction Structure:**
```r
convert_to_legacy <- function(conn = NULL, entity_id) {
  # Connection management
  should_close <- is.null(conn)
  if (should_close) {
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  }

  tryCatch({
    # BEGIN TRANSACTION
    dbExecute(conn, "BEGIN TRANSACTION")

    # Step 1: Validate
    validation <- can_convert_to_mode(conn, entity_id)
    if (!validation$valid) {
      dbExecute(conn, "ROLLBACK")
      log_warn("Conversion failed validation: {validation$reason}")
      return(FALSE)
    }

    # Store metadata for audit
    old_count <- validation$metadata$projected_count

    # Step 2: Delete projected data
    deleted <- dbExecute(conn, "
      DELETE FROM data_items
      WHERE entity_id = ? AND type = 'projected'
    ", params = list(entity_id))

    log_info("Deleted {deleted} projected items")

    # Step 3: Update state
    updated <- dbExecute(conn, "
      UPDATE entities
      SET mode = 'legacy',
          updated_at = ?
      WHERE id = ?
    ", params = list(Sys.time(), entity_id))

    if (updated == 0) {
      dbExecute(conn, "ROLLBACK")
      log_error("Failed to update entity state")
      return(FALSE)
    }

    # Step 4: Create audit log
    audit_entry <- tibble::tibble(
      audit_id = generate_id(),
      entity_id = entity_id,
      action = "converted_to_legacy",
      timestamp = Sys.time(),
      old_count = old_count,
      new_count = 0
    )

    dbWriteTable(conn, "audit_log", audit_entry, append = TRUE)

    # COMMIT - all steps succeeded
    dbExecute(conn, "COMMIT")
    log_info("Successfully converted entity {entity_id}")
    return(TRUE)

  }, error = function(e) {
    # ROLLBACK on any error
    tryCatch(dbExecute(conn, "ROLLBACK"), error = function(e2) NULL)
    log_error("Conversion failed: {e$message}")
    return(FALSE)
  })
}
```

---

### Step 4: UI Integration

**Button Rendering:**
```r
# Calculate visibility condition
can_show_button <-
  entity$mode == "dynamic" &&
  entity$status == "open" &&
  nrow(projected_data) > 0

# Conditional button
if (can_show_button) {
  tags$button(
    id = sprintf("convert_%s", entity$id),
    class = "btn btn-sm btn-info",
    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                      ns("convert_clicked"), entity$id),
    icon("archive"),
    " Convert to Legacy"
  )
}
```

**Click Handler:**
```r
observeEvent(input$convert_clicked, {
  entity_id <- input$convert_clicked

  # Get preview
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  preview <- preview_conversion(conn, entity_id)

  if (!preview$has_data) {
    showNotification("No data to convert", type = "warning")
    return()
  }

  # Build modal content
  event_list <- lapply(names(preview$by_category), function(cat) {
    info <- preview$by_category[[cat]]
    tags$li(sprintf("%s: %d items totaling %s",
                   cat, info$count, format_currency(info$amount)))
  })

  # Show confirmation modal
  showModal(modalDialog(
    title = "Convert to Legacy Mode?",
    tags$div(
      tags$h5("What will happen:"),
      tags$ul(
        tags$li("Mode changes from Dynamic to Legacy"),
        tags$li("All projected data permanently deleted"),
        tags$li("Actual data preserved"),
        tags$li("Manual tracking required going forward")
      ),
      tags$hr(),
      tags$h5("Data to be deleted:"),
      tags$ul(style = "color: #dc3545;", event_list),
      tags$p(sprintf("Total: %s", format_currency(preview$total_amount)),
             style = "font-weight: bold;"),
      tags$hr(),
      tags$div(
        class = "alert alert-warning",
        tags$strong("Warning: "),
        "This action cannot be undone."
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      tags$button(
        type = "button",
        class = "btn btn-info",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                          ns("confirm_convert"), entity_id),
        icon("check"),
        " Convert to Legacy"
      )
    ),
    size = "m",
    easyClose = FALSE
  ))
})
```

**Confirmation Handler:**
```r
observeEvent(input$confirm_convert, {
  entity_id <- input$confirm_convert

  # Execute conversion
  conn <- get_db_connection()
  result <- convert_to_mode(conn, entity_id)
  dbDisconnect(conn)

  if (result) {
    showNotification(
      "Successfully converted to Legacy mode",
      type = "message",
      duration = 5
    )

    # Trigger UI refresh
    entity_version(entity_version() + 1)
  } else {
    showNotification(
      "Conversion failed. Please check logs.",
      type = "error",
      duration = 5
    )
  }

  removeModal()
})
```

---

## Consequences

### Benefits

1. **Data Integrity**
   - Atomic transactions prevent partial conversions
   - Historical data preserved automatically
   - Audit trail for all transitions

2. **User Confidence**
   - Preview shows exact consequences
   - Clear warnings prevent accidents
   - Explicit confirmation required

3. **Maintainability**
   - Clear separation of concerns (validate/preview/execute)
   - Easy to test each component
   - Reusable pattern for similar features

4. **Flexibility**
   - Connection parameter supports batch operations
   - Structured return values support varied UI needs
   - Extensible (can add more validation rules)

### Drawbacks

1. **Complexity**
   - Three functions instead of one
   - More code to maintain
   - Transaction management overhead

2. **Irreversibility**
   - No built-in undo mechanism
   - Requires separate archive feature if needed
   - Users must be certain before converting

3. **Performance**
   - Preview query adds overhead
   - Transaction locks during conversion
   - Not suitable for mass batch operations (without modification)

### Trade-offs

**Simplicity vs Safety:**
- Could just delete and update in one query
- Preview + transaction adds complexity
- But prevents accidental data loss

**Flexibility vs Constraints:**
- Could allow partial conversions
- All-or-nothing approach simpler
- But less flexible for some use cases

---

## Testing Strategy

### Test Categories

**Validation Tests:**
- Valid entity → returns TRUE
- Invalid state → returns FALSE with reason
- Closed entity → returns FALSE with reason
- No data to remove → returns FALSE with reason
- Non-existent entity → returns FALSE with reason

**Preview Tests:**
- Single category → correct count/amount
- Multiple categories → breakdown accurate
- No data → has_data = FALSE
- Large amounts → precision maintained

**Conversion Tests:**
- Happy path → all steps succeed
- Validation failure → rollback occurs
- Update failure → rollback occurs
- Audit log created → entry correct

**Integration Tests:**
- End-to-end workflow → validate → preview → convert → verify

**Edge Cases:**
- Many data items (100+)
- Mixed actual and projected data
- Concurrent modifications
- Database connection failures

### Test Template

```r
test_that("Pattern: conversion succeeds for valid entity", {
  # Setup
  entity_id <- create_test_entity(mode = "dynamic", status = "open")
  create_test_data(entity_id, type = "projected", count = 5)
  create_test_data(entity_id, type = "actual", count = 2)

  # Validate
  validation <- can_convert_to_mode(conn, entity_id)
  expect_true(validation$valid)
  expect_equal(validation$metadata$projected_count, 5)

  # Preview
  preview <- preview_conversion(conn, entity_id)
  expect_true(preview$has_data)
  expect_equal(preview$total_count, 5)

  # Convert
  result <- convert_to_mode(conn, entity_id)
  expect_true(result)

  # Verify
  entity <- get_entity(entity_id)
  expect_equal(entity$mode, "legacy")

  data <- get_entity_data(entity_id)
  expect_equal(nrow(data %>% filter(type == "projected")), 0)
  expect_equal(nrow(data %>% filter(type == "actual")), 2)

  audit <- get_latest_audit(entity_id)
  expect_equal(audit$action, "converted_to_legacy")
  expect_equal(audit$old_count, 5)
  expect_equal(audit$new_count, 0)

  # Cleanup
  cleanup_test_entity(entity_id)
})
```

---

## Related Patterns

### Similar Patterns

**Soft Delete Pattern:**
- Also removes data visibility without hard delete
- Different: doesn't change mode, just marks deleted
- Use when: data might be restored later

**Archive Pattern:**
- Moves data to archive table
- Different: preserves deleted data
- Use when: data has regulatory retention requirements

**State Machine Pattern:**
- Manages complex state transitions
- Different: handles multiple states and transitions
- Use when: many possible state changes

### Pattern Combinations

**With Command Pattern:**
- Wrap conversion in command object
- Enables undo functionality
- Store deleted data in command for restoration

**With Observer Pattern:**
- Notify listeners when conversion occurs
- Trigger side effects (emails, analytics)
- Decouple conversion from notifications

**With Strategy Pattern:**
- Different conversion strategies for different entity types
- Common interface, varied implementations
- Use when: multiple conversion types needed

---

## Examples in investR

### Convert to Legacy Covered Call

**Context:** Dynamic Covered Calls positions project future dividends and option gains. When call expires worthless, projections are invalid.

**Implementation:**
- **Validation:** `can_convert_to_legacy()` - checks strategy type, status, projected events
- **Preview:** `preview_legacy_conversion()` - shows breakdown by event type
- **Conversion:** `convert_to_legacy_covered_call()` - deletes projections, updates strategy type
- **Audit:** Logs to `projection_recalculations` table

**Files:**
- `R/fct_portfolio_groups_database.R` (functions)
- `R/utils_group_cards.R` (button rendering)
- `R/mod_portfolio_groups_cards.R` (event handlers)

---

## Known Uses

### Other Potential Applications in investR

1. **Watchlist to Portfolio**
   - Convert watchlist items to portfolio positions
   - Remove projected returns, add actual holdings

2. **Auto to Manual Rebalancing**
   - Switch from automatic rebalancing to manual
   - Remove rebalance schedules, preserve history

3. **Tracked to Ignored**
   - Stop tracking position performance
   - Archive tracking data, update status

4. **Systematic to Opportunistic**
   - Change from rules-based to discretionary strategy
   - Remove automatic triggers, preserve trades

---

## Checklist for Applying Pattern

**Design Phase:**
- [ ] Identify current and target states
- [ ] List data to be removed during conversion
- [ ] List data to be preserved
- [ ] Define validation rules
- [ ] Design preview data structure
- [ ] Plan audit log schema

**Implementation Phase:**
- [ ] Create validation function with structured return
- [ ] Create preview function with category breakdown
- [ ] Create conversion function with transaction safety
- [ ] Add audit logging
- [ ] Implement UI button with conditional visibility
- [ ] Implement confirmation modal with preview display
- [ ] Add success/error notifications

**Testing Phase:**
- [ ] Test all validation rules
- [ ] Test preview accuracy
- [ ] Test successful conversion
- [ ] Test conversion failures (rollback)
- [ ] Test edge cases (empty, large, mixed data)
- [ ] Test transaction safety (atomicity)
- [ ] Integration test (end-to-end workflow)

**Documentation Phase:**
- [ ] User guide explaining when and how to convert
- [ ] Technical docs with function specifications
- [ ] Pattern documentation for future reuse
- [ ] Code comments for complex logic

---

## References

### Pattern Sources
- Martin Fowler - "Patterns of Enterprise Application Architecture"
- Gang of Four - "Design Patterns"
- Domain-Driven Design - State transitions

### Related Documentation
- [Convert to Legacy User Guide](../user-guide/convert-to-legacy-covered-call.md)
- [Technical Architecture](../technical/convert-to-legacy-architecture.md)
- [PDCA Documentation](../pdca/convert-to-legacy/)

### Code Examples
- `R/fct_portfolio_groups_database.R` - Reference implementation
- `tests/testthat/test-fct_convert_to_legacy.R` - Test examples
