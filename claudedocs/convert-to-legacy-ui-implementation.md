# Convert to Legacy Covered Call - UI Implementation

## Summary

Successfully implemented UI components for the "Convert to Legacy Covered Call" feature in the R Shiny application. This allows users to convert Dynamic Covered Call positions to manual tracking mode when projected cash flows become invalid (e.g., when a call option expires worthless).

## Files Modified

### 1. R/utils_group_cards.R

**Changes**: Added conditional "Convert to Legacy" button to open group cards

**Location**: Lines 485-521 (action buttons section of `create_open_group_card()`)

**Implementation**:
- Added `can_convert_to_legacy` logic that checks:
  - Strategy type is "Dynamic Covered Calls"
  - Has projected cash flows (status == "projected")
- Button specifications:
  - ID pattern: `convert_legacy_<group_id>`
  - Label: "Convert to Legacy"
  - Icon: `icon("archive")`
  - Style: `btn btn-sm btn-info`
  - Positioned between "Analyze Risk" and "Close Group" buttons
- Button only renders when conversion criteria are met

### 2. R/mod_portfolio_groups_cards.R

**Changes**: Added two event handlers for the conversion workflow

**Location**: Lines 779-915 (after Edit Members handlers, before module closure)

#### Handler 1: Button Click (`convert_legacy_clicked`)
**Triggers**: When "Convert to Legacy" button is clicked

**Functionality**:
1. Gets database connection
2. Calls `preview_legacy_conversion(conn, group_id)` to fetch preview data
3. Validates preview data exists
4. Builds modal content showing:
   - What will happen (strategy change, projections deleted)
   - Detailed list of events to be deleted (by type with counts and amounts)
   - Total projected amount
   - Warning about irreversibility
   - Explanation of "Legacy Covered Call" mode
5. Shows confirmation modal using `showModal()`

**Modal Structure**:
- Title: "Convert to Legacy Covered Call?" with archive icon
- Body sections:
  - "What will happen" - explains conversion effects
  - "Events to be deleted" - detailed breakdown by event type
  - Warning alert - irreversibility notice
  - "What is Legacy Covered Call mode?" - educational explanation
- Footer: [Cancel] [Convert to Legacy] buttons

#### Handler 2: Confirmation (`confirm_convert_legacy`)
**Triggers**: When user clicks "Convert to Legacy" in confirmation modal

**Functionality**:
1. Gets database connection
2. Calls `convert_to_legacy_covered_call(conn, group_id)` to execute conversion
3. Shows success notification on completion
4. Increments `card_version()` to force card re-render (updates button visibility)
5. Shows error notification on failure
6. Removes modal using `removeModal()`

## Backend Integration

The UI components integrate with these database functions from `R/fct_portfolio_groups_database.R`:

1. **`can_convert_to_legacy(conn, group_id)`**
   - Validates conversion eligibility
   - Returns list with `valid` boolean and either `reason` or `projected_count`/`projected_amount`

2. **`preview_legacy_conversion(conn, group_id)`**
   - Returns detailed breakdown of projected events by type
   - Structure: `list(has_events, total_count, total_amount, by_type)`

3. **`convert_to_legacy_covered_call(conn, group_id)`**
   - Executes atomic conversion (transaction-based)
   - Deletes projected cash flows
   - Updates strategy_type to "Legacy Covered Call"
   - Logs conversion in projection_recalculations table
   - Returns TRUE on success, FALSE on failure

## UI/UX Features

### Button Visibility
- Only shows for "Dynamic Covered Calls" strategy groups
- Only shows when projected cash flows exist
- Automatically hidden after successful conversion

### Modal Design
- Info color scheme (blue) to distinguish from destructive actions
- Clear warning styling for events to be deleted (red text)
- Educational content explaining Legacy mode
- Cannot be dismissed by clicking outside (easyClose = FALSE)

### User Feedback
- Console logging for debugging
- Success notification with confirmation message
- Error notification with actionable message
- Automatic card refresh to reflect new state

### Accessibility
- Proper button labels and icons
- Clear modal structure with headings
- Warning alerts for destructive actions
- Descriptive notification messages

## Testing Checklist

- [ ] Button appears only for Dynamic Covered Calls groups
- [ ] Button hidden when no projected cash flows exist
- [ ] Button hidden after successful conversion
- [ ] Modal displays correct event breakdown
- [ ] Modal shows accurate total projected amount
- [ ] Cancel button closes modal without changes
- [ ] Convert button executes conversion successfully
- [ ] Card refreshes to show "Legacy Covered Call" strategy
- [ ] Success notification appears with correct message
- [ ] Error handling works for failed conversions
- [ ] Database transaction rollback works on errors

## Code Quality

- Follows existing Shiny/Golem patterns from codebase
- Uses reactive programming model (observeEvent with ignoreInit = TRUE)
- Matches UI styling and component patterns
- Includes proper error handling
- Uses logger for debugging
- Proper accessibility (button labels, modal structure)
- Connection management with on.exit() cleanup
- Follows tidyverse coding style

## Future Enhancements

Potential improvements for future iterations:

1. Add loading spinner during conversion
2. Add option to download deleted projections as CSV before conversion
3. Show comparison of before/after metrics
4. Add undo feature (time-limited reversal window)
5. Batch conversion for multiple groups
