# Cash-Secured Puts Database Integration Design

## Executive Summary

This document details the minimal schema changes needed to support cash-secured put (CSP) position groups in the investR application's DuckDB database. The design leverages existing infrastructure for covered calls and requires only **configuration changes** with **zero database schema modifications**.

## Current Database Architecture

### Existing Tables

1. **position_groups** - Group metadata
   - `group_id` (PK), `group_name`, `strategy_type`, `account_number`
   - `status` ('open'/'closed'), `created_at`, `updated_at`
   - P&L columns: `total_return_pct`, `total_return_amount`, `annualized_return_pct`

2. **position_group_members** - Position-to-group mapping
   - `group_id` (FK), `account_number`, `symbol`, `role`
   - `allocated_quantity` (for stock splitting), `added_at`
   - Primary key: `(group_id, symbol)`

3. **position_group_cash_flows** - Cash flow events
   - `event_id` (PK), `group_id` (FK), `event_date`, `event_type`
   - `amount`, `status` ('projected'/'actual'), `confidence`
   - `created_at`, `updated_at`

4. **account_activities** - All transactions
   - `activity_id` (PK), `account_number`, `symbol`, `action`
   - `type` (transaction type), `quantity`, `price`, `net_amount`
   - `group_id` (FK nullable), `is_processed`, `trade_date`

### Current Strategy Types (inst/golem-config.yml:185-194)
```yaml
strategy_types:
  - "Dividend Aristocrats"
  - "Zero-Dividend Stocks"
  - "Dynamic Covered Calls"
  - "Legacy Covered Call"
  - "Collar Strategy"
  - "Weekly Dividend Capture"
  - "Monthly Dividend Capture"
  - "Russell 2000 High-Yield"
  - "Other"
```

### Current Position Roles (inst/golem-config.yml:195-197)
```yaml
position_roles:
  - "underlying_stock"
  - "short_call"
```

## Architectural Differences: CSP vs Covered Calls

| Aspect | Covered Call | Cash-Secured Put |
|--------|-------------|------------------|
| **Stock Position** | Own 100 shares | NO stock position |
| **Option Position** | Short CALL | Short PUT |
| **Cash Collateral** | None (shares as collateral) | Strike × 100 in cash |
| **Income** | Dividends + Call premium | Put premium only |
| **Projections** | Dividends (recurring) + Option gain | NO projections (realized immediately) |
| **Risk** | Capped upside, unlimited downside | Assignment at strike price |
| **Member Count** | 2 (stock + short_call) | 1 (short_put only) |

## Design Decision: No Schema Changes Required

### Key Insight
Cash-secured puts require **SIMPLER** database structure than covered calls:

1. **No stock member needed** - CSP groups have only the PUT option
2. **No dividend projections** - No underlying stock = no dividends
3. **No option gain projections** - Premium received immediately (actual event)
4. **Simpler cash flow tracking** - Single premium event when sold

### Schema Compatibility
Existing schema already supports CSP requirements:
- ✅ `position_group_members` allows single-member groups
- ✅ `position_group_cash_flows` supports actual-only events (no projections)
- ✅ `account_activities` tracks PUT transactions via `type` field

## Configuration Changes Required

### 1. Add CSP Strategy Type

**File**: `inst/golem-config.yml`
**Location**: Lines 185-194 (strategy_types list)

```yaml
strategy_types:
  - "Dividend Aristocrats"
  - "Zero-Dividend Stocks"
  - "Dynamic Covered Calls"
  - "Legacy Covered Call"
  - "Collar Strategy"
  - "Weekly Dividend Capture"
  - "Monthly Dividend Capture"
  - "Russell 2000 High-Yield"
  - "S&P 500 Cash-Secured Puts"        # NEW
  - "Other"
```

**Rationale**: Naming convention matches existing strategy "S&P 500 Cash-Secured Puts (Dividend-Paying)" from analysis UI.

### 2. Add short_put Position Role

**File**: `inst/golem-config.yml`
**Location**: Lines 195-197 (position_roles list)

```yaml
position_roles:
  - "underlying_stock"
  - "short_call"
  - "short_put"                         # NEW
```

**Rationale**: Distinguishes PUT options from CALL options in position_group_members table.

## Data Flow: CSP Position Lifecycle

### Phase 1: Position Creation (Sell PUT)

```
1. User executes PUT SELL transaction in Questrade
   ↓
2. Activity fetched via API → account_activities
   - type: "Options"
   - action: "Sell to open"
   - symbol: "AAPL15Jan26P220.00"
   - net_amount: +$450 (credit)
   ↓
3. User links transaction to new CSP group
   ↓
4. create_position_group() creates:

   position_groups:
   ┌─────────────┬────────────────────┬─────────────────────────────┐
   │ group_id    │ strategy_type      │ status                      │
   ├─────────────┼────────────────────┼─────────────────────────────┤
   │ CSP_AAPL_01 │ S&P 500 Cash-Sec.. │ open                        │
   └─────────────┴────────────────────┴─────────────────────────────┘

   position_group_members:
   ┌─────────────┬──────────────────────┬────────────┐
   │ group_id    │ symbol               │ role       │
   ├─────────────┼──────────────────────┼────────────┤
   │ CSP_AAPL_01 │ AAPL15Jan26P220.00   │ short_put  │
   └─────────────┴──────────────────────┴────────────┘

   position_group_cash_flows:
   ┌──────────┬─────────────┬────────────┬────────────┬────────┬────────┐
   │ event_id │ group_id    │ event_type │ event_date │ amount │ status │
   ├──────────┼─────────────┼────────────┼────────────┼────────┼────────┤
   │ EVENT_01 │ CSP_AAPL_01 │ put_premium│ 2025-11-26 │ 450.00 │ actual │
   └──────────┴─────────────┴────────────┴────────────┴────────┴────────┘

   account_activities:
   ┌──────────────┬─────────────┬────────────┐
   │ activity_id  │ group_id    │ is_processed│
   ├──────────────┼─────────────┼─────────────┤
   │ ACT_12345    │ CSP_AAPL_01 │ TRUE        │
   └──────────────┴─────────────┴─────────────┘
```

### Phase 2A: Position Expiry (PUT Expires Worthless)

```
Option expires → No transaction generated
↓
User closes group manually
↓
close_position_group(group_id):
  - Sets status = 'closed'
  - Calculates P&L:
    * total_return_amount = +$450 (premium kept)
    * total_return_pct = $450 / $22,000 = 2.05%
    * annualized_return = (2.05% / 60 days) × 365 = 12.5%
  - Deletes NO cash flows (actual event remains for history)
```

**Key Difference from Covered Calls**: No projected events to clean up.

### Phase 2B: Position Assignment (PUT Assigned)

```
Stock drops below strike → Assignment at expiry
↓
Questrade executes assignment → New transactions:
  1. BUY 100 shares @ strike ($220)
  2. Cash debit: -$22,000
↓
account_activities:
┌──────────────┬───────┬────────┬────────────┬──────────┬─────────────┐
│ activity_id  │ symbol│ action │ quantity   │net_amount│ group_id    │
├──────────────┼───────┼────────┼────────────┼──────────┼─────────────┤
│ ACT_67890    │ AAPL  │ Buy    │ 100        │ -22000   │ CSP_AAPL_01 │
└──────────────┴───────┴────────┴────────────┴──────────┴─────────────┘
↓
User options:
  a) Close CSP group (P&L = premium - assignment loss)
  b) Convert to Covered Call group (add short_call member)
```

### Phase 2C: Early Close (Buy to Close PUT)

```
User wants to exit early
↓
BUY transaction to close PUT
↓
account_activities:
┌──────────────┬──────────────────────┬────────┬──────────┬─────────────┐
│ activity_id  │ symbol               │ action │net_amount│ group_id    │
├──────────────┼──────────────────────┼────────┼──────────┼─────────────┤
│ ACT_99999    │ AAPL15Jan26P220.00   │ Buy    │ -200.00  │ CSP_AAPL_01 │
└──────────────┴──────────────────────┴────────┴──────────┴─────────────┘
↓
close_position_group(group_id):
  - P&L = +$450 (premium) - $200 (buy back) = +$250
```

## Cash Flow Event Types

### Existing Event Types
- `"dividend"` - Dividend payments (covered calls only)
- `"option_gain"` - Projected option expiry gains (covered calls only)
- `"option_premium"` - Option roll premiums (covered calls - see test line 609)

### New Event Type Required
- `"put_premium"` - Premium received from PUT sale

**Rationale**: Distinguishes PUT premiums from CALL premiums in reporting and analytics.

## Database Function Adaptations

### Functions Requiring NO Changes

All core database functions work unchanged for CSP:

1. **create_position_group()** (fct_portfolio_groups_database.R:223)
   - Already handles single-member groups
   - Already handles members without allocated_quantity
   - ✅ Works as-is

2. **save_cash_flow_event()** (fct_income_projection_database.R:101)
   - Already supports actual-only events (status = "actual")
   - ✅ Works as-is with new event_type = "put_premium"

3. **close_position_group()** (fct_group_pnl.R)
   - Already deletes only projected events (CSP has none)
   - Already calculates P&L from account_activities
   - ✅ Works as-is

4. **get_group_cash_flows()** (fct_income_projection_database.R:297)
   - Already filters by group_id
   - ✅ Works as-is

### Functions Requiring Conditional Logic

1. **Income Projection Calculation** (fct_cash_flow_projection.R:184)
   - Current: Excludes "Other" and "Legacy Covered Call" from projections
   - **Add**: Exclude "S&P 500 Cash-Secured Puts" (no recurring income)

```r
# Line 184-186: Add CSP to exclusion list
result <- result %>%
  filter(!strategy_type %in% c(
    "Other",
    "Legacy Covered Call",
    "S&P 500 Cash-Secured Puts"  # NEW - CSP has no projections
  ))
```

2. **Group Display Logic** (UI components)
   - Display logic must handle single-member groups
   - Role display: "short_put" instead of "short_call"
   - **No database changes needed** - UI adaptation only

## Validation Rules

### Group Creation Validation
```r
# Pseudo-code for CSP group validation
validate_csp_group <- function(members, strategy_type) {
  if (strategy_type == "S&P 500 Cash-Secured Puts") {
    # CSP groups must have exactly 1 member
    if (nrow(members) != 1) {
      stop("CSP groups must have exactly one PUT option member")
    }

    # Member must be short_put role
    if (members$role[1] != "short_put") {
      stop("CSP group member must have role = 'short_put'")
    }

    # Symbol must be PUT option (validation via naming pattern)
    if (!grepl("P\\d+\\.\\d+$", members$symbol[1])) {
      stop("CSP member symbol must be a PUT option")
    }
  }
}
```

### Cash Flow Validation
```r
# CSP cash flows are always actual (never projected)
if (strategy_type == "S&P 500 Cash-Secured Puts" &&
    event_type == "put_premium" &&
    status != "actual") {
  stop("CSP premium events must have status = 'actual'")
}
```

## Migration Strategy

### Phase 1: Configuration (Zero Risk)
1. Add "S&P 500 Cash-Secured Puts" to strategy_types list
2. Add "short_put" to position_roles list
3. Deploy configuration changes

**Impact**: None - existing data unaffected

### Phase 2: Code Adaptation (Low Risk)
1. Add projection exclusion for CSP strategy
2. Add UI display logic for single-member groups
3. Add validation helpers for CSP group creation

**Impact**: None - backward compatible additions

### Phase 3: Testing (Validation)
1. Create test CSP group with synthetic data
2. Verify cash flow tracking (actual premium)
3. Verify group closure with correct P&L
4. Verify assignment scenario handling

**Rollback**: Delete test groups via `unlink_position_group()`

### Phase 4: Production Rollout
1. Enable CSP analysis in UI
2. Allow users to create CSP groups
3. Monitor data quality and validation

**Safety**: All operations use existing proven infrastructure

## Comparison: CSP vs Covered Call Data Structures

### Covered Call Group Example
```
position_groups:
  group_id: "CC_TGT_01"
  strategy_type: "Dynamic Covered Calls"
  status: "open"

position_group_members:
  1. symbol: "TGT", role: "underlying_stock"
  2. symbol: "TGT15Jan27C75.00", role: "short_call"

position_group_cash_flows:
  1. event_type: "dividend", status: "projected", amount: $25.00
  2. event_type: "dividend", status: "projected", amount: $25.00
  3. event_type: "option_gain", status: "projected", amount: $450.00

account_activities (linked via group_id):
  - Stock purchase (BUY TGT)
  - Option sale (SELL TGT15Jan27C75.00)
  - Dividend receipts (actual)
```

### Cash-Secured Put Group Example
```
position_groups:
  group_id: "CSP_AAPL_01"
  strategy_type: "S&P 500 Cash-Secured Puts"
  status: "open"

position_group_members:
  1. symbol: "AAPL15Jan26P220.00", role: "short_put"

position_group_cash_flows:
  1. event_type: "put_premium", status: "actual", amount: $450.00

account_activities (linked via group_id):
  - Option sale (SELL AAPL15Jan26P220.00)
  [Additional: BUY if assigned, BUY if closed early]
```

## Benefits of This Design

### 1. Zero Schema Changes
- No ALTER TABLE migrations required
- No database downtime
- No data migration scripts
- Instant rollback capability (config revert)

### 2. Reuses Proven Infrastructure
- Same CRUD operations (create_position_group, close_position_group)
- Same cash flow tracking mechanism
- Same P&L calculation engine
- Same transaction linking logic

### 3. Simpler Than Covered Calls
- Fewer members per group (1 vs 2)
- No projection recalculations needed
- No dividend reconciliation logic
- Immediate P&L visibility (all actual events)

### 4. Clear Separation of Concerns
- Strategy type distinguishes CSP from other strategies
- Position role distinguishes PUT from CALL options
- Event type distinguishes PUT premium from CALL premium
- Single source of truth for each data element

### 5. Future Extensibility
- Framework ready for other single-leg strategies
- Pattern established for non-dividend strategies
- Cash flow model supports any event type
- Member model supports N-member groups (collars, spreads, etc.)

## Implementation Checklist

### Configuration Changes
- [ ] Add "S&P 500 Cash-Secured Puts" to inst/golem-config.yml strategy_types
- [ ] Add "short_put" to inst/golem-config.yml position_roles

### Code Adaptations
- [ ] Exclude CSP from projection calculations (fct_cash_flow_projection.R)
- [ ] Add CSP group validation helper
- [ ] Add UI display logic for short_put role
- [ ] Add "put_premium" event type to cash flow helpers

### Testing
- [ ] Unit test: Create CSP group with single short_put member
- [ ] Unit test: Save put_premium cash flow (actual status)
- [ ] Unit test: Close CSP group and verify P&L
- [ ] Integration test: Full lifecycle (create → premium → close)
- [ ] UI test: Display single-member group correctly

### Documentation
- [ ] Update user guide with CSP workflow
- [ ] Document CSP vs covered call differences
- [ ] Add CSP examples to code comments
- [ ] Create CSP troubleshooting guide

## Risk Assessment

### High Risk (None Identified)
No high-risk changes - all modifications are additive and backward compatible.

### Medium Risk
**Projection Exclusion Logic**
- Risk: Accidentally excluding other strategies from projections
- Mitigation: Explicit string matching, comprehensive testing
- Impact: Incorrect income projections in UI

### Low Risk
**Configuration Changes**
- Risk: Typos in strategy/role names
- Mitigation: Testing before production deployment
- Impact: UI dropdown values incorrect (easily fixed)

**UI Display Logic**
- Risk: Single-member group display formatting
- Mitigation: Preview in dev environment
- Impact: Cosmetic display issues only

## Performance Considerations

### Database Impact
- **Positive**: CSP groups have fewer rows than covered calls
  - 1 member vs 2 members (50% reduction)
  - 1 cash flow vs 3+ cash flows (66%+ reduction)
- **Neutral**: Same query patterns (indexed lookups)
- **Result**: Better database performance for CSP vs covered calls

### Query Optimization
No new indexes required - existing indexes handle CSP queries:
- `idx_cash_flows_group` - get_group_cash_flows()
- `idx_members_group` - get_group_members()
- `idx_activities_group` - P&L calculation

## Conclusion

Cash-secured put position groups integrate seamlessly into the existing investR database architecture with **zero schema changes**. The design leverages proven infrastructure from covered calls while actually requiring **less complexity** due to the simpler nature of CSP positions (no stock, no projections).

The minimal configuration changes (2 new enum values) enable full CSP support while maintaining complete backward compatibility and allowing instant rollback if needed.

This design embodies the principle of **maximum reuse, minimal change** - the hallmark of robust software architecture.

---

**Document Version**: 1.0
**Date**: 2025-11-26
**Author**: Backend Architect (Claude Code)
**Review Status**: Ready for Implementation
