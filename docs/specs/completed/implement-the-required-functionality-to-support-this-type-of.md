# Spec: Covered Call Roll Transaction Support

**Started:** 2026-04-10
**Status:** Ready for Implementation

---

## Overview

Support the "covered call roll" transaction pattern in investR. A roll consists of two option trades: a Buy-to-Close (BTC) on an existing short call and a Sell-to-Open (STO) on a new call with a later expiry date. The triggering case is the WMT Dividend Aristocrat position, which was rolled from a June 2027 covered call to a December 2027 or January 2028 covered call.

Research (April 9, 2026) confirmed that roll detection, member symbol update, and option gain projection regeneration already exist and work. The primary gap is that **dividend projections are not extended after a roll**. Two secondary gaps were also identified and are in scope.

---

## Constraints

**Files to Modify:**
- `R/fct_income_projection_engine.R` — extend `regenerate_projections_after_roll` to also regenerate dividend projections
- `R/fct_activities_database.R` — call `enrich_blank_symbols_unlinked()` before roll detection during manual linking
- `R/utils_activity_linking.R` — same blank symbol enrichment call during batch linking

**Files NOT to Modify:**
- Any `mod_*.R` or `page_*.R` files — no UI changes required
- Database schema — no new tables, no schema migrations
- `utils_transaction_helpers.R` — `detect_option_roll` logic is correct as-is; same-day requirement is acceptable

**New Files Allowed:**
- Test file only: `tests/testthat/test-fct_roll_transactions.R`

**New Dependencies Allowed:**
- None

**Existing Code to Reuse:**
- `generate_dividend_events()` in `fct_income_projection_engine.R` — already generates dividend projections; call it from `regenerate_projections_after_roll`
- `delete_projected_cash_flows_by_month()` — NOT the right tool; use a new delete-all-projected-dividends call (see below)
- `delete_projected_option_gains()` in `fct_income_projection_database.R` — pattern to follow for dividend deletion
- `enrich_blank_symbols_unlinked()` in `fct_activities_database.R` — already exists, just not called during manual link
- `setup.R` / `teardown.R` in `tests/testthat/` — already creates isolated temp databases; tests MUST use this pattern

**Out of Scope:**
- UI changes of any kind
- Handling rolls where BTC and STO are on different calendar days (same-day requirement is acceptable)
- Any changes to how BTC or STO actual cash flows are recorded (already works correctly)
- Risk dashboard or home dashboard changes (they pull from group data on the fly; no special handling needed)
- Changes to any strategy other than covered call strategies that use projections

---

## Problem Statement

When a covered call is rolled (BTC old call + STO new call linked to the same group on the same day), the following currently happens automatically and correctly:

- The group's tracked short call symbol is updated to the new option (new expiry encoded in symbol)
- The projected option gain at expiry is deleted and regenerated using the new expiry date and accumulated premiums
- The BTC cost appears as a negative actual cash flow for the month the trade occurred
- The STO credit appears as a positive actual cash flow for the month the trade occurred

The following does **NOT** happen and must be built:

- **Dividend projections are NOT extended.** `regenerate_projections_after_roll` only handles option gain, not dividends. After a roll, the dividend projection rows still project through the old expiry date — the extended period has no dividend projections.

Secondary gaps (in scope):

- **Blank option symbol skips roll detection.** If Questrade delivers the STO with a blank symbol, `is_option_symbol()` returns FALSE and roll detection silently skips. `enrich_blank_symbols_unlinked()` already fixes blank symbols during Questrade sync, but is not called during manual activity linking.

---

## User Stories

### US-1: Dividend projections extend after a covered call roll

**Description:** When a covered call roll is detected (BTC old call + STO new call linked to the same group on the same day), dividend projections are deleted and regenerated through the new option's expiry date.

**Acceptance Criteria:**
- [ ] After linking a BTC and STO for the same underlying on the same day, all projected dividend rows (`event_type = 'dividend'`, `status = 'projected'`) for the group are deleted and regenerated through the new expiry date
- [ ] The dividend regeneration uses the same `generate_dividend_events()` function used during initial group projection creation
- [ ] If the dividend staleness check fails (dividend appears suspended), no dividend rows are generated and a warning is logged — same behavior as initial projection
- [ ] Existing actual dividend rows (`status = 'actual'`) are never deleted
- [ ] The option gain projection continues to be regenerated correctly (existing behavior preserved)
- [ ] `devtools::test()` passes with no regressions

### US-2: Blank option symbol does not silently skip roll detection during manual linking

**Description:** When a user manually links activities to a group and the STO transaction has a blank symbol field from Questrade, the app attempts to enrich the blank symbol before running roll detection.

**Acceptance Criteria:**
- [ ] `enrich_blank_symbols_unlinked()` is called (or its enrichment logic is applied) before `detect_option_roll` is invoked in both `link_activity_to_group` (`fct_activities_database.R`) and the batch linking path in `utils_activity_linking.R`
- [ ] If symbol enrichment succeeds, roll detection proceeds normally
- [ ] If symbol enrichment fails (symbol remains blank), roll detection skips gracefully with a warning log — no error thrown
- [ ] `devtools::test()` passes with no regressions

### US-3: Roll transaction test coverage

**Description:** Automated tests verify the full roll flow using isolated databases (no production data touched).

**Acceptance Criteria:**
- [ ] `tests/testthat/test-fct_roll_transactions.R` exists
- [ ] Test creates an isolated temp SQLite database (using existing `setup.R` pattern — `withr::local_db_connection` or equivalent)
- [ ] Test creates a WMT Dividend Aristocrat group with a June 2027 covered call and initial dividend projections through June 2027
- [ ] Test links a BTC (June 2027 call, Buy action) and STO (December 2027 call, Sell action) on the same trade_date
- [ ] After linking, test asserts: projected dividend rows exist with `event_date` beyond June 2027 (through approximately December 2027)
- [ ] After linking, test asserts: no projected dividend rows exist with `event_date` before today (stale projections cleaned up)
- [ ] After linking, test asserts: projected option gain row reflects December 2027 expiry
- [ ] After linking, test asserts: two actual cash flow rows exist — one negative (BTC cost) and one positive (STO credit) — both with `status = 'actual'`
- [ ] Production database (`inst/database/portfolio.sqlite`) is never opened or modified by the test
- [ ] `devtools::test()` passes

---

## Technical Design

### Gap 1: Dividend regeneration in `regenerate_projections_after_roll`

**File:** `R/fct_income_projection_engine.R`

**Change:** After the option gain projection is regenerated (currently the last step in the function), add a dividend regeneration step:

1. Look up the underlying stock ticker from `position_group_members` where `role = 'underlying_stock'`
2. Delete all projected dividend rows for the group — follow the same pattern as `delete_projected_option_gains()` but filter on `event_type = 'dividend'` and `status = 'projected'`
3. Call `generate_dividend_events(ticker, shares, end_date = expiry_date)` — `expiry_date` is already parsed from the new option symbol earlier in the function
4. Save each resulting dividend event to `position_group_cash_flows` with `status = 'projected'` and `confidence = 'medium'` (same as initial projection)
5. Log the count of new dividend rows generated

The `shares` variable is already computed earlier in `regenerate_projections_after_roll` (from stock purchase activities). The `expiry_date` is also already parsed. No new database queries are needed beyond fetching the ticker.

**Pattern to follow:** The initial projection path in `generate_group_projections()` (around line 119) calls `generate_dividend_events()` and saves results. Follow that exact pattern, adapted for the roll context.

### Gap 2: Blank symbol enrichment before roll detection

**Files:** `R/fct_activities_database.R` (line ~562) and `R/utils_activity_linking.R` (line ~360)

**Change:** In both places where `detect_option_roll` is called:

Before the `is_option_symbol(full_activity$symbol)` check that guards roll detection, call `enrich_blank_symbols_unlinked()` (or apply equivalent enrichment for the specific activity). If after enrichment the symbol is still blank or not an option symbol, skip roll detection gracefully with a `log_warn`.

**Important:** `enrich_blank_symbols_unlinked()` currently operates across all unlinked activities. In the manual linking context, the activity is already being linked. The implementer should check whether calling the full function is safe here, or whether a targeted single-activity enrichment is needed. If the full function has side effects beyond the current activity, a targeted approach is preferred.

### Data model impact

No schema changes. All changes are in R business logic. Existing tables used:
- `position_group_cash_flows` — dividend rows added/deleted here
- `position_group_members` — read to get underlying ticker
- `account_activities` — read to get trade details
- `projection_recalculations` — existing audit log should be updated to include dividend count (already done for option gain)

---

## Integration Points

- `generate_dividend_events()` — already handles dividend staleness check internally; no changes needed
- `save_cash_flow_event()` — used to persist each dividend row; no changes needed
- `log_projection_recalculation()` — update the call in `regenerate_projections_after_roll` to also pass dividend count for audit trail

---

## Edge Cases

| Case | Expected Behavior |
|------|-------------------|
| Dividend history fetch fails during roll | Log error, skip dividend regeneration, return FALSE |
| Dividend appears stale (suspended) | Log warning, no dividend rows generated, option gain still regenerated |
| STO symbol blank after enrichment attempt | Log warning, skip roll detection, BTC/STO still recorded as actual cash flows |
| Roll detected but no stock buy in group | `regenerate_projections_after_roll` already returns FALSE in this case — no change needed |
| Strategy type not in projection list (e.g., Legacy Covered Call) | Already handled — function returns early for these strategies |
| Multiple rolls on same group | Each roll wipes and regenerates all projected dividends — correct by KISS/DRY principle |

---

## Implementation Phases

### Phase 1: Dividend projection regeneration after roll

Modify `R/fct_income_projection_engine.R` — extend `regenerate_projections_after_roll` to:
1. Fetch the underlying ticker from `position_group_members`
2. Delete all projected dividend rows for the group
3. Call `generate_dividend_events()` with new expiry date
4. Save dividend events to `position_group_cash_flows`
5. Log dividend count alongside option gain in `log_projection_recalculation`

**Verification:** `devtools::test()` — no regressions. Manually inspect that WMT group shows dividend projections through December 2027 after linking the roll transactions.

### Phase 2: Blank symbol enrichment before roll detection

Modify `R/fct_activities_database.R` and `R/utils_activity_linking.R` to call enrichment before roll detection guard.

**Verification:** `devtools::test()` — no regressions. Log output shows enrichment attempt when a blank-symbol activity is linked.

### Phase 3: Test coverage

Write `tests/testthat/test-fct_roll_transactions.R` covering the full roll flow with isolated temp database.

**Verification:** `devtools::test()` — all tests in the new file pass. `devtools::check()` passes cleanly.

---

## Definition of Done

- [ ] All acceptance criteria in US-1, US-2, US-3 pass
- [ ] `devtools::test()` passes with no regressions
- [ ] `devtools::check()` passes with no new warnings or errors
- [ ] Production database (`inst/database/portfolio.sqlite`) untouched by tests
- [ ] WMT Dividend Aristocrat group card shows dividend projections through the new expiry date after the real transactions are ingested

---

## Verification Commands

```r
devtools::test()
devtools::check()
devtools::load_all()
```

---

## Open Questions / Implementation Notes

- Implementer must verify whether calling `enrich_blank_symbols_unlinked()` in the manual linking path has unintended side effects (it may touch all unlinked activities, not just the one being linked). If so, extract the single-activity enrichment logic into a targeted helper.
- The `generate_dividend_events()` function makes an external call to fetch dividend history. In tests, this must be mocked (or a fixture used) to avoid hitting real APIs and to ensure test isolation.
