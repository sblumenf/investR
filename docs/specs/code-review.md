# Specification: Full Codebase Audit — investR

## Overview

A systematic, full-codebase audit of the investR Shiny application. The audit is organized into 9 strategy-based file-group batches, each producing a separate findings document. A master summary aggregates all findings with statistics. The primary output is a prioritized task list of issues, weighted by severity with data integrity concerns ranked highest.

## Goals

1. **Tech debt inventory:** Catalog all code quality issues across the codebase, categorized by severity and dimension.
2. **Quality baseline:** Establish current quality metrics (findings per severity, per dimension, per file group).
3. **Quality gates:** Propose ongoing coding guidelines for new code, documented as a guidelines reference.
4. **Dead code identification:** Flag functions that exist in R/ but are never called from any module, page, or other function.

## Scope

### In Scope
- All files under `R/` (~130 files: fct_*.R, mod_*.R, page_*.R, utils_*.R, app_*.R, run_app.R)
- Configuration files: `golem-config.yml`, `inst/` contents
- Actively-used root scripts (audit_all_groups.R, diagnose_app.R)
- `DESCRIPTION`, `NAMESPACE`

### Out of Scope
- Test files (`tests/testthat/`) — test review is excluded
- One-off investigation scripts (`investigate_*.R`)
- `conversion_plans/` directory
- `money_market_analysis/` directory (unless files are imported by package code)
- Documentation files (*.md)
- `claudedocs/` directory

## Review Dimensions

Each dimension has a weight that determines the severity floor for findings in that category.

| Dimension | Weight | Description |
|-----------|--------|-------------|
| Correctness | Critical | Calculation accuracy, logic errors. Spot-check key pricing formulas (Black-Scholes, Greeks via RQuantLib) against theory. Focus on implementation correctness: wrong variables, type coercion, NA handling, edge cases. |
| Data Integrity | Critical | Database operations, transaction safety, write paths. Any code that writes to DuckDB, modifies portfolio state, or produces financial calculations used for investment decisions. |
| Error Handling | High | API failures, fallback behavior, silent failures. How the app behaves when Questrade API fails, Yahoo Finance fallback activates, or data is missing/malformed. |
| Maintainability | Medium | Code duplication, dead code, unclear patterns, unused functions. |
| Performance | Medium | Unnecessary computation, N+1 queries, redundant API calls. |
| Security | Medium | Token handling, credential storage, input validation. |
| Code Style | Low | Naming consistency, pattern adherence. |

## Per-File Review Checklist

Every file reviewed must be checked against these items (where applicable):

1. **NA/NULL handling** — Are calculations protected against NA/NULL inputs? Do they propagate NAs silently or handle them explicitly?
2. **Error propagation** — Does the code use tryCatch appropriately? Are errors silently swallowed? Does the user get informed when something fails?
3. **DB connection lifecycle** — Are connections closed with `on.exit(dbDisconnect(con))` or equivalent? Are transactions rolled back on error?
4. **Hardcoded values** — Are there magic numbers or strings that should come from `golem-config.yml` or `STRATEGY_CONFIG` objects?
5. **Option symbol parsing** — Is `parse_option_details()` used consistently? Is there any manual string parsing of option symbols?
6. **Shared risk engine** — Do all risk simulations use `shared_risk_engine.R`? Are there any files running their own simulation logic?
7. **Reactive isolation** — In modules (mod_*.R), are reactive expressions properly isolated? Are there reactive side effects leaking across namespaces?
8. **Dead/unreachable code** — Are there functions, branches, or code blocks that can never be reached? Functions exported but never called?

## Config Chain Verification

The audit must verify the 3-tier config system is used consistently:
- `golem-config.yml` → `STRATEGY_CONFIG` objects in `utils_*_config.R` → `get_golem_config_value()` with fallbacks
- Check for files that bypass this chain (direct environment variable reads, hardcoded defaults that differ from config, etc.)

## Batch Organization

### Batch 1: Cash-Secured Puts
**Files (11):**
- `fct_cash_secured_puts.R`
- `fct_sp500_cash_secured_puts.R`
- `fct_etf_cash_secured_puts_yfscreen.R`
- `mod_cash_secured_puts.R`
- `mod_cash_secured_puts_results_table.R`
- `mod_sp500_cash_secured_puts.R`
- `mod_etf_cash_secured_puts_yfscreen.R`
- `utils_cash_secured_puts_config.R`
- `page_cash_secured_puts.R`
- `page_sp500_cash_secured_puts.R`
- `page_etf_cash_secured_puts_yfscreen.R`

**Output:** `docs/audit/batch-01-csp.md`

### Batch 2: Covered Calls / Dynamic Covered Calls
**Files (9):**
- `fct_dynamic_covered_calls_analysis.R`
- `mod_dynamic_covered_calls_analysis.R`
- `mod_etf_covered_calls_analysis.R`
- `utils_covered_calls_shared.R`
- `utils_dynamic_covered_calls_config.R`
- `utils_dynamic_covered_calls_helpers.R`
- `utils_etf_covered_calls_config.R`
- `page_dynamic_covered_calls.R`
- `page_etf_covered_calls.R`

**Output:** `docs/audit/batch-02-covered-calls.md`

### Batch 3: Dividend Capture (weekly/monthly/high-yield)
**Files (20):**
- `fct_dividend_capture_weekly.R`
- `fct_dividend_capture_monthly.R`
- `fct_dividend_capture_monthly_high_yield.R`
- `fct_high_yield_dividend_capture.R`
- `mod_dividend_capture_weekly_controls.R`
- `mod_dividend_capture_weekly_results.R`
- `mod_dividend_capture_monthly_controls.R`
- `mod_dividend_capture_monthly_results.R`
- `mod_dividend_capture_monthly_high_yield_controls.R`
- `mod_dividend_capture_monthly_high_yield_results.R`
- `mod_high_yield_dividend_capture_controls.R`
- `mod_high_yield_dividend_capture_results.R`
- `utils_dividend_capture.R`
- `utils_dividend_capture_monthly_config.R`
- `utils_dividend_capture_monthly_high_yield_config.R`
- `utils_dividend_capture_weekly_config.R`
- `page_dividend_capture_weekly.R`
- `page_dividend_capture_monthly.R`
- `page_dividend_capture_monthly_high_yield.R`
- `page_dividend_capture_russell_2000.R`

**Output:** `docs/audit/batch-03-dividend-capture.md`

### Batch 4: Collars
**Files (6):**
- `fct_collar_analysis.R`
- `mod_collar_controls.R`
- `mod_collar_results.R`
- `utils_collar_config.R`
- `utils_collar_etf_universe.R`
- `page_collar.R`

**Output:** `docs/audit/batch-04-collars.md`

### Batch 5: Calendar Spreads
**Files (5):**
- `fct_put_calendar_spread.R`
- `mod_put_calendar_spread.R`
- `mod_put_calendar_spread_results_table.R`
- `utils_put_calendar_spread_config.R`
- `page_put_calendar_spread.R`

**Output:** `docs/audit/batch-05-calendar-spreads.md`

### Batch 6: Portfolio Core (DB, groups, risk, income, projections)
**Files (40):**
- `fct_portfolio_database.R`
- `fct_portfolio_groups_database.R`
- `fct_portfolio_groups_logic.R`
- `fct_portfolio_risk.R`
- `fct_portfolio_expected_return.R`
- `fct_income_projection_database.R`
- `fct_income_projection_engine.R`
- `fct_cash_flow_projection.R`
- `fct_activities_database.R`
- `fct_group_metrics.R`
- `fct_group_pnl.R`
- `fct_shared_risk_engine.R`
- `fct_risk_analysis.R`
- `fct_monte_carlo.R`
- `fct_lsm_engine.R`
- `fct_early_exercise.R`
- `fct_regime_detection.R`
- `fct_pattern_matching.R`
- `fct_suggestion_engine.R`
- `fct_suggestions_database.R`
- `mod_portfolio_groups.R`
- `mod_portfolio_groups_cards.R`
- `mod_portfolio_groups_dashboard.R`
- `mod_portfolio_return_summary.R`
- `mod_portfolio_risk_dashboard.R`
- `mod_position_risk.R`
- `mod_cash_flow_projection.R`
- `mod_raw_activities.R`
- `mod_review_transactions.R`
- `utils_portfolio_config.R`
- `utils_portfolio_groups_config.R`
- `utils_risk_config.R`
- `utils_risk_helpers.R`
- `utils_risk_presets.R`
- `utils_group_cards.R`
- `page_portfolio_groups.R`
- `page_portfolio_risk.R`
- `page_cash_flow_projection.R`
- `page_raw_activities.R`
- `page_home.R`

**Output:** `docs/audit/batch-06-portfolio-core.md`

### Batch 7: Questrade API + Market Data
**Files (12):**
- `fct_questrade_api.R`
- `fct_questrade_options.R`
- `fct_questrade_quotes.R`
- `fct_background_refresh.R`
- `fct_implied_volatility.R`
- `utils_questrade_healthcheck.R`
- `utils_quote_source_toggle.R`
- `utils_market_data.R`
- `utils_options_cache.R`
- `utils_sector_cache.R`
- `mod_token_settings.R`
- `page_token_settings.R`

**Output:** `docs/audit/batch-07-questrade-api.md`

### Batch 8: Aristocrats / Zero-Dividend / Other Analysis
**Files (24):**
- `fct_aristocrats_analysis.R`
- `fct_zero_dividend_analysis.R`
- `fct_etf_yfscreen_analysis.R`
- `fct_money_market_rotation.R`
- `mod_aristocrats_analysis.R`
- `mod_aristocrats_results_table.R`
- `mod_zero_dividend_analysis.R`
- `mod_zero_dividend_results_table.R`
- `mod_money_market_rotation.R`
- `mod_extrinsic_value_scanner.R`
- `mod_extrinsic_value_scanner_fct.R`
- `mod_extrinsic_value_scanner_controls.R`
- `mod_extrinsic_value_scanner_controls_fct.R`
- `utils_aristocrats_config.R`
- `utils_aristocrats_helpers.R`
- `utils_zero_dividend_config.R`
- `utils_yfscreen_etf.R`
- `utils_stock_universe.R`
- `utils_custom_ticker_lists.R`
- `page_aristocrats.R`
- `page_zero_dividend.R`
- `page_extrinsic_value_scanner.R`
- `page_money_market_rotation.R`

**Output:** `docs/audit/batch-08-other-analysis.md`

### Batch 9: UI Infrastructure + Config + Shared Utilities
**Files (17):**
- `app_config.R`
- `app_ui.R`
- `run_app.R`
- `utils_00_config.R`
- `utils_activity_linking.R`
- `utils_analysis_controls_helper.R`
- `utils_calculations.R`
- `utils_cash_equivalent.R`
- `utils_cash_equivalent_linking.R`
- `utils_explainability.R`
- `utils_formatting.R`
- `utils_globals.R`
- `utils_high_yield_capture_russell_2000.R`
- `utils_transaction_helpers.R`
- `utils_ui_components.R`
- `page_about.R`
- `_disable_autoload.R`

**Output:** `docs/audit/batch-09-ui-infra.md`

## Finding Format

Each per-batch document uses this structure:

```markdown
# Batch N: [Name] — Audit Findings

## Summary
- Files reviewed: X
- Findings: Y Critical, Z High, W Medium, V Low
- Checklist coverage: [which checklist items produced findings]

## Critical Findings
### CR-N-001: [Short title]
- **File:** `R/filename.R:123`
- **Dimension:** Correctness | Data Integrity | ...
- **Description:** [What the issue is]

## High Findings
### HI-N-001: [Short title]
...

## Medium Findings
### ME-N-001: [Short title]
...

## Low Findings
### LO-N-001: [Short title]
...

## Dead Code
### DC-N-001: [Function/block name]
- **File:** `R/filename.R:45`
- **Description:** [Why it's dead — never called, unreachable branch, etc.]
```

Finding IDs use the format: `{SEVERITY}-{BATCH}-{SEQ}` (e.g., `CR-6-003` = Critical finding #3 in Batch 6).

## Master Summary Format

`docs/audit/summary.md` aggregates all batches:

```markdown
# investR Codebase Audit — Master Summary

## Statistics
- Total files reviewed: X / Y total
- Total findings: N
  - Critical: A
  - High: B
  - Medium: C
  - Low: D
- Dead code items: E
- Checklist item hit rates: [table showing which items found issues most often]

## Findings by Batch
| Batch | Files | Critical | High | Medium | Low | Dead Code |
|-------|-------|----------|------|--------|-----|-----------|
| 1. CSP | 11 | ... | ... | ... | ... | ... |
| ... | | | | | | |

## Findings by Dimension
| Dimension | Critical | High | Medium | Low |
|-----------|----------|------|--------|-----|
| Correctness | ... | ... | ... | ... |
| ... | | | | |

## Top 10 Priority Items
[The 10 most impactful findings across all batches, ranked]

## Quality Gates (Proposed)
[Guidelines for new code based on patterns found in the audit]
```

## Quality Gates Deliverable

After the audit completes, a separate `docs/audit/quality-gates.md` document proposes guidelines:

- **fct_*.R functions:** Required patterns for error handling, NA checks, return types
- **DB operations:** Required connection lifecycle, transaction patterns, audit logging
- **API calls:** Required retry/fallback behavior, user notification on failure
- **Config usage:** When to use config vs hardcoded values, config chain adherence
- **Module patterns:** Reactive isolation requirements, namespace conventions

These are advisory guidelines (not enforced by tooling).

## Known Issues to Verify

The audit should verify this known issue is captured and check for similar patterns:
- **Questrade API silent stale data:** When token refresh fails mid-session, the app silently uses stale/cached data without informing the user. This is a data integrity concern.

## Execution Protocol

Each batch is executed as a separate Claude session:

1. Start a new session (or `/clear` between batches)
2. Load the spec: "Review batch N per docs/specs/code-review.md"
3. Read every file in the batch
4. Apply the 8-point checklist to each file
5. Categorize and severity-rate each finding
6. Write the per-batch findings document
7. After all 9 batches: compile the master summary

### Session Instructions for Each Batch

When executing a batch, the reviewer should:
- Read each file completely (not just symbols)
- For fct_*.R files: trace the data flow from input to output, checking calculations
- For mod_*.R files: verify reactive isolation and error handling in server functions
- For page_*.R files: verify module orchestration and routing
- For utils_*.R files: verify config chain adherence and helper correctness
- Cross-reference between fct/mod/page files within the batch for consistency
- Check for functions that are defined but never called (dead code candidates)
- Note any issues that span batches for the master summary

## Constraints

- **No code changes during audit.** The audit is read-only. All findings go into documents.
- **No test execution.** Tests are out of scope. Do not run `devtools::test()`.
- **All strategies reviewed equally.** No strategy gets a lighter review.
- **Findings are concise.** Each finding: severity + description + file:line reference. No reproduction steps or fix suggestions unless the finding would be unclear without them.
