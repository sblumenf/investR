# Specification Draft: ETF Collar Strategy Page

*Interview in progress - Started: 2026-03-03*

## Overview
New Brochure page at `/etf-collar` titled "ETF Collar Strategy (Synthetic Bonds)" that runs the existing collar analysis engine on a dynamically-screened ETF universe from yfscreen (Yahoo Finance ETF screener).

## Problem Statement
The existing collar strategy page uses static/manual ticker lists (S&P 500 stocks, curated ETF list, etc.). Users want to run the same collar analysis on a dynamically-filtered universe of ETFs, using the same yfscreen screening capabilities already available on the ETF Covered Calls page.

## Key Decisions Made
- **Controls UI**: Merged single panel — yfscreen filters (dividend type, yield range, AUM, top N) at top, collar params (target days, strike adjustment, workers) below, single "Run Analysis" button
- **Results module**: Reuse existing `mod_collar_results` as-is (no ETF-specific columns needed)
- **Business logic**: Thin wrapper in `fct_etf_collar_analysis.R` — calls `fetch_yfscreen_etfs()` then `analyze_collar_etfs()`
- **Route**: `/etf-collar`, Title: "ETF Collar Strategy (Synthetic Bonds)"
- **Navigation**: Placed immediately after existing collar strategy on home page

## Scope

### In Scope
- New Brochure page with yfscreen-filtered ETF collar analysis
- Controls module merging yfscreen filters + collar parameters
- Thin fct wrapper function
- Config object + golem-config.yml section
- Registration in run_app.R and home page navigation
- Quote source toggle (Questrade/Yahoo Finance)

### Out of Scope
- Changes to existing collar strategy page
- Changes to existing ETF covered calls page
- New analysis logic (reuses existing collar engine)
- ETF-specific result columns (AUM, expense ratio, etc.)

## Constraints

- **Files to Modify**: `R/run_app.R` (register page), `inst/golem-config.yml` (add config section), `R/page_home.R` (add nav card)
- **Files NOT to Modify**: `R/mod_collar_controls.R`, `R/mod_collar_results.R`, `R/fct_collar_analysis.R`, `R/mod_etf_covered_calls_analysis.R`, `R/fct_etf_yfscreen_analysis.R`, `R/utils_yfscreen_etf.R` (read-only reference; can call but not edit)
- **New Files Allowed**: Yes — `R/page_etf_collar.R`, `R/mod_etf_collar_controls.R`, `R/fct_etf_collar_analysis.R`, `R/utils_etf_collar_config.R`, `tests/testthat/test-fct_etf_collar_analysis.R`
- **New Dependencies Allowed**: No — use only existing packages in DESCRIPTION
- **Existing Code to Reuse**: `fetch_yfscreen_etfs()`, `analyze_collar_etfs()`, `mod_collar_results_ui/server`, `setup_analysis_controls()`, `quote_source_toggle_ui/server`, `ETF_SCREENER_CONFIG` patterns
- **Out of Scope**: Changes to existing collar page, changes to ETF covered calls page, new analysis algorithms, ETF-specific result columns

## User Stories

### US-1: Config and Business Logic Foundation
**Description:** Create the ETF collar config object and thin wrapper analysis function.

**Acceptance Criteria:**
- [ ] `utils_etf_collar_config.R` exports `ETF_COLLAR_CONFIG` list with max_workers, strike defaults
- [ ] `validate_etf_collar_config()` runs without error when config keys exist in golem-config.yml
- [ ] `fct_etf_collar_analysis.R` exports `analyze_etf_collar_yfscreen()` function
- [ ] Function signature accepts: dividend_filter, dividend_yield_range, min_market_cap, top_n, target_days, strike_adjustment, workers, use_questrade
- [ ] Function returns a tibble (even if empty when no ETFs match)
- [ ] Unit test passes: `devtools::test(filter = "etf_collar")`

### US-2: Controls Module
**Description:** Create the merged controls module with yfscreen filters + collar parameters.

**Acceptance Criteria:**
- [ ] `mod_etf_collar_controls_ui()` renders sidebar with: quote source toggle, dividend filter dropdown, conditional dividend yield range slider, min AUM dropdown, top N slider, target days slider, strike adjustment slider, workers slider, Run Analysis button
- [ ] `mod_etf_collar_controls_server()` returns `$results_data` (reactive) and `$status_ui` (reactive) via `setup_analysis_controls()`
- [ ] Conditional panel shows dividend yield slider only when "dividend_paying" selected
- [ ] All parameter defaults match existing collar page defaults
- [ ] Controls module loads without error in Shiny session

### US-3: Page Definition and Registration
**Description:** Create the Brochure page and wire everything together.

**Acceptance Criteria:**
- [ ] `page_etf_collar.R` defines `page_etf_collar()` returning a `brochure::page()` at href `/etf-collar`
- [ ] Page uses `mod_etf_collar_controls` for sidebar and `mod_collar_results` for main panel
- [ ] Page registered in `run_app.R` brochureApp() call
- [ ] `etf_collar` config section added to `golem-config.yml`
- [ ] `validate_etf_collar_config()` called in run_app.R startup
- [ ] Navigation card added to home page immediately after existing collar strategy
- [ ] App starts without error: `devtools::load_all(); run_app()` (manual verification)
- [ ] Navigating to `/etf-collar` shows the page with controls and empty results panel

## Technical Design

### Data Model
No new database tables. Uses existing collar analysis output format (tibble with columns: ticker, stock_price, put_strike, put_price, call_strike, call_price, net_cost, annualized_return, etc.).

### Integration Points
- `utils_yfscreen_etf.R` → `fetch_yfscreen_etfs()` for dynamic ETF screening
- `fct_collar_analysis.R` → `analyze_collar_etfs()` for collar analysis on ticker vector
- `mod_collar_results.R` → reused results display module
- `utils_analysis_controls_helper.R` → `setup_analysis_controls()` for run/progress/download pattern

## User Experience

### User Flow
1. User navigates to `/etf-collar` (from home page card or direct URL)
2. User configures yfscreen filters (dividend type, yield range, AUM, top N)
3. User configures collar params (target days, strike adjustment, workers)
4. User clicks "Run Analysis"
5. Progress indicator shows during analysis
6. Results table displays collar opportunities sorted by annualized return
7. User can download results as CSV

### Edge Cases
- yfscreen returns 0 ETFs → show notification, return empty results table
- yfscreen API unavailable → show error notification via existing error handling
- All ETFs lack options chains → results table shows 0 rows with status message
- User sets very high top_n (500) → analysis takes longer, progress bar tracks per-ticker

## Requirements

### Functional Requirements
- FR-1: Page fetches ETF universe dynamically from yfscreen based on user-selected filters
- FR-2: Leveraged/inverse ETFs are automatically excluded (existing `is_leveraged_or_inverse_etf()` logic)
- FR-3: Collar analysis uses same engine as existing collar page (`analyze_collar_etfs()`)
- FR-4: Results display in same format as existing collar results table
- FR-5: CSV download available for results
- FR-6: Quote source toggle (Questrade/Yahoo Finance) available

### Non-Functional Requirements
- NFR-1: Parallel processing via future/furrr with configurable workers (default 10)
- NFR-2: No new package dependencies
- NFR-3: Config validation at app startup prevents runtime config errors

## Implementation Phases

<!-- Break work into 2-4 incremental milestones Ralph can complete one at a time -->

### Phase 1: [Foundation/Setup]
- [ ] [Task 1]
- [ ] [Task 2]
- **Verification:** `[command to verify phase 1]`

### Phase 2: [Core Implementation]
- [ ] [Task 1]
- [ ] [Task 2]
- **Verification:** `[command to verify phase 2]`

### Phase 3: [Integration/Polish]
- [ ] [Task 1]
- [ ] [Task 2]
- **Verification:** `[command to verify phase 3]`

<!-- Add Phase 4 if needed for complex features -->

## Definition of Done

This feature is complete when:
- [ ] All acceptance criteria in user stories pass
- [ ] All implementation phases verified
- [ ] Tests pass: `[verification command]`
- [ ] Types/lint check: `[verification command]`
- [ ] Build succeeds: `[verification command]`

## Ralph Loop Command

<!-- Generated at finalization with phases and escape hatch -->

```bash
/ralph-loop "Implement help me implement this new strategy/page per spec at docs/specs/help-me-implement-this-new-strategypage.md

PHASES:
1. [Phase 1 name]: [tasks] - verify with [command]
2. [Phase 2 name]: [tasks] - verify with [command]
3. [Phase 3 name]: [tasks] - verify with [command]

VERIFICATION (run after each phase):
- [test command]
- [lint/typecheck command]
- [build command]

ESCAPE HATCH: After 20 iterations without progress:
- Document what's blocking in the spec file under 'Implementation Notes'
- List approaches attempted
- Stop and ask for human guidance

Output <promise>COMPLETE</promise> when all phases pass verification." --max-iterations 30 --completion-promise "COMPLETE"
```

## Open Questions
[To be filled during interview]

## Implementation Notes
[To be filled during interview]

---
*Interview notes will be accumulated below as the interview progresses*
---

