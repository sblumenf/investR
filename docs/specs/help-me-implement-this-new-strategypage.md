# ETF Collar Strategy Page

## Constraints

- **Files to Modify**: `R/run_app.R` (register page + config validation), `inst/golem-config.yml` (add `etf_collar` config section), `R/page_home.R` (add nav card to `collar_strategies` list)
- **Files NOT to Modify**: `R/mod_collar_controls.R`, `R/mod_collar_results.R`, `R/fct_collar_analysis.R`, `R/mod_etf_covered_calls_analysis.R`, `R/fct_etf_yfscreen_analysis.R`, `R/utils_yfscreen_etf.R` (read-only reference; can call but not edit)
- **New Files Allowed**: Yes — `R/page_etf_collar.R`, `R/mod_etf_collar_controls.R`, `R/fct_etf_collar_analysis.R`, `R/utils_etf_collar_config.R`, `tests/testthat/test-fct_etf_collar_analysis.R`
- **New Dependencies Allowed**: No — use only existing packages in DESCRIPTION
- **Existing Code to Reuse**: `fetch_yfscreen_etfs()` from `utils_yfscreen_etf.R`, `analyze_collar_etfs()` from `fct_collar_analysis.R`, `mod_collar_results_ui/server` from `mod_collar_results.R`, `setup_analysis_controls()` from `utils_analysis_controls_helper.R`, `quote_source_toggle_ui/server` from their respective utils
- **Out of Scope**: Changes to existing collar page, changes to ETF covered calls page, new analysis algorithms, ETF-specific result columns (AUM, expense ratio)

## Overview

New Brochure page at `/etf-collar` titled "ETF Collar Strategy (Synthetic Bonds)" that runs the existing collar analysis engine on a dynamically-screened ETF universe from yfscreen (Yahoo Finance ETF screener).

The existing collar strategy page at `/collar` uses static/manual ticker lists (S&P 500 stocks, curated ETF list, Finviz screener, etc.). This new page applies the identical collar analysis logic but sources its ETF universe dynamically from yfscreen — the same screening system already used by the ETF Covered Calls page at `/etf-covered-calls`.

### Key Design Decisions

| Decision | Choice |
|----------|--------|
| Controls UI | Merged single panel — yfscreen filters at top, collar params below, single "Run Analysis" button |
| Results module | Reuse existing `mod_collar_results` as-is |
| Business logic | Thin wrapper in `fct_etf_collar_analysis.R` calling `fetch_yfscreen_etfs()` then `analyze_collar_etfs()` |
| Route | `/etf-collar` |
| Page title | "ETF Collar Strategy (Synthetic Bonds)" |
| Navigation | Immediately after existing collar strategy in home page `collar_strategies` accordion |
| Config | Own `etf_collar:` section in golem-config.yml with dedicated config object |
| Collar parameters | Same as existing collar page (target days, strike adjustment %, workers) |
| Empty results handling | Same as ETF Covered Calls — show notification, return empty table |
| Testing | Unit test for wrapper function + config validation |
| Dependencies | None new — all existing packages sufficient |

## Scope

### In Scope
- New Brochure page with yfscreen-filtered ETF collar analysis
- Controls module merging yfscreen filters + collar parameters
- Thin `fct_` wrapper function
- Config object + golem-config.yml section
- Registration in `run_app.R` and home page navigation
- Quote source toggle (Questrade/Yahoo Finance)
- Unit tests for business logic and config

### Out of Scope
- Changes to existing collar strategy page
- Changes to existing ETF covered calls page
- New analysis logic (reuses existing collar engine)
- ETF-specific result columns (AUM, expense ratio, etc.)
- Integration tests hitting real yfscreen API

## User Stories

### US-1: Config and Business Logic Foundation

**Description:** Create the ETF collar config object and thin wrapper analysis function so the core logic is in place before building the UI.

**Acceptance Criteria:**
- [ ] `R/utils_etf_collar_config.R` exists and exports `ETF_COLLAR_CONFIG` list with keys: `max_workers`, `min_net_credit`, `min_open_interest`, `max_stock_price`, `shares_per_contract`, `sgov_yield_default`, `negative_return_threshold`
- [ ] `get_etf_collar_config(key)` function works identically to `get_collar_config(key)` pattern
- [ ] `validate_etf_collar_config()` runs without error when `etf_collar` section exists in `golem-config.yml`
- [ ] `etf_collar:` section added to `inst/golem-config.yml` with same defaults as `collar:` section (max_workers: 10, etc.)
- [ ] `R/fct_etf_collar_analysis.R` exists and exports `analyze_etf_collar_yfscreen()` function
- [ ] Function signature: `analyze_etf_collar_yfscreen(dividend_filter = "all", dividend_yield_min = 2, dividend_yield_max = 6, min_market_cap = 0, top_n = 50, target_days = 45, strike_adjustment_pct = 0, max_workers = ETF_COLLAR_CONFIG$max_workers, use_questrade = FALSE)`
- [ ] Function calls `fetch_yfscreen_etfs()` to get ticker vector, then passes tickers to `analyze_collar_etfs()`
- [ ] Function returns a tibble (empty tibble if no ETFs match filters)
- [ ] `tests/testthat/test-fct_etf_collar_analysis.R` exists with mocked tests
- [ ] `devtools::test(filter = "etf_collar")` passes

### US-2: Controls Module

**Description:** Create the merged controls module with yfscreen filters at top and collar parameters below. Uses `setup_analysis_controls()` helper for the run/progress/download pattern.

**Acceptance Criteria:**
- [ ] `R/mod_etf_collar_controls.R` exists with `mod_etf_collar_controls_ui(id)` and `mod_etf_collar_controls_server(id)`
- [ ] UI sidebar (width=3) contains in order:
  1. `h3("Strategy Parameters")`
  2. Quote source toggle via `quote_source_toggle_ui(ns)`
  3. `hr()`
  4. `h4("ETF Screening Criteria")`
  5. Dividend filter `selectInput` (choices: "All ETFs" = "all", "Dividend-paying only" = "dividend_paying", "Zero-dividend only" = "zero_dividend"; default "all")
  6. Conditional dividend yield range `sliderInput` (0-10%, default 2-6%, step 0.5) — shown only when "dividend_paying" selected
  7. Min ETF size `selectInput` (same choices as `mod_etf_covered_calls_analysis.R`: No minimum through $100 Billion; default "0")
  8. Top N ETFs `sliderInput` (10-500, default 50, step 10)
  9. `hr()`
  10. `h4("Collar Parameters")`
  11. Target days to expiry `sliderInput` (same range/default as existing collar controls)
  12. OTM strike adjustment % `sliderInput` (same range/default as existing collar controls)
  13. Parallel workers `sliderInput` (1-20, default from config)
  14. Run Analysis `actionButton`
- [ ] Server calls `quote_source_toggle_server(input, session, "ETF Collar Strategy")`
- [ ] Server creates analysis function that calls `analyze_etf_collar_yfscreen()` with all input values
- [ ] Server uses `setup_analysis_controls()` returning `$results_data` (reactive) and `$status_ui` (reactive)
- [ ] `conditionalPanel` correctly shows/hides dividend yield slider based on dividend_filter input

### US-3: Page Definition and Registration

**Description:** Create the Brochure page definition, register it in `run_app.R`, add config validation, and add home page navigation.

**Acceptance Criteria:**
- [ ] `R/page_etf_collar.R` exists with `page_etf_collar()` function
- [ ] Function returns `brochure::page(href = "/etf-collar", ui = ..., server = ...)`
- [ ] UI: `fluidPage` with `titlePanel("ETF Collar Strategy (Synthetic Bonds)")`, `sidebarLayout` using `mod_etf_collar_controls_ui("controls")` for sidebar, `mainPanel(width = 9)` with `uiOutput("controls_status")` and `mod_collar_results_ui("results")`
- [ ] Server: calls `mod_etf_collar_controls_server("controls")`, passes `controls$results_data` to `mod_collar_results_server("results", ...)`, renders `controls$status_ui()` to `output$controls_status`
- [ ] `page_etf_collar()` added to `brochureApp()` in `R/run_app.R` immediately after `page_collar()` (after line 121)
- [ ] `validate_etf_collar_config()` added to startup validation block in `R/run_app.R` (after line 69)
- [ ] New entry added to `R/page_home.R` in `collar_strategies` list (after existing collar entry, around line 69):
  ```r
  list(
    title = "ETF Collar Strategy (Synthetic Bonds)",
    description = c(
      "Create risk-free 'synthetic bond' positions on dynamically-screened ETFs.",
      "Uses Yahoo Finance ETF screener to find liquid ETFs, then applies collar analysis for locked returns."
    ),
    href = "/etf-collar",
    button_text = "Analyze ETF Collars"
  )
  ```
- [ ] App starts without error: `devtools::load_all()` succeeds
- [ ] Navigating to `/etf-collar` renders the page with controls sidebar and empty results panel

## Technical Design

### Architecture

```
page_etf_collar.R                    # Brochure page (URL: /etf-collar)
  ├── mod_etf_collar_controls.R      # NEW: yfscreen filters + collar params
  │     ├── quote_source_toggle      # REUSED: Questrade/Yahoo toggle
  │     ├── setup_analysis_controls  # REUSED: run/progress/download helper
  │     └── analyze_etf_collar_yfscreen()  # NEW wrapper in fct_etf_collar_analysis.R
  │           ├── fetch_yfscreen_etfs()    # REUSED: from utils_yfscreen_etf.R
  │           └── analyze_collar_etfs()    # REUSED: from fct_collar_analysis.R
  └── mod_collar_results.R           # REUSED: existing results table + download
```

### Data Flow

1. User sets yfscreen filters → `fetch_yfscreen_etfs(dividend_filter, yield_range, min_cap, top_n)` → character vector of ETF tickers
2. Ticker vector → `analyze_collar_etfs(tickers, target_days, strike_adjustment_pct, max_workers)` → tibble of collar opportunities
3. Results tibble → `mod_collar_results_server()` → DT table + CSV download

### Data Model

No new database tables. Output is the same tibble format as existing collar analysis (ticker, stock_price, put_strike, put_price, call_strike, call_price, net_cost, annualized_return, days_to_expiry, etc.).

### Integration Points

| Component | Source File | How Used |
|-----------|------------|----------|
| `fetch_yfscreen_etfs()` | `R/utils_yfscreen_etf.R` | Called to get dynamic ETF ticker list |
| `analyze_collar_etfs()` | `R/fct_collar_analysis.R` | Called with fetched tickers for collar analysis |
| `mod_collar_results_ui/server` | `R/mod_collar_results.R` | Reused for results display |
| `setup_analysis_controls()` | `R/utils_analysis_controls_helper.R` | Reused for run button/progress/download pattern |
| `quote_source_toggle_ui/server` | Respective utils files | Reused for Questrade/Yahoo toggle |

### Config Structure

Add to `inst/golem-config.yml`:
```yaml
  # ETF collar strategy configuration (yfscreen-based)
  etf_collar:
    max_workers: 10
    min_net_credit: 0.01
    min_open_interest: 10
    max_stock_price: 1000
    shares_per_contract: 100
    sgov_yield_default: 0.0414
    max_sgov_yield_sanity: 0.15
    negative_return_threshold: 0
```

`R/utils_etf_collar_config.R` follows the exact same pattern as `R/utils_collar_config.R`:
```r
ETF_COLLAR_CONFIG <- list(
  max_workers = get_golem_config_value("etf_collar", "max_workers", 10),
  # ... same keys as COLLAR_CONFIG
)
validate_etf_collar_config <- function() { ... }
get_etf_collar_config <- function(key) { ... }
```

## User Experience

### User Flow

1. User navigates to `/etf-collar` from home page "Collar Strategies" accordion or direct URL
2. User sees sidebar with ETF screening filters (top) and collar parameters (bottom)
3. User adjusts yfscreen filters: dividend type, yield range (if dividend-paying), min AUM, top N
4. User adjusts collar params: target days, strike adjustment %, workers
5. User clicks "Run Analysis"
6. Progress indicator shows — first fetching ETFs from yfscreen, then running collar analysis
7. Results table appears with collar opportunities sorted by annualized return
8. User can download results as CSV

### Edge Cases

| Scenario | Behavior |
|----------|----------|
| yfscreen returns 0 ETFs | Show notification: "No ETFs matched your screening criteria." Empty results table. |
| yfscreen API unavailable | Error notification via existing error handling in `fetch_yfscreen_etfs()` |
| All fetched ETFs lack options chains | Results table shows 0 rows with status message |
| High top_n (500) | Analysis runs longer; progress bar tracks per-ticker |
| Network timeout during collar analysis | Existing error handling in `analyze_collar_etfs()` applies |

## Requirements

### Functional Requirements

- **FR-1**: Page fetches ETF universe dynamically from yfscreen based on user-selected filters
- **FR-2**: Leveraged/inverse ETFs are automatically excluded via existing `is_leveraged_or_inverse_etf()` logic
- **FR-3**: Collar analysis uses identical engine as existing collar page (`analyze_collar_etfs()`)
- **FR-4**: Results display in same format as existing collar results table (via `mod_collar_results`)
- **FR-5**: CSV download available for results
- **FR-6**: Quote source toggle (Questrade/Yahoo Finance) available
- **FR-7**: Dividend yield range slider conditionally shown only for "dividend_paying" filter

### Non-Functional Requirements

- **NFR-1**: Parallel processing via future/furrr with configurable workers (default 10)
- **NFR-2**: No new package dependencies
- **NFR-3**: Config validation at app startup prevents runtime config errors
- **NFR-4**: Follows existing Golem + Brochure conventions (page/module/fct/utils separation)

## Implementation Phases

### Phase 1: Foundation — Config + Business Logic
- [ ] Add `etf_collar:` section to `inst/golem-config.yml`
- [ ] Create `R/utils_etf_collar_config.R` with `ETF_COLLAR_CONFIG`, `validate_etf_collar_config()`, `get_etf_collar_config()`
- [ ] Create `R/fct_etf_collar_analysis.R` with `analyze_etf_collar_yfscreen()`
- [ ] Create `tests/testthat/test-fct_etf_collar_analysis.R` with mocked tests
- **Verification:** `Rscript -e "devtools::test(filter = 'etf_collar')"`

### Phase 2: UI — Controls Module
- [ ] Create `R/mod_etf_collar_controls.R` with `mod_etf_collar_controls_ui()` and `mod_etf_collar_controls_server()`
- [ ] Include all yfscreen filter inputs + collar parameter inputs
- [ ] Wire to `analyze_etf_collar_yfscreen()` via `setup_analysis_controls()`
- **Verification:** `Rscript -e "devtools::load_all(); mod_etf_collar_controls_ui('test')"`

### Phase 3: Integration — Page + Registration + Navigation
- [ ] Create `R/page_etf_collar.R` with `page_etf_collar()` function
- [ ] Register `page_etf_collar()` in `R/run_app.R` after `page_collar()`
- [ ] Add `validate_etf_collar_config()` to startup validation in `R/run_app.R`
- [ ] Add navigation entry to `R/page_home.R` in `collar_strategies` list
- **Verification:** `Rscript -e "devtools::load_all(); cat('Load successful\n')"`

## Definition of Done

This feature is complete when:
- [ ] All acceptance criteria in US-1, US-2, US-3 pass
- [ ] All implementation phases verified
- [ ] Tests pass: `Rscript -e "devtools::test(filter = 'etf_collar')"`
- [ ] Package loads: `Rscript -e "devtools::load_all()"`
- [ ] Manual verification: navigating to `/etf-collar` shows correct page with controls and results panels

## Files Summary

### New Files (5)
| File | Purpose |
|------|---------|
| `R/page_etf_collar.R` | Brochure page definition at `/etf-collar` |
| `R/mod_etf_collar_controls.R` | Controls module: yfscreen filters + collar params |
| `R/fct_etf_collar_analysis.R` | Thin wrapper: fetch_yfscreen_etfs → analyze_collar_etfs |
| `R/utils_etf_collar_config.R` | ETF_COLLAR_CONFIG object + validation |
| `tests/testthat/test-fct_etf_collar_analysis.R` | Unit tests for wrapper + config |

### Modified Files (3)
| File | Change |
|------|--------|
| `inst/golem-config.yml` | Add `etf_collar:` section after `collar:` section |
| `R/run_app.R` | Add `validate_etf_collar_config()` + register `page_etf_collar()` |
| `R/page_home.R` | Add ETF collar entry to `collar_strategies` list |

## Ralph Loop Command

```bash
/ralph "Implement ETF Collar Strategy Page per spec at docs/specs/help-me-implement-this-new-strategypage.md

PHASES:
1. Foundation: Create utils_etf_collar_config.R, fct_etf_collar_analysis.R, add golem-config.yml section, write tests - verify with Rscript -e 'devtools::test(filter=\"etf_collar\")'
2. UI: Create mod_etf_collar_controls.R with yfscreen filters + collar params - verify with Rscript -e 'devtools::load_all(); mod_etf_collar_controls_ui(\"test\")'
3. Integration: Create page_etf_collar.R, register in run_app.R, add to page_home.R - verify with Rscript -e 'devtools::load_all(); cat(\"Load successful\n\")'

VERIFICATION (run after each phase):
- Rscript -e 'devtools::test(filter=\"etf_collar\")'
- Rscript -e 'devtools::load_all()'

ESCAPE HATCH: After 20 iterations without progress:
- Document what's blocking in the spec file under 'Implementation Notes'
- List approaches attempted
- Stop and ask for human guidance

Output <promise>COMPLETE</promise> when all phases pass verification." --max-iterations 30 --completion-promise "COMPLETE"
```

## Open Questions

None — all decisions captured during interview.

## Implementation Notes

*To be filled during implementation if needed.*
