# ETF Collar Strategy Page

## Constraints

- **Files to Modify**: `R/run_app.R` (register page + config validation), `inst/golem-config.yml` (add `etf_collar` config section), `R/page_home.R` (add second card to `collar_strategies` list)
- **Files NOT to Modify**: No hard restrictions, but prefer not modifying existing collar or ETF CC modules unless truly necessary
- **New Files Allowed**: Yes — `R/page_etf_collar.R`, `R/mod_etf_collar_controls.R`, `R/fct_etf_collar_analysis.R`, `R/utils_etf_collar_config.R`, `tests/testthat/test-fct_etf_collar_analysis.R`
- **New Dependencies Allowed**: No — use only existing packages in DESCRIPTION
- **Existing Code to Reuse**: `fetch_yfscreen_etfs()` from `utils_yfscreen_etf.R`, `analyze_collar_single()` from `fct_collar_analysis.R`, `mod_collar_results_ui/server` from `mod_collar_results.R`, `setup_analysis_controls()` from `utils_analysis_controls_helper.R`, `quote_source_toggle_ui/server`, parallel processing pattern from `analyze_collar_etfs()`
- **Out of Scope**: Changes to existing collar page, changes to existing ETF covered calls page, new analysis algorithms, ETF-specific result columns

## Overview

New Brochure page at `/etf-collar` titled "ETF Collar Strategy (Synthetic Bonds)" that runs the existing collar analysis engine on ETFs sourced dynamically from yfscreen (Yahoo Finance ETF screener).

This is a **completely separate page** from the existing `/collar` page. The home page's "Collar Strategies" accordion section currently has one card; it will have two after this feature.

The new page looks and feels identical to the existing collar page. The variant dropdown has exactly 2 options (dividend-paying ETFs and zero-dividend ETFs, both from yfscreen) instead of the existing page's 10 stock/ETF variants. All other controls (target days, strike adjustment, workers) are identical.

## Scope

### In Scope
- New Brochure page at `/etf-collar` with yfscreen-sourced ETF collar analysis
- Controls module mirroring existing collar controls (dropdown + same params)
- Variant dropdown: "Dividend-paying ETFs" and "Zero-dividend ETFs"
- Thin `fct_` wrapper that calls `fetch_yfscreen_etfs()` then parallel-processes via `analyze_collar_single()`
- Own config section in `golem-config.yml` + config utility file
- Registration in `run_app.R` and second card on home page
- Reuse `mod_collar_results` for results display
- Unit tests for wrapper + config validation

### Out of Scope
- Changes to existing collar strategy page (`/collar`)
- Changes to existing ETF covered calls page
- Exposing yfscreen filter controls (AUM, top N, yield range) to the user — use sensible defaults internally
- New analysis algorithms
- ETF-specific result columns

## User Stories

### US-1: Config and Business Logic Foundation

**Description:** Create the ETF collar config object, golem-config section, and the wrapper analysis function that fetches ETFs from yfscreen and runs collar analysis.

**Acceptance Criteria:**
- [ ] `etf_collar:` section added to `inst/golem-config.yml` following same structure as `collar:` section (max_workers, min_net_credit, min_open_interest, shares_per_contract, sgov_yield_default, etc.)
- [ ] `R/utils_etf_collar_config.R` exports `ETF_COLLAR_CONFIG` list, `validate_etf_collar_config()`, and `get_etf_collar_config(key)`
- [ ] `R/fct_etf_collar_analysis.R` exports `analyze_etf_collar_yfscreen()` function
- [ ] Function accepts: `dividend_filter` ("dividend_paying" or "zero_dividend"), `target_days`, `strike_adjustment_pct`, `max_workers`
- [ ] Function calls `fetch_yfscreen_etfs(dividend_filter = ...)` to get ticker vector
- [ ] Function runs `analyze_collar_single()` on each ticker via `future_map()` (same parallel pattern as `analyze_collar_etfs()`)
- [ ] Function returns a tibble sorted by annualized return (empty tibble if no ETFs match)
- [ ] `tests/testthat/test-fct_etf_collar_analysis.R` exists with mocked tests
- [ ] `devtools::test(filter = "etf_collar")` passes

### US-2: Controls Module

**Description:** Create controls module mirroring the existing collar controls but with a 2-option variant dropdown (dividend ETFs / zero-dividend ETFs from yfscreen).

**Acceptance Criteria:**
- [ ] `R/mod_etf_collar_controls.R` exists with `mod_etf_collar_controls_ui(id)` and `mod_etf_collar_controls_server(id)`
- [ ] UI sidebar (width=3) matches existing collar controls layout:
  1. `h3("ETF Collar Strategy")` with helpText
  2. Quote source toggle via `quote_source_toggle_ui(ns)`
  3. `hr()`
  4. `h4("Select ETF Variant")` with `selectInput` — 2 choices: "Dividend-paying ETFs" = "dividend_paying", "Zero-dividend ETFs" = "zero_dividend"
  5. `hr()`
  6. `h5("Strategy Parameters")` — Target days slider (min=45, max=850, default=300, step=5), Strike adjustment slider (min=-20, max=20, default=0, step=5, post="%")
  7. `hr()`
  8. `h5("Performance")` — Parallel workers slider (1-20, default from config)
  9. `hr()`
  10. Run Analysis `actionButton` + Download CSV `downloadButton`
  11. Home navigation link
- [ ] Server calls `quote_source_toggle_server()`
- [ ] Server creates analysis function that calls `analyze_etf_collar_yfscreen()` with `dividend_filter = input$collar_variant` and other input values
- [ ] Server uses `setup_analysis_controls()` returning `$results_data` and `$status_ui`
- [ ] All slider ranges and defaults match existing collar controls exactly

### US-3: Page Definition and Registration

**Description:** Create the Brochure page, register it in `run_app.R`, add config validation at startup, and add navigation card to home page.

**Acceptance Criteria:**
- [ ] `R/page_etf_collar.R` defines `page_etf_collar()` returning `brochure::page(href = "/etf-collar", ...)`
- [ ] Page structure matches `page_collar.R` exactly: `fluidPage` → `titlePanel("ETF Collar Strategy (Synthetic Bonds)")` → `sidebarLayout` with `mod_etf_collar_controls_ui("controls")` and `mainPanel(width=9)` with `uiOutput("controls_status")` + `mod_collar_results_ui("results")`
- [ ] Server wires `mod_etf_collar_controls_server("controls")` → `mod_collar_results_server("results", controls$results_data)` → `output$controls_status`
- [ ] `page_etf_collar()` added to `brochureApp()` in `R/run_app.R` immediately after `page_collar()` (line 121)
- [ ] `validate_etf_collar_config()` added to startup validation block in `R/run_app.R` (after `validate_collar_config()`, line 69)
- [ ] New card added to `collar_strategies` list in `R/page_home.R` (after existing collar entry, line 69):
  ```r
  list(
    title = "ETF Collar Strategy (Synthetic Bonds)",
    description = c(
      "Create risk-free 'synthetic bond' positions on dynamically-screened ETFs from Yahoo Finance.",
      "Screens for dividend-paying or zero-dividend ETFs, then applies collar analysis for locked returns."
    ),
    href = "/etf-collar",
    button_text = "Analyze ETF Collars"
  )
  ```
- [ ] `devtools::load_all()` succeeds without error
- [ ] Navigating to `/etf-collar` renders the page with controls and empty results panel

## Technical Design

### Architecture

```
page_etf_collar.R                       # Brochure page (URL: /etf-collar)
  +-- mod_etf_collar_controls.R          # NEW: 2-variant dropdown + collar params
  |     +-- quote_source_toggle          # REUSED
  |     +-- setup_analysis_controls()    # REUSED
  |     +-- analyze_etf_collar_yfscreen()  # NEW wrapper in fct_etf_collar_analysis.R
  |           +-- fetch_yfscreen_etfs()    # REUSED from utils_yfscreen_etf.R
  |           +-- analyze_collar_single()  # REUSED from fct_collar_analysis.R
  +-- mod_collar_results.R               # REUSED: existing results table + download
```

### Data Flow

1. User selects variant (dividend-paying or zero-dividend) and clicks Run
2. `analyze_etf_collar_yfscreen(dividend_filter, target_days, strike_adjustment_pct, max_workers)` is called
3. Internally: `fetch_yfscreen_etfs(dividend_filter = ...)` returns character vector of ETF tickers
4. Parallel `future_map()` calls `analyze_collar_single(ticker, target_days, strike_adjustment_pct)` for each ticker
5. Results combined into tibble, sorted by annualized return
6. Tibble passed to `mod_collar_results_server()` for display

### Key Implementation Detail

`analyze_collar_etfs()` cannot be called directly because it internally calls `get_liquid_etfs()` for its tickers. The new wrapper must replicate the parallel processing pattern from `analyze_collar_etfs()` (lines 397-456 of `fct_collar_analysis.R`) but substitute `fetch_yfscreen_etfs()` as the ticker source. The core analysis function `analyze_collar_single()` is reused directly.

### Config Structure

Add to `inst/golem-config.yml` (after existing `collar:` section):

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

## User Experience

### User Flow

1. User navigates to `/etf-collar` from home page "Collar Strategies" accordion (second card) or direct URL
2. User selects variant: "Dividend-paying ETFs" or "Zero-dividend ETFs"
3. User adjusts collar parameters (target days, strike adjustment %, workers) — same controls as existing collar page
4. User clicks "Run Analysis"
5. Progress indicator shows during analysis
6. Results table displays collar opportunities sorted by annualized return
7. User can download results as CSV

### Edge Cases

| Scenario | Behavior |
|----------|----------|
| yfscreen returns 0 ETFs | Show notification, return empty results table |
| yfscreen API unavailable | Error notification via existing error handling |
| All fetched ETFs lack options chains | Results table shows 0 rows with status message |

## Requirements

### Functional Requirements
- **FR-1**: Page fetches ETF universe dynamically from yfscreen based on selected variant (dividend-paying or zero-dividend)
- **FR-2**: Leveraged/inverse ETFs automatically excluded (existing `is_leveraged_or_inverse_etf()` logic in `fetch_yfscreen_etfs()`)
- **FR-3**: Collar analysis uses `analyze_collar_single()` — same per-ticker logic as existing collar page
- **FR-4**: Results display via reused `mod_collar_results` module
- **FR-5**: CSV download available
- **FR-6**: Quote source toggle (Questrade/Yahoo Finance) available

### Non-Functional Requirements
- **NFR-1**: Parallel processing via future/furrr with configurable workers (default 10)
- **NFR-2**: No new package dependencies
- **NFR-3**: Config validation at app startup
- **NFR-4**: Follows existing Golem + Brochure conventions

## Implementation Phases

### Phase 1: Foundation — Config + Business Logic
- [ ] Add `etf_collar:` section to `inst/golem-config.yml`
- [ ] Create `R/utils_etf_collar_config.R` with `ETF_COLLAR_CONFIG`, `validate_etf_collar_config()`, `get_etf_collar_config()`
- [ ] Create `R/fct_etf_collar_analysis.R` with `analyze_etf_collar_yfscreen()`
- [ ] Create `tests/testthat/test-fct_etf_collar_analysis.R`
- **Verification:** `Rscript -e "devtools::test(filter = 'etf_collar')"`

### Phase 2: UI — Controls Module
- [ ] Create `R/mod_etf_collar_controls.R` matching existing collar controls layout with 2-variant dropdown
- **Verification:** `Rscript -e "devtools::load_all(); mod_etf_collar_controls_ui('test')"`

### Phase 3: Integration — Page + Registration + Navigation
- [ ] Create `R/page_etf_collar.R`
- [ ] Register `page_etf_collar()` in `R/run_app.R` after `page_collar()`
- [ ] Add `validate_etf_collar_config()` to startup validation in `R/run_app.R`
- [ ] Add second card to `collar_strategies` in `R/page_home.R`
- **Verification:** `Rscript -e "devtools::load_all(); cat('Load successful\n')"`

## Definition of Done

This feature is complete when:
- [ ] All acceptance criteria in US-1, US-2, US-3 pass
- [ ] Tests pass: `Rscript -e "devtools::test(filter = 'etf_collar')"`
- [ ] Package loads: `Rscript -e "devtools::load_all()"`
- [ ] Home page shows two cards in "Collar Strategies" section
- [ ] Navigating to `/etf-collar` shows correct page with 2-variant dropdown and collar controls

## Files Summary

### New Files (5)
| File | Purpose |
|------|---------|
| `R/page_etf_collar.R` | Brochure page definition at `/etf-collar` |
| `R/mod_etf_collar_controls.R` | Controls module: 2-variant dropdown + collar params |
| `R/fct_etf_collar_analysis.R` | Wrapper: fetch_yfscreen_etfs → parallel analyze_collar_single |
| `R/utils_etf_collar_config.R` | ETF_COLLAR_CONFIG object + validation |
| `tests/testthat/test-fct_etf_collar_analysis.R` | Unit tests for wrapper + config |

### Modified Files (3)
| File | Change |
|------|--------|
| `inst/golem-config.yml` | Add `etf_collar:` section |
| `R/run_app.R` | Add `validate_etf_collar_config()` + register `page_etf_collar()` |
| `R/page_home.R` | Add second card to `collar_strategies` list |

## Open Questions

None — all decisions confirmed during interview.

## Implementation Notes

*To be filled during implementation if needed.*
