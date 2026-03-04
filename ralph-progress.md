# Ralph Progress — ETF Collar Strategy Page

## Completed

### Phase 1: Foundation — Config + Business Logic (US-1) ✅

- [x] Fixed `.scope-lock.json` — was generated with raw markdown text instead of file paths; corrected to list actual file paths
- [x] Added `etf_collar:` section to `inst/golem-config.yml` (after `etf_covered_calls:` section)
- [x] Created `R/utils_etf_collar_config.R` — exports `ETF_COLLAR_CONFIG`, `validate_etf_collar_config()`, `get_etf_collar_config()`
- [x] Created `R/fct_etf_collar_analysis.R` — exports `analyze_etf_collar_yfscreen()` with parallel pattern matching `analyze_collar_etfs()`
- [x] Created `tests/testthat/test-fct_etf_collar_analysis.R` — 25 tests, all passing
- [x] Ran `devtools::document()` to regenerate NAMESPACE with new exports
- [x] `devtools::test(filter = 'etf_collar')` → FAIL 0 | PASS 25

### Phase 2: UI — Controls Module (US-2) ✅

- [x] Created `R/mod_etf_collar_controls.R` — exports `mod_etf_collar_controls_ui()` and `mod_etf_collar_controls_server()`
  - Sidebar width=3 matching existing collar controls layout exactly
  - 2-option variant dropdown: "Dividend-paying ETFs" = "dividend_paying", "Zero-dividend ETFs" = "zero_dividend"
  - Identical sliders: target_days (45-850, default 300), strike_adjustment (-20 to 20, step 5, post="%"), max_workers (1-20, default from ETF_COLLAR_CONFIG)
  - Run Analysis + Download CSV buttons + Home nav link
  - Uses `quote_source_toggle_ui/server` and `setup_analysis_controls()`
  - `devtools::load_all()` + `devtools::test(filter='etf_collar')` → FAIL 0 | PASS 25

## Remaining

### Phase 3: Integration — Page + Registration + Navigation (US-3)
- [ ] Create `R/page_etf_collar.R`
- [ ] Register `page_etf_collar()` in `R/run_app.R` after `page_collar()`
- [ ] Add `validate_etf_collar_config()` to startup validation in `R/run_app.R`
- [ ] Add second card to `collar_strategies` list in `R/page_home.R`

## Notes

- `shiny::showNotification()` removed from `fct_etf_collar_analysis.R` — not usable outside Shiny session; empty-universe case logs a warning instead. The controls module's `setup_analysis_controls()` will handle user-visible notifications.
- Config `max_workers` comes back as integer from `get_golem_config_value()`, not double — test uses `is.numeric()` instead of `expect_type(..., "double")`
