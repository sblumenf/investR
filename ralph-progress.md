# Ralph Progress — Volatility Skew Signal

## Completed

### Phase 1: Business logic + tests ✅
- Created `R/fct_skew_signal.R` with:
  - `compute_skew_signal(ticker)` — full implementation with Questrade → Yahoo fallback
  - `build_skew_modal(ticker, result)` — shared modal rendering helper for both modules
  - `build_skew_table_html()` / `format_iv_diff_cell()` — internal HTML builders
  - `normalize_iv_column()` — handles Questrade (percent) vs Yahoo (decimal) IV scale difference
  - `add_bs_delta()` — Black-Scholes delta computation (chain has no delta column)
  - `match_nearest_delta()` — nearest-delta matching with tie-breaking to lower abs delta
  - `parse_chain_expiry_dates()` — handles both "Mar.21.2025" and "2025-03-21" formats
- Created `tests/testthat/test-fct_skew_signal.R` — 26 tests, all passing
- `devtools::check()` — pre-existing ERROR unrelated to these changes (missing DESCRIPTION entries for DBI, MASS, glue, shinyBS, tidyr)
- Committed: `74b96ee`

### Implementation note
The spec says to modify `mod_aristocrats_analysis.R` and `mod_zero_dividend_analysis.R`, but candidate cards are
actually rendered in `mod_aristocrats_results_table.R` and `mod_zero_dividend_results_table.R`. Phase 2 will add
the Fetch Skew button to the results table modules (where the cards live), which is consistent with the spec's
intent even if the file names differ. The spec constraint already allows modifying results table modules since
they aren't in "Files NOT to Modify".

## Remaining

### Phase 2: Module integration
- [ ] Add "Fetch Skew" button to each card in `mod_aristocrats_results_table.R`
  - Button ID: `ns(paste0("skew_btn_", idx))`
  - Disable during fetch, re-enable after (use `shinyjs` or reactive flag)
  - Wire `observeEvent` (1-50 pattern like existing risk buttons) to call `compute_skew_signal()`
  - Show `showModal(build_skew_modal(ticker, result))` after fetch
  - Error path: `build_skew_modal` handles errors already
- [ ] Same for `mod_zero_dividend_results_table.R`
- Verify: `devtools::load_all()` and `source("dev/run_dev.R")`

### Phase 3: Visual polish
- Visual formatting is already implemented in `fct_skew_signal.R` (arrows, colors, percentage format)
- Phase 3 is primarily verification in browser
- Verify: Manual browser test

## Known Issues
- `devtools::check()` ERROR is pre-existing (DBI, MASS, glue, shinyBS, tidyr missing from DESCRIPTION) — confirmed pre-dates this work
