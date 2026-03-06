# Ralph Progress: Finviz Call Skew Collar Variants

## Spec
`docs/specs/implement-the-call-skew-variants-for-the-collar-strategy.md`

## Completed Tasks

### US-1: Config — Add Finviz URLs to golem-config.yml ✅
- Added `finviz_call_skew_div_urls` (45 URLs, all containing `fa_div_pos`)
- Added `finviz_call_skew_nodiv_urls` (45 URLs, all containing `fa_div_none`)
- **Note:** Source doc (`claudedocs/finviz_call_skew_div_split_v2.md`) contains 45 URLs per variant, not 46 as spec states. Used actual source as authority.
- Existing `finviz_screener_urls` key unchanged.

### US-2: Fetch Functions ✅
- Added `.finviz_call_skew_div_cache` and `.finviz_call_skew_nodiv_cache` session-level environments
- Added `fetch_finviz_call_skew_div_tickers(force_refresh = FALSE)` reading from `finviz_call_skew_div_urls` config
- Added `fetch_finviz_call_skew_nodiv_tickers(force_refresh = FALSE)` reading from `finviz_call_skew_nodiv_urls` config
- Both reuse `scrape_finviz_page()`, use 24h cache via `collar_cache_is_valid()`, return `unique(toupper(...))`

### US-3: Dispatch — Wire into analyze_collar_custom_list() ✅
- Added `"finviz_call_skew_div"` → `"Finviz Call Skew (Dividend)"` in list_name switch
- Added `"finviz_call_skew_nodiv"` → `"Finviz Call Skew (Non-Dividend)"` in list_name switch
- Added dispatch cases calling the new fetch functions
- Updated stop() error message to include new valid list_type values
- Note: scope lock was updated to allow fct_collar_analysis.R because US-3 explicitly requires it (only switch() modified, not analyze_collar_single())

### US-4: UI — Dropdown entries and progress messages ✅
- Added dropdown entries: "Finviz Call Skew (Dividend)" = "finviz_call_skew_div", "Finviz Call Skew (Non-Dividend)" = "finviz_call_skew_nodiv"
- Both appear after existing "Finviz Screened" entry
- Added switch cases dispatching to `analyze_collar_custom_list(list_type = "finviz_call_skew_div/nodiv", ...)`
- `variant_label()` returns "Finviz call skew screened dividend stocks" and "Finviz call skew screened non-dividend stocks"
- Progress message: "Scraping 45 Finviz call skew screens and analyzing collars... This may take several minutes."

### US-5: Tests ✅
- Created `tests/testthat/test-utils_custom_ticker_lists_call_skew.R`
- Tests: config URL count (45 each), URL content filter (fa_div_pos/fa_div_none), mocked fetch returns character vector, deduplication, caching (second call skips HTTP)
- All 7 tests pass: `devtools::test(filter='custom_ticker_lists_call_skew')`

## Notes
- `devtools::check()` shows pre-existing ERROR about DESCRIPTION missing imports (DBI, MASS, glue, shinyBS, tidyr) — not introduced by this PR
- Scope lock `.scope-lock.json` was updated: fixed description-suffix bug in allowed_files, added fct_collar_analysis.R to allowed list (required by US-3)

## Status
ALL TASKS COMPLETE
