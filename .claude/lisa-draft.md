# Specification Draft: Finviz Call Skew Collar Variants

*Interview in progress - Started: 2026-03-05*

## Overview
Add two new collar strategy variants (#11 and #12) to the existing 10 on `/collar`:
- **Finviz Call Skew (Dividend)** — scrapes 46 `fa_div_pos` Finviz URLs across 15 categories
- **Finviz Call Skew (Non-Dividend)** — scrapes 46 `fa_div_none` Finviz URLs across 15 categories

The existing Finviz Screened variant (#10) remains unchanged.

## Decisions Made

### URL Handling
- Both variants scrape ALL their respective URLs in a single run (no sub-filtering by category)

### UI Labels
- Dropdown labels: "Finviz Call Skew (Dividend)" and "Finviz Call Skew (Non-Dividend)"
- Variant IDs: `finviz_call_skew_div` and `finviz_call_skew_nodiv`

### URL Storage
- URLs stored in `golem-config.yml` under two new config keys:
  - `finviz_call_skew_div_urls`
  - `finviz_call_skew_nodiv_urls`

### Caching
- Same pattern as existing finviz_screened: separate session-level cache env per variant, 24-hour TTL
- Cache envs: `.finviz_call_skew_div_cache` and `.finviz_call_skew_nodiv_cache`

### Dividend Fetching
- No change to analyze_collar_single() -- non-div tickers will get zero dividend history naturally

### Progress Messages
- Include URL count: "Scraping 46 Finviz call skew screens and analyzing collars..."

### Testing
- Unit tests for the new fetch functions with mocked HTML responses
- Verify URL config loading and deduplication

## Files to Modify (preliminary)
- `R/mod_collar_controls.R` -- add dropdown entries and switch cases
- `R/utils_custom_ticker_lists.R` -- add fetch functions and cache envs
- `inst/golem-config.yml` -- add 92 URLs under two new keys

## New Files
- `tests/testthat/test-utils_custom_ticker_lists_call_skew.R` -- unit tests

## Open Questions
- Variant description text for the results panel
- Constraints section (files NOT to modify, out of scope)
