# Spec: Finviz Call Skew Collar Variants

## Overview

Add two new collar strategy variants (#11 and #12) to the existing 10 on `/collar`:
- **Finviz Call Skew (Dividend)** -- scrapes 46 `fa_div_pos` Finviz URLs across 15 screening categories
- **Finviz Call Skew (Non-Dividend)** -- scrapes 46 `fa_div_none` Finviz URLs across 15 screening categories

The existing Finviz Screened variant (#10) remains unchanged. URL source: `claudedocs/finviz_call_skew_div_split_v2.md`.

## Constraints

### Files to Modify
- `R/mod_collar_controls.R` -- add dropdown entries, switch cases, variant labels, and progress messages
- `R/utils_custom_ticker_lists.R` -- add two fetch functions and two session-level cache environments
- `inst/golem-config.yml` -- add 92 URLs under two new config keys

### Files NOT to Modify
- `R/fct_collar_analysis.R` -- core collar engine (`analyze_collar_single()`) is off-limits
- `R/shared_risk_engine.R`
- Database schema files
- Any `page_*.R` files (no new routes needed)

### New Files Allowed
- `tests/testthat/test-utils_custom_ticker_lists_call_skew.R` -- unit tests for the two new fetch functions

### New Dependencies Allowed
- No. All required packages (rvest, logger, etc.) are already in use.

### Existing Code to Reuse
- `analyze_collar_custom_list()` in `fct_collar_analysis.R` -- add two new `list_type` cases to the existing switch
- `scrape_finviz_page()` in `utils_custom_ticker_lists.R` -- reuse for scraping individual URLs
- `collar_cache_is_valid()` in `utils_custom_ticker_lists.R` -- reuse for cache TTL checks
- `get_golem_config_value()` -- for reading URL lists from config

### Out of Scope
- Changes to the ETF collar page (`/etf-collar`)
- Changes to `analyze_collar_single()` or any `fct_*.R` business logic
- Adding call skew to other strategies (aristocrats, weekly capture, etc.)
- Sub-filtering by Finviz category (all URLs scraped in a single run)
- Any UI layout changes beyond the dropdown and progress messages

## User Stories

### US-1: Config -- Add 92 Finviz URLs to golem-config.yml

**Description:** As a developer, I want the 92 call skew Finviz URLs stored in config so they can be loaded at runtime.

**Acceptance Criteria:**
- [ ] `golem-config.yml` has key `custom_ticker_lists.finviz_call_skew_div_urls` with exactly 46 URLs (all containing `fa_div_pos`)
- [ ] `golem-config.yml` has key `custom_ticker_lists.finviz_call_skew_nodiv_urls` with exactly 46 URLs (all containing `fa_div_none`)
- [ ] URLs match those in `claudedocs/finviz_call_skew_div_split_v2.md` exactly
- [ ] Existing `finviz_screener_urls` key is unchanged

### US-2: Fetch Functions -- Add ticker scraping for both variants

**Description:** As a developer, I want two new fetch functions in `utils_custom_ticker_lists.R` so the collar engine can retrieve tickers from the call skew Finviz screens.

**Acceptance Criteria:**
- [ ] `fetch_finviz_call_skew_div_tickers(force_refresh = FALSE)` exists and reads URLs from `finviz_call_skew_div_urls` config key
- [ ] `fetch_finviz_call_skew_nodiv_tickers(force_refresh = FALSE)` exists and reads URLs from `finviz_call_skew_nodiv_urls` config key
- [ ] Each function has its own session-level cache environment (`.finviz_call_skew_div_cache`, `.finviz_call_skew_nodiv_cache`)
- [ ] Cache uses 24-hour TTL (same as existing `ticker_cache_hours` config)
- [ ] Functions reuse `scrape_finviz_page()` for individual URL scraping
- [ ] Functions return `unique(toupper(all_tickers))` -- deduplicated character vector
- [ ] Functions log the number of URLs being scraped and final ticker count
- [ ] Functions handle errors gracefully (log warning, skip failed URLs, continue)

### US-3: Dispatch -- Wire new variants into analyze_collar_custom_list()

**Description:** As a developer, I want the collar custom list dispatcher to recognize the two new list types.

**Acceptance Criteria:**
- [ ] `analyze_collar_custom_list()` switch in `fct_collar_analysis.R` accepts `list_type = "finviz_call_skew_div"` and dispatches to `fetch_finviz_call_skew_div_tickers()`
- [ ] `analyze_collar_custom_list()` switch accepts `list_type = "finviz_call_skew_nodiv"` and dispatches to `fetch_finviz_call_skew_nodiv_tickers()`
- [ ] The list_name mapping includes entries for both new types: "Finviz Call Skew (Dividend)" and "Finviz Call Skew (Non-Dividend)"
- [ ] The stop() error message is updated to include the new valid list_type values

**Note:** This requires modifying only the `switch()` statements and error message in `analyze_collar_custom_list()`, not the core engine.

### US-4: UI -- Add dropdown entries and progress messages

**Description:** As a user, I want to select the two new call skew variants from the collar strategy dropdown.

**Acceptance Criteria:**
- [ ] Dropdown in `mod_collar_controls.R` includes "Finviz Call Skew (Dividend)" = "finviz_call_skew_div"
- [ ] Dropdown includes "Finviz Call Skew (Non-Dividend)" = "finviz_call_skew_nodiv"
- [ ] Both entries appear after the existing "Finviz Screened" entry
- [ ] Switch case in server dispatches to `analyze_collar_custom_list(list_type = "finviz_call_skew_div", ...)` and `...nodiv...`
- [ ] `variant_label()` returns "Finviz call skew screened dividend stocks" for `finviz_call_skew_div`
- [ ] `variant_label()` returns "Finviz call skew screened non-dividend stocks" for `finviz_call_skew_nodiv`
- [ ] Progress message says "Scraping 46 Finviz call skew screens and analyzing collars... This may take several minutes."

### US-5: Tests -- Unit tests for fetch functions

**Description:** As a developer, I want tests verifying the new fetch functions load config and deduplicate correctly.

**Acceptance Criteria:**
- [ ] Test file at `tests/testthat/test-utils_custom_ticker_lists_call_skew.R`
- [ ] Tests verify `get_golem_config_value()` returns the correct number of URLs (46 each)
- [ ] Tests verify all div URLs contain `fa_div_pos` and all nodiv URLs contain `fa_div_none`
- [ ] Tests mock HTTP responses and verify `fetch_finviz_call_skew_div_tickers()` returns a character vector
- [ ] Tests verify deduplication (duplicate tickers across URLs are collapsed)
- [ ] Tests verify caching (second call returns cached data without HTTP requests)
- [ ] All tests pass: `devtools::test(filter = "custom_ticker_lists_call_skew")`

## Technical Design

### Data Flow
```
golem-config.yml (46 URLs)
  -> fetch_finviz_call_skew_div_tickers()     [utils_custom_ticker_lists.R]
    -> scrape_finviz_page() per URL            [utils_custom_ticker_lists.R]
    -> deduplicate + cache
  -> analyze_collar_custom_list(list_type)     [fct_collar_analysis.R]
    -> analyze_collar_single() per ticker      [fct_collar_analysis.R - UNCHANGED]
    -> tibble of collar opportunities
```

### Config Structure (in golem-config.yml under custom_ticker_lists)
```yaml
custom_ticker_lists:
  finviz_screener_urls:         # existing - unchanged
    - "https://finviz.com/..."
  finviz_call_skew_div_urls:    # NEW - 46 URLs with fa_div_pos
    - "https://finviz.com/..."
  finviz_call_skew_nodiv_urls:  # NEW - 46 URLs with fa_div_none
    - "https://finviz.com/..."
```

### Function Signatures
```r
# In utils_custom_ticker_lists.R
fetch_finviz_call_skew_div_tickers <- function(force_refresh = FALSE)
fetch_finviz_call_skew_nodiv_tickers <- function(force_refresh = FALSE)
```

## Implementation Phases

### Phase 1: Config + Fetch Functions
- [ ] Add 92 URLs to `inst/golem-config.yml` (US-1)
- [ ] Add two fetch functions and cache envs to `R/utils_custom_ticker_lists.R` (US-2)
- **Verification:** `devtools::load_all(); length(get_golem_config_value("custom_ticker_lists", "finviz_call_skew_div_urls"))` returns 46

### Phase 2: Dispatch + UI Wiring
- [ ] Add switch cases in `analyze_collar_custom_list()` (US-3)
- [ ] Add dropdown entries, dispatch, labels, and progress messages in `mod_collar_controls.R` (US-4)
- **Verification:** `devtools::load_all(); devtools::check()` passes without errors

### Phase 3: Tests
- [ ] Write unit tests for both fetch functions (US-5)
- **Verification:** `devtools::test(filter = "custom_ticker_lists_call_skew")` -- all tests pass

## Definition of Done

This feature is complete when:
- [ ] All acceptance criteria in US-1 through US-5 pass
- [ ] `devtools::test()` -- all tests pass (existing + new)
- [ ] `devtools::check()` -- no errors
- [ ] Both new variants appear in the collar dropdown and run successfully
- [ ] Existing 10 collar variants are unaffected

## Ralph Loop Command

```bash
/ralph docs/specs/implement-the-call-skew-variants-for-the-collar-strategy.md
```
