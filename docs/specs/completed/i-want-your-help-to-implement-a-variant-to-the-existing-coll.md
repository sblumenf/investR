# IV Skew Collar Screener

## Overview

Add a new collar variant **"IV Skew Screener"** to the existing collar strategy dropdown on the `/collar` page. This variant screens the Russell 1000 equity universe for stocks where ATM call IV is high relative to ATM put IV (favorable skew for collar construction), then runs standard collar analysis on the top 20 results.

## Constraints

### Files to Modify
- `R/fct_collar_analysis.R` — Add `fetch_iwb_holdings()`, `compute_iv_skew_ratio()`, and `analyze_collar_iv_skew()` functions
- `R/mod_collar_controls.R` — Add `"iv_skew"` to dropdown choices and dispatch switch
- `R/utils_collar_config.R` — Add IV skew config entries to `COLLAR_CONFIG`
- `inst/golem-config.yml` — Add `iv_skew` section under `collar`

### Files NOT to Modify
- `R/mod_collar_results.R` — Result cards stay standard; no IV ratio display needed
- `R/page_collar.R` — Page routing unchanged
- `R/fct_questrade_options.R` — Existing options API functions are sufficient
- `R/fct_implied_volatility.R` — Not used for this screening; we use raw option-level IV

### New Files Allowed
- No new R source files. All logic goes in existing files.
- Move `claudedocs/IWB_holdings.csv` to `inst/cache/IWB_holdings.csv` (fallback data)

### New Dependencies Allowed
- Yes, if needed. However, `readr`, `httr`, `dplyr`, `purrr`, `furrr`, `future`, and `logger` are already available and should suffice.

### Existing Code to Reuse
- `analyze_collar_single(ticker, target_days, strike_adjustment_pct)` — Core collar analysis per ticker
- `analyze_collar_custom_list()` — Pattern for custom-list collar variants (fetch list → parallel analyze → sort)
- `fetch_questrade_options_chain()` / `fetch_questrade_option_quotes()` — Options chain + IV data
- `setup_parallel_processing()` / `furrr::future_map()` — Parallel execution pattern
- `get_golem_config_value()` — Config access pattern
- `COLLAR_CONFIG` — Config object pattern
- `setup_analysis_controls()` — UI analysis controls helper in mod_collar_controls

### Out of Scope
- No caching of IV screening results (parallel + rate limiting is sufficient)
- No new UI widgets or sliders for the IV skew variant
- No IV ratio display on result cards
- No pre-filtering by weight, market cap, or volume before IV screening
- No Yahoo Finance fallback for IV data (Questrade only for this screener)

## User Stories

### US-1: Fetch and Parse Russell 1000 Holdings

**Description:** As a user, I want the system to download and parse the iShares Russell 1000 ETF holdings CSV, filtering to equities only, so that the IV screener has a universe of stocks to analyze.

**Acceptance Criteria:**
- [ ] `fetch_iwb_holdings()` downloads CSV from configured URL
- [ ] Correctly skips the 9 header rows in the iShares CSV format
- [ ] Filters to rows where Asset Class == "Equity"
- [ ] Returns a character vector of ticker symbols
- [ ] On download failure: shows user-facing error message AND falls back to `inst/cache/IWB_holdings.csv`
- [ ] Handles the iShares CSV footer/disclaimer rows (stops at blank line or non-data rows)
- [ ] Unit test: parsing of the cached CSV returns expected number of equity tickers

### US-2: Compute IV Skew Ratio for a Single Ticker

**Description:** As a user, I want the system to fetch ATM call IV and ATM put IV for a single ticker at the closest expiry to 45 days (within 45-60 day window), and compute their ratio.

**Acceptance Criteria:**
- [ ] `compute_iv_skew_ratio(ticker)` fetches the options chain via existing Questrade functions
- [ ] Finds the expiration date closest to 45 days from today, within the 45-60 day window
- [ ] If no expiry exists in the 45-60 day window, returns NULL (skip silently)
- [ ] Selects ATM strike as the closest strike to current stock price
- [ ] Extracts call IV and put IV from the options chain IV column
- [ ] If either IV is NA or put IV is 0, returns NULL (skip silently)
- [ ] Returns a list with: `ticker`, `call_iv`, `put_iv`, `iv_ratio` (call_iv / put_iv), `current_price`, `expiry_date`
- [ ] Unit test: mock Questrade response returns correct IV ratio calculation

### US-3: Screen Russell 1000 Universe for Top IV Skew

**Description:** As a user, I want to screen all Russell 1000 equities for call/put IV skew and get the top 20 stocks by highest ratio.

**Acceptance Criteria:**
- [ ] `analyze_collar_iv_skew()` orchestrates: fetch holdings → parallel IV screening → sort → top 20 → collar analysis
- [ ] Uses `future_map()` with parallel workers (from `COLLAR_CONFIG$max_workers`)
- [ ] Respects Questrade rate limits (20 market data requests/second, 15,000/hour)
- [ ] Skips tickers silently when: no options chain, no expiry in 45-60 day window, missing IV data
- [ ] Ranks all successful results by `iv_ratio` descending
- [ ] Takes top 20 and runs `analyze_collar_single()` on each with `target_days` from the 45-60 day expiry
- [ ] Final output is standard collar results tibble sorted by `annualized_return` (same format as other variants)
- [ ] Passes `target_days` and `strike_adjustment_pct` from UI sliders to `analyze_collar_single()`
- [ ] Logs progress: number of tickers to scan, number with valid IV data, number of collar opportunities found

### US-4: Integrate IV Skew Variant into Collar Page UI

**Description:** As a user, I want to select "IV Skew Screener" from the collar dropdown, click Analyze, and see standard collar result cards for the top 20 IV-skew stocks.

**Acceptance Criteria:**
- [ ] New dropdown option `"IV Skew Screener" = "iv_skew"` appears in `mod_collar_controls_ui`
- [ ] Switch case in `analysis_function()` dispatches to `analyze_collar_iv_skew()`
- [ ] `variant_label()` returns `"IV skew screened stocks"` for the `"iv_skew"` case
- [ ] Existing parameter sliders (target_days, strike_adjustment) remain fully interactive
- [ ] Progress message indicates the longer scan time: "Screening Russell 1000 for IV skew..."
- [ ] Result cards are identical to other collar variants (no changes to mod_collar_results)

### US-5: Configuration for IV Skew Screener

**Description:** As a user, I want the IV skew screener's parameters to be configurable via golem-config.yml.

**Acceptance Criteria:**
- [ ] `golem-config.yml` has new `iv_skew` section under `collar` with: `holdings_url`, `top_n` (default 20), `screening_target_days` (default 45), `screening_max_days` (default 60)
- [ ] `COLLAR_CONFIG` includes new IV skew entries via `get_golem_config_value()`
- [ ] Fallback CSV path resolves to `inst/cache/IWB_holdings.csv`

## Technical Design

### Data Flow

```
User selects "IV Skew Screener" → clicks Analyze
  → fetch_iwb_holdings()
    → Download CSV from URL (or fallback to inst/cache/IWB_holdings.csv)
    → Parse, filter Asset Class == "Equity"
    → Return ~1000 ticker symbols
  → Parallel: compute_iv_skew_ratio(ticker) for each
    → fetch_questrade_options_chain(ticker)
    → Find expiry closest to 45 days (within 45-60 day window)
    → Extract ATM call IV and ATM put IV
    → Return {ticker, call_iv, put_iv, iv_ratio}
  → Sort by iv_ratio descending, take top 20
  → Parallel: analyze_collar_single(ticker, target_days, strike_adjustment_pct) for each top 20
    → Standard collar analysis (options chain, net credit, dividends, returns)
  → Return standard collar results tibble
  → Display as collar cards in mod_collar_results
```

### API Call Budget

Per ticker (screening phase):
1. Symbol search: `GET v1/symbols/search?prefix={ticker}` — 1 call
2. Options structure: `GET v1/symbols/{id}/options` — 1 call
3. Option quotes: `POST v1/markets/quotes/options` — 1 call (batch ATM options)

Total for ~1000 equities: ~3000 calls
At 20 requests/second with parallel workers: ~2.5 minutes

Per ticker (collar analysis phase, top 20 only):
Same 3-call pattern × 20 = ~60 calls (~3 seconds)

Total: ~3060 API calls, well within the 15,000/hour limit.

### Questrade Options Quotes Filter Optimization

The `POST v1/markets/quotes/options` endpoint supports a `filters` parameter:
```json
{
  "filters": [{
    "optionType": "Call",
    "underlyingId": 12345,
    "expiryDate": "2026-04-10T00:00:00.000000-04:00",
    "minstrikePrice": 145.0,
    "maxstrikePrice": 155.0
  }]
}
```

This can be used to fetch only ATM options at the target expiry, reducing data transfer. The existing `fetch_questrade_option_quotes()` already supports this via batch option IDs, but the filter approach could skip the structure fetch entirely. Implementation should evaluate whether using filters is cleaner than the existing structure → extract IDs → fetch quotes flow.

### CSV Parsing

The iShares CSV format:
- Lines 1-9: Header metadata (fund name, date, shares outstanding, etc.)
- Line 10: Column headers: `Ticker,Name,Sector,Asset Class,Market Value,Weight (%),Notional Value,Quantity,Price,Location,Exchange,Currency,FX Rate,Market Currency,Accrual Date`
- Lines 11+: Data rows (quoted fields with commas in numbers)
- End: Blank line followed by disclaimer text

Parsing approach: `read.csv()` or `readr::read_csv()` with `skip = 9`, stop reading at first blank/non-data row. Filter `Asset Class == "Equity"`.

### Configuration Additions

In `golem-config.yml` under `collar`:
```yaml
collar:
  iv_skew:
    holdings_url: "https://www.ishares.com/us/products/239707/ishares-russell-1000-etf/1467271812596.ajax?fileType=csv&fileName=IWB_holdings&dataType=fund"
    top_n: 20
    screening_target_days: 45
    screening_max_days: 60
```

In `COLLAR_CONFIG`:
```r
iv_skew_holdings_url = get_golem_config_value("collar", "iv_skew.holdings_url", "https://..."),
iv_skew_top_n = get_golem_config_value("collar", "iv_skew.top_n", 20),
iv_skew_screening_target_days = get_golem_config_value("collar", "iv_skew.screening_target_days", 45),
iv_skew_screening_max_days = get_golem_config_value("collar", "iv_skew.screening_max_days", 60)
```

## Edge Cases

1. **No expiry in 45-60 day window:** Skip ticker silently. Some small-cap stocks may only have monthly expirations that don't fall in the window.
2. **IV is NA or zero:** Skip ticker silently. Some illiquid options have no IV from Questrade.
3. **Put IV is 0:** Skip (division by zero). Return NULL.
4. **CSV download fails:** Show user-facing warning, fall back to `inst/cache/IWB_holdings.csv`.
5. **Ticker not found on Questrade:** Existing `search_questrade_symbol()` returns NULL → skip silently.
6. **All tickers skipped:** Return empty tibble. UI shows "No collar opportunities found" message (existing behavior).
7. **Fewer than 20 stocks with valid IV data:** Return however many have valid data. Don't pad.
8. **Rate limit hit (429):** Let existing Questrade error handling manage. The parallel workers with controlled concurrency should stay under 20 req/s.

## Implementation Phases

### Phase 1: Data Foundation
- [ ] Move `claudedocs/IWB_holdings.csv` to `inst/cache/IWB_holdings.csv`
- [ ] Implement `fetch_iwb_holdings()` in `fct_collar_analysis.R`
- [ ] Add IV skew config entries to `golem-config.yml` and `utils_collar_config.R`
- [ ] Unit test: CSV parsing returns correct equity tickers
- **Verification:** `devtools::test(filter = "collar")`

### Phase 2: IV Screening Engine
- [ ] Implement `compute_iv_skew_ratio(ticker)` in `fct_collar_analysis.R`
- [ ] Implement `analyze_collar_iv_skew()` orchestration function in `fct_collar_analysis.R`
- [ ] Unit test: IV ratio computation with mocked Questrade data
- [ ] Unit test: Top-N selection logic
- **Verification:** `devtools::test(filter = "collar")`

### Phase 3: UI Integration
- [ ] Add `"IV Skew Screener" = "iv_skew"` to dropdown in `mod_collar_controls_ui`
- [ ] Add dispatch case in `analysis_function()` switch
- [ ] Add label in `variant_label()` reactive
- [ ] Manual test: select variant, run analysis, verify collar cards appear
- **Verification:** `devtools::load_all(); source("dev/run_dev.R")` — navigate to /collar, select IV Skew Screener, run analysis

## Definition of Done

This feature is complete when:
- [ ] All acceptance criteria in user stories pass
- [ ] All implementation phases verified
- [ ] Tests pass: `devtools::test(filter = "collar")`
- [ ] Package check: `devtools::check()` passes with no errors
- [ ] Manual verification: selecting "IV Skew Screener" from dropdown, running analysis, and seeing standard collar cards for top 20 IV-skew stocks

## Ralph Loop Command

```bash
/ralph docs/specs/i-want-your-help-to-implement-a-variant-to-the-existing-coll.md
```
