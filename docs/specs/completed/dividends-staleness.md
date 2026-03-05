# Specification: Dividend Staleness Guard for Strategy Screening

## Overview

Extend the existing dividend staleness guard (currently only in the post-trade projection engine) to the pre-trade strategy screening phase. Stocks with stale/suspended dividends should be filtered out of results when running Dividend Aristocrats or Collar strategy screens, preventing the user from entering positions based on phantom dividend income.

## Constraints

### Files to Modify
- `R/utils_market_data.R` — add shared `is_dividend_stale()` helper
- `R/fct_aristocrats_analysis.R` — call helper in `get_stock_data()` (~line 379)
- `R/fct_collar_analysis.R` — call helper in `analyze_collar_single()` (~line 68)
- `R/fct_income_projection_engine.R` — refactor `generate_dividend_events()` to use shared helper instead of inline staleness check
- `tests/testthat/test-dividend_staleness.R` — new test file

### Files NOT to Modify
- UI/page files (`page_*.R`) — no UI changes needed
- Module files (`mod_*.R`) — filtering happens in the analysis layer
- Config files — no new config values needed (threshold is algorithmic: 1.5x avg interval)
- Database schema — no changes

### New Files Allowed
- `tests/testthat/test-dividend_staleness.R`

### New Dependencies Allowed
- No

### Existing Code to Reuse
- `fetch_dividend_history()` in `utils_market_data.R` — already fetches Yahoo Finance dividend data
- The inline staleness check in `generate_dividend_events()` at `fct_income_projection_engine.R:221-231` — this logic becomes the basis for the shared helper
- Logging via `logger` package (`log_warn`, `log_info`)

### Out of Scope
- UI notifications or visual indicators for stale dividends
- Changes to non-dividend collar variants (zero-dividend, leveraged ETF, most-shorted)
- Changes to the dividend data source (Yahoo Finance via quantmod)
- Any changes to the post-trade position group creation flow (already has its own guard)
- Adding configurable thresholds to golem-config.yml

## User Stories

### US-1: Shared Staleness Helper Function

**Description:** As a developer, I want a single reusable function that determines whether a stock's dividend history is stale, so that all strategies use the same logic.

**Acceptance Criteria:**
- [ ] `is_dividend_stale(dividend_history)` exists in `utils_market_data.R`
- [ ] Accepts an xts object (output of `fetch_dividend_history()`)
- [ ] Returns a list with `is_stale` (logical), `last_date` (Date or NA), `days_since` (numeric or NA), `threshold` (numeric or NA)
- [ ] Uses 1.5x average interval logic: if `days_since_last > avg_days_between * 1.5`, returns `is_stale = TRUE`
- [ ] Returns `is_stale = FALSE` for NULL/empty input or fewer than 2 records (insufficient data to judge)
- [ ] Unit tests pass with mock data covering: quarterly payer (not stale), suspended quarterly payer (stale), annual payer (not stale mid-cycle), NULL input, single-record input

### US-2: Filter Stale Dividends in Aristocrats Screening

**Description:** As an investor running a Dividend Aristocrats screen, I want stocks with suspended dividends automatically excluded from results so I don't evaluate positions based on phantom income.

**Acceptance Criteria:**
- [ ] `get_stock_data()` in `fct_aristocrats_analysis.R` calls `is_dividend_stale()` after fetching dividend history
- [ ] If stale, the function returns NULL (same as existing error/skip behavior) causing the stock to be excluded from results
- [ ] A `log_warn` message is emitted for each stale stock: "Screening: Skipping {ticker} - dividend appears suspended (last payment {date}, {days} days ago)"
- [ ] All Aristocrats variants (Kings, Champions, Contenders, Challengers) are covered since they all flow through `get_stock_data()`
- [ ] A stock like HE (last dividend Aug 2023) would be excluded from results

### US-3: Filter Stale Dividends in Collar Screening

**Description:** As an investor running a Collar strategy screen, I want stocks with suspended dividends excluded from dividend-paying variants so my projected income is realistic.

**Acceptance Criteria:**
- [ ] `analyze_collar_single()` in `fct_collar_analysis.R` calls `is_dividend_stale()` after fetching dividend history
- [ ] If stale, the function returns NULL causing the stock to be excluded from results
- [ ] Same `log_warn` message format as US-2
- [ ] Only applied when the variant expects dividends (dividend-paying S&P 500, Finviz screened, IV skew, overbought/oversold). NOT applied to zero-dividend, leveraged ETF, or most-shorted variants
- [ ] The check respects the existing `dividend_filter` parameter or equivalent variant indicator in the collar pipeline

### US-4: Unify Projection Guard with Shared Helper

**Description:** As a developer, I want the existing inline staleness check in `generate_dividend_events()` replaced with a call to the shared `is_dividend_stale()` helper, so there's one source of truth.

**Acceptance Criteria:**
- [ ] The inline staleness check at `fct_income_projection_engine.R:221-231` is replaced with a call to `is_dividend_stale()`
- [ ] Behavior is identical: same threshold (1.5x), same empty tibble return, same log warning
- [ ] Existing projection behavior is unchanged — HE collar group still gets no dividend projections

### US-5: Summary Log for Filtered Stocks

**Description:** As an investor, I want to see a console log summary of which stocks were filtered out due to stale dividends, so I'm aware of what was excluded.

**Acceptance Criteria:**
- [ ] After a screening run completes, a single `log_info` message summarizes filtered stocks: "Screening complete: Filtered {n} stocks with stale dividends: {ticker1}, {ticker2}, ..."
- [ ] Message only appears if at least 1 stock was filtered
- [ ] The summary is logged at the orchestrator level (`analyze_aristocrats()` or `analyze_collar_stocks()` / `analyze_collar_custom_list()` / etc.)

## Technical Design

### Shared Helper: `is_dividend_stale()`

```r
# Location: R/utils_market_data.R

is_dividend_stale <- function(dividend_history) {
  # Returns list(is_stale, last_date, days_since, threshold)

  # Guard: insufficient data to judge
  if (is.null(dividend_history) || nrow(dividend_history) < 2) {
    return(list(is_stale = FALSE, last_date = NA, days_since = NA, threshold = NA))
  }

  div_dates <- zoo::index(dividend_history)
  days_between <- as.numeric(diff(div_dates))
  avg_days_between <- mean(days_between)
  days_since_last <- as.numeric(Sys.Date() - max(div_dates))
  threshold <- avg_days_between * 1.5

  list(
    is_stale = days_since_last > threshold,
    last_date = max(div_dates),
    days_since = round(days_since_last),
    threshold = round(threshold)
  )
}
```

### Integration Points

**Aristocrats** — in `get_stock_data()` after `dividends <- fetch_dividend_history(...)`:
```r
staleness <- is_dividend_stale(dividends)
if (staleness$is_stale) {
  log_warn("Screening: Skipping {ticker} - dividend appears suspended (last payment {staleness$last_date}, {staleness$days_since} days ago)")
  return(NULL)
}
```

**Collar** — in `analyze_collar_single()` after `dividends <- fetch_dividend_history(...)`, but only when variant expects dividends:
```r
staleness <- is_dividend_stale(dividends)
if (staleness$is_stale) {
  log_warn("Screening: Skipping {ticker} - dividend appears suspended (last payment {staleness$last_date}, {staleness$days_since} days ago)")
  return(NULL)
}
```

**Projection engine** — in `generate_dividend_events()`, replace inline check with:
```r
staleness <- is_dividend_stale(div_history)  # div_history is already an xts object here
if (staleness$is_stale) {
  log_warn("Income Projection: Dividend appears suspended for {ticker} - last payment {staleness$last_date}, {staleness$days_since} days ago (threshold: {staleness$threshold} days)")
  return(tibble::tibble(...))
}
```

### Non-Dividend Variant Handling

The collar pipeline has a `dividend_filter` parameter or variant type that determines whether the screen targets dividend-paying stocks. The staleness check should only be applied when dividends are expected. The exact mechanism depends on how the variant is identified in `analyze_collar_single()` — either via a parameter passed down from the orchestrator, or by checking whether dividends were fetched at all.

## Implementation Phases

### Phase 1: Create Shared Helper + Tests
- [ ] Add `is_dividend_stale()` to `R/utils_market_data.R`
- [ ] Create `tests/testthat/test-dividend_staleness.R` with mock data tests
- **Verification:** `devtools::test(filter = "dividend_staleness")`

### Phase 2: Integrate into Aristocrats and Collar Screening
- [ ] Call `is_dividend_stale()` in `get_stock_data()` (fct_aristocrats_analysis.R)
- [ ] Call `is_dividend_stale()` in `analyze_collar_single()` (fct_collar_analysis.R) for dividend-expecting variants only
- [ ] Add summary log at orchestrator level
- **Verification:** `devtools::test()` — no regressions. Manual test: run Aristocrats screen and verify HE-like stocks are excluded with console log.

### Phase 3: Unify Projection Guard
- [ ] Replace inline staleness check in `generate_dividend_events()` (fct_income_projection_engine.R) with call to `is_dividend_stale()`
- [ ] Verify identical behavior for existing HE collar group
- **Verification:** `devtools::test()` — no regressions. Manually verify HE collar group still has no dividend projections.

## Definition of Done

This feature is complete when:
- [ ] All acceptance criteria in US-1 through US-5 pass
- [ ] All implementation phases verified
- [ ] Tests pass: `devtools::test(filter = "dividend_staleness")`
- [ ] Full test suite: `devtools::test()` — no new failures
- [ ] Manual verification: run Aristocrats screen, confirm stale-dividend stocks are excluded and logged

## Ralph Loop Command

```bash
/ralph docs/specs/dividends-staleness.md
```
