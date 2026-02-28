# Ralph Progress: IV Skew Collar Screener

## Completed Tasks

### Phase 1: Data Foundation ✓
- [x] `inst/cache/IWB_holdings.csv` — moved from `claudedocs/`, committed with `-f`
- [x] `inst/golem-config.yml` — iv_skew section added (holdings_url, top_n=20, screening_target_days=45, screening_max_days=60)
- [x] `R/utils_collar_config.R` — iv_skew config entries added to COLLAR_CONFIG
- [x] `fetch_iwb_holdings()` — parses iShares CSV, falls back to inst/cache on failure

### Phase 2: IV Screening Engine ✓
- [x] `compute_iv_skew_ratio(ticker)` — finds expiry in 45-60 day window, extracts ATM call/put IV
- [x] `analyze_collar_iv_skew()` — full pipeline: fetch holdings → parallel IV screen → top 20 → collar analysis

### Phase 3: UI Integration ✓
- [x] `"IV Skew Screener" = "iv_skew"` in dropdown
- [x] dispatch in analysis_function() switch
- [x] variant_label() returns "IV skew screened stocks"
- [x] Progress message: "Screening Russell 1000 for IV skew..."

### Tests ✓
- [x] `tests/testthat/test-fct_collar_iv_skew.R` — CSV parsing, IV ratio math, top-N selection
- [x] `devtools::test(filter = "collar")`: 28 tests, 0 failures

### Scope Lock Fix ✓
- [x] Fixed `~/.claude/scripts/ralph-loop.sh` — BSD sed compatible `[[:space:]]` + strip em-dash descriptions

## Verification Status
- [x] All phases complete
- [x] Tests pass: 28 passing
- [x] devtools::check() error is pre-existing (not introduced by this feature)
- [ ] Manual test: requires live Questrade connection

## ALL TASKS COMPLETE
