# Batch 4: Collars — Audit Findings

## Summary
- Files reviewed: 6
- Findings: 2 Critical, 4 High, 3 Medium, 2 Low
- Checklist coverage: NA/NULL handling, Error propagation, Config usage, Reactive isolation, Dead code

## Critical Findings

### CR-4-001: Silent fallback to config default without logging when SGOV yield is out of range
- **File:** `R/fct_collar_analysis.R:556-570`
- **Dimension:** Data Integrity
- **Description:** In `get_reinvestment_rate()`, if the fetched SGOV yield is NA, <=0, or >= `max_sgov_yield_sanity`, the function silently falls back to `COLLAR_CONFIG$sgov_yield_default` without logging why. This yields incorrect dividend reinvestment projections if SGOV API data is stale or malformed. The user is never informed that the critical reinvestment rate is coming from a fallback instead of live data. The outer functions log "SGOV yield for reinvestment: X%" but don't distinguish live vs fallback.

### CR-4-002: Duplicated list items in progress file (task tracking bug)
- **File:** `ralph-progress.md:12-15`
- **Dimension:** Maintainability
- **Description:** Batch 4 and Batch 5 are duplicated in the "Remaining" section of the progress file, indicating a task tracking issue that could lead to repeated work or confusion about completion status.

## High Findings

### HI-4-001: Hardcoded max workers default in UI module does not use config value
- **File:** `R/mod_collar_controls.R:73`
- **Dimension:** Maintainability
- **Description:** The `sliderInput` for `max_workers` defaults to 4, but this hardcoded value bypasses `COLLAR_CONFIG$max_workers` (which is 10 per the config file). If the config is updated, the UI will still show 4 as the default, creating inconsistency between the config layer and the UI layer.

### HI-4-002: Hardcoded shares_per_contract in parallel workers bypasses config chain
- **File:** `R/fct_collar_analysis.R:207`
- **Dimension:** Maintainability
- **Description:** The calculation `capital_required <- current_price * COLLAR_CONFIG$shares_per_contract` uses `COLLAR_CONFIG$shares_per_contract` directly in the analysis function. This is correct. However, if a worker process loads a stale version of `COLLAR_CONFIG`, it could use a different value than the main process. This pattern is safe only if `COLLAR_CONFIG` is immutable after load.

### HI-4-003: NA handling for option volume/OI uses zero-length check but doesn't log when fallback is used
- **File:** `R/fct_collar_analysis.R:234-237`
- **Dimension:** Error Handling
- **Description:** The code checks for zero-length and NA values in volume/OI fields and falls back to 0. However, it doesn't log when this fallback happens. If options data is incomplete, the user sees 0 volume/OI in results without knowing whether it's real data or a fallback, which could mislead them into thinking low-liquidity options are safe to trade.

### HI-4-004: Web scraping function `get_yahoo_most_active_etfs` has no caching, could hit rate limits
- **File:** `R/utils_collar_etf_universe.R:165-202`
- **Dimension:** Performance
- **Description:** The function scrapes Yahoo Finance every time it's called with no caching mechanism. If the collar analysis is run multiple times in a session (e.g., user experiments with different parameters), each run re-scrapes Yahoo, which is inefficient and could trigger rate limiting. Other strategies use caching (see `DIVIDEND_CAPTURE_CONFIG$cache_ttl_days`). This function should cache results with a TTL.

## Medium Findings

### ME-4-001: Unused function `create_collar_opportunity_card` (dead code)
- **File:** `R/mod_collar_results.R:291-386`
- **Dimension:** Maintainability
- **Description:** The function `create_collar_opportunity_card()` is defined but never called. The module uses `create_collar_card_with_risk()` instead. This is likely legacy code from before the risk analysis integration. Should be removed to reduce maintenance burden.

### ME-4-002: Magic number 50 for max button observers not explained
- **File:** `R/mod_collar_results.R:107`
- **Dimension:** Maintainability
- **Description:** The code creates button observers for indices 1-50 with a comment "reasonable max", but there's no validation that the results data will never exceed 50 rows. If a collar analysis returns more than 50 opportunities, buttons beyond #50 will not work. This limit should either come from config or be validated against the results data.

### ME-4-003: Config validation function `validate_collar_config` is exported but never called
- **File:** `R/utils_collar_config.R:57`
- **Dimension:** Maintainability
- **Description:** The function `validate_collar_config()` is exported and contains useful validation logic, but it's never invoked anywhere in the codebase. If invalid config values are set in `golem-config.yml`, the app will silently use them, potentially causing runtime errors. This validation should be called at package load time or in `run_app()`.

## Low Findings

### LO-4-001: Inconsistent comment style: "# For now, use curated list" vs formal docs
- **File:** `R/utils_collar_etf_universe.R:104`
- **Dimension:** Code Style
- **Description:** The comment "For now, use curated list" and "Future enhancement" suggest incomplete implementation, but the function is exported and documented as if it's production-ready. Either implement the dynamic filtering or remove the "future enhancement" comments to avoid confusion.

### LO-4-002: Duplicate fields in result tibble: `investment` and `capital_required` are identical
- **File:** `R/fct_collar_analysis.R:254-255`
- **Dimension:** Maintainability
- **Description:** The result tibble includes both `investment = capital_required` and `capital_required = capital_required`, which are identical. Similarly, `net_profit` and `max_profit` are identical (line 261-262). This duplication exists for backward compatibility but creates confusion. Consider deprecating the redundant fields with a migration plan.

## Dead Code

### DC-4-001: Function `create_collar_opportunity_card` is never called
- **File:** `R/mod_collar_results.R:291`
- **Description:** Defined as a legacy card constructor but replaced by `create_collar_card_with_risk()` in the current implementation. Can be safely removed.

### DC-4-002: Export `validate_collar_config` is never invoked
- **File:** `R/utils_collar_config.R:57`
- **Description:** Exported function with validation logic, but no call sites exist in the codebase. Either call it at startup or remove the export.

## Config Chain Verification

### Batch 4 Config Chain Status: ✅ Mostly Compliant

All config values are pulled from `golem-config.yml` via `get_golem_config_value()` in `utils_collar_config.R` and accessed through the `COLLAR_CONFIG` object. No direct environment variable reads or hardcoded values that bypass the config chain.

**Minor issue:** UI module default (4 workers) doesn't use `COLLAR_CONFIG$max_workers` (10), creating a mismatch between UI defaults and config defaults. See HI-4-001.

## Checklist Results

| Item | Issues Found |
|------|-------------|
| 1. NA/NULL handling | Yes - HI-4-003 (volume/OI fallback not logged), CR-4-001 (SGOV yield fallback silent) |
| 2. Error propagation | Yes - CR-4-001 (silent fallback without user notification) |
| 3. DB connection lifecycle | N/A - No database operations in this batch |
| 4. Hardcoded values | Yes - HI-4-001 (max_workers default in UI) |
| 5. Option symbol parsing | No issues - Uses `parse_option_details()` consistently |
| 6. Shared risk engine | Yes - Module integrates with `mod_position_risk_server()` correctly |
| 7. Reactive isolation | No issues - Modules use `moduleServer()` and `NS()` correctly |
| 8. Dead/unreachable code | Yes - DC-4-001, DC-4-002 (unused functions) |

## Risk-Specific Observations

1. **Dividend projection logic:** The collar strategy uses `calculate_dividend_projections()` to estimate dividend income during the holding period, which affects total return calculations. The accuracy depends on the SGOV yield for reinvestment (see CR-4-001).

2. **Net credit filtering:** The strategy filters out positions with `net_credit <= min_net_credit`, which prevents users from seeing collar positions that require net debit. This is correct for the "synthetic bond" interpretation but could hide some viable collar strategies where the user is willing to pay for downside protection.

3. **Strike selection with adjustment:** The `strike_adjustment_pct` parameter allows users to select OTM calls/puts instead of ATM. The code correctly applies this to the target strike (line 141), but the UI labels this as "Strike Adjustment" without explaining that positive values create an OTM collar (call struck above, put struck above) which changes the risk/return profile significantly.
