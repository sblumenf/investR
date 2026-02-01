# Batch 5: Calendar Spreads — Audit Findings

## Summary
- Files reviewed: 5
- Findings: 3 Critical, 5 High, 4 Medium, 1 Low
- Checklist coverage: NA/NULL handling, Error propagation, Hardcoded values, Dead code

## Critical Findings

### CR-5-001: Long put Ask price estimation uses hardcoded 5% spread when Ask is missing
- **File:** `R/fct_put_calendar_spread.R:114-119`
- **Dimension:** Correctness
- **Description:** In `calculate_calendar_spread_metrics()`, if the long put's Ask price is NA or missing, the code estimates it as `long_put$Bid * 1.05` (5% spread). This hardcoded 5% assumption is not validated against typical bid-ask spreads for options and could significantly underestimate the true entry cost for illiquid options where spreads can be 10-20%+. This affects the net_debit calculation, which in turn affects all return metrics. There's no logging to indicate when this fallback is used, so users see estimated costs without knowing they're not real quotes.

### CR-5-002: Estimated max profit uses hardcoded 75% multiplier with no validation
- **File:** `R/fct_put_calendar_spread.R:133`
- **Dimension:** Correctness
- **Description:** The code calculates `estimated_max_profit <- net_debit * 0.75` with a comment "Conservative estimate", but this 0.75 multiplier is hardcoded and not pulled from config. The comment on line 131-132 mentions "50-100% of debit" but the actual calculation uses 75% without explanation. This is a critical financial calculation that affects ROI, annualized return, and scoring. The multiplier should come from `PUT_CALENDAR_SPREAD_CONFIG` with clear documentation of why 75% is appropriate for dividend aristocrats.

### CR-5-003: Config object not pulled from golem-config.yml - hardcoded values break config chain
- **File:** `R/utils_put_calendar_spread_config.R:40-103`
- **Dimension:** Data Integrity
- **Description:** Unlike other strategy configs in the codebase, `PUT_CALENDAR_SPREAD_CONFIG` is entirely hardcoded in the R file and does not call `get_golem_config_value()` to pull values from `golem-config.yml`. This breaks the 3-tier config chain documented in CLAUDE.md. If `golem-config.yml` has calendar spread config values, they will be ignored. This creates inconsistency across the codebase where all other strategies (CSP, covered calls, dividend capture, collars) use the config chain but calendar spreads do not.

## High Findings

### HI-5-001: Scoring function uses hardcoded thresholds not from config for multi-tier scoring
- **File:** `R/utils_put_calendar_spread_config.R:298-350`
- **Dimension:** Maintainability
- **Description:** The `calculate_calendar_score()` function uses numerous hardcoded numeric thresholds for scoring (e.g., `net_theta > 0.05`, `net_vega > 0.15`, `open_interest > 500`, `debit_profit_ratio <= 0.4`). While the config object contains some thresholds like `iv_rank_excellent`, many scoring thresholds are hardcoded in the function itself. If these thresholds need tuning based on backtesting or market conditions, they require code changes instead of config changes.

### HI-5-002: No validation that long_ask estimation fallback was used
- **File:** `R/fct_put_calendar_spread.R:114-119`
- **Dimension:** Error Handling
- **Description:** When `long_put$Ask` is NA and the code falls back to estimating `long_put$Bid * 1.05`, there's no logging or result field to indicate this fallback was used. The user sees a `long_ask` value in the results but has no way to know if it's a real quote or an estimate. This is especially problematic for illiquid options where the estimation could be wildly inaccurate.

### HI-5-003: NA handling for Greeks uses silent NA propagation
- **File:** `R/fct_put_calendar_spread.R:147-169`
- **Dimension:** Error Handling
- **Description:** The code checks for NA values in Greeks (Theta, Vega, Delta) and calculates net values only if both legs have non-NA Greeks. Otherwise, it sets net values to NA. This is correct NA handling logic, but there's no logging to indicate when Greeks are unavailable. The opportunity score calculation (via `calculate_calendar_score()`) receives NA values and silently assigns 0 points to those components, which could make an otherwise good opportunity appear poor if Greeks data is unavailable.

### HI-5-004: Hardcoded profit target multipliers in return metrics calculation
- **File:** `R/fct_put_calendar_spread.R:76-78`
- **Dimension:** Maintainability
- **Description:** The profit target calculations use `PUT_CALENDAR_SPREAD_CONFIG$profit_target_conservative` (etc.), which is correct. However, these config values are hardcoded in the config file (0.15, 0.20, 0.25) rather than being pulled from `golem-config.yml`. If strategy parameters need tuning, they require code deployment instead of config file updates.

### HI-5-005: Input validation uses stop() instead of returning NULL with reason
- **File:** `R/fct_put_calendar_spread.R:32-37, 58-63`
- **Dimension:** Error Handling
- **Description:** Helper functions `calculate_calendar_net_debit()` and `calculate_calendar_return_metrics()` use `stop()` for invalid inputs, which will crash the analysis if called with bad data. While these are internal functions, if bad data propagates from options chain parsing (e.g., negative bid/ask due to data provider issues), the entire analysis stops instead of skipping that ticker. Consider using warning + return NULL pattern for more graceful degradation.

## Medium Findings

### ME-5-001: Function `validate_calendar_spread_config` is exported but never called
- **File:** `R/utils_put_calendar_spread_config.R:117`
- **Dimension:** Maintainability
- **Description:** The function `validate_calendar_spread_config()` is exported and contains comprehensive validation logic, but it's never invoked in the codebase (only in `get_calendar_spread_config()` when overrides are provided). The config is never validated at package load or app startup, so invalid hardcoded config values could cause runtime errors. This validation should be called during package initialization.

### ME-5-002: Hardcoded UI max_workers default (4) differs from config default (10)
- **File:** `R/mod_put_calendar_spread.R:70`
- **Dimension:** Maintainability
- **Description:** Same issue as collar strategy - the UI `sliderInput` defaults to 4 workers, but `PUT_CALENDAR_SPREAD_CONFIG$max_workers` is 10. This creates a mismatch between the config layer and the UI layer. Users see 4 as the default in the UI but the config says 10.

### ME-5-003: Progress bar enabled for parallel processing but no logging of individual failures
- **File:** `R/fct_put_calendar_spread.R:543`
- **Dimension:** Error Handling
- **Description:** The analysis uses `.progress = TRUE` for `future_map()`, which shows a progress bar, but individual stock failures are silently discarded (wrapped in `possibly()` returning NULL). If many stocks fail for a systematic reason (e.g., options API down), the user just sees fewer results without knowing why. Consider aggregating failure reasons and logging a summary.

### ME-5-004: Scoring function assumes dividend aristocrats are always range-bound
- **File:** `R/fct_put_calendar_spread.R:200-201`
- **Dimension:** Correctness
- **Description:** In `calculate_calendar_spread_metrics()`, the code passes `is_range_bound = TRUE` and `dividend_safe = TRUE` as hardcoded assumptions to the scoring function. While dividend aristocrats are often stable, these should be validated against actual price movement metrics (e.g., ATR, Bollinger Bands) and ex-dividend dates rather than assumed. This gives every opportunity +10 points (5 for range-bound, 5 for dividend-safe) regardless of actual conditions.

## Low Findings

### LO-5-001: Inconsistent indentation on line 77 (missing leading spaces)
- **File:** `R/fct_put_calendar_spread.R:77`
- **Dimension:** Code Style
- **Description:** Line 77 (`profit_target_20pct <- ...`) has no leading whitespace while surrounding lines are indented. This appears to be a formatting error that makes the code harder to read.

## Dead Code

No dead code found in this batch. All exported functions are called and all internal helpers are used.

## Config Chain Verification

### Batch 5 Config Chain Status: ❌ NON-COMPLIANT

**CRITICAL ISSUE:** The entire `PUT_CALENDAR_SPREAD_CONFIG` object is hardcoded in `utils_put_calendar_spread_config.R` and does NOT use `get_golem_config_value()` to pull values from `golem-config.yml`. This is a fundamental violation of the 3-tier config architecture used by all other strategies in the codebase.

**Comparison with other strategies:**
- Cash-Secured Puts: ✅ Uses `get_golem_config_value("cash_secured_puts", ...)`
- Covered Calls: ✅ Uses `get_golem_config_value("covered_calls", ...)`
- Dividend Capture: ✅ Uses `get_golem_config_value("dividend_capture", ...)`
- Collars: ✅ Uses `get_golem_config_value("collar", ...)`
- **Calendar Spreads: ❌ Hardcoded values only**

This means:
1. Config changes require code deployment (not just config file updates)
2. Environment-specific overrides (dev/prod) don't work for calendar spreads
3. The strategy is inconsistent with the rest of the codebase

## Checklist Results

| Item | Issues Found |
|------|-------------|
| 1. NA/NULL handling | Yes - HI-5-003 (Greeks NA propagation silent), HI-5-002 (Ask fallback not logged) |
| 2. Error propagation | Yes - HI-5-005 (stop() instead of graceful degradation), ME-5-003 (no failure summaries) |
| 3. DB connection lifecycle | N/A - No database operations in this batch |
| 4. Hardcoded values | Yes - CR-5-001 (5% spread estimate), CR-5-002 (75% max profit), CR-5-003 (entire config object), HI-5-001 (scoring thresholds) |
| 5. Option symbol parsing | No issues - Uses shared options chain functions |
| 6. Shared risk engine | N/A - Calendar spreads don't integrate with risk module yet |
| 7. Reactive isolation | No issues - Modules use `moduleServer()` and `NS()` correctly |
| 8. Dead/unreachable code | No - ME-5-001 (validation function never called but not dead, just unused) |

## Strategy-Specific Observations

1. **Calendar spread mechanics:** The strategy correctly implements the calendar spread logic (sell short-dated put, buy long-dated put at same strike), but the estimated max profit calculation is oversimplified. Real calendar spread max profit depends on IV skew, interest rates, and the stock price at front-month expiration. The 75% estimate may be too aggressive or too conservative depending on these factors.

2. **Opportunity scoring:** The 100-point scoring framework is well-designed with documented weights for each component (IV rank 25pts, IV ratio 15pts, net theta 15pts, etc.). However, many of the scoring thresholds are hardcoded in the function body rather than in the config object, making them hard to tune.

3. **Dividend aristocrats focus:** The strategy is specifically designed for dividend aristocrats (stable, range-bound stocks), but it hardcodes these assumptions (`is_range_bound = TRUE`) rather than validating them against actual data. A more robust implementation would check recent volatility, Bollinger Band width, or other technical indicators.

4. **Greeks availability:** The code gracefully handles missing Greeks data by setting NA values, but this silently penalizes opportunities in the scoring system. If options data providers don't include Greeks, every opportunity will score lower regardless of actual quality.

5. **Strike selection:** The strategy targets 95% strike (ATM/slightly OTM puts), which is appropriate for dividend aristocrats. The configurable `strike_pct` parameter allows users to adjust this, but there's no validation that the selected strike makes sense for a calendar spread (e.g., very deep ITM or OTM strikes might not have liquid markets).
