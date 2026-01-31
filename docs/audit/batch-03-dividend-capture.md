# Batch 3: Dividend Capture — Audit Findings

## Summary
- Files reviewed: 20
- Findings: 2 Critical, 5 High, 7 Medium, 2 Low
- Checklist coverage: NA/NULL handling, error propagation, hardcoded values, config chain, reactive isolation

## Critical Findings

### CR-3-001: Division by zero risk in drop_ratio calculation
- **File:** `R/fct_dividend_capture_weekly.R:80-84`, `R/fct_dividend_capture_monthly.R:103-107`, `R/fct_dividend_capture_monthly_high_yield.R:93-97`, `R/fct_high_yield_dividend_capture.R:151-155`
- **Dimension:** Correctness
- **Description:** All statistics calculation functions use `if (avg_dividend != 0)` to guard division, but this checks exact equality with floating point. If `avg_dividend` is very small but non-zero (e.g., 1e-10), division could produce misleading values. Should use tolerance-based comparison `abs(avg_dividend) < 1e-8` instead of exact equality.

### CR-3-002: Unprotected std_deviation in Sharpe ratio calculation
- **File:** `R/fct_high_yield_dividend_capture.R:192-196`
- **Dimension:** Correctness
- **Description:** Sharpe ratio calculation checks `if (std_deviation != 0)` but does not check if `std_deviation` is NA or Inf. If trade_results contains insufficient data or identical returns, `sd()` could return NA, leading to NA propagation without warning. Other files use utility functions from `utils_calculations.R` which may handle this, but this file computes inline without protection.

## High Findings

### HI-3-001: Missing NA checks in business days calculation
- **File:** `R/fct_high_yield_dividend_capture.R:44-61`
- **Dimension:** Error Handling
- **Description:** `calculate_business_days()` returns `NA_integer_` if inputs are NA, but does not validate that inputs are actual Date objects. If passed non-Date types, the function could fail silently or produce incorrect results. Should add `inherits(from_date, "Date")` checks.

### HI-3-002: SOFR fetch failure not handled in batch functions
- **File:** `R/fct_dividend_capture_weekly.R:350`, `R/fct_dividend_capture_monthly.R:310`, `R/fct_dividend_capture_monthly_high_yield.R:393`, `R/fct_high_yield_dividend_capture.R:493`
- **Dimension:** Data Integrity
- **Description:** All batch analysis functions call `fetch_sofr_rate()` once and pass it to per-ticker analysis, but do not check if the fetch failed (returned NULL or NA). If SOFR fetch fails, all subsequent Sharpe/Sortino calculations will be incorrect. Should validate `annual_sofr` is a valid numeric > 0 before proceeding.

### HI-3-003: Web scraping failures silently fall back without user notification
- **File:** `R/utils_dividend_capture_weekly_config.R:67-74`, `R/utils_dividend_capture_monthly_high_yield_config.R:83-91`
- **Dimension:** Error Handling
- **Description:** Both dynamic ticker fetching functions catch errors and silently fall back to hardcoded lists without informing the user. Users may not realize they're analyzing stale/incomplete data. Should log warning and display UI notification when fallback is used.

### HI-3-004: Hardcoded string parsing in web scraping
- **File:** `R/utils_dividend_capture_monthly_high_yield_config.R:140-152`
- **Dimension:** Maintainability
- **Description:** `scrape_monthly_high_yield_etfs()` uses hardcoded pattern "every 5 cells" and regex `gsub('%', '', yield_pct)` to parse HTML table. If stockanalysis.com changes their table structure, this will silently fail. Should add validation that parsed data matches expected schema (e.g., check that `n_etfs` is reasonable, validate yield values are in expected range).

### HI-3-005: Cache validity check does not protect against corrupted cache
- **File:** `R/utils_dividend_capture_weekly_config.R:130-144`, `R/utils_dividend_capture_monthly_high_yield_config.R:166-180`
- **Dimension:** Error Handling
- **Description:** Both `*_cache_is_valid()` functions check timestamp but do not validate that cached data is structurally correct. If cache environment exists but `$etfs` or `$tickers` is NULL or corrupted, functions will return TRUE and cause failures downstream. Should add `!is.null()` and length checks.

## Medium Findings

### ME-3-001: Inconsistent payment frequency estimation method
- **File:** `R/fct_high_yield_dividend_capture.R:168-176`
- **Dimension:** Correctness
- **Description:** High-yield stats function estimates `payments_per_year` from average interval between dividends (`365 / avg_interval`). This is fragile for stocks with irregular payment schedules. If a stock paid monthly for 6 months then stopped, average interval would still suggest monthly frequency. Other files use hardcoded frequency (52 weeks, 12 months) which is more predictable. Consider adding validation or capping payments_per_year to reasonable range.

### ME-3-002: Hardcoded investment amount (10000) in calculations
- **File:** `R/fct_dividend_capture_weekly.R:105-106`, `R/fct_dividend_capture_monthly.R:128-129`, `R/fct_dividend_capture_monthly_high_yield.R:118-119`, `R/fct_high_yield_dividend_capture.R:187-188`
- **Dimension:** Maintainability
- **Description:** All statistics functions use `config$investment_amount` (10000) directly in income projection calculations. This value appears in UI text as "$10k" but is not validated or explained. If config value changes, UI text would be misleading. Consider adding validation that investment_amount == 10000 or making UI text dynamic.

### ME-3-003: Redundant config accessor pattern
- **File:** `R/utils_dividend_capture_weekly_config.R:221-235`, `R/utils_dividend_capture_monthly_config.R:178-192`, `R/utils_dividend_capture_monthly_high_yield_config.R:261-275`
- **Dimension:** Maintainability
- **Description:** All three config modules define identical `get_*_config()` accessor functions with same error handling logic. This is code duplication. Could extract to shared utility function: `get_strategy_config(config_object, key)`.

### ME-3-004: Mixed use of lubridate and base R date functions
- **File:** `R/fct_high_yield_dividend_capture.R:58` (uses `wday`), `R/utils_dividend_capture.R:75` (uses `weekdays`)
- **Dimension:** Code Style
- **Description:** Codebase mixes lubridate::wday and base::weekdays for day-of-week extraction. This is inconsistent and could cause issues with locale settings (weekdays is locale-dependent). Should standardize on lubridate functions.

### ME-3-005: Page server functions do not handle module initialization failures
- **File:** `R/page_dividend_capture_weekly.R:40-45`, `R/page_dividend_capture_monthly.R:40-45`, `R/page_dividend_capture_monthly_high_yield.R:41-46`, `R/page_dividend_capture_russell_2000.R:43-48`
- **Dimension:** Error Handling
- **Description:** All page server functions call controls/results modules but do not check if they return NULL or fail to initialize. If module initialization fails (e.g., missing config), page will crash with obscure error. Should wrap in tryCatch or validate return values.

### ME-3-006: Filtering logic hardcoded in module instead of shared utility
- **File:** `R/mod_dividend_capture_monthly_results.R:98-116`
- **Dimension:** Maintainability
- **Description:** `match_days_range()` function hardcodes mapping of range strings to numeric ranges. This logic is specific to monthly capture UI but duplicates the config definition in `utils_dividend_capture_monthly_config.R:154-160`. If config changes, UI logic must also change. Should derive ranges from config instead of hardcoding.

### ME-3-007: No validation of quote_source option in parallel workers
- **File:** `R/fct_dividend_capture_weekly.R:294-300`, `R/fct_dividend_capture_monthly_high_yield.R:332-341`, `R/fct_high_yield_dividend_capture.R:417-423`
- **Dimension:** Error Handling
- **Description:** Batch functions capture `quote_source <- get_quote_source()` and pass to workers via `options(investR.quote_source = quote_source)`, but do not validate that get_quote_source() succeeded or that the option was set correctly in workers. If get_quote_source() returns NULL, workers would use default behavior without notification.

## Low Findings

### LO-3-001: Generic error message in config accessor
- **File:** `R/utils_dividend_capture_weekly_config.R:227-230`, `R/utils_dividend_capture_monthly_config.R:184-187`, `R/utils_dividend_capture_monthly_high_yield_config.R:267-270`
- **Dimension:** Code Style
- **Description:** All config accessor functions throw same generic error "Configuration key '%s' not found". Could include strategy name in error message for clarity when debugging multi-strategy issues.

### LO-3-002: Unused parameter in analyze_monthly_etf
- **File:** `R/fct_dividend_capture_monthly.R:208`
- **Dimension:** Maintainability
- **Description:** Function signature is `analyze_monthly_etf(ticker, schedule_type = NULL, annual_sofr)` but `annual_sofr` is not marked as required in function definition, suggesting it was added later. Should add documentation clarifying that annual_sofr is required (not optional) despite lack of default value.

## Dead Code
None identified.

## Notes
- **Positive:** All dividend capture strategies follow consistent DRY patterns, reusing `analyze_dividend_events()` and `should_filter_dividend_opportunity()` from utils_dividend_capture.R.
- **Positive:** Two-phase filtering approach (lightweight screening → heavy backtesting) is well-implemented and reduces API call volume.
- **Positive:** Config chain is followed correctly - all files access golem config via `get_golem_config_value()` with fallback defaults.
- **Observation:** High-yield strategy is more complex than weekly/monthly due to dynamic universe and business day calculations, which contributes to higher finding count in that file specifically.
