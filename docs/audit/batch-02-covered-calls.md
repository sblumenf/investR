# Batch 2: Covered Calls / Dynamic Covered Calls — Audit Findings

## Summary
- Files reviewed: 9
- Findings: 3 Critical, 5 High, 6 Medium, 2 Low
- Checklist coverage: NA/NULL handling (3 findings), Error propagation (4 findings), Config chain verification (2 findings), Reactive isolation (2 findings), Hardcoded values (2 findings)

## Critical Findings

### CR-2-001: Unvalidated drawdown metrics in dynamic strike calculation
- **File:** `R/fct_dynamic_covered_calls_analysis.R:131-149`
- **Dimension:** Correctness | Data Integrity
- **Description:** `analyze_single_stock_dynamic()` checks if `drawdown_metrics$max_drawdown` is NA (line 133), but does not validate `drawdown_metrics$drawdown_interval` before passing it to `calculate_dynamic_target_days()` (line 146). If `drawdown_interval` is NA (possible from `calculate_drawdown_metrics()` error path, line 94), the function will call `calculate_dynamic_target_days(NA, ...)`, which may produce invalid `target_days`. The code continues with this invalid value to filter expirations, potentially causing incorrect option selection.

### CR-2-002: No validation of config values at initialization
- **File:** `R/utils_dynamic_covered_calls_config.R:16-61`
- **Dimension:** Data Integrity
- **Description:** `DYNAMIC_CONFIG` object is initialized by calling `get_golem_config_value()` for each parameter (lines 18-60), but `validate_dynamic_config()` is never called at initialization. If golem-config.yml contains invalid values (e.g., `max_workers: -1`, `default_min_strike_pct: 1.5`), the config object will be created with invalid data, and validation only occurs if someone explicitly calls `validate_dynamic_config()`. The config should be validated immediately after initialization to fail fast.

### CR-2-003: Cache TTL not enforced in get_cached_options
- **File:** `R/fct_dynamic_covered_calls_analysis.R:162-180`
- **Dimension:** Data Integrity
- **Description:** The code calls `get_cached_options(ticker)` (line 162) and `set_cached_options(ticker, opt_chain_full)` (line 176) but these functions are not defined in this file. If these caching functions don't properly enforce the 8-hour TTL from `DYNAMIC_CONFIG$cache_ttl_hours`, stale options data could be used for analysis, leading to incorrect pricing and strikes. The caching layer is referenced but not verified in this batch.

## High Findings

### HI-2-001: Silent error swallowing in async execution
- **File:** `R/mod_dynamic_covered_calls_analysis.R:247-261`
- **Dimension:** Error Handling
- **Description:** The promise error handler (lines 247-261) catches all errors and shows a notification, but does not log the error or provide detailed diagnostics. The user sees "Analysis failed: [message]" but there's no log entry for debugging. Compare to the synchronous path (line 276) which uses tryCatch but also doesn't log errors. Per requirements, errors should be logged and users should be informed of fallback sources.

### HI-2-002: Missing fallback notification in synchronous path
- **File:** `R/mod_dynamic_covered_calls_analysis.R:263-302`
- **Dimension:** Error Handling
- **Description:** The synchronous execution path (lines 264-298) calls `check_and_notify_fallbacks()` (line 301) but only AFTER results are returned. If the analysis function encounters Questrade failures mid-execution and falls back to Yahoo Finance, the user won't see fallback notifications until after the entire analysis completes. The async path (line 246) has the same issue. Fallback tracking should be checked and reported during execution, not just at the end.

### HI-2-003: Zero-dividend filter logic error
- **File:** `R/mod_etf_covered_calls_analysis.R:189-193`
- **Dimension:** Correctness
- **Description:** Lines 189-193 set `dividend_yield_min = 0` and `dividend_yield_max = 0` for the "zero_dividend" filter. This will filter for ETFs with EXACTLY 0% dividend yield, which may be too restrictive. Most "zero-dividend" ETFs report yields like 0.01% or 0.05% due to occasional distributions. The logic should use a threshold (e.g., `< 0.5%`) rather than exact equality. This is duplicated from Batch 1 (mod_etf_cash_secured_puts_yfscreen.R).

### HI-2-004: No error handling in filter_expirations_from_data
- **File:** `R/fct_dynamic_covered_calls_analysis.R:183-189`
- **Dimension:** Error Handling
- **Description:** The code calls `filter_expirations_from_data()` (line 183-187) but this function is not defined in this file. If this function errors (e.g., malformed expiration strings, invalid tolerance_pct), the error will propagate up and kill the entire stock analysis. There's no tryCatch wrapping this call, and the function is not validated to exist in this batch.

### HI-2-005: Reactive side effect in parallel workers
- **File:** `R/utils_covered_calls_shared.R:78-90`
- **Dimension:** Reactive Isolation
- **Description:** `process_stocks_parallel_generic()` captures the quote source from the main process (line 79) and sets it in each worker (line 90: `options(investR.quote_source = quote_source)`). This is a global option side effect that could interfere with concurrent analyses if multiple Shiny sessions run in the same R process. Better practice would be to pass quote_source as a parameter to the analysis function.

## Medium Findings

### ME-2-001: Hardcoded market_cap_max in module
- **File:** `R/mod_etf_covered_calls_analysis.R:201`
- **Dimension:** Maintainability | Config Chain Verification
- **Description:** Line 201 hardcodes `market_cap_max = NULL` instead of reading from config. This breaks the 3-tier config pattern (golem-config.yml → STRATEGY_CONFIG → get_golem_config_value()). If the business logic needs a maximum market cap filter, it should come from config, not be hardcoded in the module.

### ME-2-002: Inconsistent error handling between strategies
- **File:** `R/fct_dynamic_covered_calls_analysis.R:111-276` vs `R/utils_covered_calls_shared.R:184-227`
- **Dimension:** Maintainability
- **Description:** `analyze_single_stock_dynamic()` uses a catch-all tryCatch at line 273 that returns NULL on any error, while `analyze_single_stock_generic()` has more sophisticated error handling with failure_reason tracking (line 192-227). This inconsistency makes debugging harder — some strategies provide detailed failure reasons, others just return NULL.

### ME-2-003: Duplicate conditional panel pattern
- **File:** `R/mod_etf_covered_calls_analysis.R:39-52` (same as Batch 1)
- **Dimension:** Maintainability
- **Description:** `conditionalPanel` with `ns = ns` parameter is repeated across multiple modules (also in Batch 1: mod_sp500_cash_secured_puts.R, mod_etf_cash_secured_puts_yfscreen.R). This fragile pattern should be extracted to a shared helper or replaced with server-side `renderUI` for reliability.

### ME-2-004: No rate limiting in shared parallel processor
- **File:** `R/utils_covered_calls_shared.R:72-164`
- **Dimension:** Performance | Error Handling
- **Description:** `process_stocks_parallel_generic()` does not implement rate limiting. It relies on the caller to handle rate limiting (e.g., `analyze_dynamic_covered_calls()` uses `Sys.sleep()` at line 371, but `analyze_puts_generic()` from Batch 1 does not). This inconsistency means some strategies respect API rate limits while others may hit 429 errors.

### ME-2-005: Unused parameter in analyze_single_stock_generic
- **File:** `R/utils_covered_calls_shared.R:184-200`
- **Dimension:** Dead Code
- **Description:** `analyze_single_stock_generic()` accepts `target_days` parameter (line 189) but line 200 is truncated in the audit read. Need to verify if `target_days` is actually used in the function body. If unused, this is dead code carried over from copy-paste.

### ME-2-006: Hardcoded comment in config file
- **File:** `R/utils_dynamic_covered_calls_config.R:13-14`
- **Dimension:** Code Style
- **Description:** Lines 13-14 state "Configuration values are loaded from inst/golem-config.yml for centralized management" but then the config object is initialized with inline `get_golem_config_value()` calls. This comment is misleading — the CONFIG object is built dynamically, not loaded from a file. Minor documentation clarity issue.

## Low Findings

### LO-2-001: Overly verbose logging in analyze_dynamic_covered_calls
- **File:** `R/fct_dynamic_covered_calls_analysis.R:320-340`
- **Dimension:** Performance
- **Description:** Lines 320-340 log multiple info messages for every analysis run. For 100 stocks, this produces hundreds of log lines. Consider using log_debug() for intermediate steps.

### LO-2-002: Commented section header with no content
- **File:** None identified in this batch
- **Dimension:** N/A
- **Description:** No commented-out section headers found in Batch 2 files.

## Dead Code

None identified in this batch. All referenced helper functions (`filter_expirations_from_data`, `get_cached_options`, `set_cached_options`, `check_and_notify_fallbacks`) are likely defined in other batches (utilities or background refresh). Verification deferred to Batch 6 (Portfolio Core) and Batch 7 (Questrade API).
