# Batch 8: Aristocrats / Zero-Dividend / Other Analysis — Audit Findings

## Summary
- Files reviewed: 24
- Findings: 3 Critical, 6 High, 8 Medium, 3 Low
- Checklist coverage: NA/NULL handling, error propagation, config chain adherence, dead code

## Critical Findings

### CR-8-001: Money Market Rotation TWR Calculation Incorrect
- **File:** `R/fct_money_market_rotation.R:604-608`
- **Dimension:** Correctness
- **Description:** Modified Dietz TWR formula is incorrect. Line 605 divides by `beginning_value`, but the correct Modified Dietz formula requires dividing by `beginning_value + weighted_cash_flows`. This produces incorrect returns when capital flows occur. Additionally, the formula at 605 calculates `(ending_value - beginning_value - net_cash_flows) / beginning_value` but should be `(ending_value - beginning_value - net_cash_flows) / (beginning_value + weighted_flows)`. The weighted flows calculation at line 600 (`weighted_flows = net_flows * 0.5`) is a simplification that assumes mid-period flows, which may be inaccurate for actual flow timing.

### CR-8-002: Zero xts Object nrow() Returns NULL
- **File:** `R/fct_aristocrats_analysis.R:44,56,58,379`
- **Dimension:** Correctness
- **Description:** Code checks `nrow(dividends)` after checking `length(dividends)` with comment noting "For empty xts objects, nrow() returns NULL, so check length() first". This pattern appears at lines 44, 56, 58, and 379. If the length() check is removed or bypassed, `nrow()` could return NULL causing comparison errors. The code is defensive but fragile—better pattern would be to use `NROW()` (capital) which always returns an integer, or wrap in `is.null()` checks.

### CR-8-003: Stock Universe Fallback List Hardcoded and Stale
- **File:** `R/utils_stock_universe.R:15-27`
- **Dimension:** Data Integrity
- **Description:** SP500_FALLBACK_TICKERS is a hardcoded list of 110 tickers with comment "current as of 2025" but no mechanism to detect when it becomes stale. If both Wikipedia and DataHub fetches fail (lines 228-238), the function silently uses this fallback with only a warning. S&P 500 constituents change quarterly, so this list could be months out of date. The function should either fail with an error when both sources are unavailable, or include a last-updated timestamp and refuse to use fallback data older than 90 days.

## High Findings

### HI-8-001: Database Connection Not Closed in Money Market Module
- **File:** `R/fct_money_market_rotation.R:56-74,88-167,178-240`
- **Dimension:** Data Integrity
- **Description:** Functions `get_first_week_date`, `fetch_mm_rotation_transactions`, and `fetch_mm_rotation_dividends` all accept a DuckDB connection as parameter but never close it. While the caller is responsible for connection lifecycle, there's no `on.exit(dbDisconnect(con))` guard. If these functions are called standalone (e.g., during testing or debugging), connections will leak. Better pattern: functions should own connection lifecycle OR document connection ownership in roxygen.

### HI-8-002: Dividend Attribution Algorithm Has Edge Cases
- **File:** `R/fct_money_market_rotation.R:362-428`
- **Dimension:** Correctness
- **Description:** `attribute_dividends_to_rotations` uses complex logic (lines 392-422) with nested conditionals and early breaks that make it hard to verify correctness. Edge case: if a dividend posts more than 5 days after sell (line 407), it falls through to the "fallback" logic at 415-421 which attributes to "most recent rotation for this ticker", but this could be a *previous* rotation if a new one started. The fallback uses `first()` at line 420 which returns NA_integer_ if vector is empty, but this should never happen due to line 416 filter—yet no explicit NA check exists.

### HI-8-003: Period Returns Calculation Has Duplicate Logic
- **File:** `R/fct_money_market_rotation.R:717-840,1066-1218`
- **Dimension:** Maintainability
- **Description:** `calculate_period_returns` (717-840) and `calculate_returns_by_year` (1066-1218) contain nearly identical TWR calculation logic (lines 804-820 vs 1183-1201). The formulas for `weighted_flows`, `twr_pct`, and special cases are duplicated. If the TWR bug (CR-8-001) is fixed, it must be fixed in *three* places (calculate_twr, calculate_period_returns, calculate_returns_by_year). Extract shared logic to `calculate_twr_for_period(start_value, end_value, net_flows)`.

### HI-8-004: Parallel Processing Plan Not Restored on Early Exit
- **File:** `R/utils_stock_universe.R:317-318,383-384`
- **Dimension:** Error Handling
- **Description:** `get_zero_dividend_stocks` and `get_dividend_paying_sp500` use `on.exit(future::plan(oplan), add = TRUE)` to restore the original future plan, but if an error occurs between line 304/370 (get_sp500_stocks call) and line 317/383 (plan setup), the function exits without restoring. While unlikely, if `get_sp500_stocks()` throws an error, the original plan is safe. However, if the `future::plan()` call itself fails (line 317/383), the on.exit handler is never registered and the session's future plan could be left in an inconsistent state.

### HI-8-005: Aristocrats Web Scraping Has Silent Failure Path
- **File:** `R/fct_aristocrats_analysis.R:268-331`
- **Dimension:** Error Handling
- **Description:** `get_dividend_aristocrats` tries StockAnalysis.com (272-295), then Wikipedia (300-324), returning NULL on both failures. At line 326-328, if both sources fail, it throws an error with message "Unable to fetch Dividend Aristocrats from any source". However, the tryCatch blocks return NULL on error without re-throwing, so the error at 327 will always execute if both fail. The user sees an error but has no context about *why* both sources failed (network issue, site structure change, etc.). The function should log the specific error from each source before throwing the generic error.

### HI-8-006: Missing Input Validation in Zero Dividend Functions
- **File:** `R/fct_zero_dividend_analysis.R:47-84,112-162,195-230,266-301`
- **Dimension:** Error Handling
- **Description:** Functions `analyze_zero_dividend`, `analyze_zero_dividend_custom_list`, `analyze_zero_dividend_etfs`, and `analyze_zero_dividend_yahoo_active_etfs` accept numeric parameters (strike_threshold_pct, min_days, max_days, count, max_workers) but perform zero validation. All parameters are passed directly to downstream functions or config objects. If a user calls `analyze_zero_dividend(strike_threshold_pct = 2.0)` (200%), the function would attempt the analysis with nonsensical parameters. Add validation: `if (strike_threshold_pct <= 0 || strike_threshold_pct > 1) stop("...")`.

## Medium Findings

### ME-8-001: Rotation History Builder Doesn't Handle Partial Sells
- **File:** `R/fct_money_market_rotation.R:254-347`
- **Dimension:** Correctness
- **Description:** `build_rotation_history` assumes each Buy is fully closed by a single Sell (1:1 pairing). Lines 300-314 close the *entire* position on Sell. If a user sells only *part* of their position (e.g., Buy 100 shares, Sell 50 shares), the function incorrectly treats it as a full rotation close. Money market funds typically trade in round lots so partial sells are rare, but the function should either handle this case or explicitly document the assumption.

### ME-8-002: Cache Directory Creation Race Condition
- **File:** `R/utils_stock_universe.R:33-47`
- **Dimension:** Maintainability
- **Description:** `get_cache_dir` checks `!dir.exists(cache_dir)` at line 42, then creates the directory at line 43. In a multi-process environment (unlikely with Shiny single-process model, but possible with parallel workers), two processes could both check at line 42, both see FALSE, and both attempt to create. The `dir.create` call uses `showWarnings = FALSE` which silences the error, but this masks legitimate failures. Better: `dir.create(..., showWarnings = FALSE)` should be `dir.create(..., recursive = TRUE)` without suppressing warnings, as `recursive = TRUE` already handles existing directories gracefully.

### ME-8-003: Aristocrats Config get_config() Has Duplicate Name in Package
- **File:** `R/utils_aristocrats_config.R:98-109`
- **Dimension:** Maintainability
- **Description:** Exported function `get_config()` is a generic name that could conflict with other packages or future investR modules. The function is specific to aristocrats strategy but doesn't include "aristocrats" in its name. Compare with `get_zero_dividend_config()` in utils_zero_dividend_config.R (line 87) which is properly namespaced. Rename to `get_aristocrats_config()` for consistency and to avoid namespace pollution.

### ME-8-004: Money Market Rotation SQL Uses String Interpolation
- **File:** `R/fct_money_market_rotation.R:92-119,182-204`
- **Dimension:** Security
- **Description:** Functions use `glue::glue()` to build SQL with `tickers_sql <- paste0("'", paste(always_tickers, collapse = "', '"), "'")` at lines 90 and 180. While `always_tickers` comes from a trusted internal function, if `get_always_included_tickers()` were ever modified to accept user input, this would be vulnerable to SQL injection. Better: use DBI parameterized queries with `?` placeholders and `params` argument to `dbGetQuery()`. DuckDB supports this via `dbBind()` or inline `$1, $2` parameters.

### ME-8-005: Duplicate Empty Tibble Schema Definitions
- **File:** `R/fct_money_market_rotation.R:128-139,156-167,211-218,256-271,328-343,444-456,509-515`
- **Dimension:** Maintainability
- **Description:** Seven functions define empty tibble schemas with identical column structures for error/empty returns. Each function manually lists all columns with types. If the schema changes (e.g., add a new column to rotation history), all seven locations must be updated. Extract to shared helper: `empty_rotation_history_tibble()`, `empty_transactions_tibble()`, etc.

### ME-8-006: Covered Calls Generic Delegation Has No Error Context
- **File:** `R/fct_aristocrats_analysis.R:698-705, R/fct_zero_dividend_analysis.R:71-83`
- **Dimension:** Error Handling
- **Description:** `analyze_aristocrats` and `analyze_zero_dividend` delegate to `analyze_covered_calls_generic` with strategy-specific parameters but provide no error handling. If `analyze_covered_calls_generic` throws an error, the user sees a generic traceback without knowing which strategy or ticker caused the failure. Wrap the call in tryCatch to add context: `tryCatch(..., error = function(e) stop("Aristocrats analysis failed: ", e$message))`.

### ME-8-007: Stock Universe Caching Doesn't Validate Data After Load
- **File:** `R/utils_stock_universe.R:222-225,298-301,364-367`
- **Dimension:** Data Integrity
- **Description:** Functions `get_sp500_stocks`, `get_zero_dividend_stocks`, and `get_dividend_paying_sp500` load cached data with `load_from_cache()` and return it immediately without validation. If the cached RDS file is corrupted or contains unexpected data (e.g., numeric vector instead of character vector), the function silently returns bad data. Add validation after cache load: `if (!is.character(cached_data) || length(cached_data) == 0) { ... invalidate cache ... }`.

### ME-8-008: Aristocrats Config Validation Never Called
- **File:** `R/utils_aristocrats_config.R:66-80`
- **Dimension:** Maintainability
- **Description:** `validate_config()` function exists but is never called anywhere in the codebase. The ARISTOCRATS_CONFIG object is constructed at package load time (lines 16-57) but no validation occurs. If `golem-config.yml` contains invalid values (e.g., `strike_threshold_pct: 2.0`), the error won't surface until analysis runs and produces nonsensical results. Call `validate_config()` at the end of the CONFIG definition or in `.onLoad()`.

## Low Findings

### LO-8-001: Inconsistent Logging Levels
- **File:** `R/fct_aristocrats_analysis.R:269,287,314,489,493,506,577`
- **Dimension:** Code Style
- **Description:** `get_dividend_aristocrats` uses `log_info`, `log_success`, `log_warn` inconsistently. Line 269 logs "Fetching..." as info, line 287 logs "Found X aristocrats" as success, but line 314 logs same message as success for Wikipedia fallback. Inconsistent whether finding data is "info" or "success". Standardize: `log_info` for status updates, `log_success` only for final result, `log_warn` for fallbacks.

### LO-8-002: Magic Number 50 in Button Observer Loop
- **File:** `R/mod_aristocrats_results_table.R:80, R/mod_zero_dividend_results_table.R:93`
- **Dimension:** Maintainability
- **Description:** Both modules use `lapply(1:50, ...)` to create observers for risk analysis buttons. The number 50 is hardcoded with comment "reasonable max". If more than 50 results are returned, buttons 51+ won't trigger risk analysis. Extract to config: `RESULTS_TABLE_MAX_ROWS = 50` or dynamically create observers based on `nrow(results_data())`.

### LO-8-003: Unused Function create_zero_dividend_opportunity_card
- **File:** `R/mod_zero_dividend_results_table.R:247-312`
- **Dimension:** Dead Code
- **Description:** Function `create_zero_dividend_opportunity_card` is defined but never called. The module uses `create_zero_dividend_card_with_risk` instead (line 80). This appears to be legacy code from before risk analysis integration. Can be safely removed.

## Dead Code

### DC-8-001: Aristocrats Config validate_config Never Invoked
- **File:** `R/utils_aristocrats_config.R:66-80`
- **Description:** Function `validate_config()` is defined and exported but never called in any module, page, or function. See ME-8-008 for details.

### DC-8-002: Zero Dividend Config validate_zero_dividend_config Never Invoked
- **File:** `R/utils_zero_dividend_config.R:55-69`
- **Description:** Function `validate_zero_dividend_config()` is defined but never called anywhere in the codebase. Same issue as aristocrats config validation.

### DC-8-003: Legacy Card Constructor Not Used
- **File:** `R/mod_zero_dividend_results_table.R:247-312`
- **Description:** `create_zero_dividend_opportunity_card()` is dead code—never called after risk analysis integration. See LO-8-003.
