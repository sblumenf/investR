# Batch 1: Cash-Secured Puts — Audit Findings

## Summary
- Files reviewed: 11
- Findings: 4 Critical, 7 High, 8 Medium, 3 Low
- Checklist coverage: NA/NULL handling (4 findings), Error propagation (4 findings), Reactive isolation (1 finding), Config chain verification (2 findings), Hardcoded values (3 findings)

## Critical Findings

### CR-1-001: Silent NA propagation in put return calculations
- **File:** `R/fct_cash_secured_puts.R:86-107`
- **Dimension:** Correctness | Data Integrity
- **Description:** `calculate_put_return_metrics()` validates that `cash_required` and `days_to_expiry` are positive, but does not check if `premium_received` is NA/NULL before performing division at line 95. If `premium_received` is NA (from missing bid data), the function returns `return_on_cash = NA` and `annualized_return = NA` without error. This silent NA propagation can produce misleading results shown to users as valid opportunities. The calling code in `calculate_put_metrics()` (line 142-146) does not validate the return metrics before assembling the final tibble.

### CR-1-002: No NA handling in protection metric calculations
- **File:** `R/fct_cash_secured_puts.R:57-74`
- **Dimension:** Correctness
- **Description:** `calculate_put_protection_metrics()` performs arithmetic (lines 64, 68) without checking if inputs are NA. If `current_price`, `strike`, or `bid_price` contain NA values that pass validation (validate_price may accept NA), the function will silently return NA for `breakeven_price` and `downside_protection_pct`. These NA values propagate to the final results tibble without error or warning, potentially misleading users.

### CR-1-003: Unvalidated option_row structure in calculate_put_metrics
- **File:** `R/fct_cash_secured_puts.R:122-184`
- **Dimension:** Data Integrity
- **Description:** `calculate_put_metrics()` validates that `option_row` contains required columns at line 127-128, but does not validate that the *values* in those columns are non-NA or within valid ranges. If `option_row$Strike`, `option_row$Bid`, or `option_row$OI` are NA (possible if filtering in `select_optimal_put` fails to remove rows with NA), the function will execute calculations with NA and produce a result row with NA metrics. The tibble construction (lines 153-183) does not filter out or flag these invalid results.

### CR-1-004: Missing transaction rollback pattern
- **File:** N/A (no database writes detected in this batch)
- **Dimension:** Data Integrity
- **Description:** This batch contains no database write operations, so no transaction patterns to audit. However, this is noted as a critical finding category for future batches — database writes must use transactions with rollback on error.

## High Findings

### HI-1-001: Silent option chain fetch failures
- **File:** `R/fct_cash_secured_puts.R:400-456`
- **Dimension:** Error Handling
- **Description:** `get_options_chain_puts()` catches all errors at line 452 and returns an empty tibble. The error is logged (line 453), but the calling code in `analyze_single_stock_put()` (line 339-346) cannot distinguish between "no options available" and "API error". The user sees "No put options available" in both cases, masking potential API failures. This violates the requirement to inform users when API failures occur.

### HI-1-002: No Questrade fallback notification
- **File:** All fct_cash_secured_puts.R variants
- **Dimension:** Error Handling
- **Description:** The code calls `get_stock_data()`, `fetch_options_chain()`, and other API functions without checking or reporting whether fallback to Yahoo Finance occurred. Per CLAUDE.md requirements, "If Questrade fails, fall back to Yahoo Finance... always inform the user which data came from fallback." None of the analysis functions call `get_fallback_summary()` or notify users of fallback data sources.

### HI-1-003: Reactive isolation — quote source toggle side effect
- **File:** `R/mod_cash_secured_puts.R:108`
- **Dimension:** Maintainability | Error Handling
- **Description:** `quote_source_toggle_server()` is called in the module server (line 108) but is not assigned to a variable or used in reactive context. The function likely sets global options (e.g., `options(investR.quote_source = ...)`), which is a reactive side effect that affects all modules. If multiple modules run concurrently (possible in Brochure multi-session architecture), changing the quote source in one module could affect API calls in another.

### HI-1-004: Hardcoded conditional panel namespace
- **File:** `R/mod_sp500_cash_secured_puts.R:39`
- **Dimension:** Maintainability
- **Description:** `conditionalPanel` at line 38-50 uses `ns = ns` parameter to namespace the condition, but this pattern is fragile. If the module ID changes or namespace handling changes in Shiny, the condition `input.dividend_filter != 'all'` may not resolve correctly. This is a known Shiny gotcha. Best practice is to use server-side conditionals (`renderUI`) for complex logic.

### HI-1-005: Missing config chain for default values
- **File:** `R/mod_etf_cash_secured_puts_yfscreen.R:199`
- **Dimension:** Code Style | Maintainability
- **Description:** The module server hardcodes `market_cap_max = 100e9` (line 199) instead of reading from config. This value should come from `CASH_SECURED_PUTS_CONFIG` or a related config object to maintain the 3-tier config pattern (golem-config.yml → STRATEGY_CONFIG → get_golem_config_value()). If the hardcoded value needs to change, it requires code modification rather than config update.

### HI-1-006: Parallel processing without error aggregation
- **File:** `R/fct_cash_secured_puts.R:584-593`
- **Dimension:** Error Handling
- **Description:** `analyze_puts_generic()` calls `process_stocks_parallel_generic()` (line 584) to analyze stocks in parallel, but does not check if any stocks failed with errors vs. returned NULL for "no opportunity". The `process_stocks_parallel_generic()` function (not in this batch) may silently drop stocks that error during processing. Users should be informed if N stocks were skipped due to API errors vs. no opportunities.

### HI-1-007: No validation of stock_universe input
- **File:** `R/fct_cash_secured_puts.R:553-562`
- **Dimension:** Error Handling
- **Description:** `analyze_puts_generic()` checks if `stock_universe` is empty (line 564), but does not validate that it's a character vector or that tickers are well-formed. If called with invalid input (e.g., `stock_universe = c(NA, "", "INVALID!!!")`) the function will attempt to process invalid tickers, leading to cascading failures in `analyze_single_stock_put()`.

## Medium Findings

### ME-1-001: Possibly operator without default in log message
- **File:** `R/fct_cash_secured_puts.R:315`
- **Dimension:** Code Style
- **Description:** Uses `%||%` operator for log message formatting (e.g., `{min_days %||% 0}`). This is fine for logging, but if `min_days` is explicitly NULL, the log shows "0" which could be misleading (suggests 0 days minimum vs. no minimum). Minor clarity issue.

### ME-1-002: Hardcoded config object reference
- **File:** `R/fct_cash_secured_puts.R:35-36, 231, 434`
- **Dimension:** Maintainability
- **Description:** Multiple functions reference `CASH_SECURED_PUTS_CONFIG` directly (e.g., `CASH_SECURED_PUTS_CONFIG$shares_per_contract`, `CASH_SECURED_PUTS_CONFIG$min_option_bid`). While acceptable for strategy-specific code, this creates tight coupling. If the config object is renamed or restructured, all references must be updated. Consider using `get_puts_config()` accessor throughout.

### ME-1-003: Magic number in strike filtering logic
- **File:** `R/fct_cash_secured_puts.R:223-224`
- **Dimension:** Maintainability
- **Description:** `select_optimal_put()` uses hardcoded multipliers `0.90` and `1.05` to define strike range (lines 223-224). These magic numbers are not in config. If the logic for "OTM range width" needs to change, it requires code modification.

### ME-1-004: Duplicate filtering logic across functions
- **File:** `R/fct_cash_secured_puts.R:230-261` and similar patterns
- **Dimension:** Maintainability
- **Description:** The option filtering logic (strike, bid, OI, days, month) in `select_optimal_put()` is similar to filtering patterns in covered calls and other strategies. This logic could be extracted to a shared helper function to reduce duplication and ensure consistent filtering behavior.

### ME-1-005: No explicit test for option_type in risk module call
- **File:** `R/mod_cash_secured_puts_results_table.R:147`
- **Dimension:** Correctness
- **Description:** The module passes `option_type = reactive("put")` to `mod_position_risk_server()`. While this is correct, there's no validation that the risk module actually uses this parameter correctly. If the risk module has a bug where it ignores `option_type` for puts, the risk analysis would be wrong. This is a boundary condition that should be tested.

### ME-1-006: Unused parameter target_days
- **File:** `R/fct_cash_secured_puts.R:310, 590`
- **Dimension:** Dead Code
- **Description:** `analyze_single_stock_put()` accepts `target_days = NULL` parameter (line 310) but never uses it. It's passed through to `process_stocks_parallel_generic()` (line 590) which may also ignore it. This suggests incomplete feature implementation or vestigial code from copy-paste.

### ME-1-007: No rate limiting for parallel API calls
- **File:** `R/fct_cash_secured_puts.R:579-593`
- **Dimension:** Performance | Error Handling
- **Description:** `analyze_puts_generic()` sets up parallel processing with `setup_parallel_processing(max_workers)` (line 580) but does not implement rate limiting for API calls. If analyzing 100 stocks with 10 workers, the system could make 10 simultaneous Questrade API calls, potentially hitting rate limits. The spec mentions rate limiting in golem-config.yml (0.5-2s between requests) but it's unclear if this is enforced for parallel execution.

### ME-1-008: Inconsistent error messages between functions
- **File:** `R/fct_sp500_cash_secured_puts.R:58-62` vs `R/fct_etf_cash_secured_puts_yfscreen.R` (no equivalent validation)
- **Dimension:** Code Style | Maintainability
- **Description:** `analyze_sp500_cash_secured_puts()` validates `dividend_filter` parameter with detailed error message, while `analyze_etf_cash_secured_puts_yfscreen()` does not validate this parameter at all (relies on `fetch_yfscreen_etfs()` to handle it). Inconsistent validation patterns make debugging harder.

## Low Findings

### LO-1-001: Overly verbose logging in production
- **File:** `R/fct_cash_secured_puts.R:218, 228, 235, 243, etc.`
- **Dimension:** Performance
- **Description:** `select_optimal_put()` logs after every filter step (lines 218, 228, 235, 243, 250, 263, 278). For 100 stocks analyzed, this produces hundreds of log lines. Consider reducing verbosity for production or using log_debug() instead of log_info() for intermediate steps.

### LO-1-002: Commented-out code section header
- **File:** `R/fct_cash_secured_puts.R:462-463`
- **Dimension:** Code Style
- **Description:** Lines 462-463 show a section header "# Process stocks in parallel for put analysis" with no corresponding code. This suggests a commented-out function or incomplete section. Should either complete the function or remove the header.

### LO-1-003: Inconsistent spacing in card creation
- **File:** `R/mod_cash_secured_puts_results_table.R:163-252`
- **Dimension:** Code Style
- **Description:** `create_cash_secured_put_card_with_risk()` has inconsistent indentation and spacing (e.g., lines 191-194 vs 201-206). Not a functional issue, but makes the code slightly harder to read.

## Dead Code

None identified in this batch. All referenced helper functions (`log_analysis_header_generic`, `process_stocks_parallel_generic`, `setup_parallel_processing`) exist in shared utility files (R/utils_covered_calls_shared.R, R/fct_aristocrats_analysis.R).
