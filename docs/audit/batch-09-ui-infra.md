# Batch 9: UI Infrastructure + Config + Shared Utilities — Audit Findings

## Summary
- Files reviewed: 17
- Findings: 2 Critical, 4 High, 5 Medium, 2 Low
- Checklist coverage: Error propagation, config chain adherence, hardcoded values, dead code

## Critical Findings

### CR-9-001: Config Validation Calls Have No Error Handling
- **File:** `R/run_app.R:65-71`
- **Dimension:** Error Handling
- **Description:** `run_app()` calls six validation functions (`validate_config()`, `validate_zero_dividend_config()`, etc.) sequentially without any error handling (lines 65-71). If any validation fails, it throws an uncaught error and prevents the app from starting. The user sees a raw R error message with no context about *which* config is invalid or *how* to fix it. Worse, if the first validation (aristocrats) fails, the remaining five strategies are never validated, hiding additional config errors. Wrap in tryCatch to provide user-friendly error messages: "Configuration error in aristocrats strategy: strike_threshold_pct must be between 0 and 1. Check inst/golem-config.yml."

### CR-9-002: Background Refresh Promise Chain Has Silent Failure Path
- **File:** `R/run_app.R:78-86,89-103`
- **Dimension:** Error Handling
- **Description:** The promise chain for background refresh uses `%...!%` error handlers (lines 84-86, 98-102) that log errors but never surface them to the user or trigger alerts. If the initial refresh fails at line 79-86 (e.g., Questrade token is invalid), the error is logged but the app continues to run with *no data*. The hourly refresh at lines 89-103 has the same issue: if it fails, it logs an error and schedules the next attempt, but the user is never notified that live data is unavailable. This is the "silent stale data" problem identified in previous batches (CR-6-001, CR-7-001). The app should store refresh status in a reactive value and display a warning banner when data refresh fails.

## High Findings

### HI-9-001: golem_add_external_resources Never Validates Resource Paths
- **File:** `R/app_ui.R:22-37`
- **Dimension:** Error Handling
- **Description:** `golem_add_external_resources()` calls `add_resource_path("www", app_sys("app/www"))` at line 23-26 and `bundle_resources(path = app_sys("app/www"), ...)` at line 30-32 without checking if the directory exists. `app_sys()` uses `system.file()` which returns an empty string ("") if the path doesn't exist. If the `inst/app/www` directory is missing (e.g., incomplete installation), the function silently fails and resources aren't loaded. The app runs but has broken CSS, missing favicon, and non-functional JavaScript. Validate paths exist: `www_path <- app_sys("app/www"); if (www_path == "" || !dir.exists(www_path)) stop("...")`

### HI-9-002: get_golem_config_value Silently Swallows All Errors
- **File:** `R/utils_00_config.R:27-35`
- **Dimension:** Error Handling
- **Description:** Function uses bare `tryCatch` with `error = function(e) fallback` (lines 32-33) that catches *all* errors and returns the fallback value without logging. If `golem::get_golem_config()` fails because the YAML file is malformed, the function returns the fallback with no indication something went wrong. Worse, if the fallback is `NULL` (default), the caller gets NULL with no context. This creates a scenario where half the config values are loaded from YAML and half are fallbacks from code, leading to inconsistent behavior that's hard to debug. Add logging: `error = function(e) { log_warn("Config error for {section}.{key}: {e$message}"); fallback }`.

### HI-9-003: Annualized Return Calculation Has Edge Case Error
- **File:** `R/utils_calculations.R:27-44`
- **Dimension:** Correctness
- **Description:** `calculate_annualized_return` checks `if (days <= 0)` at line 30 and returns 0, but then checks `if (years > 0)` at line 37 which is redundant (years can never be negative if days > 0). The real issue: if `days_per_year` is passed as 0 or negative, line 34 `years <- days / days_per_year` will produce `Inf` or negative years, causing line 38 `(1 + total_return)^(1/years)` to return nonsensical values or NaN. Add validation: `if (days_per_year <= 0) stop("days_per_year must be positive")`.

### HI-9-004: Max Drawdown Calculation Returns Inconsistent NA Types
- **File:** `R/utils_calculations.R:74-100`
- **Dimension:** Correctness
- **Description:** Function returns `NA` (logical NA) at lines 76 and 98, but the function signature and examples imply it should return a numeric. This type inconsistency causes issues when results are used in calculations. For example, if `max_drawdown` is NA (logical), then `format_percentage(max_drawdown)` may fail or produce "NA%" instead of handling it gracefully. Use `NA_real_` for numeric return values. Additionally, the tryCatch at line 79-99 catches all errors and returns NA without logging, making it impossible to debug why a drawdown calculation failed.

## Medium Findings

### ME-9-001: Sharpe and Sortino Ratios Return 0 for Invalid Inputs
- **File:** `R/utils_calculations.R:119-136,177-203`
- **Dimension:** Correctness
- **Description:** Both `calculate_sharpe_ratio` and `calculate_sortino_ratio` return 0 when standard deviation is zero or NA (lines 128-130, 196-198). A Sharpe ratio of 0 means "risk-adjusted return equals risk-free rate", but zero volatility is a special case that should return `Inf` (infinite Sharpe ratio, perfect risk-adjusted return). Similarly, when `length(returns) == 0`, the functions return 0 (lines 120-122, 178-180), but this should be `NA` (insufficient data to calculate). Returning 0 for error cases masks the distinction between "poor performance" and "unable to calculate".

### ME-9-002: Sortino Ratio Returns Inf for All-Positive Returns
- **File:** `R/utils_calculations.R:186-191`
- **Dimension:** Correctness
- **Description:** When there are no downside returns (all returns exceed risk-free rate), the function returns `Inf` at line 190. While mathematically correct (no downside volatility = infinite Sortino ratio), `Inf` values break downstream calculations and are not handled gracefully in UI formatting. The `format_percentage()` function would display "Inf%" which is confusing to users. Better: return a sentinel value like 999.99 or `NA_real_` with a flag indicating "perfect Sortino (no downside)".

### ME-9-003: utils_globals Declarations Are Incomplete
- **File:** `R/utils_globals.R:7-29`
- **Dimension:** Maintainability
- **Description:** The `utils::globalVariables()` declaration lists variables used in dplyr operations to avoid R CMD check NOTEs. However, it's a static list that must be manually updated when new columns are added to data frames. Based on previous batch reviews, columns like `option`, `rotation_id`, `warning_flag`, `is_yfscreen`, `payment_date` are used in dplyr operations but missing from this list. When new columns are added, developers must remember to update this file or face R CMD check warnings. Consider using .data$ syntax in dplyr verbs instead: `.data$column_name` eliminates the need for global variable declarations.

### ME-9-004: app_sys Returns Empty String on Failure
- **File:** `R/app_config.R:11-13`
- **Dimension:** Error Handling
- **Description:** `app_sys()` uses `system.file(..., package = "investR")` which returns "" (empty string) when the file/directory doesn't exist. This is a confusing error mode because "" is a valid string but not a valid path. Functions that call `app_sys()` must remember to check for "" before using the result. Better pattern: wrap the call and throw an informative error when the path doesn't exist: `path <- system.file(...); if (path == "") stop("Resource not found: ", ...); return(path)`.

### ME-9-005: _disable_autoload Has No Documentation
- **File:** `R/_disable_autoload.R:1-2`
- **Dimension:** Maintainability
- **Description:** This file contains only a comment "Disables shiny autoload in the R/ folder" with no actual code. The underscore prefix in the filename is a Shiny convention to disable autoloading of R scripts in the R/ directory. However, there's no documentation explaining *why* this is necessary for this specific app, *what* would break if autoloading were enabled, or *how* this interacts with the Golem/Brochure architecture. Developers unfamiliar with this pattern may delete the file thinking it's empty/unused. Add a roxygen2 comment block explaining the purpose and consequences of removal.

## Low Findings

### LO-9-001: Inconsistent Handling of Zero-Length Inputs in Calculations
- **File:** `R/utils_calculations.R`
- **Dimension:** Code Style
- **Description:** Functions handle zero-length inputs inconsistently. `calculate_annualized_return` returns 0 for `days <= 0` (line 30-32), `calculate_sharpe_ratio` returns 0 for `length(returns) == 0` (line 120-122), but `calculate_success_rate` returns 0 for `total == 0` (line 221-223). There's no documented standard for "what should financial calculations return when inputs are empty/invalid?" Standardize: return `NA` for insufficient data, throw error for invalid parameters (negative days, etc.), return 0 only when mathematically correct.

### LO-9-002: run_app Comment Has Outdated Page List
- **File:** `R/run_app.R:26-31`
- **Dimension:** Maintainability
- **Description:** The @section Architecture comment lists example pages "page_home(), page_aristocrats(), page_about()" but the actual app (lines 116-137) defines 21 pages. The comment is outdated and misleading for new developers trying to understand the app structure. Update the comment to say "See lines 116-137 for complete page list" instead of providing a stale example.

## Dead Code

None found. All utility functions are actively used across multiple strategies.
