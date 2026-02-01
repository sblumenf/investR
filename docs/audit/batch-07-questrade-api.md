# Batch 7: Questrade API + Market Data — Audit Findings

## Summary
- Files reviewed: 12
- Findings: 3 Critical, 7 High, 7 Medium, 4 Low
- Dead code items: 1
- Checklist coverage:
  - ✅ NA/NULL handling: Multiple findings (HI-7-005, ME-7-006)
  - ✅ Error propagation: Multiple critical findings (CR-7-001, CR-7-003, HI-7-006, HI-7-007)
  - ✅ DB connection lifecycle: N/A (no database operations in this batch)
  - ✅ Hardcoded values: Multiple findings (HI-7-001, HI-7-003, ME-7-006, ME-7-007)
  - ⚠️ Option symbol parsing: N/A (options chain fetching, not parsing)
  - ⚠️ Shared risk engine: N/A (no risk simulations in this batch)
  - ✅ Reactive isolation: Findings (LO-7-003, LO-7-004)
  - ✅ Dead/unreachable code: 1 finding (DC-7-001)

## Critical Findings

### CR-7-001: Silent stale data on 401 retry failure
- **File:** `R/fct_questrade_api.R:163`, `R/fct_questrade_api.R:265`, `R/fct_questrade_api.R:400`
- **Dimension:** Data Integrity
- **Description:** When 401 occurs and fresh auth fails in `fetch_questrade_accounts()`, `fetch_questrade_positions()`, and `fetch_questrade_activities()`, the functions return empty tibbles instead of signaling to the user that authentication has completely failed. The app continues running with no data, silently degrading. User is never informed that Questrade connection is broken. This matches the known issue documented in the spec (Questrade API token refresh failure causes silent stale data usage).

### CR-7-002: refresh_token preservation vulnerable to race condition
- **File:** `R/fct_questrade_api.R:154-161`, `R/fct_questrade_options.R:68-75`, `R/fct_questrade_quotes.R:58-65`
- **Dimension:** Data Integrity
- **Description:** The 401 recovery pattern reads cached token, preserves refresh_token, deletes cache, then uses preserved token. If another concurrent request triggers the same 401 between "read" and "delete", both will preserve the same refresh_token, but only one exchange will succeed (tokens are single-use). The other will fail with 400/500. No locking mechanism prevents this race.

### CR-7-003: Token exchange failure returns NULL with insufficient error context
- **File:** `R/fct_questrade_api.R:93-102`
- **Dimension:** Error Handling (Critical → Data Integrity)
- **Description:** `get_questrade_auth()` logs errors on 400/500 from token exchange but returns NULL without throwing or setting a global error state. Calling code (positions, accounts, activities) continues executing with NULL auth and returns empty tibbles. User sees empty screens with no explanation. Should halt the app or display a modal error requiring user action.

## High Findings

### HI-7-001: Hardcoded 60-second safety buffer
- **File:** `R/fct_questrade_api.R:28`
- **Dimension:** Maintainability
- **Description:** Token expiry check uses hardcoded `time_until_expiry > 60` instead of reading from config. Should use `QUESTRADE_CONFIG$token_safety_buffer_seconds` or similar.

### HI-7-002: Refresh token consumed even when access token is still valid
- **File:** `R/fct_questrade_api.R:24-34`
- **Dimension:** Correctness
- **Description:** If `time_until_expiry` is exactly 60 seconds or less, the code proceeds to exchange the refresh token even though the access token is technically still valid for up to 60 seconds. This wastes the single-use refresh token unnecessarily. The condition should be `<= 0` for actual expiry, not `<= 60`.

### HI-7-003: Options quote batch size hardcoded
- **File:** `R/fct_questrade_options.R:149`
- **Dimension:** Maintainability
- **Description:** `fetch_questrade_option_quotes()` uses `batch_size = 100` as a default parameter instead of reading from `golem-config.yml`. Questrade API limits may change, and this should be configurable.

### HI-7-004: No rate limiting between Questrade API calls
- **File:** `R/fct_questrade_api.R`, `R/fct_questrade_options.R`, `R/fct_questrade_quotes.R`
- **Dimension:** Correctness / Error Handling
- **Description:** None of the Questrade API functions implement rate limiting (no `Sys.sleep()` calls, no rate limiter object). The spec states rate limiting is configured in `golem-config.yml` (0.5-2s between requests), but it's not actually implemented. This risks hitting API rate limits and getting temporarily blocked.

### HI-7-005: Missing NA/NULL protection in volatility blending calculation
- **File:** `R/fct_implied_volatility.R:73`
- **Dimension:** Correctness
- **Description:** `blended <- blend_weight * implied_vol + (1 - blend_weight) * historical_vol` occurs after checking `!is.na(implied_vol) && !is.na(historical_vol)`, but `blend_weight` is not validated. If `blend_weight` is NA or outside [0,1], result will be incorrect. Should validate: `blend_weight <- max(0, min(1, blend_weight %||% 0.70))`.

### HI-7-006: Background refresh promises never checked or handled
- **File:** `R/fct_background_refresh.R:56-89`
- **Dimension:** Error Handling
- **Description:** `refresh_questrade_activities()` and `refresh_questrade_positions()` return promises, but there's no documented mechanism for the caller to check promise resolution or handle errors. Errors are logged but not surfaced to the UI (via reactive values or session state). User never knows background refresh failed unless they manually check logs.

### HI-7-007: Black-Scholes IV solver can return NA without signaling why
- **File:** `R/fct_implied_volatility.R:365`
- **Dimension:** Correctness / Error Handling
- **Description:** `calculate_implied_vol_from_price()` fails convergence after 20 iterations and returns NA with only a debug log. Calling code in `fetch_implied_vol_yahoo()` silently drops the NA. User never knows IV calculation failed due to numerical instability vs. missing data vs. bad inputs.

## Medium Findings

### ME-7-001: Excessive logging of token prefixes exposes security info
- **File:** `R/fct_questrade_api.R:24`, `R/fct_questrade_api.R:160`, `R/utils_questrade_healthcheck.R:128`
- **Dimension:** Security
- **Description:** Multiple log statements output `substring(token, 1, 10)...` which exposes 10 characters of access/refresh tokens. Even though these are debug logs, they could leak into log files or monitoring systems. Should reduce to 4-6 characters or obfuscate entirely in production.

### ME-7-002: Questrade API timezone format assumes specific OS locale
- **File:** `R/fct_questrade_api.R:460-463`
- **Dimension:** Correctness
- **Description:** `format_questrade_time()` uses `format(time, "%Y-%m-%dT%H:%M:%S%z")` which relies on OS timezone settings. On systems with unusual locale settings or UTC-only timezones, this may produce malformed timestamps that Questrade rejects. Should explicitly set timezone to EST/EDT (Questrade's timezone).

### ME-7-003: Duplicate 401 retry logic across 6 functions
- **File:** `R/fct_questrade_api.R:145-175`, `R/fct_questrade_options.R:58-90`, `R/fct_questrade_quotes.R:48-80` (and 3 more)
- **Dimension:** Maintainability
- **Description:** The 401 recovery pattern (preserve refresh token → delete cache → re-auth → retry) is copy-pasted across 6 functions with identical logic. Should be extracted to a shared retry wrapper: `with_questrade_retry(fn, ...)` that handles 401 automatically.

### ME-7-004: Options chain date parsing vulnerable to locale changes
- **File:** `R/fct_implied_volatility.R:150`
- **Dimension:** Correctness
- **Description:** `as.Date(exp_dates, format = "%b.%d.%Y")` parses Questrade date strings like "Nov.07.2025" using `%b` (abbreviated month). This is locale-dependent—on non-English systems, "Nov" won't parse. Should force `Sys.setlocale("LC_TIME", "C")` temporarily or use numeric month parsing.

### ME-7-005: Sector cache has no expiration or size limit
- **File:** `R/utils_sector_cache.R:42`
- **Dimension:** Performance
- **Description:** `.sector_cache$data` grows unbounded during a session (no TTL, no max size). If a user analyzes hundreds of tickers, the cache could consume significant memory. Options cache has TTL; sector cache should too.

### ME-7-006: fetch_sofr_rate() fallback silently uses stale default
- **File:** `R/utils_market_data.R:157`
- **Dimension:** Correctness
- **Description:** `fetch_sofr_rate(fallback_rate = 0.0414)` uses a hardcoded default (4.14%) when FRED fetch fails. The default is from a specific point in time and will become increasingly stale. Should read fallback from config and warn user prominently when using fallback instead of just debug logging.

### ME-7-007: get_volatility() horizon thresholds hardcoded with config fallback
- **File:** `R/fct_implied_volatility.R:42-43`
- **Dimension:** Maintainability
- **Description:** `short_horizon <- RISK_CONFIG$implied_vol_short_horizon %||% 90` uses `%||%` operator to provide inline defaults instead of defining defaults in the config system itself. If `RISK_CONFIG` is NULL, this silently uses 90/365 with no warning. Should validate config is loaded and throw if missing.

## Low Findings

### LO-7-001: Questrade API uses httr instead of httr2
- **File:** All `R/fct_questrade_*.R` files
- **Dimension:** Maintainability
- **Description:** Uses legacy `httr` package (`GET`, `POST`, `add_headers`) instead of modern `httr2` which has better retry/timeout/error handling built-in. Migrating would reduce custom 401 retry boilerplate.

### LO-7-002: Null coalescing operator defined but never exported
- **File:** `R/fct_questrade_api.R:550`, `R/fct_questrade_options.R:467`, `R/fct_questrade_quotes.R:300`
- **Dimension:** Maintainability
- **Description:** `%||%` operator stub appears at end of 3 files with incomplete documentation. It's used extensively but never actually defined or exported here. Either remove the stubs or complete the definition once in a shared utils file.

### LO-7-003: Background refresh status not reactive
- **File:** `R/fct_background_refresh.R:12-17`
- **Dimension:** Maintainability
- **Description:** `refresh_status` is a plain environment, not a reactive value. `get_refresh_status()` returns a static snapshot. If UI calls this, it won't update automatically when background refresh completes. Should use `reactiveValues()` or trigger invalidation.

### LO-7-004: quote_source_toggle sets global option instead of reactive
- **File:** `R/utils_quote_source_toggle.R:42`
- **Dimension:** Maintainability
- **Description:** `options(investR.quote_source = input$quote_source)` sets a global R option, which leaks across sessions if multiple users share the same R process. Should use session-scoped reactive values instead.

## Dead Code

### DC-7-001: set_quote_source() never called
- **File:** `R/utils_quote_source_toggle.R:50-55`
- **Description:** Documented as "helper function" but `@noRd` means it's not exported, and grepping the codebase shows it's never called by any module. The functionality is handled entirely by the reactive observer in `quote_source_toggle_server()`.
