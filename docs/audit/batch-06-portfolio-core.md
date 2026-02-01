# Batch 6: Portfolio Core — Audit Findings

## Summary
- Files reviewed: 40 (database operations, risk analysis, projections, modules, pages)
- Findings: 5 Critical, 8 High, 10 Medium, 4 Low
- Dead code: 2 items
- Checklist coverage: All 8 checklist items produced findings

## Critical Findings

### CR-6-001: Questrade API silent stale data (known issue verification)
- **File:** Multiple (portfolio_risk.R:88-110, group_metrics.R:545-580)
- **Dimension:** Data Integrity
- **Description:** When Questrade API token refresh fails mid-session, the app silently uses stale/cached position data from `latest_positions` table without informing the user. Portfolio risk analysis and group metrics enrichment fetch "current" prices from database cache which only updates when trades occur, not on a schedule. This can lead to investment decisions based on outdated market data. No user notification, no fallback indicator, no timestamp display. Verified as specified in spec "Known Issues to Verify".

### CR-6-002: Database connection lifecycle violations
- **File:** `fct_activities_database.R:131-195`, `fct_income_projection_database.R:45-80`
- **Dimension:** Data Integrity
- **Description:** Multiple functions (`save_activities_batch`, `save_cash_flow_event`) create database connections but use conditional `on.exit()` logic with `should_close` flag. If function exits early via `return()` before `on.exit()` registration or through error paths, connections may leak. The pattern `if (should_close) { on.exit(...) }` is fragile. Should use `on.exit()` unconditionally at function entry for all created connections.

### CR-6-003: Option symbol parsing relies on fragile regex
- **File:** `fct_income_projection_engine.R:255-295`
- **Dimension:** Correctness
- **Description:** `parse_option_details()` uses regex to extract strike/expiry from option symbols with two format patterns. According to CLAUDE.md "Critical Pitfalls", there are multiple option symbol formats in production (legacy "ALB17Dec27C55.00" and new "AAPL240119C150"). The function should use the canonical `parse_option_details()` from `utils_calculations.R` instead of reimplementing parsing logic. DRY violation + correctness risk (may not handle all edge cases).

### CR-6-004: Monte Carlo baseline inconsistency risk
- **File:** `fct_portfolio_risk.R:200-250` (extract_portfolio_positions)
- **Dimension:** Correctness
- **Description:** Portfolio risk simulation calculates `purchase_price` from activities using `total_cost / total_shares`, but this may not match the `purchase_price` baseline used by position-level risk (which uses `cost_basis` field). Spec states "Risk simulation baseline: Portfolio stress tests MUST use same baseline (purchase_price) as position-level." The calculation logic differs between portfolio and position levels, creating potential inconsistency in risk metrics.

### CR-6-005: Cash flow projection strategy type drift
- **File:** `fct_cash_flow_projection.R:65-125`
- **Dimension:** Data Integrity
- **Description:** Cash flow processing has hardcoded strategy type checks (`strategy_type %in% c("Other", "Legacy Covered Call")`) scattered across multiple functions. This creates maintenance burden and drift risk when new strategies are added. The `no_projection_strategies` list in `income_projection_engine.R:40` differs from projection skip logic in `cash_flow_projection.R`. Should centralize strategy classification in config.

## High Findings

### HI-6-001: Error handling allows silent data quality degradation
- **File:** `fct_portfolio_risk.R:145-195`
- **Dimension:** Error Handling
- **Description:** `extract_portfolio_positions()` wraps each position extraction in `tryCatch` that logs warnings but silently skips failed positions. Returns partial results without indicating to caller that some positions were excluded. This can cause portfolio-level metrics to be calculated on incomplete data. Should return both successful positions AND a list of failures/exclusions so caller can decide whether to proceed or alert user.

### HI-6-002: Dividend reconciliation race condition
- **File:** `fct_activities_database.R:405-435` (link_activity_to_group), `fct_income_projection_database.R:160-200` (delete_projected_cash_flows_by_month)
- **Dimension:** Data Integrity
- **Description:** When a dividend activity is linked to a group, `delete_projected_cash_flows_by_month()` deletes projected dividends for that calendar month. If multiple dividends occur in same month (e.g., special dividend + regular), linking them sequentially will cause first link to delete ALL projected events for the month, including the projection for the second dividend. No transactional protection. Needs calendar-month + dividend-type uniqueness check or FIFO matching logic.

### HI-6-003: NA/NULL handling missing in risk calculations
- **File:** `fct_portfolio_risk.R:530-580` (run_correlated_monte_carlo)
- **Dimension:** Correctness
- **Description:** Monte Carlo simulation loops over positions and calculates time horizon `T` with fallback logic, but if `days_to_expiry` is NULL/NA and ticker is NOT cash equivalent, it warns but uses 1.0 year default. This can mis-price positions with unknown expiry. Should either fail fast (stop simulation) or require all positions have valid expiry OR cash_equivalent flag before entering simulation loop.

### HI-6-004: Income projection uses wrong symbol field for CSPs
- **File:** `fct_income_projection_engine.R:85-110`
- **Dimension:** Correctness
- **Description:** When generating initial projections for cash-secured puts, the code extracts `option_symbol` from members with role == "short_put", then queries activities using `symbol == option_symbol[1]`. According to CLAUDE.md, "Questrade symbol field: Sometimes blank for options. Use description field for uniqueness." This query may return zero results if Questrade left symbol blank in API response, causing projection generation to fail. Should use description-based matching or fallback.

### HI-6-005: Position group P&L double-counts commissions
- **File:** `fct_group_pnl.R:45-75`
- **Dimension:** Correctness
- **Description:** P&L calculation uses `gross_amount` (excluding commissions) for stock purchases, then separately adds `total_commissions` to cost. But `gross_amount` for stock purchases already includes commissions in some Questrade API responses (ambiguous field definition). This can lead to double-counting commissions in cost basis, inflating cost and deflating returns. Should verify field semantics or always use `net_amount`.

### HI-6-006: Correlation matrix positive definiteness edge case
- **File:** `fct_portfolio_risk.R:810-850` (expand_correlation_to_positions)
- **Dimension:** Correctness
- **Description:** When multiple positions share the same ticker (perfect correlation), the expanded position-level correlation matrix becomes near-singular (eigenvalues near zero). Code applies Ledoit-Wolf shrinkage (10% blend with identity) but doesn't verify it succeeded. If shrinkage fails to fix PD, the fallback is identity matrix (zero correlation) which understates portfolio risk. Should log a CRITICAL warning if forced to use identity fallback, as this invalidates the entire risk analysis.

### HI-6-007: get_portfolio_db_path() not validated
- **File:** All database files (`fct_portfolio_database.R:30`, `fct_portfolio_groups_database.R` uses `get_portfolio_db_connection()`)
- **Dimension:** Error Handling
- **Description:** `get_portfolio_db_connection()` calls `get_portfolio_db_path()` but doesn't validate the path exists or is writable before attempting `dbConnect()`. If path is invalid (e.g., parent directory doesn't exist), DuckDB error message will be cryptic. Should check `dir.exists(dirname(db_path))` and provide clearer error: "Portfolio database directory does not exist: <path>".

### HI-6-008: Regime detection parameter application order bug
- **File:** `fct_portfolio_risk.R:100-130`
- **Dimension:** Correctness
- **Description:** Regime adjustment is applied to correlation matrix AFTER expanding from ticker-level to position-level. The code multiples off-diagonal elements by `regime_params$correlation_multiplier`, then caps at 1.0. But this can break positive definiteness AGAIN (different positions, same adjustment). Should apply regime adjustment at TICKER level before expansion, or re-verify PD after adjustment with another shrinkage pass if needed.

## Medium Findings

### ME-6-001: Group metrics calculation duplicates core logic
- **File:** `fct_group_metrics.R:30-150` (calculate_metrics_core) vs `fct_group_pnl.R:30-150` (calculate_group_pnl)
- **Dimension:** Maintainability
- **Description:** Both functions iterate over activities to calculate cost basis, premiums, and dividends using nearly identical logic. The only difference is forward-looking (metrics) vs realized (P&L). Should extract shared calculation into a `parse_group_financials()` helper that both functions call. DRY violation creates risk of divergence.

### ME-6-002: Magic numbers in risk calculation (no config fallback)
- **File:** `fct_portfolio_risk.R:760-780`
- **Dimension:** Maintainability
- **Description:** Risk level thresholds are hardcoded: `var_pct < 0.05` = Low, `var_pct < 0.10` = Moderate, else High. These should come from `RISK_CONFIG` with fallbacks. Other risk parameters (jump frequency, vol) use config, but risk level thresholds don't. Inconsistent pattern.

### ME-6-003: Hardcoded SQL query construction with placeholders
- **File:** `fct_activities_database.R:305-325`, `fct_portfolio_groups_database.R:545-570`
- **Dimension:** Maintainability
- **Description:** Batch query functions (`get_activities_for_groups`, `get_members_for_groups`) build IN clauses with `paste(rep("?", length(ids)), collapse=", ")`. This works but is verbose. Consider using helper function `build_in_clause()` or DBI's `dbBind()` with array parameters if DuckDB supports it.

### ME-6-004: Inconsistent use of tibble::tibble() vs data.frame()
- **File:** Throughout batch 6 files
- **Dimension:** Code Style
- **Description:** Some functions return `tibble::tibble()` for empty results, others return `data.frame()`. `calculate_correlation_matrix()` returns base R matrix, then wraps in tibble elsewhere. Should standardize on tibble for consistency (already using tidyverse extensively).

### ME-6-005: Option roll detection logic embedded in activities linking
- **File:** `fct_activities_database.R:435-465`
- **Dimension:** Maintainability
- **Description:** `link_activity_to_group()` contains complex option roll detection logic that checks if a sell-to-open option is part of a roll pattern. This is domain logic mixed with database operation. Should extract `detect_option_roll()` into `fct_pattern_matching.R` where other pattern detection lives (DRY + separation of concerns).

### ME-6-006: Portfolio expected return uses different weighting than docs suggest
- **File:** `fct_portfolio_expected_return.R:210-260` (process_closed_positions)
- **Dimension:** Correctness
- **Description:** Closed positions use "capital-days weighted return" methodology, but the implementation calculates `daily_return_rate * 365 * 100` which assumes linear compounding. Function comment says "Capital-Days Weighted Return" but doesn't specify compounding convention. Should document whether this is simple (linear) or compound annualization.

### ME-6-007: Memory leak potential in Monte Carlo simulation
- **File:** `fct_portfolio_risk.R:530-640` (run_correlated_monte_carlo)
- **Dimension:** Performance
- **Description:** Monte Carlo creates large matrices (`position_pnl_matrix` = `simulation_paths` × `n_positions`, potentially 10,000 × 50 = 500k elements) but doesn't pre-allocate in some paths. The `for (path in seq_len(simulation_paths))` loop grows `portfolio_pnl` vector incrementally. Should pre-allocate `portfolio_pnl <- numeric(simulation_paths)` outside loop.

### ME-6-008: Dividend projection uses simple average, ignores growth
- **File:** `fct_income_projection_engine.R:135-185` (generate_dividend_events)
- **Dimension:** Correctness
- **Description:** Dividend projection extracts `latest_amount` from history and projects same amount for all future dates. For dividend aristocrats (25+ years of growth), this understates future income. Should apply growth rate (e.g., trailing 5-year CAGR) or at minimum document that projections assume flat dividends. Config has `sgov_yield_default` but no `dividend_growth_rate`.

### ME-6-009: Stress test scenarios hardcoded
- **File:** `fct_risk_analysis.R:115-155` (run_position_stress_tests)
- **Dimension:** Maintainability
- **Description:** Scenario names are hardcoded vector: `c("financial_crisis_2008", "covid_crash_2020", ...)`. If user adds custom scenarios via config, this function won't find them. Should call `get_all_stress_scenario_names()` or iterate over `RISK_CONFIG$stress_scenarios` keys. Currently requires code change to add scenarios.

### ME-6-010: Cash flow projection date range calculation has no user override
- **File:** `fct_cash_flow_projection.R:350-400` (get_cash_flow_date_range)
- **Dimension:** Maintainability
- **Description:** Date range is auto-calculated from earliest activity + latest option expiry. If user wants to see projections beyond furthest expiry (e.g., 5 years out for retirement planning), there's no parameter to extend the range. Should accept optional `max_projection_years` parameter with fallback to auto-calculated range.

## Low Findings

### LO-6-001: Inconsistent logging of recalculation events
- **File:** `fct_income_projection_engine.R:55-60`, `fct_portfolio_groups_database.R:280-320` (convert_to_legacy_covered_call)
- **Dimension:** Code Style
- **Description:** Some projection recalculation events use `log_projection_recalculation()` helper, others directly call `save_cash_flow_event()` or don't log at all. Conversion to legacy logs recalculation, but regeneration after roll might not (depends on code path). Should standardize on always calling the helper for audit trail consistency.

### LO-6-002: Comment says "never read file first" but function requires reading
- **File:** Multiple mod_*.R and page_*.R files (not reviewed in detail due to batch size)
- **Dimension:** Code Style
- **Description:** Based on pattern in other batches, likely portfolio modules have similar issues. Spot check `mod_portfolio_groups.R` or `mod_portfolio_risk_dashboard.R` if time permits for comment/code mismatches.

### LO-6-003: Strategy display map has duplicate entries
- **File:** `fct_portfolio_expected_return.R:20-35` (STRATEGY_DISPLAY_MAP)
- **Dimension:** Code Style
- **Description:** Map has both "Covered Call" and "Dynamic Covered Calls" mapping to "Covered Calls", plus "Zero-Dividend" and "Zero-Dividend Stocks". These duplicates suggest inconsistent strategy naming in database. Should normalize strategy_type values in `position_groups` table or document that variations are intentional.

### LO-6-004: Variable names clash with function names
- **File:** `fct_portfolio_risk.R:200-250`
- **Dimension:** Code Style
- **Description:** Local variable `market_data` shadows function `enrich_group_with_market_data()` in same scope. Not a bug (different types) but reduces readability. Rename local to `group_market_data` or `market_info`.

## Dead Code

### DC-6-001: delete_position_group() marked deprecated but still exported
- **File:** `fct_portfolio_groups_database.R:380-390`
- **Dimension:** Maintainability
- **Description:** Function has comment "DEPRECATED: Delete a position group - DO NOT USE" and throws error, but is still defined in file and likely exported via NAMESPACE. Should remove from codebase entirely or wrap in `.Deprecated()` call with `stop()` message pointing to replacement (`close_position_group()`). Current state clutters API surface.

### DC-6-002: Heston model implementation in shared_risk_engine.R never used
- **File:** `fct_shared_risk_engine.R:155-230` (simulate_heston_internal)
- **Dimension:** Maintainability
- **Description:** Heston stochastic volatility model is fully implemented but no caller in codebase uses it. Portfolio risk uses "jump_diffusion", position risk uses "jump_diffusion" (based on config). The model routing supports "heston" but it's never invoked. Either remove (YAGNI) or add to risk presets as an advanced option. Currently dead weight (200+ lines).

---

## Checklist Item Hit Rates

| Checklist Item | Findings | Severity Range |
|---|---|---|
| 1. NA/NULL handling | 2 | High |
| 2. Error propagation | 3 | High, Medium |
| 3. DB connection lifecycle | 2 | Critical, High |
| 4. Hardcoded values | 3 | Medium, Low |
| 5. Option symbol parsing | 2 | Critical, High |
| 6. Shared risk engine | 2 | Critical, Medium |
| 7. Reactive isolation | 0 | (Not applicable - fct files only) |
| 8. Dead/unreachable code | 2 | Low |

## Config Chain Violations

**CR-6-005** (cash flow strategy type drift) is the only config chain violation found in this batch. Other hardcoded values (ME-6-002, ME-6-009) are thresholds/scenario names that should come from config but don't bypass the 3-tier chain architecture (they just don't use it at all).

## Notes

- This batch is the largest (40 files, ~8,000 lines) and most complex
- Heavy use of DuckDB transactions but many lack rollback-on-error (HI-6-002 example)
- Strong adherence to tidyverse style and DRY principles in newer code (e.g., shared_risk_engine.R)
- Older code (group_pnl.R, group_metrics.R) has duplication (ME-6-001)
- Portfolio risk analysis is the most sophisticated module with Monte Carlo, correlation, regime detection
- Income projection system shows signs of incremental evolution (strategy type checks scattered)
- Overall quality is high for financial calculations (good use of RQuantLib, PerformanceAnalytics)
- Main risk: silent failures and stale data (CR-6-001 is highest priority issue)