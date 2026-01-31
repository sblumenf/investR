# Code Review - Full Codebase Audit (Draft)

## Feature Summary
A systematic, full-codebase audit of the investR Shiny application, organized into strategy-based file-group batches, producing per-batch findings documents and a master summary. The audit covers all quality dimensions weighted by severity, with data integrity issues receiving highest priority.

## Motivation
- Specific concerns about API/external dependencies, business logic, and UI/module layer
- Need a structured tech debt inventory
- Establish a quality baseline and propose ongoing quality gates

## Scope
- **In scope:** All R/ package code, tests/, inst/, config, plus actively-used root scripts
- **Out of scope:** One-off investigation scripts (investigate_*.R), conversion_plans/, money_market_analysis/ (unless actively used)
- **Test review:** Excluded (production code only)
- **All strategies reviewed equally** — no shortcuts for less-used strategies

## Priority Model
- **Data integrity first** — anything that could corrupt DuckDB or produce incorrect financial calculations
- Severity: Critical > High > Medium > Low

## Review Dimensions (weighted)
| Dimension | Weight | Focus |
|-----------|--------|-------|
| Correctness | Critical | Calculation accuracy, logic errors, formula verification (spot-check) |
| Data Integrity | Critical | DB operations, transaction safety, write paths |
| Error Handling | High | API failures, fallback behavior, silent failures |
| Maintainability | Medium | Code duplication, dead code, unclear patterns |
| Performance | Medium | Unnecessary computation, N+1 queries |
| Security | Medium | Token handling, input validation |
| Code Style | Low | Consistency, naming |

## Per-File Review Checklist
1. NA/NULL handling in calculations
2. Error propagation (tryCatch vs silent failure)
3. DB connection lifecycle (on.exit cleanup)
4. Hardcoded values that should be config
5. Correct use of parse_option_details() vs manual parsing
6. Consistent use of shared_risk_engine for simulations
7. Proper reactive isolation in modules
8. Dead/unreachable code paths

## Math Review
- Spot-check key pricing formulas against theory (Black-Scholes, Greeks via RQuantLib)
- Focus mainly on implementation correctness: wrong variables, type coercion, NA handling, edge cases

## Batch Organization (by strategy)

### Batch 1: Cash-Secured Puts
- fct_cash_secured_puts.R
- fct_sp500_cash_secured_puts.R
- fct_etf_cash_secured_puts_yfscreen.R
- mod_cash_secured_puts.R
- mod_cash_secured_puts_results_table.R
- mod_sp500_cash_secured_puts.R
- mod_etf_cash_secured_puts_yfscreen.R
- utils_cash_secured_puts_config.R
- page_cash_secured_puts.R
- page_sp500_cash_secured_puts.R
- page_etf_cash_secured_puts_yfscreen.R

### Batch 2: Covered Calls / Dynamic Covered Calls
- fct_dynamic_covered_calls_analysis.R
- mod_dynamic_covered_calls_analysis.R
- mod_etf_covered_calls_analysis.R
- utils_covered_calls_shared.R
- utils_dynamic_covered_calls_config.R
- utils_dynamic_covered_calls_helpers.R
- utils_etf_covered_calls_config.R
- page_dynamic_covered_calls.R
- page_etf_covered_calls.R

### Batch 3: Dividend Capture (weekly/monthly/high-yield)
- fct_dividend_capture_weekly.R
- fct_dividend_capture_monthly.R
- fct_dividend_capture_monthly_high_yield.R
- fct_high_yield_dividend_capture.R
- mod_dividend_capture_weekly_controls.R
- mod_dividend_capture_weekly_results.R
- mod_dividend_capture_monthly_controls.R
- mod_dividend_capture_monthly_results.R
- mod_dividend_capture_monthly_high_yield_controls.R
- mod_dividend_capture_monthly_high_yield_results.R
- mod_high_yield_dividend_capture_controls.R
- mod_high_yield_dividend_capture_results.R
- utils_dividend_capture.R
- utils_dividend_capture_monthly_config.R
- utils_dividend_capture_monthly_high_yield_config.R
- utils_dividend_capture_weekly_config.R
- page_dividend_capture_weekly.R
- page_dividend_capture_monthly.R
- page_dividend_capture_monthly_high_yield.R
- page_dividend_capture_russell_2000.R

### Batch 4: Collars
- fct_collar_analysis.R
- mod_collar_controls.R
- mod_collar_results.R
- utils_collar_config.R
- utils_collar_etf_universe.R
- page_collar.R

### Batch 5: Calendar Spreads
- fct_put_calendar_spread.R
- mod_put_calendar_spread.R
- mod_put_calendar_spread_results_table.R
- utils_put_calendar_spread_config.R
- page_put_calendar_spread.R

### Batch 6: Portfolio Core (DB, groups, risk, income, projections)
- fct_portfolio_database.R
- fct_portfolio_groups_database.R
- fct_portfolio_groups_logic.R
- fct_portfolio_risk.R
- fct_portfolio_expected_return.R
- fct_income_projection_database.R
- fct_income_projection_engine.R
- fct_cash_flow_projection.R
- fct_activities_database.R
- fct_group_metrics.R
- fct_group_pnl.R
- fct_shared_risk_engine.R
- fct_risk_analysis.R
- fct_monte_carlo.R
- fct_lsm_engine.R
- fct_early_exercise.R
- fct_regime_detection.R
- fct_pattern_matching.R
- fct_suggestion_engine.R
- fct_suggestions_database.R
- mod_portfolio_groups.R
- mod_portfolio_groups_cards.R
- mod_portfolio_groups_dashboard.R
- mod_portfolio_return_summary.R
- mod_portfolio_risk_dashboard.R
- mod_position_risk.R
- mod_cash_flow_projection.R
- mod_raw_activities.R
- mod_review_transactions.R
- utils_portfolio_config.R
- utils_portfolio_groups_config.R
- utils_risk_config.R
- utils_risk_helpers.R
- utils_risk_presets.R
- utils_group_cards.R
- page_portfolio_groups.R
- page_portfolio_risk.R
- page_cash_flow_projection.R
- page_raw_activities.R
- page_home.R

### Batch 7: Questrade API + Market Data
- fct_questrade_api.R
- fct_questrade_options.R
- fct_questrade_quotes.R
- fct_background_refresh.R
- fct_implied_volatility.R
- utils_questrade_healthcheck.R
- utils_quote_source_toggle.R
- utils_market_data.R
- utils_options_cache.R
- utils_sector_cache.R
- mod_token_settings.R
- page_token_settings.R

### Batch 8: Aristocrats / Zero-Dividend / Other Analysis
- fct_aristocrats_analysis.R
- fct_zero_dividend_analysis.R
- fct_etf_yfscreen_analysis.R
- fct_money_market_rotation.R
- mod_aristocrats_analysis.R
- mod_aristocrats_results_table.R
- mod_zero_dividend_analysis.R
- mod_zero_dividend_results_table.R
- mod_money_market_rotation.R
- mod_extrinsic_value_scanner.R
- mod_extrinsic_value_scanner_fct.R
- mod_extrinsic_value_scanner_controls.R
- mod_extrinsic_value_scanner_controls_fct.R
- utils_aristocrats_config.R
- utils_aristocrats_helpers.R
- utils_zero_dividend_config.R
- utils_yfscreen_etf.R
- utils_stock_universe.R
- utils_custom_ticker_lists.R
- page_aristocrats.R
- page_zero_dividend.R
- page_extrinsic_value_scanner.R
- page_money_market_rotation.R

### Batch 9: UI Infrastructure + Config + Shared Utilities
- app_config.R
- app_ui.R
- run_app.R
- utils_00_config.R
- utils_activity_linking.R
- utils_analysis_controls_helper.R
- utils_calculations.R
- utils_cash_equivalent.R
- utils_cash_equivalent_linking.R
- utils_dividend_capture.R (if not in Batch 3)
- utils_explainability.R
- utils_formatting.R
- utils_globals.R
- utils_high_yield_capture_russell_2000.R
- utils_transaction_helpers.R
- utils_ui_components.R
- page_about.R
- _disable_autoload.R

## Output Format
- **Per-batch:** Separate findings document (e.g., docs/audit/batch-01-csp.md)
- **Master summary:** docs/audit/summary.md with aggregate statistics
- Each finding: severity, description, file:line reference
- Summary statistics: files reviewed, findings per severity, checklist item hit rates

## Known Issues to Verify
- Questrade API: silent stale data when token refresh fails (user-reported)

## Quality Gates (to propose after audit)
- Coding standards for new fct_*.R functions
- DB operation patterns
- Error handling requirements
- Config usage patterns

## Verification
- Summary statistics showing files reviewed, findings per severity, checklist item hit rates
- Every batch produces a findings document
- Master summary aggregates all batches
