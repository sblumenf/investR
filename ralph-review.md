### Review: Dividend Staleness Guard — Phase 1 (Shared Helper + Tests)

**Verdict**: PASS

**Spec compliance**: Does the implementation match what the spec asked for?
- `is_dividend_stale()` exists in `R/utils_market_data.R`: PASS
- Accepts xts object (output of `fetch_dividend_history()`): PASS
- Returns list with `is_stale`, `last_date`, `days_since`, `threshold`: PASS
- Uses 1.5x average interval logic: PASS
- Returns `is_stale = FALSE` for NULL input: PASS
- Returns `is_stale = FALSE` for fewer than 2 records: PASS
- Unit tests cover all 6 required scenarios (quarterly active, suspended, annual, NULL, single-record, threshold boundary): PASS

**Code quality**: Any obvious problems?
- Implementation exactly matches the Technical Design pseudocode in the spec: PASS
- Uses `zoo::index()` and `Sys.Date()` correctly for xts date extraction: PASS
- `@export` tag present — function will be available package-wide: PASS
- No magic numbers; threshold is computed as `avg_days_between * 1.5`: PASS

**Tests**: Are changes covered?
- New test file at `tests/testthat/test-dividend_staleness.R`: PASS
- 16 assertions across 6 test blocks, all passing: PASS
- Test file placed in correct directory per constraints: PASS

**Constraint compliance**:
- Files modified only in allowed list (`R/utils_market_data.R`, test file): PASS
- No off-limits files touched (no page_*, mod_*, config files): PASS
- New file in allowed directory (`tests/testthat/`): PASS
- No new dependencies added: PASS

**Issues**: None

**Scope**: Only Phase 1 complete. Phases 2 and 3 remain (Aristocrats/Collar integration, projection engine unification).
