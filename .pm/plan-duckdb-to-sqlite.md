# PM Plan: Migrate DuckDB to SQLite

## Current State

The investR app uses DuckDB for all persistent storage via the `duckdb` R package and DBI interface. There are 7 tables with at most 17K rows. DuckDB is an OLAP engine being used for OLTP workloads, causing known file bloat issues that VACUUM cannot fix.

The DBI abstraction is clean — all SQL is standard with two exceptions:
1. `information_schema.columns` queries (5 occurrences in one file)
2. `ALTER TABLE ADD COLUMN IF NOT EXISTS` syntax (6 occurrences in two files)

## Breaking Changes (DuckDB -> SQLite)

| Issue | Impact | Fix |
|-------|--------|-----|
| `information_schema.columns` not in SQLite | 5 queries in 1 file | Use `PRAGMA table_info()` |
| `ALTER TABLE ADD COLUMN IF NOT EXISTS` not in SQLite | 6 occurrences in 2 files | Check column existence in R first |
| `dbConnect(duckdb::duckdb(), dbdir=)` | Every connection | `dbConnect(RSQLite::SQLite(), dbname=)` |
| `dbDisconnect(conn, shutdown=TRUE)` | Unknown count | Remove `shutdown` param |
| `read_only = TRUE` param | ~5 script files | Use `flags = RSQLite::SQLITE_RO` |
| TIMESTAMP/DATE types | Schema definitions | SQLite stores as TEXT; RSQLite handles conversion |
| BOOLEAN type | Schema definitions | SQLite stores as INTEGER 0/1; transparent |

## Steps

### Step 1: Create data migration script (DuckDB -> SQLite)
- **What:** Write an R script that reads all 7 tables from the existing DuckDB file and writes them into a new SQLite file, preserving all data.
- **Files:** NEW `scripts/migrate_duckdb_to_sqlite.R`
- **Agent:** implementer
- **Notes:** This runs BEFORE any code changes. Both duckdb and RSQLite packages must be available. Script should verify row counts match after transfer.

### Step 2: Update DESCRIPTION
- **What:** Replace `duckdb` dependency with `RSQLite` in Imports.
- **Files:** `DESCRIPTION`
- **Agent:** implementer

### Step 3: Update core connection function
- **What:** Rewrite `get_portfolio_db_connection()` in `fct_portfolio_database.R` to use RSQLite. Change `duckdb::duckdb()` to `RSQLite::SQLite()`, `dbdir` to positional arg, remove `shutdown` from disconnect, handle `read_only` via `flags`.
- **Files:** `R/fct_portfolio_database.R`
- **Agent:** implementer

### Step 4: Update all `@importFrom duckdb` roxygen tags
- **What:** Replace `@importFrom duckdb duckdb dbConnect dbDisconnect` with `@importFrom RSQLite SQLite` across all fct_* and utils_* files. These files don't create connections directly — they call `get_portfolio_db_connection()` — so they only need the import tag fixed.
- **Files:** `R/fct_activities_database.R`, `R/fct_portfolio_groups_database.R`, `R/fct_income_projection_database.R`, `R/fct_cash_flow_projection.R`, `R/fct_suggestions_database.R`, `R/utils_activity_linking.R`, `R/utils_cash_equivalent_linking.R`
- **Agent:** implementer

### Step 5: Fix information_schema queries and ALTER TABLE IF NOT EXISTS
- **What:** Rewrite the 5 `information_schema.columns` queries and 6 `ALTER TABLE ADD COLUMN IF NOT EXISTS` statements in `fct_portfolio_groups_database.R` and `fct_activities_database.R`. Use a helper function that checks `PRAGMA table_info()` for column existence, then conditionally runs `ALTER TABLE ADD COLUMN`.
- **Files:** `R/fct_portfolio_groups_database.R`, `R/fct_activities_database.R`
- **Agent:** implementer
- **Critical:** This is the highest-risk change. Must be tested thoroughly.

### Step 6: Update config and path resolution
- **What:** Change default database filename from `portfolio.duckdb` to `portfolio.sqlite` in `utils_portfolio_config.R` and `golem-config.yml`.
- **Files:** `R/utils_portfolio_config.R`, `inst/golem-config.yml`
- **Agent:** implementer

### Step 7: Update test infrastructure
- **What:** Change `library(duckdb)` to `library(RSQLite)` in setup.R. Update `helper-test-db.R` to use RSQLite connections. Update teardown.R file pattern from `.duckdb` to `.sqlite`. Update the in-memory connection in `test-fct_portfolio_groups_database.R`.
- **Files:** `tests/testthat/setup.R`, `tests/testthat/teardown.R`, `tests/testthat/helper-test-db.R`, `tests/testthat/test-fct_portfolio_groups_database.R`
- **Agent:** implementer

### Step 8: Update standalone scripts
- **What:** Replace `library(duckdb)` with `library(RSQLite)` and update `dbConnect()` calls in all 14 standalone script files. Mechanical find-and-replace.
- **Files:** `investigate_ben_final_report.R`, `investigate_ben_capital.R`, `investigate_ben_capital_detailed.R`, `investigate_cve_group.R`, `audit_all_groups.R`, `scripts/cleanup_duplicates_dryrun.R`, `scripts/cleanup_duplicates_execute.R`, `scripts/cleanup_test_data_dryrun.R`, `scripts/cleanup_test_data_execute.R`, `scripts/fix_rolled_option_groups.R`, `scripts/manual_activity_insert.R`, `scripts/tgt_position_create.R`, `scripts/tgt_position_dryrun.R`, `scripts/repair_csp_database.R`
- **Agent:** implementer

### Step 9: Run data migration
- **What:** Execute the migration script from Step 1 to create `portfolio.sqlite` from the existing `portfolio.duckdb`.
- **Files:** `inst/database/portfolio.sqlite` (new), `inst/database/portfolio.duckdb` (keep as backup)
- **Agent:** tester
- **Notes:** Verify all 7 tables migrated with correct row counts.

### Step 10: Run tests
- **What:** Run `devtools::test()` and fix any failures.
- **Agent:** tester

### Step 11: Final review
- **What:** Review all changes for correctness, missed references, and edge cases.
- **Agent:** reviewer

## Execution Strategy

- **Sequential:** Steps 1-8 must be sequential (each builds on prior)
- **Steps 3-4** can be parallelized (independent file changes)
- **Steps 5 and 6** can be parallelized (independent file changes)
- **Step 9** depends on Steps 1-8 being complete
- **Step 10** depends on Step 9
- **Step 11** depends on Step 10

## Risks

1. **TIMESTAMP/DATE handling** — SQLite stores dates as TEXT. RSQLite should handle conversion transparently for reads/writes via DBI, but date comparison queries (`WHERE event_date > ?`) may behave differently. Need to verify ISO 8601 format is used consistently.
2. **information_schema rewrite** — The PRAGMA-based column check is straightforward but must be tested against all 6 migration paths.
3. **dbDisconnect shutdown param** — Need to find ALL occurrences including in error handlers and tryCatch blocks.
4. **Data fidelity** — Must verify TIMESTAMP values survive the DuckDB->SQLite transfer without precision loss or timezone shifts.

## Estimated Scope

- **Files affected:** 28 (10 core R, 4 test, 14 scripts)
- **New files:** 2 (migration script, new .sqlite database)
- **Subagents needed:** implementer, tester, reviewer
- **Risk level:** Medium (well-scoped changes, but date handling and schema migration are sensitive)

## Status

- [ ] Step 1: Create data migration script
- [ ] Step 2: Update DESCRIPTION
- [ ] Step 3: Update core connection function
- [ ] Step 4: Update @importFrom tags (7 files)
- [ ] Step 5: Fix information_schema + ALTER TABLE IF NOT EXISTS
- [ ] Step 6: Update config and path resolution
- [ ] Step 7: Update test infrastructure
- [ ] Step 8: Update standalone scripts (14 files)
- [ ] Step 9: Run data migration
- [ ] Step 10: Run tests
- [ ] Step 11: Final review
