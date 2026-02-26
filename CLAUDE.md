# investR - Investment Research Shiny Application

## Quick Commands
```r
# Run the app (dev mode)
source("dev/run_dev.R")

# Run tests
devtools::test()

# Type/syntax check
devtools::check()

# Generate documentation
devtools::document()

# Load package without installing
devtools::load_all()
```

## Architecture

Production-grade Shiny app built with Golem + Brochure. Each page runs in an independent Shiny session via Brochure routing. Modules compose the UI on each page.

| Prefix | Purpose | Side Effects |
|--------|---------|-------------|
| `page_*.R` | Brochure page definitions (URL routing) | Orchestrates modules |
| `mod_*.R` | Shiny modules (UI + server in one file) | Reactive I/O |
| `fct_*.R` | Business logic (pure functions) | None — testable in isolation |
| `utils_*.R` | Configuration and helpers | Read-only config access |

Key patterns:
- Modules use `moduleServer()` with `NS()` for namespace isolation
- Business logic in `fct_*.R` must be pure — no Shiny reactivity, no side effects
- Configuration flows through a 3-tier system: `golem-config.yml` → `STRATEGY_CONFIG` objects in `utils_*_config.R` → `get_golem_config_value()` with fallbacks
- Never hard-code magic numbers — always use config objects

## Database (SQLite)

Primary database at `inst/database/portfolio.sqlite`. Core tables: portfolio_positions, portfolio_activities, portfolio_groups, income_projections, cash_flow_projections.

Rules:
- Always close connections with `on.exit(dbDisconnect(con))` or `tryCatch` finally blocks
- Use transactions for multi-step operations with rollback on error
- Audit significant changes in `projection_recalculations` table
- Tests use isolated temp databases (see `tests/testthat/setup.R`)

## Questrade API

READ-ONLY access to 3 accounts (RRSP, SRRSP, LIRA). Tokens cached in `~/.investR_tokens.json` with auto-refresh.

- On 401 errors: attempt token refresh, then provide path to generate new token. Never silently fail.
- If Questrade fails, fall back to Yahoo Finance via `quantmod`. Track fallbacks with `get_fallback_summary()` and always inform the user which data came from fallback.
- Rate limiting configured in `golem-config.yml` (0.5-2s between requests)
- Background refresh runs hourly via `later::later()` with promise chains

## Critical Pitfalls

1. **Option symbol formats**: Two formats exist in production — legacy ("ALB17Dec27C55.00") and new ("AAPL250119C150"). Always use `parse_option_details()`, never parse manually.
2. **Questrade symbol field**: Sometimes blank for options. Use description field for uniqueness.
3. **Risk simulation baseline**: Portfolio stress tests MUST use same baseline (purchase_price) as position-level. Use `shared_risk_engine.R` for all simulations.
4. **Cash flow projection rules vary by strategy**: Covered calls = dividends + option gains. Cash-secured puts = premium only. Legacy covered calls = actuals only. Always check `strategy_type` before generating.
5. **Database connections**: Always use `on.exit()` for cleanup. Never silently fail — log and inform user.

## Testing

- Framework: testthat 3.0.0+ (Edition 3)
- Location: `tests/testthat/`
- Isolation: `setup.R` creates temp SQLite databases, `teardown.R` cleans up
- Pattern: `test-{component}.R` (e.g., `test-fct_monte_carlo.R`)
- Use `withr::with_options()` for config isolation in tests
- Mock external APIs (Questrade) — never hit real APIs in tests

## Code Style

- Tidyverse syntax (pipes, dplyr verbs)
- Logger package for all application logging
- Golem conventions for module structure
- RQuantLib for all options pricing (Greeks, American options)
- Parallel processing via `future` + `furrr` for strategy analysis

---

# CODE CHANGE APPROVAL WORKFLOW

You are PROHIBITED from using Write, Edit, or NotebookEdit tools until you receive explicit approval with words like "approved", "yes", "proceed", "make the changes", or "go ahead".

When the user mentions bugs, issues, improvements, or features:
1. ANALYZE — Use Read, Grep, Glob, Bash to investigate
2. EXPLAIN — What the problem is, what caused it, which files/lines need to change, what the proposed changes would be
3. ASK — "Would you like me to make these changes?"
4. WAIT — Do not proceed until you receive explicit approval

Exceptions — proceed directly only when the user says: "Make the changes now", "Just fix it", "Apply the fix", or explicitly approves your proposal.

Safety rules:
- Never install packages or restart services without approval
- Never modify database schema without approval
- Never delete data without confirmation
