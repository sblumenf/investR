# Database Schema (DuckDB)

## Core Tables
- positions_history: Daily position snapshots
- account_activities: Trades, dividends, fees (group_id links to groups)
- position_groups: Strategy groupings (status: active/closed/ignored)
- position_group_members: Stocks/options in each group (role: underlying/short_call/short_put)
- income_projections: Actual + projected cash flows per group

## Connection Pattern
```r
conn <- get_portfolio_db_connection(read_only = FALSE)
on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)
```

## Key Constraints
- account_activities: UNIQUE on (account_number, description, trade_date, action, quantity, net_amount)
- Uses description not symbol (Questrade API inconsistency)

## Migration Pattern
- Use ALTER TABLE ... ADD COLUMN IF NOT EXISTS
- Migration functions in fct_portfolio_groups_database.R
- Always test on existing data
