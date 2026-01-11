# investR - Investment Research Shiny Application

## Project Overview

Production-grade Shiny application built with Golem + Brochure for multi-page investment research and options trading analysis. Integrates with Questrade API for real-time market data.

## Technology Stack

| Layer | Technologies |
|-------|--------------|
| **Framework** | Golem (app structure), Brochure (multi-page routing) |
| **UI** | bslib, Shiny, plotly, DT |
| **Database** | DuckDB (primary), SQLite |
| **Options Pricing** | RQuantLib (Greeks, American options), Monte Carlo simulation |
| **External API** | Questrade (quotes, options chains, account data) |
| **Data Processing** | tidyverse (dplyr, purrr, tidyr, stringr, lubridate) |
| **Testing** | testthat with test isolation (setup.R/teardown.R) |

## Feature Areas

### Portfolio Management
- **Portfolio Groups** - Track positions grouped by strategy
- **Cash Flow Projections** - Dividend and option income forecasting
- **Risk Dashboard** - Portfolio-level risk metrics and stress testing
- **Raw Activities** - Transaction import and reconciliation

### Options Strategies
- **Covered Calls** - Dynamic and ETF-based covered call analysis
- **Cash-Secured Puts** - Equity, ETF, and S&P 500 put strategies
- **Collars** - Protective collar strategy analysis
- **Put Calendar Spreads** - Calendar spread opportunity scanning

### Dividend Strategies
- **Dividend Aristocrats** - Quality dividend stock screening
- **Dividend Capture** - Weekly, monthly, and high-yield capture strategies
- **Russell 2000** - Small-cap dividend opportunities

### Analysis Tools
- **Extrinsic Value Scanner** - Options premium analysis
- **Zero-Dividend Analysis** - Non-dividend stock covered calls

## File Naming Conventions

| Prefix | Purpose | Example |
|--------|---------|---------|
| `page_*.R` | Brochure page definitions | `page_portfolio_groups.R` |
| `mod_*.R` | Shiny modules (UI + server) | `mod_portfolio_groups_cards.R` |
| `fct_*.R` | Business logic functions | `fct_monte_carlo.R` |
| `utils_*.R` | Utility/helper functions | `utils_formatting.R` |

## Database Patterns

- Use transactions for multi-step operations with rollback on error
- Always close connections in `tryCatch` finally blocks
- Audit logging in `projection_recalculations` table for significant changes
- Test isolation: tests use separate databases, cleaned in teardown.R

## Code Style Requirements

- Always use tidyverse syntax (pipes, dplyr verbs)
- Follow Golem framework best practices for Shiny modules
- Use testthat for all testing
- Use logger package for application logging

---

# CRITICAL: CODE CHANGE APPROVAL WORKFLOW

## ABSOLUTE RULE: NO CODE CHANGES WITHOUT EXPLICIT APPROVAL

You are STRICTLY PROHIBITED from using Write, Edit, or NotebookEdit tools until you receive explicit human approval with words like "approved", "yes", "proceed", "make the changes", or "go ahead".

## Mandatory Workflow for ALL Code-Related Requests

When the user mentions bugs, issues, improvements, features, or asks you to analyze/investigate code:

1. **STOP** - Do not make any code changes
2. **ANALYZE** - Use Read, Grep, Glob, Bash to investigate
3. **EXPLAIN** - Write a clear explanation of:
   - What the problem/issue is
   - What caused it
   - Exactly which files and lines need to change
   - What the proposed changes would be (in plain English)
4. **ASK** - End with: "Would you like me to make these changes?"
5. **WAIT** - Do not proceed until you receive explicit approval

## Examples of What Requires Approval

- "Fix this bug" → Analyze + Explain + Wait for approval
- "This isn't working correctly" → Analyze + Explain + Wait for approval
- "Add feature X" → Analyze + Explain + Wait for approval
- "The sort order is wrong, analyze and explain" → Analyze + Explain + Wait for approval
- "Refactor this code" → Analyze + Explain + Wait for approval

## The ONLY Exceptions

You may proceed directly to code changes ONLY when the user says:
- "Make the changes now"
- "Proceed without asking me"
- "Just fix it"
- "Apply the fix"
- Or explicitly approves your proposed changes

## Other Safety Rules

- Never install packages or restart services without approval
- Never modify database schema without explicit approval
- Never delete data without confirmation

## If You Break These Rules

If you accidentally make code changes without approval, immediately:
1. Apologize
2. Explain what you changed
3. Offer to revert the changes
