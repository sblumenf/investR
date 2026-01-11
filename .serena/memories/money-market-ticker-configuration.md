# Money Market / Cash Equivalent Ticker Configuration

## Overview

Money market ETFs (cash equivalents) receive special treatment in investR:
- No expiration date required
- Expected returns based on SEC yield (not projected annualized ROI)
- Minimal time horizon in Monte Carlo simulations (0.001 years)
- Auto-linking to "Cash Equivalent" groups

## Configuration Location

Tickers are defined in `inst/golem-config.yml` under the `cash_equivalents` section:

```yaml
cash_equivalents:
  tickers:
    - SGOV       # iShares 0-3 Month Treasury Bond ETF
    - ZMMK.TO    # BMO Money Market ETF (CAD)
    - BIL        # SPDR Bloomberg 1-3 Month T-Bill ETF
    - WEEK       # SPDR 1-3 Month T-Bill ETF (Weekly)
    - JMMF       # JPMorgan Money Market ETF
```

## Adding New Tickers

To add a new money market ticker, simply edit `inst/golem-config.yml` and add to the `tickers` list. No R code changes required.

## Accessor Functions

Located in `R/utils_cash_equivalent.R`:

| Function | Purpose |
|----------|---------|
| `get_cash_equivalent_tickers()` | Returns vector of tickers from config |
| `is_cash_equivalent(ticker)` | Check if ticker is money market (case-insensitive) |
| `get_cash_equivalent_sql_list()` | Returns SQL IN clause string, e.g., `('SGOV', 'ZMMK.TO', ...)` |

## Files Using Cash Equivalent Logic

| File | Usage |
|------|-------|
| `R/fct_portfolio_risk.R` | Exempts from expiration requirement, minimal time horizon in MC |
| `R/fct_portfolio_expected_return.R` | Maps to "Money Market" strategy, uses SEC yield for returns |
| `R/mod_review_transactions.R` | Assigns `cash_equivalent` role to members |
| `R/utils_cash_equivalent_linking.R` | Auto-links activities to Cash Equivalent groups |

## Special Treatment Summary

| Aspect | Money Market | Other Positions |
|--------|--------------|-----------------|
| Strategy Display | "Money Market" | Actual strategy type |
| Expected Return | SEC yield (default 5%) | Projected annualized ROI |
| Capital at Risk | Cost basis only | Complex option calculations |
| Risk Time Horizon | 0.001 years (~4 hours) | Days to expiry |
| Expiration Required | No | Yes |
| Income Projections | None created | Monthly cash flow events |
