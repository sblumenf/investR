# Dividend Staleness Guard

## What It Does

When the app creates a new position group (covered call, collar, etc.), it projects future dividend income based on the stock's historical dividend payments from Yahoo Finance. The staleness guard prevents the app from projecting dividends for stocks that have stopped paying them.

Without this guard, a stock like HE (Hawaiian Electric) — which suspended its dividend in August 2023 after the Maui wildfire — would still show projected quarterly dividends of $72, as if nothing happened. The app would blindly use the last known dividend amount and repeat it into the future.

## How It Works

The guard checks one simple thing: **how long has it been since the company last paid a dividend, relative to how often they normally pay?**

1. The app looks up the stock's dividend history (e.g., quarterly payments every ~91 days).
2. It calculates the average interval between payments.
3. It checks how many days have passed since the last payment.
4. If the gap exceeds **1.5 times the average interval**, the dividend is treated as suspended — no dividend projections are generated, and a warning is logged.

### Example: HE (Hawaiian Electric)

- Average dividend interval: ~91 days (quarterly)
- Threshold: 91 x 1.5 = ~136 days
- Days since last dividend (Aug 2023): ~930 days
- 930 > 136, so the guard kicks in and suppresses dividend projections

### Example: A Normal Quarterly Payer

- Average interval: ~91 days
- Days since last dividend: 60 days
- 60 < 136, so dividends are projected normally

## Where It Lives

- **File:** `R/fct_income_projection_engine.R`
- **Function:** `generate_dividend_events()`
- The check runs after the average payment interval is calculated and before any projections are generated.

## What Gets Logged

When a dividend is suppressed, the app logs a warning like:

```
WARN: Income Projection: Dividend appears suspended for HE -
  last payment 2023-08-17, 930 days ago (threshold: 136 days)
```

The group still gets created — it just has no dividend projections. Option-related cash flows (premiums, projected gains at expiry) are unaffected.

## Known Limitation

The 1.5x threshold works well for quarterly payers but could be too aggressive for stocks that pay annually. A stock paying once a year (every ~365 days) would be flagged as suspended after ~547 days — roughly 6 months into a normal annual cycle. This has not been an issue so far since the portfolio consists mostly of quarterly payers, but it is worth being aware of if an annual payer is added in the future.

## Date Added

March 4, 2026 — introduced alongside the collar cost basis fix and the mixed-type timestamp repair.
