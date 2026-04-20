# Specification Draft: Volatility Skew Signal

*Interview in progress - Started: 2026-04-19*

## Overview
Add a "Fetch Skew" button to each candidate card in the Dividend Aristocrats and Zero-Dividend Stocks screens. Clicking the button fetches the nearest 30-day options chain for that ticker and displays a modal showing IV asymmetry between calls and puts at five delta levels. This is the skew signal identified in binomial testing (p<0.001) as a real-time indicator of post-entry stock move risk.

## Decisions Captured

### UI
- **Placement:** Modal popup (not inline panel)
- **Modal header:** Shows ticker + expiry date + days to expiry (e.g., "IV Skew Signal — AAPL | Expiry: May 16, 2026 (27 days)")
- **Loading:** Button shows spinner while fetching; modal opens only after data is ready
- **Error state:** Modal opens with error message if fetch fails or no options exist
- **Data source notice:** Footnote in modal if Yahoo fallback used ("Source: Yahoo Finance (Questrade unavailable)")

### Data
- **Expiry selection:** Always use nearest available to 30 days; always show actual expiry + days in modal header
- **Delta matching:** Nearest available delta (no interpolation)
- **Caching:** No caching — re-fetch on every button click
- **Button visibility:** Shown on all cards; fails gracefully

### Aggregate statistic
- **Formula:** Delta-weighted average of IV differences
  - `aggregate = sum(delta_i * IV_diff_i) / sum(delta_i)`
  - Weights: 0.20, 0.30, 0.40, 0.50, 0.60 (higher delta = more ATM = more weight)
- Shown as a bottom row in the table

### Visual design
- **IV Diff column:** Arrow indicator + color
  - Positive (call IV > put IV, bullish skew): up arrow + green
  - Negative (bearish skew): down arrow + red
  - Near-zero (within ±0.5%): dash, default color

## Table Output

| Delta | Call Strike | Call Ask | Call IV | Put Strike | Put Ask | Put IV | IV Diff (C-P) |
|-------|------------|----------|---------|------------|---------|--------|----------------|
| 0.20  |            |          |         |            |         |        | ↑ +3.1% (green)|
| 0.30  |            |          |         |            |         |        | ↓ -2.4% (red)  |
| 0.40  |            |          |         |            |         |        | — 0.1%         |
| 0.50  |            |          |         |            |         |        |                |
| 0.60  |            |          |         |            |         |        |                |
| **Wtd Avg** | | | | | | | **delta-weighted** |

## Files

### Files to modify
- `R/mod_aristocrats_analysis.R` — add Fetch Skew button and modal trigger per card
- `R/mod_zero_dividend_analysis.R` — same

### New files
- `R/fct_skew_signal.R` — pure function: fetches chain, matches deltas, computes skew table + weighted aggregate

### Files NOT to modify
- `R/fct_aristocrats_analysis.R`
- `R/fct_zero_dividend_analysis.R`
- `R/fct_questrade_options.R`
- `R/fct_implied_volatility.R`
- Database schema

## Open Questions
- Does Questrade options chain response actually include delta per option? (Must verify against `fct_questrade_options.R` before implementing delta-matching logic)
- Exact column names returned by `fetch_options_chain()` — need to confirm field names for IV and delta

---
*Interview notes accumulated below*
