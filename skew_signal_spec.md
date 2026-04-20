# Volatility Skew Signal — Implementation Spec

## What This Is

A button added to each candidate card in the Dividend Aristocrats and Zero-Dividend Stocks strategy screens. When clicked, it fetches the nearest 30-day options chain for that stock and displays a table showing implied volatility asymmetry between calls and puts at matching delta levels. This asymmetry is the skew signal.

## Why

Deep ITM covered call positions in this portfolio show statistically significant post-entry stock moves (proven via binomial tests, p<0.001). Research concluded that IV skew in the short-dated options chain at the time of entry is the most theoretically grounded real-time signal available from a single chain snapshot.

## The Signal Explained

For a given stock, fetch the ~30-day options chain. At each of five delta levels (0.20, 0.30, 0.40, 0.50, 0.60):
- Find the call option at that delta
- Find the put option at that delta (same magnitude, e.g., -0.40 delta put)
- Record ask price and implied volatility for each

**Why IV, not raw price:**
A 0.40 delta call and 0.40 delta put sit at different strikes — the call is closer to ATM by construction (lognormality). Raw price comparison is structurally biased toward calls appearing more expensive regardless of market direction. IV normalizes for strike location, isolating the directional signal.

**Signal interpretation:**
- Call IV > Put IV at a given delta → market pricing more upside uncertainty → bullish skew
- Put IV > Call IV → market pricing more downside uncertainty → bearish skew
- Near zero difference → market neutral

## Output Table

| Delta | Call Strike | Call Ask | Call IV | Put Strike | Put Ask | Put IV | IV Diff (C-P) |
|-------|------------|----------|---------|------------|---------|--------|----------------|
| 0.20  |            |          |         |            |         |        |                |
| 0.30  |            |          |         |            |         |        |                |
| 0.40  |            |          |         |            |         |        |                |
| 0.50  |            |          |         |            |         |        |                |
| 0.60  |            |          |         |            |         |        |                |
| **Aggregate** | | | | | | | **TBD** |

## Aggregate Statistic

**NOT YET DESIGNED** — this needs deliberate thought in the implementation session. Questions to resolve:
- Simple average of IV differences across five deltas?
- Weighted by delta magnitude?
- Normalized against ATM IV?
- What threshold constitutes a meaningful signal vs noise?

Do not invent an aggregate without research/reasoning. Flag this as an open question.

## Data Source

- Primary: Questrade API — `fetch_options_chain()` in `fct_questrade_options.R`
- Fallback: Yahoo Finance via `quantmod::getOptionChain()`
- Target expiration: nearest available to 30 days from today
- Delta matching: options chain from Questrade includes delta — find closest available to target delta

## Where to Add the Button

- `R/mod_aristocrats_analysis.R` — Dividend Aristocrats card UI
- `R/mod_zero_dividend_analysis.R` — Zero-Dividend Stocks card UI

The button triggers a modal or expandable panel showing the table above.

## Existing Infrastructure to Reuse

- `fct_implied_volatility.R` — already has IV extraction from options chain
- `fct_questrade_options.R` — already fetches options chains with delta and IV columns
- `utils_market_data.R` — Questrade → Yahoo fallback pattern already established
- Parallel processing via `future_map()` already in place — this fetch is per-card on demand, not batch

## What NOT to Do

- Do NOT fetch historical options chain data — only today's snapshot
- Do NOT use the long-dated chain (the one used for the actual position) for the skew signal — use 30-day chain only
- Do NOT compute IV from scratch using Black-Scholes — use the IV column already returned by Questrade/Yahoo
- Do NOT redesign the existing screening pipeline — this is additive only
- Do NOT implement the aggregate statistic without resolving the open question above

## Files to Modify

- `R/mod_aristocrats_analysis.R` — add button and modal/panel UI
- `R/mod_zero_dividend_analysis.R` — add button and modal/panel UI
- New file: `R/fct_skew_signal.R` — business logic for fetching and computing the skew table

## Files NOT to Modify

- `R/fct_aristocrats_analysis.R` — screening logic unchanged
- `R/fct_zero_dividend_analysis.R` — screening logic unchanged
- `R/fct_questrade_options.R` — use as-is
- `R/fct_implied_volatility.R` — use as-is
- Database schema — no changes

## Open Questions to Resolve in Implementation Session

1. Does the Questrade options chain actually return delta for each option? Verify before designing the delta-matching logic.
2. What happens when no option exists at exactly the target delta — what interpolation or nearest-match logic to use?
3. Aggregate statistic design — needs deliberate research before coding.
4. UI placement: modal popup vs inline expandable panel vs separate tab?
5. Should the table be cached per session or re-fetched each time the button is clicked?
