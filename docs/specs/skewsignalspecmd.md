# Volatility Skew Signal — Implementation Spec

*Finalized: 2026-04-19*

## Overview

Add a "Fetch Skew" button to each candidate card in the Dividend Aristocrats and Zero-Dividend Stocks strategy screens. When clicked, it fetches the nearest 30-day options chain for that ticker and displays a modal showing IV asymmetry between calls and puts at five delta levels (0.20, 0.30, 0.40, 0.50, 0.60). This is the skew signal identified via binomial testing (p<0.001) as a real-time indicator of post-entry stock move risk for deep ITM covered call positions.

## Constraints

**Files to Modify:**
- `R/mod_aristocrats_analysis.R` — add Fetch Skew button + modal trigger per card
- `R/mod_zero_dividend_analysis.R` — same

**New Files Allowed:**
- `R/fct_skew_signal.R` — business logic only (pure function, no Shiny reactivity)
- `tests/testthat/test-fct_skew_signal.R` — unit tests

**Files NOT to Modify:**
- `R/fct_aristocrats_analysis.R`
- `R/fct_zero_dividend_analysis.R`
- `R/fct_questrade_options.R`
- `R/fct_implied_volatility.R`
- Database schema (no new tables, no schema changes)

**New Dependencies Allowed:** None — use existing Shiny modal infrastructure

**Existing Code to Reuse:**
- `fct_questrade_options.R` — `fetch_options_chain()` for chain data
- `fct_implied_volatility.R` — IV extraction utilities
- `utils_market_data.R` — Questrade → Yahoo fallback pattern

**Out of Scope:**
- Historical options chain data (today's snapshot only)
- Long-dated chain analysis (use 30-day chain only)
- Computing IV from scratch via Black-Scholes
- Redesigning the existing screening pipeline
- Persisting skew data to the database
- Batch pre-fetching skew for all cards

---

## Problem Statement

Deep ITM covered call entries in this portfolio show statistically significant post-entry stock moves (p<0.001, binomial test). IV skew in the short-dated options chain at entry time is the most theoretically grounded real-time signal available from a single chain snapshot. The app currently has no way to surface this signal at the point of candidate evaluation.

---

## User Stories

### US-1: Skew computation function

**Description:** As a developer, I want a pure function that accepts a ticker symbol and returns a structured skew table so that the Shiny modules can call it without embedding business logic.

**Acceptance Criteria:**
- [ ] `compute_skew_signal(ticker)` in `R/fct_skew_signal.R` returns a list with:
  - `$table` — data frame with columns: `delta`, `call_strike`, `call_ask`, `call_iv`, `put_strike`, `put_ask`, `put_iv`, `iv_diff`
  - `$aggregate` — named numeric: delta-weighted average of `iv_diff` = `sum(delta * iv_diff) / sum(delta)` using weights 0.20, 0.30, 0.40, 0.50, 0.60
  - `$expiry_date` — Date of the chain used
  - `$days_to_expiry` — integer days from today to expiry
  - `$data_source` — character: "Questrade" or "Yahoo Finance"
- [ ] Delta matching uses nearest-available (no interpolation); ties broken by taking the lower delta
- [ ] Expiry selection uses the chain nearest to 30 days from today (no range restriction)
- [ ] Falls back to Yahoo Finance (`quantmod::getOptionChain()`) if Questrade fails; `$data_source` reflects which was used
- [ ] Returns a list with `$error` character field (and other fields NULL) if both sources fail or no options exist
- [ ] Unit tests pass: `devtools::test(filter = "fct_skew_signal")`
- [ ] `devtools::check()` passes (no new warnings or errors)

### US-2: Fetch Skew button and modal in Dividend Aristocrats cards

**Description:** As an investor, I want a "Fetch Skew" button on each Dividend Aristocrats candidate card so that I can inspect the IV skew signal before entering a covered call position.

**Acceptance Criteria:**
- [ ] Each candidate card in `mod_aristocrats_analysis.R` has a "Fetch Skew" button
- [ ] Button shows a spinner while the fetch is in progress and is disabled during the fetch
- [ ] Modal opens after fetch completes with:
  - Header: "IV Skew Signal — {TICKER}"
  - Subtitle: "Expiry: {MMM DD, YYYY} ({N} days)"
  - Five-row table: delta, call strike, call ask, call IV%, put strike, put ask, put IV%, IV Diff (C-P)
  - Aggregate row: delta-weighted avg IV diff
  - Footnote (only if Yahoo fallback): "Source: Yahoo Finance (Questrade unavailable)"
- [ ] If fetch fails, modal opens with error message instead of table
- [ ] `devtools::load_all()` succeeds without errors
- [ ] App runs without errors: `source("dev/run_dev.R")`

### US-3: Fetch Skew button and modal in Zero-Dividend Stocks cards

**Description:** As an investor, I want the same Fetch Skew functionality on Zero-Dividend Stocks candidate cards.

**Acceptance Criteria:**
- [ ] Same acceptance criteria as US-2, applied to `mod_zero_dividend_analysis.R`
- [ ] Code is not duplicated — `compute_skew_signal()` is called from both modules; any shared modal rendering helper lives in `fct_skew_signal.R` or a utility, not copy-pasted

### US-4: Visual formatting of IV Diff column

**Description:** As an investor, I want the IV Diff column to show directional arrows and color coding so I can read the skew at a glance.

**Acceptance Criteria:**
- [ ] IV Diff > +0.5%: up arrow (↑) + value in green
- [ ] IV Diff < -0.5%: down arrow (↓) + value in red
- [ ] IV Diff within ±0.5%: dash (—) + value in default text color
- [ ] Aggregate row uses same color/arrow logic
- [ ] All values displayed as percentage with one decimal place (e.g., "+3.1%", "-2.4%")
- [ ] Visual verified in browser via `source("dev/run_dev.R")`

---

## Technical Design

### Data Flow

```
User clicks "Fetch Skew" on card for TICKER
  → button disables, spinner shows
  → compute_skew_signal(ticker) called
    → fetch_options_chain(ticker) via fct_questrade_options.R
      → on failure: quantmod::getOptionChain(ticker) fallback
    → select expiry nearest to today + 30 days
    → for each target delta in c(0.20, 0.30, 0.40, 0.50, 0.60):
        find call with nearest delta
        find put with nearest abs(delta)
        record strike, ask, iv for each
        compute iv_diff = call_iv - put_iv
    → compute aggregate = sum(delta * iv_diff) / sum(delta)
    → return structured list
  → button re-enables
  → modal opens with table
```

### `compute_skew_signal()` signature

```r
compute_skew_signal <- function(ticker) {
  # Returns list:
  # $table        — data.frame (5 rows)
  # $aggregate    — numeric scalar
  # $expiry_date  — Date
  # $days_to_expiry — integer
  # $data_source  — character ("Questrade" | "Yahoo Finance")
  # $error        — character | NULL (NULL means success)
}
```

### Aggregate formula

```
aggregate = sum(delta_i * iv_diff_i) / sum(delta_i)
         = (0.20*d1 + 0.30*d2 + 0.40*d3 + 0.50*d4 + 0.60*d5) / 2.00
```

Where `d_i = call_iv_i - put_iv_i` at each delta level.

### Delta matching

For each target delta `t` in {0.20, 0.30, 0.40, 0.50, 0.60}:
- Calls: find option where `abs(delta - t)` is minimized
- Puts: find option where `abs(abs(delta) - t)` is minimized (puts have negative delta)
- On tie: use the lower absolute delta value

### Expiry selection

Select from available expiry dates the one minimizing `abs(days_to_expiry - 30)`. Always use it regardless of how far it is from 30 days — show actual `days_to_expiry` in modal header so the user has context.

---

## Implementation Phases

### Phase 1: Business logic + tests

- [ ] Create `R/fct_skew_signal.R` with `compute_skew_signal(ticker)`
- [ ] Verify Questrade options chain field names for delta and IV columns (read `fct_questrade_options.R` before implementing)
- [ ] Implement delta matching (nearest-available)
- [ ] Implement expiry selection (nearest to 30 days)
- [ ] Implement Yahoo fallback
- [ ] Compute weighted aggregate
- [ ] Create `tests/testthat/test-fct_skew_signal.R`:
  - Test delta matching with mock chain data
  - Test aggregate formula
  - Test error return when no options exist
  - Test Yahoo fallback path (mock Questrade failure)
- **Verification:** `devtools::test(filter = "fct_skew_signal")` — all tests pass; `devtools::check()` — no new warnings

### Phase 2: Module integration

- [ ] Add "Fetch Skew" button to each card in `mod_aristocrats_analysis.R`
  - Button ID namespaced per ticker: `paste0("skew_btn_", ticker)`
  - Disable during fetch, re-enable after
  - Spinner via `shinyWidgets::addSpinner()` or equivalent already in use
- [ ] Wire `observeEvent` to call `compute_skew_signal(ticker)` and open Shiny modal
- [ ] Modal renders: header, subtitle (expiry), table (plain, no colors yet), aggregate row, footnote if fallback
- [ ] Error path: if `$error` is non-NULL, modal shows error message
- [ ] Repeat for `mod_zero_dividend_analysis.R`
- **Verification:** `devtools::load_all()` — no errors; `source("dev/run_dev.R")` — app launches, button visible on cards, modal opens with table

### Phase 3: Visual polish

- [ ] Apply arrow + color formatting to IV Diff column:
  - Use `shiny::tags` or inline CSS to color cells
  - Positive (> +0.5%): ↑ green
  - Negative (< -0.5%): ↓ red
  - Neutral: — default color
- [ ] Same formatting on aggregate row
- [ ] Format all IV values as percentages with one decimal place
- [ ] Source footnote for Yahoo fallback
- [ ] Verify in browser: open Dividend Aristocrats, click Fetch Skew, confirm modal layout matches spec mockup
- **Verification:** Manual browser test via `source("dev/run_dev.R")`; `devtools::check()` — clean

---

## Definition of Done

- [ ] All acceptance criteria in US-1 through US-4 pass
- [ ] All three implementation phases verified
- [ ] Tests pass: `devtools::test()`
- [ ] Package check clean: `devtools::check()` — no new warnings or errors
- [ ] App loads: `source("dev/run_dev.R")` — no errors
- [ ] Skew modal verified in browser for at least one Dividend Aristocrat and one Zero-Dividend ticker
- [ ] No business logic in module files (all computation in `fct_skew_signal.R`)

---

## Ralph Loop Command

```
/ralph "Implement the Volatility Skew Signal per spec at docs/specs/skewsignalspecmd.md

PHASES:
1. Business logic: create R/fct_skew_signal.R + tests/testthat/test-fct_skew_signal.R - verify with devtools::test(filter='fct_skew_signal') and devtools::check()
2. Module integration: add Fetch Skew button + modal to mod_aristocrats_analysis.R and mod_zero_dividend_analysis.R - verify with devtools::load_all() and source('dev/run_dev.R')
3. Visual polish: arrow indicators + color coding for IV Diff column, source footnote, percentage formatting - verify manually in browser

VERIFICATION (run after each phase):
- devtools::test()
- devtools::check()

CRITICAL CONSTRAINTS:
- Do NOT modify fct_aristocrats_analysis.R, fct_zero_dividend_analysis.R, fct_questrade_options.R, fct_implied_volatility.R, or database schema
- All business logic in fct_skew_signal.R only — no computation in module files
- Verify Questrade options chain field names before writing delta-matching logic (read fct_questrade_options.R first)

ESCAPE HATCH: After 20 iterations without progress:
- Document what's blocking under 'Implementation Notes' in the spec
- List approaches attempted
- Stop and ask for human guidance

Output <promise>COMPLETE</promise> when all phases pass verification."
```

---

## Open Questions (Pre-Implementation)

1. Verify the exact column names returned by `fetch_options_chain()` for delta and IV fields — read `fct_questrade_options.R` before writing delta-matching code.
2. Confirm which spinner/loading utility is already in use in the modules to avoid introducing a new dependency.
