# Risk Management Implementation Status

## ✅ COMPLETED (Files Created)

### 1. Configuration & Core Engine
- ✅ `R/utils_risk_config.R` - Complete configuration with:
  - Monte Carlo parameters (10K/50K paths)
  - Jump diffusion & Heston model parameters
  - 5 pre-built stress scenarios (2008 Crisis, COVID, Rising Rates, Stagflation, Vol Spike)
  - Sector mapping for 40+ common tickers
  - Risk thresholds and alert levels

- ✅ `R/fct_monte_carlo.R` - Full Monte Carlo simulation engine with:
  - Jump diffusion price path generator
  - Heston stochastic volatility generator
  - Dividend schedule builder (reuses existing logic)
  - Early exercise detection along each path
  - Covered call payoff calculator
  - Main `run_monte_carlo_simulation()` function
  - Returns complete statistics: expected return, percentiles, early exercise prob

- ✅ `R/fct_early_exercise.R` - RQuantLib integration:
  - `calculate_early_exercise_probability()` using RQuantLib with discrete dividends
  - Greeks calculation (Delta, Gamma, Vega, Theta, Rho)
  - Per-dividend exercise probability calculation
  - `calculate_risk_adjusted_return()` function

## 🚧 REMAINING WORK

### Critical Components Needed:

#### 1. Core Risk Analysis Orchestrator (`R/fct_risk_analysis.R`)
**Purpose:** Main entry point that combines Monte Carlo + RQuantLib + existing metrics

**Key function:**
```r
analyze_position_risk(
  ticker, strike, expiration, premium_received,
  simulation_paths = 10000,
  use_rquantlib = TRUE,
  use_monte_carlo = TRUE
)
```

**Returns:** Complete risk profile with:
- Early exercise probability (from RQuantLib)
- Monte Carlo distribution (from fct_monte_carlo.R)
- Risk-adjusted return
- Stress test results
- Greeks

**Reuses:**
- `fetch_price_history()`, `fetch_dividend_history()` from utils_market_data.R
- `calculate_dividend_projections()` from fct_aristocrats_analysis.R
- `build_dividend_schedule()` from fct_monte_carlo.R (already created)

#### 2. Portfolio Risk Aggregator (`R/fct_portfolio_risk.R`)
**Purpose:** Portfolio-level metrics with correlation

**Key functions:**
- `analyze_portfolio_risk()` - Main orchestrator
- `calculate_correlation_matrix()` - From historical returns
- `run_correlated_monte_carlo()` - Correlated price paths for all positions
- `calculate_portfolio_var()` - Using PerformanceAnalytics
- `calculate_portfolio_cvar()` - Expected shortfall
- `apply_stress_scenario()` - Apply pre-built or custom scenarios

**Reuses:**
- Portfolio groups from database
- Parallel processing pattern from all strategies
- PerformanceAnalytics::VaR(), PerformanceAnalytics::ES()

#### 3. UI Module: Position Risk Analysis (`R/mod_position_risk.R`)
**Purpose:** Shiny module for "Analyze Risk" button functionality

**Structure:**
```r
mod_position_risk_ui(id)  # Modal dialog UI
mod_position_risk_server(id, ticker, strike, expiration, ...)
```

**Features:**
- Triggered by button click
- Shows loading spinner
- Async computation using promises + future
- Displays results in modal with tabs:
  - Tab 1: Summary metrics
  - Tab 2: Distribution chart
  - Tab 3: Dividend timeline
  - Tab 4: Scenario analysis

**Reuses:**
- Async pattern from mod_dynamic_covered_calls_analysis.R
- Modal patterns from existing modules
- Chart components (will need plotly or ggplot2)

#### 4. UI Module: Portfolio Dashboard (`R/mod_portfolio_risk_dashboard.R`)
**Purpose:** Complete portfolio risk dashboard

**Components:**
- Summary cards (value boxes)
- Risk metrics display
- Position breakdown table
- Correlation heatmap
- Stress test results
- Custom scenario builder form

**Reuses:**
- Card layouts from utils_ui_components.R
- Table patterns from mod_aristocrats_results_table.R

#### 5. Page: Portfolio Risk (`R/page_portfolio_risk.R`)
**Purpose:** New page for portfolio-level risk analysis

**Structure:** Similar to page_aristocrats.R but for portfolio view

#### 6. Database Schema (`R/fct_risk_database.R` - NEW)
**Purpose:** Create and manage risk analysis tables

**Tables needed:**
```sql
CREATE TABLE position_risk_analysis (
  analysis_id INTEGER PRIMARY KEY,
  ticker TEXT,
  strike REAL,
  expiration TEXT,
  analysis_timestamp TEXT,
  simulation_paths INTEGER,
  early_exercise_prob REAL,
  risk_adjusted_return REAL,
  expected_return REAL,
  var_95 REAL,
  cvar_95 REAL,
  delta REAL,
  gamma REAL,
  theta REAL,
  vega REAL,
  stress_test_results TEXT  -- JSON
);

CREATE TABLE portfolio_risk_snapshots (
  snapshot_id INTEGER PRIMARY KEY,
  snapshot_date TEXT,
  total_positions INTEGER,
  portfolio_var_95 REAL,
  portfolio_cvar_95 REAL,
  risk_score REAL,
  correlation_matrix TEXT,  -- JSON
  stress_test_summary TEXT  -- JSON
);

CREATE TABLE custom_scenarios (
  scenario_id INTEGER PRIMARY KEY,
  scenario_name TEXT,
  created_date TEXT,
  parameters TEXT  -- JSON with all scenario settings
);
```

**Reuses:**
- Database connection pattern from fct_portfolio_groups_database.R
- DuckDB patterns from existing code

#### 7. UI Integration - Add Risk Buttons to Strategy Results

**Files to modify:**
- `R/mod_aristocrats_results_table.R` - Add "Analyze Risk" button to each card
- `R/mod_zero_dividend_results_table.R` - Same
- `R/mod_collar_results.R` - Same
- `R/mod_dynamic_covered_calls_analysis.R` - Same

**Pattern:** Add button to card header or Quick Overview section:
```r
actionButton(ns("analyze_risk_", row$ticker), "Analyze Risk", class = "btn-sm btn-info")
```

**Server logic:** Call mod_position_risk_server() when clicked

#### 8. UI Integration - Portfolio Groups Risk
**File to modify:** `R/mod_portfolio_groups_cards.R`

**Add:**
- Traffic light indicators (green/yellow/red) based on adaptive thresholds
- "Analyze Risk" button per position
- Risk summary in card (early exercise %, days since analysis)

#### 9. App Routing
**File to modify:** `R/run_app.R`

**Add:** Portfolio Risk page to brochure routing

#### 10. Risk UI Components (`R/utils_risk_ui_components.R`)
**Purpose:** Reusable UI helpers for risk displays

**Functions needed:**
- `create_risk_summary_card()`
- `create_distribution_chart()`
- `create_dividend_timeline()`
- `create_stress_test_table()`
- `create_correlation_heatmap()`

## 📊 IMPLEMENTATION ESTIMATE

**What's Done:** ~30% of code (core engines complete)

**Remaining:**
- fct_risk_analysis.R: ~300 lines
- fct_portfolio_risk.R: ~400 lines
- fct_risk_database.R: ~200 lines
- mod_position_risk.R: ~300 lines
- mod_portfolio_risk_dashboard.R: ~500 lines
- page_portfolio_risk.R: ~100 lines
- utils_risk_ui_components.R: ~300 lines
- Modifications to 5 existing files: ~200 lines total

**Total remaining:** ~2,300 lines

## 🎯 RECOMMENDED NEXT STEPS

Given token limits and implementation complexity, I recommend:

### Option A: Complete MVP in Phases
**Phase 1 (Next):**
- Create fct_risk_analysis.R (orchestrator)
- Create mod_position_risk.R (UI module)
- Modify mod_aristocrats_results_table.R only (test with one strategy)
- Test end-to-end: Click "Analyze Risk" on aristocrats → See results

**Phase 2:**
- Extend to all strategy modules
- Add portfolio groups integration

**Phase 3:**
- Build portfolio dashboard
- Add stress testing
- Create database tables

### Option B: I Continue Full Implementation
I can continue implementing all remaining files, but will need multiple interactions due to token limits. Each response would complete 1-2 files.

### Option C: Provide Implementation Guide
I provide detailed pseudo-code and specifications for each remaining file, you review, then I implement based on your feedback.

## 🔧 TESTING CHECKLIST (Once Complete)

- [ ] Install libraries: `Rscript -e "install.packages(c('RQuantLib', 'LSMRealOptions', 'PerformanceAnalytics', 'MASS'))"`
- [ ] Test Monte Carlo simulation standalone
- [ ] Test RQuantLib integration with known option prices
- [ ] Test aristocrats risk analysis button
- [ ] Test full workflow: strategy → risk analysis → view results
- [ ] Test portfolio dashboard
- [ ] Test stress scenarios
- [ ] Test database persistence
- [ ] Performance test with 50K paths

## 📝 NOTES

**Libraries Used:**
- RQuantLib: American option pricing with discrete dividends
- LSMRealOptions: (Created monte carlo engine from scratch, can integrate this later for validation)
- PerformanceAnalytics: VaR, CVaR, risk metrics
- MASS: Multivariate normal for correlated simulations

**Key Design Decisions:**
- No caching (per user request)
- Async UI (promises + future) for responsiveness
- Adaptive alert thresholds (calculated from portfolio norms)
- Reuses existing dividend projection logic
- Card-based UI consistent with existing patterns
- Jump diffusion as default model (realistic tails)
- Heston for enhanced analysis (volatility modeling)

**DRY Principles Applied:**
- Reused dividend projection from fct_aristocrats_analysis.R
- Reused parallel processing from all strategies
- Reused card builders from utils_ui_components.R
- Reused database patterns from portfolio_groups
- Reused market data fetching from utils_market_data.R
