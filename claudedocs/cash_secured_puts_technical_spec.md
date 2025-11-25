# Cash-Secured Puts Strategy - Technical Specification

## Executive Summary

This document specifies the implementation of a Cash-Secured Puts strategy feature for the investR Shiny application. The strategy will mirror the existing covered calls architecture, targeting dividend aristocrats and providing multiple "flavors" similar to the zero-dividend covered calls implementation.

**Strategy Overview:**
- **Underlying Universe**: Dividend Aristocrats (stocks with 25+ years of dividend increases)
- **Option Type**: Cash-secured puts (sell put, hold cash collateral)
- **Strike Selection**: X% **below** current price (inverse of covered calls)
- **Risk Profile**: Obligated to buy stock at strike if assigned
- **Capital Requirement**: Strike price × 100 shares (cash collateral)
- **Return Calculation**: Premium / Cash Required

---

## 1. Architecture Analysis

### 1.1 Existing Covered Calls Pattern

The codebase follows a **generic strategy framework** with clear separation of concerns:

```
┌─────────────────────────────────────────────────────┐
│  Strategy-Specific Layer                            │
│  - Stock universe selection (fct_*_analysis.R)      │
│  - Configuration (utils_*_config.R)                 │
│  - UI parameters (mod_*_analysis.R)                 │
└─────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────┐
│  Generic Analysis Layer                             │
│  - analyze_covered_calls_generic()                  │
│  - analyze_single_stock_generic()                   │
│  - Metric calculation (utils_covered_calls_shared)  │
└─────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────┐
│  Data Layer                                         │
│  - get_stock_data() - price, dividends, history     │
│  - get_options_chain() - options from Questrade/YF  │
│  - select_optimal_option() - filtering logic        │
└─────────────────────────────────────────────────────┘
```

### 1.2 Key Reusable Components

**Fully Reusable:**
- `get_dividend_aristocrats()` - fetches aristocrats list
- `get_stock_data()` - retrieves stock price, dividends, history
- `get_options_chain()` - fetches options (needs `puts` parameter)
- `setup_parallel_processing()` - parallel execution framework
- `finalize_results()` - result aggregation and sorting
- UI components from `utils_ui_components.R`

**Requires Adaptation:**
- `select_optimal_option()` - needs put-specific ITM/OTM logic
- `calculate_metrics()` - needs put P&L calculations
- Metric calculation functions - premium, protection, returns for puts

**Strategy-Specific (New):**
- Put-specific analysis orchestrator
- Put configuration constants
- Put UI module

---

## 2. Data Architecture

### 2.1 Options Chain Modifications

Current `get_options_chain()` fetches **calls only**. Required changes:

```r
# Current signature (from fct_aristocrats_analysis.R:408)
get_options_chain <- function(ticker, current_price)

# Proposed signature
get_options_chain <- function(ticker, current_price, option_type = "calls")

# Behavior:
# - option_type = "calls" → return opt_chain[[exp_date]]$calls
# - option_type = "puts"  → return opt_chain[[exp_date]]$puts
```

**Impact:** Low-risk enhancement. Existing calls functionality unchanged.

### 2.2 Strike Selection Logic

**Covered Calls** (ITM when strike < current_price):
```r
strike_threshold <- current_price * strike_threshold_pct  # e.g., 0.85
filtered_options <- options_df %>% filter(Strike <= strike_threshold)
```

**Cash-Secured Puts** (ITM when strike > current_price):
```r
strike_threshold <- current_price * strike_threshold_pct  # e.g., 0.95
filtered_options <- options_df %>% filter(Strike >= strike_threshold)
```

**Note:** For puts, higher percentage = more conservative (closer to ATM)

### 2.3 Metric Calculations

#### Cash Flow Components (Puts)

```r
# For Cash-Secured Put
cash_required     = strike_price × 100           # Collateral needed
premium_received  = bid_price × 100             # Income from selling put
net_outlay        = cash_required - premium_received
```

#### Profitability Scenarios

**Scenario 1: Not Assigned (stock > strike at expiry)**
```r
profit = premium_received
return = premium_received / cash_required
```

**Scenario 2: Assigned (stock <= strike at expiry)**
```r
stock_acquired_cost = strike_price × 100
effective_cost_basis = stock_acquired_cost - premium_received
# Effectively bought stock at (strike - premium)
```

#### Protection Metrics (Puts)

```r
# Downside protection (how far stock can fall before loss)
breakeven_price = strike_price - bid_price
downside_protection_pct = (current_price - breakeven_price) / current_price

# Example: AAPL @ $150, sell $140 put for $2
# Breakeven = $138 ($140 strike - $2 premium)
# Protection = 8% (stock can fall 8% to $138 before loss)
```

---

## 3. File Inventory

### 3.1 New Files to Create

#### Analysis Layer
```
R/fct_cash_secured_puts_analysis.R
├── analyze_cash_secured_puts()           # Main aristocrats analyzer
├── analyze_puts_custom_list()            # Custom universes (future)
└── analyze_puts_etfs()                   # ETF universe (future)
```

#### Configuration
```
R/utils_cash_secured_puts_config.R
├── CASH_SECURED_PUTS_CONFIG              # Strategy constants
├── validate_puts_config()                # Validation function
└── get_puts_config()                     # Accessor function
```

#### UI Module
```
R/mod_cash_secured_puts_analysis.R
├── mod_cash_secured_puts_analysis_ui()   # Sidebar parameters
└── mod_cash_secured_puts_analysis_server() # Analysis execution
```

#### Results Display
```
R/mod_cash_secured_puts_results_table.R
├── mod_cash_secured_puts_results_table_ui()
├── mod_cash_secured_puts_results_table_server()
└── create_put_opportunity_card()         # Card builder
```

#### Page Integration
```
R/page_cash_secured_puts.R
└── page_cash_secured_puts()              # Brochure page definition
```

### 3.2 Files to Modify

#### Core Functions (Minor)
```
R/utils_covered_calls_shared.R
├── get_options_chain()                   # Add option_type parameter
├── select_optimal_option()               # Add put ITM/OTM logic
└── analyze_single_stock_generic()        # Support put metrics
```

#### Metric Calculations (New Functions)
```
R/utils_covered_calls_shared.R (or new utils_put_metrics.R)
├── calculate_put_cash_flows()            # Put-specific cash flow
├── calculate_put_protection_metrics()    # Put downside protection
└── calculate_put_metrics()               # Orchestrator for puts
```

#### Home Page
```
R/page_home.R
└── strategies_data$cash_secured_puts     # Add new strategy section
```

#### App Routing
```
R/run_app.R
└── Add route for /cash-secured-puts
```

### 3.3 Test Files to Create

```
tests/testthat/test-fct_cash_secured_puts_analysis.R
tests/testthat/test-utils_cash_secured_puts_config.R
tests/testthat/test-put_metrics.R
```

---

## 4. Function Specifications

### 4.1 Main Analysis Function

```r
#' Analyze dividend aristocrats for cash-secured put opportunities
#'
#' Analyzes dividend aristocrats for ATM/OTM cash-secured put opportunities.
#' Targets high-quality dividend stocks with put options priced above fair value.
#'
#' @param limit Optional limit on number of stocks to analyze
#' @param strike_threshold_pct Minimum strike as % of current price (default 0.95)
#'        Higher % = more conservative (closer to ATM)
#'        Lower % = more aggressive (deeper OTM)
#' @param min_days Minimum days to expiry (default 45)
#' @param max_days Maximum days to expiry (default 120)
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers
#' @return Tibble with all opportunities sorted by annualized return
#' @export
analyze_cash_secured_puts <- function(
  limit = NULL,
  strike_threshold_pct = CASH_SECURED_PUTS_CONFIG$strike_threshold_pct,
  min_days = CASH_SECURED_PUTS_CONFIG$min_days,
  max_days = CASH_SECURED_PUTS_CONFIG$max_days,
  expiry_month = NULL,
  max_workers = CASH_SECURED_PUTS_CONFIG$max_workers
) {
  # Get dividend aristocrats (reuse existing function!)
  aristocrats <- get_dividend_aristocrats()

  # Apply limit if specified
  if (!is.null(limit)) {
    aristocrats <- head(aristocrats, limit)
    log_info("Limiting analysis to first {limit} stocks")
  }

  # Call generic analyzer with put-specific parameters
  analyze_puts_generic(
    stock_universe = aristocrats,
    strategy_name = "Cash-Secured Puts - Dividend Aristocrats",
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    max_workers = max_workers,
    result_flags = list(
      is_put = TRUE,
      is_aristocrat = TRUE
    )
  )
}
```

### 4.2 Generic Put Analyzer

```r
#' Generic cash-secured puts analysis orchestrator
#'
#' Orchestrates the complete cash-secured put analysis workflow for any stock universe.
#' Mirrors analyze_covered_calls_generic() but for puts.
#'
#' @param stock_universe Character vector of tickers to analyze
#' @param strategy_name Name for logging
#' @param strike_threshold_pct Minimum strike as % of current price
#' @param min_days Minimum days to expiry
#' @param max_days Maximum days to expiry
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers
#' @param result_flags Named list of additional flags to add to results
#' @return Tibble with all opportunities sorted by annualized return
#' @export
analyze_puts_generic <- function(
  stock_universe,
  strategy_name,
  strike_threshold_pct = 0.95,
  min_days = NULL,
  max_days = NULL,
  expiry_month = NULL,
  max_workers = 10,
  result_flags = list()
) {
  # Validate inputs
  if (length(stock_universe) == 0) {
    stop("stock_universe cannot be empty")
  }

  # Log analysis start
  log_analysis_header_generic(strategy_name)
  log_analysis_params_generic(
    n_stocks = length(stock_universe),
    strategy_name = strategy_name,
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    max_workers = max_workers
  )

  # Setup parallel processing
  oplan <- setup_parallel_processing(max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Process stocks in parallel (similar to calls)
  results <- process_stocks_parallel_put(
    stock_universe = stock_universe,
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    result_flags = result_flags
  )

  # Finalize and sort results (reuse existing!)
  results_df <- finalize_results(results)

  # Log completion
  log_analysis_footer(nrow(results_df))

  results_df
}
```

### 4.3 Put Metrics Calculator

```r
#' Calculate all cash-secured put metrics
#'
#' Comprehensive metric calculation for put options
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @param option_row Selected put option (tibble row)
#' @param stock_data Stock data list from get_stock_data()
#' @param warning_flag Warning flag from option selection
#' @return Tibble row with all calculated metrics
#' @noRd
calculate_put_metrics <- function(ticker, current_price, option_row,
                                  stock_data, warning_flag) {
  # Extract option data
  strike <- option_row$Strike
  bid_price <- option_row$Bid
  days_to_expiry <- option_row$days_to_expiry
  expiration <- option_row$expiration
  open_interest <- option_row$OI

  # Cash flow calculations for puts
  cash_required <- strike * CASH_SECURED_PUTS_CONFIG$shares_per_contract
  premium_received <- bid_price * CASH_SECURED_PUTS_CONFIG$shares_per_contract
  net_outlay <- cash_required - premium_received

  # Returns (assuming not assigned - premium only)
  total_return <- premium_received / cash_required
  annualized_return <- calculate_annualized_return(
    total_return = total_return,
    days = days_to_expiry,
    days_per_year = CASH_SECURED_PUTS_CONFIG$days_per_year
  )

  # Protection metrics
  breakeven_price <- strike - bid_price
  downside_protection_pct <- (current_price - breakeven_price) / current_price

  # Option value decomposition
  intrinsic_value <- max(0, strike - current_price)  # PUT intrinsic value
  extrinsic_value <- bid_price - intrinsic_value

  # Assemble result
  tibble(
    ticker = ticker,
    company_name = stock_data$company_name,
    current_price = current_price,
    strike = strike,
    expiration = as.character(expiration),
    days_to_expiry = days_to_expiry,
    bid_price = bid_price,
    open_interest = open_interest,
    # Cash flows
    cash_required = cash_required,
    premium_received = premium_received,
    net_outlay = net_outlay,
    # Returns
    total_return = total_return,
    annualized_return = annualized_return,
    # Risk metrics
    max_drawdown = stock_data$max_drawdown,
    current_yield = stock_data$current_yield,
    # Protection
    breakeven_price = breakeven_price,
    downside_protection_pct = downside_protection_pct,
    # Option values
    intrinsic_value = intrinsic_value,
    extrinsic_value = extrinsic_value,
    annual_dividend = stock_data$annual_dividend,
    # Flags
    warning_flag = warning_flag,
    is_put = TRUE,
    is_aristocrat = TRUE
  )
}
```

### 4.4 Put Option Selection

```r
#' Select optimal put option based on strategy parameters
#'
#' Filters put options for OTM/ATM opportunities with sufficient liquidity.
#' For puts: ITM when strike > current_price
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @param options_df Tibble of available put options
#' @param strike_threshold_pct Minimum strike as % of current price
#' @param min_days Minimum days to expiry
#' @param max_days Maximum days to expiry
#' @param expiry_month Optional month number (1-12) to filter
#' @return List with optimal option row and warning_flag
#' @noRd
select_optimal_put <- function(ticker, current_price, options_df,
                               strike_threshold_pct = 0.95,
                               min_days = NULL,
                               max_days = NULL,
                               expiry_month = NULL) {

  warning_flag <- FALSE

  # Calculate strike threshold (for puts: minimum strike)
  strike_threshold <- current_price * strike_threshold_pct
  log_info("{ticker}: Strike filter: >=${sprintf('%.2f', strike_threshold)} ({sprintf('%.0f%%', strike_threshold_pct*100)} of ${sprintf('%.2f', current_price)})")

  # Filter by strike (puts: want OTM/ATM, so strike >= threshold)
  filtered_options <- options_df %>%
    filter(Strike >= strike_threshold)
  log_info("{ticker}: After strike filter: {nrow(filtered_options)} options")

  # Filter by minimum open interest
  min_oi <- CASH_SECURED_PUTS_CONFIG$min_open_interest
  if (min_oi > 0) {
    filtered_options <- filtered_options %>%
      filter(OI >= min_oi)
    log_info("{ticker}: After OI filter (>={min_oi}): {nrow(filtered_options)} options")
  }

  # Filter by expiry month if specified
  if (!is.null(expiry_month)) {
    filtered_options <- filtered_options %>%
      filter(lubridate::month(expiration) == expiry_month)
    log_info("{ticker}: After month filter: {nrow(filtered_options)} options")
  }

  # Filter by days to expiry range
  if (!is.null(min_days)) {
    filtered_options <- filtered_options %>%
      filter(days_to_expiry >= min_days)
  }
  if (!is.null(max_days)) {
    filtered_options <- filtered_options %>%
      filter(days_to_expiry <= max_days)
  }

  # Fallback if no options meet criteria
  if (nrow(filtered_options) == 0) {
    return(NULL)
  }

  # Select optimal: longest dated for maximum time premium
  optimal_option <- filtered_options %>%
    arrange(desc(expiration), desc(OI)) %>%
    slice(1)

  list(
    option = optimal_option,
    warning_flag = warning_flag
  )
}
```

---

## 5. Configuration Specification

### 5.1 Cash-Secured Puts Configuration

```r
# R/utils_cash_secured_puts_config.R

CASH_SECURED_PUTS_CONFIG <- list(
  # Option Selection
  strike_threshold_pct = get_golem_config_value("cash_secured_puts", "strike_threshold_pct", 0.95),
  min_days = get_golem_config_value("cash_secured_puts", "min_days", 45),
  max_days = get_golem_config_value("cash_secured_puts", "max_days", 120),

  # Parallel Processing
  max_workers = get_golem_config_value("cash_secured_puts", "max_workers", 10),

  # Data Validation
  min_option_bid = get_golem_config_value("cash_secured_puts", "min_option_bid", 0.01),
  min_open_interest = get_golem_config_value("cash_secured_puts", "min_open_interest", 10),
  max_stock_price = get_golem_config_value("cash_secured_puts", "max_stock_price", 250),

  # Financial Constants
  shares_per_contract = get_golem_config_value("shared", "shares_per_contract", 100),
  days_per_year = get_golem_config_value("shared", "days_per_year", 365),

  # Date Ranges
  history_years = get_golem_config_value("cash_secured_puts", "history_years", 5),

  # Thresholds
  short_expiry_warning_days = get_golem_config_value("cash_secured_puts", "short_expiry_warning_days", 14),
  negative_return_threshold = get_golem_config_value("cash_secured_puts", "negative_return_threshold", 0),

  # Output
  default_top_n = get_golem_config_value("cash_secured_puts", "default_top_n", 10),
  output_dir = get_golem_config_value("cash_secured_puts", "output_dir", "strategies")
)
```

### 5.2 Golem Config YAML Additions

```yaml
# inst/golem-config.yml

cash_secured_puts:
  strike_threshold_pct: 0.95      # 95% of current price (OTM/ATM)
  min_days: 45                    # Minimum days to expiration
  max_days: 120                   # Maximum days to expiration
  max_workers: 10                 # Parallel processing workers
  min_option_bid: 0.01            # Filter out low-premium options
  min_open_interest: 10           # Liquidity filter
  max_stock_price: 250            # Skip very expensive stocks
  history_years: 5                # Historical data lookback
  short_expiry_warning_days: 14   # Warn on short expirations
  negative_return_threshold: 0    # Filter negative returns
  default_top_n: 10               # Default results display
  output_dir: strategies          # Output directory
```

---

## 6. UI Component Specifications

### 6.1 Module UI Structure

```r
# R/mod_cash_secured_puts_analysis.R

mod_cash_secured_puts_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h3("Strategy Parameters"),

      # Quote source toggle
      quote_source_toggle_ui(ns),
      hr(),

      # Strike threshold slider (50-100%, default 95%)
      sliderInput(
        ns("strike_threshold"),
        "Strike Threshold (% of current price)",
        min = 50,
        max = 100,
        value = 95,
        step = 5,
        post = "%"
      ),
      helpText("Higher % = More conservative (closer to ATM)"),

      # Days to expiry range
      sliderInput(
        ns("days_range"),
        "Days to Expiry Range",
        min = 30,
        max = 365,
        value = c(45, 120),
        step = 5
      ),

      # Parallel workers
      sliderInput(
        ns("max_workers"),
        "Parallel Workers",
        min = 1,
        max = 20,
        value = 4,
        step = 1
      ),

      # Run analysis button
      actionButton(
        ns("run_analysis"),
        "Run Analysis",
        class = "btn-primary btn-lg btn-block",
        icon = icon("chart-line")
      ),

      # Download button
      downloadButton(
        ns("download_results"),
        "Download CSV",
        class = "btn-success btn-block"
      ),

      # Home navigation
      tags$a(
        href = "/",
        class = "btn btn-default btn-block",
        icon("home"), " Home"
      )
    )
  )
}
```

### 6.2 Results Card Layout

```r
create_put_opportunity_card <- function(opportunity) {
  # Header: Company name and current price
  header <- create_generic_card_header(
    primary_text = sprintf("%s (%s)", opportunity$company_name, opportunity$ticker),
    secondary_text = sprintf("Current: %s | Strike: %s",
                            format_currency(opportunity$current_price),
                            format_currency(opportunity$strike))
  )

  # Quick Overview
  quick_overview <- create_accordion_section(
    title = "Quick Overview",
    is_open = TRUE,
    create_metric_row("Annualized Return",
                     format_percentage(opportunity$annualized_return),
                     is_primary = TRUE),
    create_metric_row("Cash Required",
                     format_currency(opportunity$cash_required)),
    create_metric_row("Premium Received",
                     format_currency(opportunity$premium_received)),
    create_metric_row("Days to Expiration",
                     format_number(opportunity$days_to_expiry))
  )

  # Risk Analysis
  risk_section <- create_accordion_section(
    title = "Risk Analysis",
    is_open = FALSE,
    create_metric_row("Breakeven Price",
                     format_currency(opportunity$breakeven_price)),
    create_metric_row("Downside Protection",
                     format_percentage(opportunity$downside_protection_pct)),
    create_metric_row("Max Drawdown (5yr)",
                     format_percentage(opportunity$max_drawdown)),
    create_metric_row("Current Dividend Yield",
                     format_percentage(opportunity$current_yield))
  )

  # Option Details
  option_details <- create_accordion_section(
    title = "Option Details",
    is_open = FALSE,
    create_metric_row("Expiration Date",
                     format(opportunity$expiration, "%b %d, %Y")),
    create_metric_row("Open Interest",
                     format_number(opportunity$open_interest)),
    create_metric_row("Intrinsic Value",
                     format_currency(opportunity$intrinsic_value)),
    create_metric_row("Extrinsic Value",
                     format_currency(opportunity$extrinsic_value))
  )

  # Assemble card
  body <- bslib::card_body(
    quick_overview,
    risk_section,
    option_details
  )

  create_standard_card(header, body)
}
```

---

## 7. Data Flow Diagram

```
User Input (UI Module)
   │
   ├─ Strike threshold (95% default)
   ├─ Days range (45-120)
   ├─ Max workers (4)
   └─ Quote source (Questrade/Yahoo)
   │
   ↓
analyze_cash_secured_puts()
   │
   ├─ get_dividend_aristocrats()  → [Reused: Fetches aristocrats list]
   │
   ↓
analyze_puts_generic()
   │
   ├─ Log analysis header/params   → [Reused: Generic logging]
   ├─ Setup parallel workers       → [Reused: setup_parallel_processing()]
   │
   ↓
process_stocks_parallel_put()  [PARALLEL]
   │
   ├─ For each ticker:
   │   │
   │   ├─ get_stock_data(ticker)          → [Reused: Price, dividends, history]
   │   ├─ get_options_chain(ticker, "puts") → [Modified: Add puts support]
   │   ├─ select_optimal_put()            → [New: Put filtering logic]
   │   └─ calculate_put_metrics()         → [New: Put P&L calculations]
   │
   ↓
finalize_results()  → [Reused: Compact, bind, sort by annualized return]
   │
   ↓
Results Table (UI)
   │
   └─ Display cards with put-specific metrics
```

---

## 8. Implementation Sequence

### Phase 1: Core Infrastructure (2-3 hours)
1. ✅ Create `R/utils_cash_secured_puts_config.R`
2. ✅ Add config to `inst/golem-config.yml`
3. ✅ Modify `get_options_chain()` to support puts
4. ✅ Write tests for config and option fetching

### Phase 2: Metric Calculations (3-4 hours)
5. ✅ Create `calculate_put_cash_flows()`
6. ✅ Create `calculate_put_protection_metrics()`
7. ✅ Create `calculate_put_metrics()` orchestrator
8. ✅ Create `select_optimal_put()` with put ITM logic
9. ✅ Write comprehensive tests for all metric functions

### Phase 3: Analysis Engine (2-3 hours)
10. ✅ Create `R/fct_cash_secured_puts_analysis.R`
11. ✅ Implement `analyze_cash_secured_puts()`
12. ✅ Implement `analyze_puts_generic()`
13. ✅ Implement `process_stocks_parallel_put()`
14. ✅ Write tests for analysis functions

### Phase 4: UI Components (3-4 hours)
15. ✅ Create `R/mod_cash_secured_puts_analysis.R` (UI + server)
16. ✅ Create `R/mod_cash_secured_puts_results_table.R`
17. ✅ Create `create_put_opportunity_card()`
18. ✅ Test UI module independently

### Phase 5: Page Integration (2 hours)
19. ✅ Create `R/page_cash_secured_puts.R`
20. ✅ Add route in `R/run_app.R`
21. ✅ Update `R/page_home.R` with new strategy card
22. ✅ Update NAMESPACE and documentation

### Phase 6: Testing & Validation (2-3 hours)
23. ✅ End-to-end testing with real data
24. ✅ Verify strike selection logic (puts vs calls)
25. ✅ Validate metric calculations
26. ✅ Test parallel processing
27. ✅ Cross-browser UI testing

**Total Estimated Time:** 14-19 hours

---

## 9. Risk Assessment

### 9.1 Technical Risks

| Risk | Severity | Mitigation |
|------|----------|------------|
| **Breaking existing calls functionality** | High | Thorough testing of `get_options_chain()` modifications; use optional parameters |
| **Incorrect put ITM/OTM logic** | High | Unit tests with known-good examples; validate strike filtering |
| **Metric calculation errors** | Medium | Test against manual calculations; peer review formulas |
| **Performance degradation** | Low | Reuse existing parallel framework; no new data sources |

### 9.2 Business Risks

| Risk | Severity | Mitigation |
|------|----------|------------|
| **User confusion (calls vs puts)** | Medium | Clear UI labels; help text explaining strategy differences |
| **Aristocrats data staleness** | Low | Reuse existing 30-day cache; aristocrats change infrequently |
| **Options data quality** | Medium | Inherit existing validation from calls strategy |

---

## 10. Dependencies

### 10.1 External Dependencies
- **Questrade API**: Primary options data source (already integrated)
- **Yahoo Finance**: Fallback options data (already integrated)
- **Dividend Aristocrats List**: StockAnalysis.com / Wikipedia (already cached)

### 10.2 Internal Dependencies
- `utils_covered_calls_shared.R` → Core option chain fetching
- `utils_aristocrats_config.R` → Aristocrats list fetching
- `utils_ui_components.R` → Card and accordion UI components
- `utils_formatting.R` → Currency/percentage formatting
- `utils_analysis_controls.R` → Analysis execution framework

---

## 11. Success Criteria

### 11.1 Functional Requirements
- ✅ Fetch dividend aristocrats successfully
- ✅ Retrieve put options chain for each stock
- ✅ Filter puts by strike threshold (≥ X% of current price)
- ✅ Calculate cash required, premium, and annualized return correctly
- ✅ Display results in sortable table with filterable cards
- ✅ Support CSV download of results

### 11.2 Performance Requirements
- ✅ Analysis completes within 5 minutes for ~65 aristocrats
- ✅ Parallel processing scales linearly with workers (1-10)
- ✅ UI remains responsive during analysis

### 11.3 Quality Requirements
- ✅ 90%+ test coverage for new functions
- ✅ No regressions in existing covered calls functionality
- ✅ Clear documentation in all function headers
- ✅ Consistent code style (tidyverse, Golem patterns)

---

## 12. Future Enhancements

### Phase 2 Flavors (Post-MVP)
1. **Zero-Dividend Puts**: Cash-secured puts on growth stocks
2. **Liquid ETF Puts**: Puts on high-volume ETFs
3. **Dynamic Strike Selection**: Historical volatility-based strikes
4. **Screener-Based Puts**: Custom ETF/stock screener integration

### Advanced Features
1. **Put/Call Comparison**: Side-by-side strategy comparison
2. **Collar Integration**: Combine puts with position management
3. **Risk Analysis**: Portfolio-level put exposure tracking
4. **Backtesting**: Historical put performance simulation

---

## 13. Glossary

**Cash-Secured Put**: Selling a put option while holding sufficient cash to purchase the underlying stock if assigned.

**Strike Threshold (Puts)**: Minimum strike price as percentage of current price. Higher % = more conservative (closer to ATM).

**ITM (In-The-Money) Put**: Strike price > current stock price (intrinsic value exists).

**OTM (Out-of-The-Money) Put**: Strike price < current stock price (only extrinsic/time value).

**Cash Required**: Strike price × 100 shares (collateral needed to secure the put).

**Downside Protection**: How far the stock can fall before the position loses money (strike - premium).

**Annualized Return**: (Premium / Cash Required) × (365 / Days to Expiry).

---

## 14. Appendix: Code Reuse Matrix

| Component | Source File | Reusability | Modification Needed |
|-----------|-------------|-------------|---------------------|
| `get_dividend_aristocrats()` | fct_aristocrats_analysis.R | 100% | None |
| `get_stock_data()` | fct_aristocrats_analysis.R | 100% | None |
| `get_options_chain()` | fct_aristocrats_analysis.R | 90% | Add `option_type` param |
| `calculate_dividend_projections()` | fct_aristocrats_analysis.R | 100% | None (puts benefit from dividends) |
| `setup_parallel_processing()` | utils_covered_calls_shared.R | 100% | None |
| `finalize_results()` | fct_aristocrats_analysis.R | 100% | None |
| `select_optimal_option()` | fct_aristocrats_analysis.R | 60% | Invert ITM/OTM logic |
| `calculate_metrics()` | fct_aristocrats_analysis.R | 40% | Different P&L for puts |
| UI components | utils_ui_components.R | 100% | None |
| Analysis controls | utils_analysis_controls.R | 100% | None |

**Estimated Code Reuse**: ~75-80%
**New Code Required**: ~20-25%

---

## 15. Validation Checklist

Before marking implementation complete:

- [ ] All new files created and added to git
- [ ] NAMESPACE updated with new exports
- [ ] All functions documented with roxygen2
- [ ] Tests pass: `devtools::test()`
- [ ] No regressions: Covered calls still work
- [ ] Strike filtering validated: Puts select strikes ≥ threshold
- [ ] Metric calculations verified: Manual spot-check 3-5 examples
- [ ] UI displays correctly: Test on Chrome, Firefox, Safari
- [ ] CSV download works: Verify column headers and data
- [ ] Home page updated: New strategy card appears
- [ ] Route works: `/cash-secured-puts` loads successfully
- [ ] Parallel processing works: Test with 1, 4, 10 workers
- [ ] Code style consistent: `styler::style_pkg()`
- [ ] Documentation built: `devtools::document()`

---

**Document Version**: 1.0
**Created**: 2025-11-23
**Author**: Claude (Requirements Analysis)
**Status**: Ready for Implementation Review
