# R Options Pricing & Greeks Calculation Packages Research

**Research Date:** 2025-11-23
**Purpose:** Identify R packages for cash-secured put risk analysis, options pricing, and Greeks calculation

---

## Executive Summary

Multiple mature R packages exist for options pricing and Greeks calculation, with strong put option support. The top recommendations for production use in a Shiny application are:

1. **`greeks`** - Most comprehensive, actively maintained (latest: 2025-11-17), includes Shiny app
2. **`derivmkts`** - Educational focus, excellent documentation, stable
3. **`optionstrat`** - Simple API, strategy visualization, European options focus
4. **`RQuantLib`** - Most powerful, requires QuantLib C++ installation
5. **`fOptions`** - Part of Rmetrics suite, legacy but comprehensive

---

## Package Comparison Matrix

| Package | Put Pricing | Greeks | American Options | Active Maintenance | Shiny Integration | Dependencies |
|---------|-------------|--------|------------------|-------------------|-------------------|--------------|
| **greeks** | ✅ | ✅ Full | ✅ Binomial | ✅ Active (2025) | ✅ Built-in | Minimal |
| **derivmkts** | ✅ | ✅ Full | ✅ Binomial | ✅ Stable (2022) | 🟡 Manual | Minimal |
| **optionstrat** | ✅ | ✅ Full | ❌ European only | 🟡 Stable (2019) | 🟡 Manual | Minimal |
| **RQuantLib** | ✅ | ✅ Full | ✅ Multiple methods | ✅ Active | 🟡 Manual | ⚠️ QuantLib C++ |
| **fOptions** | ✅ | ✅ Full | ✅ Multiple | 🟡 Legacy | 🟡 Manual | Heavy (Rmetrics) |

---

## Detailed Package Analysis

### 1. greeks Package ⭐ RECOMMENDED

**CRAN URL:** https://cran.r-project.org/package=greeks
**Latest Version:** 1.5.1 (2025-11-17)
**Maintainer:** Anselm Hudde
**Status:** Actively maintained

#### Capabilities

**Put Option Support:**
- European puts (Black-Scholes)
- American puts (Binomial Tree Method)
- Arithmetic Asian puts
- Geometric Asian puts
- Digital/Binary puts
- Custom payoff functions

**Greeks Calculation:**
- **First-order:** Delta, Vega, Theta, Rho
- **Second-order:** Gamma
- **Additional:** Psi (dividend sensitivity)
- All Greeks available for put options

**Special Features:**
- Implied volatility calculation for puts
- Malliavin Monte Carlo Greeks for Asian options
- Jump diffusion model support
- **Built-in Shiny app** for interactive visualization
- Vectorized operations

#### Code Examples

```r
library(greeks)

# European put option pricing and Greeks
put_analysis <- Greeks(
  initial_price = 100,
  exercise_price = 105,
  r = 0.01,
  time_to_maturity = 0.5,
  volatility = 0.25,
  payoff = "put",
  option_type = "European"
)
# Returns: fair_value, delta, vega, theta, rho, gamma

# American put option (for early exercise scenarios)
american_put <- Greeks(
  initial_price = 100,
  exercise_price = 105,
  r = 0.01,
  time_to_maturity = 1,
  volatility = 0.30,
  payoff = "put",
  option_type = "American"
)

# Cash-secured put risk metrics
put_greeks <- Greeks(
  initial_price = 530.32,
  exercise_price = 550,
  r = 0.0007,
  time_to_maturity = 283/365,
  volatility = 0.2442,
  payoff = "put",
  option_type = "American"
)
```

#### Integration with Shiny

The package includes a built-in Shiny app:

```r
library(greeks)
# Launch interactive Shiny app for Greeks visualization
run_shiny()
```

#### Dependencies

Minimal and modern:
- `Rcpp` (C++ integration for performance)
- `ggplot2`, `plotly` (visualization)
- `shiny` (interactive app)
- `tibble`, `tidyr` (tidyverse compatibility)
- `magrittr` (pipe operators)

#### Advantages for investR

✅ Most comprehensive Greeks coverage
✅ Active maintenance (latest update Nov 2025)
✅ Built-in Shiny app provides UI patterns
✅ Tidyverse-compatible (aligns with investR conventions)
✅ American options support (critical for early assignment risk)
✅ Custom payoff functions (flexibility for variants)
✅ Excellent documentation with vignettes

#### Performance

- C++ backend via Rcpp for computational efficiency
- Vectorized operations for batch calculations
- Suitable for real-time Shiny dashboards

---

### 2. derivmkts Package

**CRAN URL:** https://cran.r-project.org/package=derivmkts
**Latest Version:** 0.2.5 (2022-04-11)
**Maintainer:** Robert McDonald (Northwestern Kellogg)
**Status:** Stable, educational focus

#### Capabilities

**Put Option Support:**
- European puts: `bsput()`
- American puts: `binomopt(putopt=TRUE, american=TRUE)`
- Perpetual American puts: `putperpetual()`
- Barrier puts
- Binary/Digital puts

**Greeks Calculation:**
- Comprehensive Greeks via `greeks()` wrapper function
- Alternative `greeks2()` for named parameters
- Full vectorization support
- Greeks for vanilla and barrier options

#### Code Examples

```r
library(derivmkts)

# European put pricing
s <- 100; k <- 105; v <- 0.30; r <- 0.08; tt <- 2; d <- 0.04

# Basic put price
put_price <- bsput(s, k, v, r, tt, d)

# Put with all Greeks using wrapper
put_analysis <- greeks(bsput(s, k, v, r, tt, d))
# Returns price, delta, gamma, vega, theta, rho

# American put via binomial tree
american_put <- binomopt(
  s = 100,
  k = 105,
  v = 0.30,
  r = 0.08,
  tt = 1,
  d = 0.04,
  nstep = 100,
  american = TRUE,
  putopt = TRUE
)

# Vectorized Greeks for multiple scenarios
strikes <- c(95, 100, 105, 110)
put_greeks <- greeks(bsput(s, strikes, v, r, tt, d))

# Binomial visualization (useful for teaching/documentation)
binomplot(s, k, v, r, tt, d, nstep=6, american=TRUE, putopt=TRUE)
```

#### Special Features

- `bsopt()`: Produces prices and Greeks for both calls and puts simultaneously
- Binomial tree visualization with `binomplot()`
- Educational focus with excellent vignettes
- Accompanies "Derivatives Markets" textbook

#### Dependencies

Minimal:
- `mnormt` (multivariate normal)
- Base R graphics

#### Advantages for investR

✅ Clean, simple API
✅ Excellent documentation and vignettes
✅ Educational focus = clear examples
✅ Binomial trees for American options
✅ Minimal dependencies
✅ Stable codebase

#### Considerations

🟡 Last update 2022 (stable but not actively developed)
🟡 No built-in Shiny components (manual integration needed)

---

### 3. optionstrat Package

**CRAN URL:** https://cran.r-project.org/package=optionstrat
**Latest Version:** 1.4.1 (2019-12-03)
**Maintainer:** John T. Buynak
**Status:** Stable, mature

#### Capabilities

**Put Option Support:**
- European puts only (Black-Scholes-Merton)
- Put premium: `putpremium()`
- Put Greeks: Individual functions or combined `putgreek()`

**Greeks Calculation:**

Individual functions:
- `putdelta(s, x, sigma, t, r, d = 0)`
- `putgamma(s, x, sigma, t, r, d = 0)`
- `puttheta(s, x, sigma, t, r, d = 0)`
- `putrho(s, x, sigma, t, r, d = 0)`

Combined function:
- `putgreek(greek = c("delta", "gamma", "theta", "vega", "rho", "premium"), ...)`

**Strategy Visualization:**
- Bear put spread: `plotbearput()`
- Multiple put strategies with P&L diagrams

#### Code Examples

```r
library(optionstrat)

# Individual Greeks
s <- 100; x <- 105; sigma <- 0.25; t <- 0.5; r <- 0.01; d <- 0

delta <- putdelta(s, x, sigma, t, r, d)
gamma <- putgamma(s, x, sigma, t, r, d)
theta <- puttheta(s, x, sigma, t, r, d)
vega <- optionvega(s, x, sigma, t, r, d)  # Same for calls and puts
rho <- putrho(s, x, sigma, t, r, d)

# Combined Greeks calculation
all_greeks <- putgreek(
  greek = c("delta", "gamma", "theta", "vega", "rho", "premium"),
  s = 100,
  x = 105,
  sigma = 0.25,
  t = 0.5,
  r = 0.01,
  d = 0
)

# Cash-secured put visualization
plotbearput(s = 100, x1 = 95, x2 = 105, sigma = 0.25, t = 0.5, r = 0.01)

# Implied volatility calculation
iv.calc(
  type = "put",
  price = 5.50,
  s = 100,
  x = 105,
  t = 0.5,
  r = 0.01,
  d = 0
)
```

#### Strategy Analysis Features

```r
# Bear Put Spread (relevant for risk management)
plotbearput(s, x1, x2, sigma, t, r, d)

# Custom strategy plotting with P&L visualization
# Useful for cash-secured put scenario analysis
```

#### Dependencies

Minimal:
- Base R `graphics` and `stats`

#### Advantages for investR

✅ Simple, intuitive API
✅ Strategy visualization built-in
✅ Minimal dependencies
✅ Good for European-style options
✅ Implied volatility calculation

#### Limitations

❌ European options only (no early exercise)
🟡 Last update 2019 (stable but aging)
🟡 No built-in Shiny components

---

### 4. RQuantLib Package

**CRAN URL:** https://cran.r-project.org/package=RQuantLib
**Status:** Actively maintained
**Backend:** QuantLib C++ library

#### Capabilities

**Put Option Support:**
- European puts
- American puts (multiple methods)
- Barrier puts
- Asian puts
- All QuantLib option types

**Greeks Calculation:**
- Delta, Gamma, Vega, Theta, Rho
- DivRho (dividend sensitivity)
- Multiple pricing engines available

**Pricing Methods:**
- Black-Scholes
- Binomial trees
- Barone-Adesi-Whaley approximation
- And many more from QuantLib

#### Code Examples

```r
library(RQuantLib)

# European put option with Greeks
european_put <- EuropeanOption(
  type = "put",
  underlying = 100,
  strike = 105,
  dividendYield = 0,
  riskFreeRate = 0.01,
  maturity = 0.5,
  volatility = 0.25
)
# Returns: value, delta, gamma, vega, theta, rho, divRho

# American put option
american_put <- AmericanOption(
  type = "put",
  underlying = 100,
  strike = 105,
  dividendYield = 0,
  riskFreeRate = 0.01,
  maturity = 1,
  volatility = 0.30,
  timeSteps = 150,
  gridPoints = 149,
  engine = "BaroneAdesiWhaley"
)

# Implied volatility
iv <- EuropeanOptionImpliedVolatility(
  type = "put",
  value = 7.50,
  underlying = 100,
  strike = 105,
  dividendYield = 0,
  riskFreeRate = 0.01,
  maturity = 0.5,
  volatility = 0.30  # initial guess
)
```

#### Dependencies

⚠️ **Critical:** Requires QuantLib C++ library installation
- Linux: `sudo apt-get install libquantlib0-dev`
- macOS: `brew install quantlib`
- Windows: More complex setup

#### Advantages for investR

✅ Most comprehensive option pricing library
✅ Multiple pricing methods (flexibility)
✅ Industry-standard QuantLib backend
✅ Active maintenance
✅ Extensive model coverage

#### Considerations

⚠️ Heavy dependency on QuantLib C++ (installation complexity)
⚠️ May be overkill for basic Black-Scholes needs
🟡 Steeper learning curve

---

### 5. fOptions Package (Rmetrics)

**Part of:** Rmetrics suite
**CRAN URL:** https://cran.r-project.org/package=fOptions
**Status:** Legacy, stable

#### Capabilities

**Put Option Support:**
- `GBSOption()`: Generalized Black-Scholes for puts
- `BAWAmericanApproxOption()`: Barone-Adesi-Whaley for American puts
- Multiple exotic put variants

**Greeks Calculation:**
- `GBSGreeks()`: All standard Greeks
- Individual Greek functions

#### Code Examples

```r
library(fOptions)

# European put pricing and Greeks
put_option <- GBSOption(
  TypeFlag = "p",
  S = 100,
  X = 105,
  Time = 0.5,
  r = 0.01,
  b = 0,  # cost-of-carry (b = r - dividend_yield)
  sigma = 0.25
)

# Individual Greek
delta <- GBSGreeks(
  Selection = "delta",
  TypeFlag = "p",
  S = 100,
  X = 105,
  Time = 0.5,
  r = 0.01,
  b = 0,
  sigma = 0.25
)

# American put (Barone-Adesi-Whaley)
american_put <- BAWAmericanApproxOption(
  TypeFlag = "p",
  S = 100,
  X = 105,
  Time = 1,
  r = 0.01,
  b = 0,
  sigma = 0.30
)
```

#### Dependencies

Heavy (entire Rmetrics ecosystem):
- Multiple Rmetrics packages
- timeDate, timeSeries, fBasics, etc.

#### Advantages for investR

✅ Comprehensive coverage
✅ Part of established financial suite
✅ American options support

#### Considerations

🟡 Legacy codebase
🟡 Heavy dependencies
🟡 Less intuitive API than modern packages
🟡 Cost-of-carry parameterization vs. direct dividend yield

---

## Integration Recommendations for investR

### Primary Recommendation: `greeks` Package

**Rationale:**
1. ✅ Most actively maintained (2025-11-17)
2. ✅ Comprehensive put option and Greeks support
3. ✅ Built-in Shiny app provides reference implementation
4. ✅ Tidyverse-compatible (aligns with investR's tidyverse usage)
5. ✅ American options support (critical for early assignment risk)
6. ✅ Minimal dependencies (production-friendly)
7. ✅ Good performance (C++ backend)

### Secondary Recommendation: `derivmkts` Package

**Use Case:** Supplementary analysis and validation

**Rationale:**
1. ✅ Excellent documentation and examples
2. ✅ Simple, clean API
3. ✅ Binomial tree methods
4. ✅ Educational focus = easy to understand
5. ✅ Minimal dependencies

### Integration Pattern

```r
# Primary: greeks package for all calculations
library(greeks)

# Cash-secured put risk analysis function
analyze_cash_secured_put <- function(
  stock_price,
  strike_price,
  volatility,
  time_to_maturity,
  risk_free_rate,
  dividend_yield = 0
) {

  # Calculate all Greeks using greeks package
  put_metrics <- Greeks(
    initial_price = stock_price,
    exercise_price = strike_price,
    r = risk_free_rate,
    time_to_maturity = time_to_maturity,
    volatility = volatility,
    dividend_yield = dividend_yield,
    payoff = "put",
    option_type = "American"  # For early assignment scenarios
  )

  # Return tibble for tidyverse compatibility
  tibble::tibble(
    stock_price = stock_price,
    strike_price = strike_price,
    put_value = put_metrics$fair_value,
    delta = put_metrics$delta,
    gamma = put_metrics$gamma,
    theta = put_metrics$theta,
    vega = put_metrics$vega,
    rho = put_metrics$rho,
    # Derived metrics for cash-secured puts
    max_loss = strike_price * 100,  # Per contract
    breakeven = strike_price - put_metrics$fair_value,
    return_if_unchanged = put_metrics$fair_value / strike_price,
    assignment_risk = pnorm(
      (log(stock_price / strike_price) +
       (risk_free_rate - dividend_yield - 0.5 * volatility^2) * time_to_maturity) /
      (volatility * sqrt(time_to_maturity))
    )
  )
}

# Vectorized analysis for multiple strikes
analyze_put_ladder <- function(
  stock_price,
  strike_prices,
  volatility,
  time_to_maturity,
  risk_free_rate,
  dividend_yield = 0
) {

  purrr::map_df(
    strike_prices,
    ~ analyze_cash_secured_put(
      stock_price = stock_price,
      strike_price = .x,
      volatility = volatility,
      time_to_maturity = time_to_maturity,
      risk_free_rate = risk_free_rate,
      dividend_yield = dividend_yield
    )
  )
}
```

### Shiny Dashboard Integration

The `greeks` package includes a reference Shiny app that can be studied:

```r
# Study the built-in app
library(greeks)
run_shiny()

# Key UI patterns to adapt:
# 1. Reactive inputs for option parameters
# 2. Greeks visualization with plotly
# 3. Sensitivity analysis plots
# 4. Real-time calculation updates
```

---

## Performance Considerations

### Computational Performance

**Fast (suitable for real-time Shiny dashboards):**
- `greeks` - C++ backend via Rcpp
- `derivmkts` - Efficient R implementation
- `optionstrat` - Simple calculations

**Moderate:**
- `RQuantLib` - Powerful but heavier
- `fOptions` - Comprehensive but older code

### Memory Footprint

**Light:**
- `optionstrat` - Minimal dependencies
- `derivmkts` - Minimal dependencies

**Moderate:**
- `greeks` - Reasonable with tidyverse deps

**Heavy:**
- `fOptions` - Entire Rmetrics suite
- `RQuantLib` - QuantLib C++ library

### Recommended for Production Shiny Apps

1. **`greeks`** - Best balance of features, performance, and maintainability
2. **`derivmkts`** - Good for supplementary calculations
3. **`optionstrat`** - Good for simple European options

---

## Testing and Validation Strategy

### Cross-Package Validation

```r
# Validate Greeks calculations across packages
validate_greeks <- function(
  stock_price = 100,
  strike_price = 105,
  volatility = 0.25,
  time_to_maturity = 0.5,
  risk_free_rate = 0.01,
  dividend_yield = 0
) {

  # greeks package
  greeks_result <- greeks::Greeks(
    initial_price = stock_price,
    exercise_price = strike_price,
    r = risk_free_rate,
    time_to_maturity = time_to_maturity,
    volatility = volatility,
    dividend_yield = dividend_yield,
    payoff = "put",
    option_type = "European"
  )

  # derivmkts package
  derivmkts_result <- derivmkts::greeks(
    derivmkts::bsput(
      s = stock_price,
      k = strike_price,
      v = volatility,
      r = risk_free_rate,
      tt = time_to_maturity,
      d = dividend_yield
    )
  )

  # optionstrat package
  optionstrat_delta <- optionstrat::putdelta(
    s = stock_price,
    x = strike_price,
    sigma = volatility,
    t = time_to_maturity,
    r = risk_free_rate,
    d = dividend_yield
  )

  # Compare results
  tibble::tibble(
    package = c("greeks", "derivmkts", "optionstrat"),
    delta = c(
      greeks_result$delta,
      derivmkts_result["Delta"],
      optionstrat_delta
    ),
    gamma = c(
      greeks_result$gamma,
      derivmkts_result["Gamma"],
      optionstrat::putgamma(stock_price, strike_price, volatility,
                            time_to_maturity, risk_free_rate, dividend_yield)
    )
  )
}
```

### Unit Testing with testthat

```r
# Test Greeks calculations
test_that("Put option Greeks are calculated correctly", {

  result <- analyze_cash_secured_put(
    stock_price = 100,
    strike_price = 105,
    volatility = 0.25,
    time_to_maturity = 0.5,
    risk_free_rate = 0.01
  )

  # Delta should be negative for puts
  expect_true(result$delta < 0)
  expect_true(result$delta > -1)

  # Gamma should be positive
  expect_true(result$gamma > 0)

  # Theta should be negative (time decay)
  expect_true(result$theta < 0)

  # Put value should be positive
  expect_true(result$put_value > 0)

  # Breakeven should be less than strike
  expect_true(result$breakeven < result$strike_price)
})
```

---

## Installation Instructions

### greeks Package (Recommended)

```r
# From CRAN
install.packages("greeks")

# Development version from GitHub
# install.packages("devtools")
devtools::install_github("ahudde/greeks")

# Load and test
library(greeks)
run_shiny()  # Test the built-in app
```

### derivmkts Package

```r
# From CRAN
install.packages("derivmkts")

# Load and test
library(derivmkts)
bsput(100, 105, 0.25, 0.01, 0.5, 0)
```

### optionstrat Package

```r
# From CRAN
install.packages("optionstrat")

# Load and test
library(optionstrat)
putdelta(100, 105, 0.25, 0.5, 0.01)
```

### RQuantLib Package (Advanced)

```r
# Install system dependencies first
# Ubuntu/Debian:
# sudo apt-get install libquantlib0-dev libboost-all-dev

# macOS:
# brew install quantlib boost

# Then install R package
install.packages("RQuantLib")
```

---

## Code Patterns for Cash-Secured Put Analysis

### Basic Risk Metrics

```r
library(greeks)
library(tidyverse)

calculate_csp_risk_metrics <- function(
  stock_price,
  strike_price,
  premium_received,
  volatility,
  days_to_expiration,
  risk_free_rate = 0.01
) {

  time_to_maturity <- days_to_expiration / 365

  # Get Greeks
  greeks_data <- Greeks(
    initial_price = stock_price,
    exercise_price = strike_price,
    r = risk_free_rate,
    time_to_maturity = time_to_maturity,
    volatility = volatility,
    payoff = "put",
    option_type = "American"
  )

  # Calculate risk metrics
  tibble(
    current_price = stock_price,
    strike = strike_price,
    dte = days_to_expiration,

    # Premium analysis
    premium_received = premium_received,
    theoretical_value = greeks_data$fair_value,
    premium_differential = premium_received - greeks_data$fair_value,

    # Return metrics
    max_profit = premium_received,
    max_profit_pct = premium_received / strike_price,

    # Risk metrics
    max_loss = strike_price - premium_received,
    breakeven = strike_price - premium_received,
    breakeven_pct = (stock_price - breakeven) / stock_price,

    # Greeks
    delta = greeks_data$delta,
    gamma = greeks_data$gamma,
    theta = greeks_data$theta,
    vega = greeks_data$vega,

    # Position-level Greeks (per contract = 100 shares)
    position_delta = greeks_data$delta * 100,
    position_theta = greeks_data$theta * 100,
    position_vega = greeks_data$vega * 100,

    # Assignment probability (approximation)
    prob_itm = pnorm(
      (log(stock_price / strike_price) +
       (risk_free_rate - 0.5 * volatility^2) * time_to_maturity) /
      (volatility * sqrt(time_to_maturity))
    ),

    # Risk score (custom metric)
    risk_score = case_when(
      breakeven_pct > 0.15 ~ "LOW",
      breakeven_pct > 0.05 ~ "MODERATE",
      TRUE ~ "HIGH"
    )
  )
}
```

### Stress Testing

```r
stress_test_csp <- function(
  base_price,
  strike_price,
  premium,
  volatility,
  dte,
  price_shocks = seq(-0.20, 0.20, by = 0.05),
  vol_shocks = seq(-0.50, 0.50, by = 0.25)
) {

  expand_grid(
    price_shock = price_shocks,
    vol_shock = vol_shocks
  ) %>%
    mutate(
      shocked_price = base_price * (1 + price_shock),
      shocked_vol = volatility * (1 + vol_shock)
    ) %>%
    rowwise() %>%
    mutate(
      metrics = list(calculate_csp_risk_metrics(
        stock_price = shocked_price,
        strike_price = strike_price,
        premium_received = premium,
        volatility = shocked_vol,
        days_to_expiration = dte
      ))
    ) %>%
    unnest(metrics)
}
```

### Greeks Sensitivity Visualization

```r
library(plotly)

plot_greeks_sensitivity <- function(
  strike_price = 105,
  volatility = 0.25,
  dte = 30,
  price_range = seq(80, 120, by = 1)
) {

  # Calculate Greeks across price range
  greeks_data <- map_df(price_range, function(price) {
    result <- Greeks(
      initial_price = price,
      exercise_price = strike_price,
      r = 0.01,
      time_to_maturity = dte / 365,
      volatility = volatility,
      payoff = "put",
      option_type = "American"
    )

    tibble(
      stock_price = price,
      put_value = result$fair_value,
      delta = result$delta,
      gamma = result$gamma,
      theta = result$theta,
      vega = result$vega
    )
  })

  # Create interactive plot
  plot_ly(greeks_data) %>%
    add_trace(
      x = ~stock_price,
      y = ~delta,
      name = "Delta",
      type = "scatter",
      mode = "lines"
    ) %>%
    add_trace(
      x = ~stock_price,
      y = ~gamma * 10,  # Scale for visibility
      name = "Gamma (×10)",
      type = "scatter",
      mode = "lines"
    ) %>%
    add_trace(
      x = ~stock_price,
      y = ~theta,
      name = "Theta",
      type = "scatter",
      mode = "lines"
    ) %>%
    layout(
      title = "Put Option Greeks Sensitivity",
      xaxis = list(title = "Stock Price"),
      yaxis = list(title = "Greek Value"),
      hovermode = "x unified"
    )
}
```

---

## Common Pitfalls and Solutions

### 1. Time Convention Differences

**Problem:** Different packages use different time conventions
- `greeks`: Time in years (1.0 = 1 year)
- `optionstrat`: Time in years
- Some financial APIs: Days to expiration

**Solution:**
```r
# Always convert to years
days_to_years <- function(days) days / 365
years_to_days <- function(years) years * 365

# Use consistently
time_to_maturity <- days_to_years(dte)
```

### 2. Dividend Yield vs Cost-of-Carry

**Problem:** Different parameterizations
- `greeks`, `derivmkts`: Separate dividend_yield parameter
- `fOptions`: Cost-of-carry parameter (b = r - q)

**Solution:**
```r
# Convert between conventions
cost_of_carry <- risk_free_rate - dividend_yield
dividend_yield <- risk_free_rate - cost_of_carry
```

### 3. American vs European Options

**Problem:** Cash-secured puts can be assigned early (American-style)

**Solution:**
```r
# Always use American option type for realistic risk assessment
Greeks(
  ...,
  option_type = "American"  # Not "European"
)
```

### 4. Theta Sign Convention

**Problem:** Different sign conventions for theta
- Some packages: Negative theta = time decay
- Others: Positive theta = time value loss

**Solution:**
```r
# Verify sign convention in package documentation
# Adjust if needed for consistent UI display
theta_per_day <- abs(theta_annual) / 365
```

---

## Next Steps for Implementation

### Phase 1: Package Setup (Week 1)
1. Install `greeks` package
2. Review built-in Shiny app (`run_shiny()`)
3. Create wrapper functions for investR patterns
4. Write unit tests for Greeks calculations

### Phase 2: Integration (Week 2)
5. Integrate into existing risk analysis module
6. Add Greeks calculations to put strategy analysis
7. Create visualization functions
8. Update documentation

### Phase 3: Enhancement (Week 3)
9. Add stress testing functionality
10. Implement scenario analysis
11. Create strategy comparison tools
12. Performance optimization

### Phase 4: Validation (Week 4)
13. Cross-validate with `derivmkts` package
14. Test edge cases (very short/long DTE, extreme volatility)
15. User acceptance testing
16. Production deployment

---

## Additional Resources

### Documentation
- `greeks` vignette: https://cran.r-project.org/web/packages/greeks/vignettes/using_greeks.html
- `derivmkts` vignette: https://cran.r-project.org/web/packages/derivmkts/vignettes/derivmkts-vignette.pdf
- `optionstrat` vignette: https://cran.r-project.org/web/packages/optionstrat/vignettes/optionstrat_vignette.html

### Academic References
- Hull, J.C. (2017). *Options, Futures, and Other Derivatives*
- McDonald, R. (2013). *Derivatives Markets*
- Hudde & Rüschendorf (2023). "European and Asian Greeks for exponential Lévy processes"

### GitHub Repositories
- greeks: https://github.com/ahudde/greeks
- derivmkts: Part of CRAN (stable release)
- RQuantLib: https://github.com/eddelbuettel/rquantlib

---

## Conclusion

The **`greeks` package** is the recommended choice for integrating options pricing and Greeks calculation into investR for the following reasons:

1. **Active Maintenance:** Latest update November 2025
2. **Comprehensive Coverage:** All required Greeks for put options
3. **American Options:** Critical for early assignment risk analysis
4. **Shiny Integration:** Built-in app provides reference implementation
5. **Tidyverse Compatibility:** Aligns with investR's coding standards
6. **Performance:** C++ backend suitable for real-time dashboards
7. **Documentation:** Excellent vignettes and examples

The `derivmkts` package serves as an excellent secondary option for validation and supplementary analysis, particularly for educational purposes and binomial tree methods.

Both packages are production-ready, well-maintained, and suitable for integration into the investR Shiny application.
