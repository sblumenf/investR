# Risk Management System - Quick Reference

**Version**: 2.0 (A+ Upgrade)

---

## Quick Start

### Analyze a Single Position

```r
library(investR)

# Full risk analysis for a covered call
result <- analyze_position_risk(
  ticker = "AAPL",
  strike = 155,
  premium_received = 5.00,
  expiration_date = as.Date("2025-12-19"),
  purchase_price = 145
)

# View key metrics
result$monte_carlo$median_return        # Expected return
result$monte_carlo$prob_profit          # Probability of profit
result$monte_carlo$early_exercise_prob  # Early exercise risk
result$monte_carlo$regime$name          # Current market regime
```

### Analyze Entire Portfolio

```r
# Portfolio-level risk metrics
portfolio <- analyze_portfolio_risk()

# Key metrics
portfolio$var_95                 # Value at Risk (95%)
portfolio$expected_return        # Expected return ($)
portfolio$risk_level             # Low/Moderate/High/Critical

# Top risk contributors
portfolio$position_contributions %>%
  arrange(desc(abs(risk_contribution))) %>%
  head(5)
```

---

## Key Functions

### Volatility

```r
# Get market-based volatility (implied + historical blend)
vol <- get_volatility("AAPL", days_to_expiry = 30)

# Pure historical volatility
hist_vol <- get_volatility("AAPL", 30, use_implied = FALSE)

# Black-Scholes Greeks
greeks <- black_scholes_price(
  stock_price = 150, strike = 155,
  time_to_expiry = 30/365, volatility = 0.25
)
```

### Market Regime

```r
# Detect current market conditions
regime <- detect_market_regime()

# Show user-friendly summary
show_current_regime()

# Get regime-adjusted parameters
params <- get_regime_adjusted_parameters(
  base_volatility = 0.20,
  base_jump_intensity = 4
)
```

### Early Exercise (LSM)

```r
# Run LSM algorithm on price paths
lsm <- run_lsm_early_exercise(
  price_paths = simulated_paths,
  strike = 155,
  dividend_schedule = dividends
)

lsm$early_exercise_prob      # Probability (0-1)
lsm$avg_exercise_timestep    # When it happens
```

---

## Market Regimes

| VIX Level | Regime   | Risk Multiplier | Description                  |
|-----------|----------|-----------------|------------------------------|
| > 35      | Crisis   | 2.0x            | Market panic, extreme vol    |
| 25-35     | Stressed | 1.5x            | Elevated uncertainty         |
| 15-25     | Normal   | 1.0x            | Typical conditions           |
| < 15      | Calm     | 0.8x            | Low volatility environment   |

---

## Risk Metrics Explained

### VaR (Value at Risk)
- **95% VaR**: Expected loss at 5th percentile (1 in 20 scenarios)
- **99% VaR**: Expected loss at 1st percentile (1 in 100 scenarios)

### CVaR (Conditional VaR)
- **Expected Shortfall**: Average loss BEYOND the VaR threshold
- More conservative than VaR (captures tail risk)

### Component VaR
- **Position Contribution**: How much each position adds to portfolio VaR
- **Property**: Sum of all Component VaRs = Portfolio VaR

### Risk-Return Ratio
```
Ratio = Expected Contribution / |Risk Contribution|

Higher = Better risk-adjusted return
```

---

## Stress Test Scenarios

| Scenario              | Stock Drop | Historical Event        |
|-----------------------|------------|-------------------------|
| 2008 Financial Crisis | -45%       | Lehman Brothers (2008)  |
| COVID-19 Crash        | -35%       | Pandemic (Mar 2020)     |
| Dot-com Bubble        | -30%       | Tech crash (2000)       |
| 1987 Black Monday     | -22%       | Flash crash (1987)      |
| 2010 Flash Crash      | -10%       | Algo trading (2010)     |

---

## Configuration Quick Reference

```r
# Access configuration
RISK_CONFIG$simulation$default_paths        # 10,000
RISK_CONFIG$lsm$polynomial_degree           # 3
RISK_CONFIG$volatility$default_vol          # 0.20 (20%)
RISK_CONFIG$regime$vix_crisis_threshold     # 35

# Override for specific analysis
analyze_position_risk(..., simulation_paths = 50000)  # High precision
```

---

## Common Use Cases

### Check Current Market Regime

```r
show_current_regime()

# Output:
# Current Market Regime: Normal
# VIX Level: 18.5 (42nd percentile)
# Risk Multiplier: 1.0x
# Description: Normal market conditions with moderate volatility
```

### Compare Positions

```r
positions <- c("AAPL", "MSFT", "GOOGL")
results <- purrr::map(positions, ~analyze_position_risk(
  ticker = .x,
  strike = ...,
  premium_received = ...,
  expiration_date = ...
))

# Compare median returns
purrr::map_dbl(results, ~.x$monte_carlo$median_return)
```

### Find Concentration Risk

```r
portfolio <- analyze_portfolio_risk()

# Ticker concentration
portfolio$concentration$by_ticker %>%
  filter(pct_of_portfolio > 0.25)  # >25% in one ticker

# Sector concentration
portfolio$concentration$by_sector %>%
  filter(pct_of_portfolio > 0.40)  # >40% in one sector

# Alerts
portfolio$concentration$alerts
```

### Test Stress Scenarios

```r
result <- analyze_position_risk(...)
stress <- result$stress_tests

# Find worst-case scenario
worst <- stress %>%
  arrange(position_return_pct) %>%
  head(1)

cat("Worst scenario:", worst$scenario, "\n")
cat("Return:", scales::percent(worst$position_return_pct), "\n")
```

---

## Interpretation Guide

### Probability of Profit

- **> 75%**: High confidence position
- **60-75%**: Moderate confidence
- **< 60%**: Consider adjusting strike or avoiding

### Early Exercise Probability

- **< 5%**: Very low risk
- **5-15%**: Monitor dividend dates
- **> 15%**: High risk, consider deeper OTM strikes

### Risk Level (Portfolio)

- **Low**: VaR < 5% of portfolio value
- **Moderate**: VaR 5-10% of portfolio
- **High**: VaR 10-15% of portfolio
- **Critical**: VaR > 15% of portfolio

### Component VaR % of Portfolio Risk

- **< 10%**: Normal diversification
- **10-15%**: Monitor position size
- **> 15%**: Consider reducing concentration

---

## Performance Tips

### Speed Up Analysis

```r
# Quick preview (1,000 paths, ~200ms)
analyze_position_risk(..., simulation_paths = 1000)

# Standard analysis (10,000 paths, ~1.2s)
analyze_position_risk(...)  # Default

# High precision (50,000 paths, ~6s)
analyze_position_risk(..., simulation_paths = 50000)
```

### Use Caching

```r
# Volatility and regime detection cache for 15 minutes
vol1 <- get_volatility("AAPL", 30)  # Fetches from Yahoo
vol2 <- get_volatility("AAPL", 30)  # Returns cached value (instant)

# Force refresh
detect_market_regime(cache_duration = 0)
```

---

## Shiny Dashboard

### Position Risk Modal

1. Click "Analyze Risk" on any position
2. View Summary tab for key metrics
3. Check Distribution tab for return histogram
4. Review Dividend Timeline for ex-div dates
5. Examine Stress Tests for downside scenarios
6. Inspect Greeks & Details for sensitivity analysis

### Portfolio Risk Dashboard

1. Navigate to "Portfolio Risk" tab
2. Click "Analyze Portfolio Risk"
3. Review overall metrics card
4. Examine position contributions table
5. Check concentration alerts
6. Review stress test results
7. View correlation heatmap

---

## Error Messages

### Common Issues

**"Insufficient in-the-money paths for regression"**
- LSM needs at least 30 ITM paths
- Increase simulation_paths or check strike price

**"Failed to fetch implied volatility"**
- Market data unavailable (delisted, after-hours)
- Falls back to historical volatility automatically

**"Database locking error"**
- Another process is using the portfolio database
- Close other investR instances or wait

**"VIX data unavailable"**
- Network issue or CBOE API down
- Regime detection falls back to "normal" with warning

---

## Testing Your Changes

```r
# Run all risk tests
devtools::test()

# Test specific module
testthat::test_file("tests/testthat/test-fct_implied_volatility.R")

# Quick smoke test
library(investR)
show_current_regime()
vol <- get_volatility("AAPL", 30)
print(vol)
```

---

## Key Files

- `R/fct_implied_volatility.R` - Volatility estimation
- `R/fct_regime_detection.R` - Market regime classification
- `R/fct_lsm_engine.R` - Early exercise algorithm
- `R/fct_portfolio_risk.R` - Portfolio-level risk
- `R/fct_position_risk.R` - Position-level risk
- `R/config_risk.R` - Configuration parameters
- `RISK_MANAGEMENT_DOCUMENTATION.md` - Full technical docs

---

## Formula Quick Reference

### Black-Scholes Call Price

```
C = S₀N(d₁) - Ke⁻ʳᵀN(d₂)

d₁ = [ln(S₀/K) + (r + σ²/2)T] / (σ√T)
d₂ = d₁ - σ√T
```

### Jump-Diffusion

```
dS/S = μdt + σdW + (J-1)dN

μ = drift, σ = volatility, J = jump size, N = Poisson process
```

### Component VaR

```
Component VaRᵢ = E[Xᵢ | X_portfolio ≤ VaR_portfolio]

Sum of all Component VaRs = Portfolio VaR
```

---

## Support

- **Full Documentation**: See `RISK_MANAGEMENT_DOCUMENTATION.md`
- **Test Examples**: Review `tests/testthat/test-fct_*.R` files
- **Code Examples**: Check function documentation with `?function_name`

---

**Last Updated**: October 22, 2025
**Version**: 2.0 (A+ Upgrade)
