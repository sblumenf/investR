# Cash-Secured Puts Filtering Criteria Explained

## Overview

The "S&P 500 Cash-Secured Puts (Dividend-Paying)" variant applies multiple layers of filtering to find high-quality cash-secured put opportunities. This document explains every filter and why positions get eliminated.

## Filter Layers (Applied in Order)

### 1. Stock Universe Selection (Dividend-Paying Filter)

**Filter**: `get_dividend_paying_sp500()`
**Source**: `R/utils_stock_universe.R` lines 360-407

**What It Does:**
- Starts with all ~500 S&P 500 stocks
- Checks each stock for dividend history (2-year lookback)
- Uses Yahoo Finance dividend data
- Parallel processing with 4 workers

**Filtering Logic:**
```r
# For each S&P 500 stock:
1. Fetch dividend history from (Today - 2 years) to Today
2. If dividends exist → Include in universe
3. If no dividends or error → Exclude from universe
```

**Expected Result**: ~400 dividend-paying stocks (out of 500)

**Cached**: Results cached for 30 days to avoid expensive scans

**Why Stocks Get Filtered Out:**
- ❌ **Zero-dividend growth stocks** (AMZN, GOOGL, META, TSLA, BRK.B, etc.)
- ❌ **Stocks with API fetch errors** (rare)
- ❌ **Recently stopped paying dividends** (within 2 years)

---

### 2. Stock Price Filter

**Filter**: `max_stock_price`
**Location**: `R/fct_cash_secured_puts.R` lines 328-335
**Config**: `CASH_SECURED_PUTS_CONFIG$max_stock_price = $250`

**What It Does:**
- Fetches current stock price from Yahoo Finance
- Excludes stocks priced above $250

**Why This Filter Exists:**
- Cash-secured puts require reserving strike × 100 shares in cash
- $250 stock = $25,000 cash required per contract
- Targets retail-accessible capital requirements

**Why Stocks Get Filtered Out:**
- ❌ **Expensive stocks** (AAPL at $230, MSFT at $420, NVDA at $140+, etc.)
- ❌ **High-priced aristocrats** (Some blue chips exceed $250)

**Example:**
- MSFT at $420 → **FILTERED OUT** ($42,000 cash per contract)
- KO at $63 → **PASSES** ($6,300 cash per contract)

---

### 3. Options Chain Availability

**Filter**: `get_options_chain_puts()`
**Location**: `R/fct_cash_secured_puts.R` lines 338-345

**What It Does:**
- Attempts to fetch put options chain from Yahoo Finance
- Requires valid options data to proceed

**Why Stocks Get Filtered Out:**
- ❌ **No listed options** (rare for S&P 500, but happens)
- ❌ **Yahoo Finance data unavailable** (temporary API issues)
- ❌ **Options expiration calendar issues** (no standard expiries)

---

### 4. Strike Price Range Filter

**Filter**: Strike threshold with range
**Location**: `R/fct_cash_secured_puts.R` lines 215-228
**Config**: `strike_threshold_pct = 0.85` (85% of current price)

**What It Does:**
- Calculates target strike: `current_price × 0.85`
- Allows strike range: `[threshold × 0.90, threshold × 1.05]`

**Example for $100 Stock:**
```
Current Price: $100
Threshold (85%): $85
Acceptable Range: $76.50 - $89.25

Strike $90 → ✅ PASSES (within range)
Strike $80 → ✅ PASSES (within range)
Strike $75 → ❌ FILTERED (too far OTM)
Strike $95 → ❌ FILTERED (too close to current price)
```

**Why This Filter Exists:**
- Targets **moderately OTM puts** (out-of-the-money)
- Balances premium income vs assignment risk
- Avoids deep OTM (tiny premiums) and near-money (high assignment risk)

**Why Options Get Filtered Out:**
- ❌ **Deep OTM strikes** (below threshold × 0.90)
- ❌ **Too close to money** (above threshold × 1.05)

---

### 5. Minimum Bid Price Filter

**Filter**: `min_option_bid`
**Location**: `R/fct_cash_secured_puts.R` lines 230-236
**Config**: `CASH_SECURED_PUTS_CONFIG$min_option_bid = $0.01`

**What It Does:**
- Filters out options with bid < $0.01

**Why This Filter Exists:**
- Eliminates worthless/illiquid options
- Ensures meaningful premium income
- $0.01 bid × 100 shares = $1 premium (not worth the effort)

**Why Options Get Filtered Out:**
- ❌ **Near-zero premium options** (bid < $0.01)
- ❌ **Extremely deep OTM** (market assigns near-zero value)

---

### 6. Open Interest (Liquidity) Filter

**Filter**: `min_open_interest`
**Location**: `R/fct_cash_secured_puts.R` lines 238-244
**Config**: `CASH_SECURED_PUTS_CONFIG$min_open_interest = 10`

**What It Does:**
- Requires minimum 10 contracts of open interest
- Ensures option has active market

**Why This Filter Exists:**
- **Liquidity**: Ability to close position if needed
- **Pricing**: More open interest = tighter bid-ask spreads
- **Validation**: Market participants confirm option has value

**Why Options Get Filtered Out:**
- ❌ **Illiquid options** (open interest < 10)
- ❌ **Unusual strikes** (market doesn't trade them)
- ❌ **New listings** (not yet established)

---

### 7. Days to Expiration Filter

**Filter**: `min_days` and `max_days`
**Location**: `R/fct_cash_secured_puts.R` lines 253-263
**Config**:
- `min_days = 45`
- `max_days = 120`

**What It Does:**
- Filters for expirations 45-120 days out
- Targets **medium-term** put selling

**Why This Filter Exists:**
- **Too short (<45 days)**: More volatility risk, less premium decay benefit
- **Too long (>120 days)**: Capital tied up, market uncertainty
- **Sweet spot (45-120)**: Balance of premium vs time commitment

**Why Options Get Filtered Out:**
- ❌ **Short-term options** (< 45 days to expiry)
- ❌ **Long-term options** (> 120 days to expiry)
- ❌ **Weekly options** (typically < 45 days)

**Example:**
- Expiry in 30 days → ❌ FILTERED (too short)
- Expiry in 60 days → ✅ PASSES
- Expiry in 180 days → ❌ FILTERED (too long)

---

## Summary: Complete Filtering Pipeline

### Input
- ~500 S&P 500 stocks

### Filter 1: Dividend-Paying
- **Remaining**: ~400 stocks
- **Filtered Out**: ~100 zero-dividend growth stocks

### Filter 2: Stock Price ≤ $250
- **Remaining**: ~300-350 stocks
- **Filtered Out**: ~50-100 expensive stocks

### Filter 3: Options Available
- **Remaining**: ~280-330 stocks
- **Filtered Out**: ~20 stocks (no options or data issues)

### Filter 4: Per-Stock Option Filtering
For each of the remaining ~300 stocks:

1. **Strike Range** (85% ± range)
   - Typical: 5-15 strikes per stock
   - Filtered: Deep OTM and near-money strikes

2. **Bid Price** (≥ $0.01)
   - Typical: Removes 0-2 worthless options

3. **Open Interest** (≥ 10)
   - Typical: Removes 2-5 illiquid options

4. **Days to Expiry** (45-120 days)
   - Typical: 1-3 expiration months qualify
   - Filtered: Near-term and long-term expiries

5. **Optimal Selection**
   - Picks 1 best option per stock
   - Closest to target strike, longest dated, highest liquidity

### Final Output
- **Expected**: 50-150 opportunities
- **Actual varies by**:
  - Current market conditions
  - Options pricing/liquidity
  - Number of stocks meeting all criteria

---

## Why So Few Results?

### Common Reasons for Low Result Counts

1. **Conservative Strike Filter (85%)**
   - 15% OTM is quite conservative
   - Many stocks won't have liquid options at this strike
   - Consider relaxing to 90% for more opportunities

2. **Open Interest = 10 Minimum**
   - Some dividend payers have thin options markets
   - Lower-volume stocks get filtered out

3. **Days Range (45-120)**
   - Restricts to specific expiration cycles
   - Misses quarterly earnings-adjacent options

4. **Max Price $250**
   - Filters out many premium blue chips
   - AAPL, MSFT, NVDA, etc. all above this

5. **Dividend-Paying Only**
   - Excludes high-growth tech (AMZN, GOOGL, META, TSLA)
   - These often have the most liquid options

### How to Increase Results

**Option 1: Relax Strike Threshold**
```r
analyze_sp500_cash_secured_puts(
  dividend_filter = "dividend_paying",
  strike_threshold_pct = 0.90  # 90% instead of 85%
)
```

**Option 2: Lower Open Interest Requirement**
```r
# Modify config before running
CASH_SECURED_PUTS_CONFIG$min_open_interest <- 5  # Lower from 10
```

**Option 3: Expand Days Range**
```r
analyze_sp500_cash_secured_puts(
  dividend_filter = "dividend_paying",
  min_days = 30,   # Lower from 45
  max_days = 180   # Raise from 120
)
```

**Option 4: Include All S&P 500**
```r
analyze_sp500_cash_secured_puts(
  dividend_filter = "all"  # Include zero-dividend stocks
)
```

**Option 5: Raise Max Stock Price**
```r
# Modify config before running
CASH_SECURED_PUTS_CONFIG$max_stock_price <- 500  # Raise from 250
```

---

## Verification: Check What's Being Filtered

To understand exactly why you're getting few results, you can enable debug logging:

```r
# Enable detailed logging
logger::log_threshold(logger::DEBUG)

# Run analysis - you'll see logs for each filter step
results <- analyze_sp500_cash_secured_puts(
  dividend_filter = "dividend_paying"
)

# Example logs you'll see:
# "AAPL: Skipping - price $185 exceeds max $250"
# "KO: After strike filter ($53-$56): 3 options"
# "KO: After OI filter (>=10): 2 options"
# "KO: Selected optimal put - Strike: $55, Expiry: 2026-03-20"
```

This will show you exactly which stocks are being filtered at each stage and why.

---

## Configuration Reference

All filters configured in `R/utils_cash_secured_puts_config.R`:

```r
CASH_SECURED_PUTS_CONFIG <- list(
  strike_threshold_pct = 0.85,    # 85% of current price
  min_days = 45,                   # Minimum 45 days to expiry
  max_days = 120,                  # Maximum 120 days to expiry
  min_option_bid = 0.01,           # Minimum $0.01 premium
  min_open_interest = 10,          # Minimum 10 contracts OI
  max_stock_price = 250,           # Maximum $250 stock price
  shares_per_contract = 100,       # Standard contract size
  max_workers = 10                 # Parallel processing workers
)
```
