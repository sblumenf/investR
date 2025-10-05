# Covered Call Aristocrat Strategy - R Implementation Plan

## Overview
Replicate the existing Python covered call aristocrat strategy in R using tidyverse syntax.

## Strategy Steps

### 1. Get List of Dividend Aristocrats
- Scrape from StockAnalysis.com (primary) or Wikipedia (fallback)
- Extract ticker symbols from HTML tables

### 2. For Each Stock (in parallel):
- Fetch current stock price and company info
- Fetch 5 years of price history (for max drawdown calculation)
- Fetch dividend history (for dividend projections)
- Get all available option expirations
- For each expiration, fetch call options chain
- Filter for ITM calls (strike < current_price)
- Calculate time_value = bid - max(current_price - strike, 0)

### 3. Select Optimal Option:
- Filter: strike ≤ (current_price × `strike_threshold_pct`, default 0.8)
- If no options found, use all ITM options as fallback and set warning_flag
- If `target_days` specified: sort by closest to target, then by open_interest desc
- If `target_days` NOT specified: sort by expiration desc, then open_interest desc
- Select first row

### 4. Calculate All Metrics:
- Investment = current_price × 100
- Premium_received = bid_price × 100
- Net_outlay = investment - premium_received
- Intrinsic_value = max(0, current_price - strike)
- Extrinsic_value = bid_price - intrinsic_value
- Project dividends for holding period (using latest dividend amount and payment frequency)
- Exercise_proceeds = strike × 100
- Net_profit = premium_received + dividend_income + exercise_proceeds - investment
- Total_return = net_profit / net_outlay
- Annualized_return = (1 + total_return)^(365/days_to_expiry) - 1
- Breakeven_price = current_price - bid_price
- Downside_protection_pct = (current_price - breakeven_price) / current_price
- Max_drawdown from 5-year price history (cumulative returns, running max, calculate drawdown series, find minimum)
- Current_yield from last 4 dividends

### 5. Filter & Sort:
- Remove stocks with annualized_return ≤ 0
- Sort by annualized_return descending

### 6. Output Results

## Required R Libraries

### Essential:
- `tidyverse` - data manipulation
- `rvest` - web scraping
- `quantmod` - stock/option data from Yahoo Finance
- `lubridate` - date handling
- `furrr` - parallel processing (replicates ThreadPoolExecutor)
- `future` - backend for furrr

## Configuration Parameters

- `strike_threshold_pct`: Maximum strike as % of current price (default 0.8)
- `target_days`: Target days to expiry (default NULL = use longest available)
- `max_workers`: Number of parallel workers (default 10)