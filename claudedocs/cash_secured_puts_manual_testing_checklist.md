# Cash-Secured Puts Manual Testing Checklist

## Date: 2025-11-23
## Version: 1.0

---

## Overview

This document provides step-by-step manual testing procedures for the cash-secured puts strategy feature. Use this checklist to verify functionality after implementation is complete.

---

## Prerequisites

- [ ] Backend functions implemented (`R/fct_cash_secured_puts.R`, `R/utils_cash_secured_puts_config.R`)
- [ ] UI modules implemented (when Phase 2 is complete)
- [ ] Application starts without errors: `shiny::runApp()`
- [ ] Test data available (internet connection for live data)

---

## 1. Pre-Testing Setup

### Environment Verification

- [ ] R session is clean: `rm(list = ls())`
- [ ] Package loaded: `devtools::load_all()`
- [ ] No console errors during load
- [ ] Test database available (if using local DB)

### Quote Source Configuration

- [ ] Verify quote source setting: `getOption('investR.quote_source')`
- [ ] Test both sources:
  - [ ] Questrade: `options(investR.quote_source = "questrade")`
  - [ ] Yahoo: `options(investR.quote_source = "yahoo")`

---

## 2. Backend Function Testing (Console)

### Test Configuration

```r
# Verify config loads
CASH_SECURED_PUTS_CONFIG
validate_puts_config(CASH_SECURED_PUTS_CONFIG)

# Test config overrides
config <- get_puts_config(strike_threshold_pct = 0.90, max_workers = 4)
config$strike_threshold_pct  # Should be 0.90
config$max_workers           # Should be 4
```

**Expected Results:**
- [ ] Config object displays all fields
- [ ] Validation returns TRUE
- [ ] Overrides work correctly

### Test Single Stock Analysis

```r
# Test with known liquid stock
result <- analyze_single_stock_put(
  ticker = "AAPL",
  strike_threshold_pct = 0.95,
  min_days = 30,
  max_days = 90
)

# Inspect result
str(result)
result$strike >= result$current_price * 0.95  # Should be TRUE
result$cash_required == result$strike * 100   # Should be TRUE
```

**Expected Results:**
- [ ] Returns tibble with single row (or NULL if no opportunities)
- [ ] All required columns present
- [ ] Strike threshold logic correct (strike >= 95% of current price)
- [ ] Cash required = strike × 100
- [ ] Premium received = bid × 100

### Test Full Analysis (Small Sample)

```r
# Test with small limit
results <- analyze_cash_secured_puts(
  limit = 5,
  strike_threshold_pct = 0.95,
  min_days = 45,
  max_days = 120,
  max_workers = 2
)

# Inspect results
nrow(results)
head(results)

# Verify sorting (should be descending by annualized_return)
all(diff(results$annualized_return) <= 0)  # Should be TRUE
```

**Expected Results:**
- [ ] Analysis completes without errors
- [ ] Returns data frame (may be empty if no opportunities)
- [ ] Results sorted by annualized_return (descending)
- [ ] Logging output visible in console
- [ ] Success/failure summary displayed

---

## 3. Metric Validation (Spot Checks)

### Select 3 Results for Manual Verification

For each selected result, verify calculations:

#### Result 1:
```r
r <- results[1, ]  # Pick first result

# Record values:
ticker: __________
current_price: $__________
strike: $__________
bid: $__________
days_to_expiry: __________
```

**Manual Calculations:**
```
Cash Required:
  strike × 100 = ________ × 100 = $_________
  Matches r$cash_required? [ ]

Premium Received:
  bid × 100 = ________ × 100 = $_________
  Matches r$premium_received? [ ]

Net Outlay:
  cash_required - premium = _________ - _________ = $_________
  Matches r$net_outlay? [ ]

Breakeven Price:
  strike - bid = ________ - ________ = $_________
  Matches r$breakeven_price? [ ]

Downside Protection %:
  (current - breakeven) / current = (_______ - _______) / _______ = _______%
  Matches r$downside_protection_pct? [ ]

Return on Cash:
  premium / cash_required = _______ / _______ = _______%
  Matches r$return_on_cash? [ ]

Annualized Return:
  return_on_cash × (365 / days) = _______ × (365 / _______) = _______%
  Matches r$annualized_return? [ ]

Intrinsic Value (ITM = strike > current):
  max(0, strike - current) = max(0, _______ - _______) = $_________
  Matches r$intrinsic_value? [ ]

Extrinsic Value:
  bid - intrinsic = _______ - _______ = $_________
  Matches r$extrinsic_value? [ ]
```

#### Result 2:
```r
r <- results[2, ]
```
**Repeat manual calculations above**
- [ ] All calculations verified

#### Result 3:
```r
r <- results[3, ]
```
**Repeat manual calculations above**
- [ ] All calculations verified

---

## 4. Edge Case Testing

### Very Low Stock Price

```r
# Test with penny stock if available
result <- analyze_single_stock_put(
  ticker = "CHEAP_STOCK",  # Replace with actual ticker
  strike_threshold_pct = 0.90
)
```

**Expected:**
- [ ] Handles low prices correctly
- [ ] No division by zero errors
- [ ] Cash required still = strike × 100

### Very High Stock Price

```r
# Test with expensive stock
result <- analyze_single_stock_put(
  ticker = "EXPENSIVE_STOCK",  # Replace with ticker > $250
  strike_threshold_pct = 0.95
)
```

**Expected:**
- [ ] Skipped due to max_stock_price filter
- [ ] Returns NULL or failure reason
- [ ] Logs appropriate message

### No Options Available

```r
# Test with stock that has no options
result <- analyze_single_stock_put(
  ticker = "NOOPTIONS",  # Replace with actual ticker
  strike_threshold_pct = 0.95
)
```

**Expected:**
- [ ] Returns NULL or failure reason
- [ ] No errors thrown
- [ ] Logs "No put options available"

### Empty Result Set (Filters Too Strict)

```r
results <- analyze_cash_secured_puts(
  limit = 5,
  strike_threshold_pct = 0.99,  # Very conservative
  min_days = 200,               # Very long minimum
  max_days = 210                # Narrow window
)
```

**Expected:**
- [ ] Returns empty data frame (0 rows)
- [ ] No errors
- [ ] Appropriate logging

---

## 5. Performance Testing

### Single Worker (Sequential)

```r
start_time <- Sys.time()
results_1 <- analyze_cash_secured_puts(
  limit = 10,
  max_workers = 1
)
time_1 <- difftime(Sys.time(), start_time, units = "secs")
```

**Record:** __________ seconds

### Multiple Workers (Parallel)

```r
start_time <- Sys.time()
results_4 <- analyze_cash_secured_puts(
  limit = 10,
  max_workers = 4
)
time_4 <- difftime(Sys.time(), start_time, units = "secs")
```

**Record:** __________ seconds

**Verification:**
- [ ] Parallel is faster or similar (allowing for overhead)
- [ ] Both return same number of results
- [ ] No race conditions or data corruption

### Full Aristocrats List

```r
start_time <- Sys.time()
results_full <- analyze_cash_secured_puts(
  strike_threshold_pct = 0.95,
  max_workers = 10
)
time_full <- difftime(Sys.time(), start_time, units = "mins")
```

**Record:** __________ minutes

**Verification:**
- [ ] Completes in < 10 minutes
- [ ] Returns results
- [ ] Logs failure summary for skipped stocks

---

## 6. UI Testing (When Phase 2 Complete)

### Page Load

- [ ] Navigate to `/cash-secured-puts` route
- [ ] Page loads without errors
- [ ] All UI elements visible
- [ ] No console errors in browser DevTools

### Parameter Controls

**Strike Threshold Slider:**
- [ ] Moves smoothly from 50% to 100%
- [ ] Default value is 95%
- [ ] Label updates with value
- [ ] Value persists during analysis

**Days to Expiry Slider:**
- [ ] Range selection works (both handles)
- [ ] Default is 45-120 days
- [ ] Min/max values update correctly
- [ ] Values persist during analysis

**Parallel Workers Slider:**
- [ ] Moves from 1 to 20
- [ ] Default value is 4
- [ ] Integer values only
- [ ] Value persists during analysis

**Quote Source Toggle:**
- [ ] Toggles between Questrade and Yahoo
- [ ] Selection persists during analysis
- [ ] Visual feedback on toggle

### Analysis Execution

**Run Analysis Button:**
- [ ] Button click triggers analysis
- [ ] Button disables during analysis
- [ ] Progress indicator appears
- [ ] Button re-enables after completion

**Progress Feedback:**
- [ ] Progress spinner/indicator visible
- [ ] Status messages display
- [ ] Estimated time or progress % shown (if implemented)

**Results Display:**
- [ ] Results appear after analysis completes
- [ ] Cards display for each opportunity
- [ ] Results sorted by annualized return (highest first)
- [ ] All metrics formatted correctly

### Results Cards

For each displayed card, verify:

**Header:**
- [ ] Company name and ticker visible
- [ ] Current price displayed
- [ ] Strike price displayed
- [ ] Expiration date displayed

**Quick Overview Section:**
- [ ] Expands by default
- [ ] Annualized return prominently displayed
- [ ] Cash required shown
- [ ] Premium received shown
- [ ] Days to expiration shown
- [ ] All values formatted ($ for currency, % for percentages)

**Risk Analysis Section:**
- [ ] Collapsed by default
- [ ] Expands on click
- [ ] Breakeven price displayed
- [ ] Downside protection % shown
- [ ] Max drawdown shown
- [ ] Current dividend yield shown

**Option Details Section:**
- [ ] Collapsed by default
- [ ] Expands on click
- [ ] Expiration date formatted correctly
- [ ] Open interest displayed
- [ ] Intrinsic value shown
- [ ] Extrinsic value shown

**Card Styling:**
- [ ] Cards have consistent spacing
- [ ] Borders and shadows render correctly
- [ ] Text is readable (font size, color)
- [ ] Accordion animations smooth

### Data Export

**Download CSV Button:**
- [ ] Button click triggers download
- [ ] CSV file downloads to browser default location
- [ ] Filename is descriptive (e.g., `cash_secured_puts_20251123.csv`)
- [ ] Open CSV and verify:
  - [ ] All columns present
  - [ ] Column headers are descriptive
  - [ ] Data matches displayed results
  - [ ] No HTML artifacts or escaped characters
  - [ ] Numbers formatted correctly (decimals preserved)
  - [ ] Dates formatted correctly

### Error Handling

**Invalid Parameter Combinations:**

Test 1: Very short days range
```
min_days = 1
max_days = 5
```
- [ ] Either succeeds with limited results or shows appropriate error

Test 2: Strike threshold at extremes
```
strike_threshold_pct = 50%
```
- [ ] Succeeds (very aggressive OTM puts)

```
strike_threshold_pct = 100%
```
- [ ] Succeeds (only ATM or ITM puts)

**Network Errors:**
- [ ] Disconnect network
- [ ] Attempt analysis
- [ ] Appropriate error message displays
- [ ] Reconnect network
- [ ] Analysis works again

**No Results Found:**
- [ ] Set very restrictive parameters
- [ ] Run analysis
- [ ] Verify "No opportunities found" message displays
- [ ] No errors or blank screens

---

## 7. Cross-Browser Testing

Test all functionality in:

### Chrome
- [ ] All UI elements display correctly
- [ ] All interactions work
- [ ] No console errors
- [ ] Performance is acceptable

### Firefox
- [ ] All UI elements display correctly
- [ ] All interactions work
- [ ] No console errors
- [ ] Performance is acceptable

### Safari (if available)
- [ ] All UI elements display correctly
- [ ] All interactions work
- [ ] No console errors
- [ ] Performance is acceptable

### Edge
- [ ] All UI elements display correctly
- [ ] All interactions work
- [ ] No console errors
- [ ] Performance is acceptable

---

## 8. Comparison with Covered Calls

### Strike Logic Verification

**Covered Calls (for reference):**
```r
# Calls: ITM when strike < current_price
# Filter: Strike <= threshold (want ITM calls)
# Example: $100 stock, 85% threshold → strikes <= $85
```

**Cash-Secured Puts (verify):**
```r
# Puts: ITM when strike > current_price
# Filter: Strike >= threshold (want OTM puts)
# Example: $100 stock, 95% threshold → strikes >= $95
```

**Manual Verification:**
- [ ] Select a result from puts analysis
- [ ] Verify: strike >= current_price × threshold
- [ ] This is OPPOSITE of calls logic
- [ ] Higher threshold % for puts = MORE conservative (closer to ATM)
- [ ] Higher threshold % for calls = LESS conservative (deeper ITM)

### Metric Differences

**Covered Calls Metrics:**
- Capital = stock purchase price
- Income = premium received + potential dividends
- Assignment = sell stock at strike (cap upside)

**Cash-Secured Puts Metrics:**
- Capital = cash collateral (strike × 100)
- Income = premium received only
- Assignment = buy stock at strike (acquire position)

**Verify:**
- [ ] Puts use cash_required (not stock purchase)
- [ ] Puts calculate return_on_cash (premium / cash_required)
- [ ] Puts show breakeven as strike - premium
- [ ] Puts show downside_protection_pct correctly

### UI Consistency

- [ ] Similar layout to covered calls page
- [ ] Consistent parameter controls
- [ ] Similar card styling
- [ ] Consistent formatting (currency, percentages)
- [ ] Similar navigation patterns

---

## 9. Integration Testing

### Home Page Integration

- [ ] Cash-secured puts card appears on home page
- [ ] Card has appropriate title and description
- [ ] Card icon/image displays correctly
- [ ] Click on card navigates to `/cash-secured-puts`

### Navigation

- [ ] "Home" button returns to home page
- [ ] Browser back button works correctly
- [ ] Direct URL navigation works: `http://localhost:PORT/cash-secured-puts`
- [ ] Route is registered in app routing

### Data Consistency

- [ ] Dividend aristocrats list is same as covered calls
- [ ] Stock data matches across strategies
- [ ] Historical data is consistent
- [ ] Quote source affects both strategies similarly

---

## 10. Regression Testing

### Verify No Broken Functionality

**Covered Calls Strategy:**
- [ ] Aristocrats analysis still works
- [ ] Zero-dividend analysis still works
- [ ] Collar analysis still works
- [ ] All covered calls metrics still correct

**Other Features:**
- [ ] Portfolio risk analysis works
- [ ] Position tracking works
- [ ] Database operations work
- [ ] Quote source toggle works across all features

**Performance:**
- [ ] No noticeable slowdown in other features
- [ ] Memory usage is reasonable
- [ ] No resource leaks

---

## 11. Documentation Verification

### Function Documentation

```r
?analyze_cash_secured_puts
?analyze_puts_generic
?CASH_SECURED_PUTS_CONFIG
```

**Verify:**
- [ ] All exported functions have help pages
- [ ] Parameters described clearly
- [ ] Return values documented
- [ ] Examples provided
- [ ] References to related functions

### Code Comments

- [ ] All functions have roxygen2 headers
- [ ] Complex logic has inline comments
- [ ] TODO items addressed or documented
- [ ] No debugging code left in

---

## 12. Final Acceptance Checklist

### Unit Tests
- [ ] All config tests pass: `testthat::test_file("tests/testthat/test-utils_cash_secured_puts_config.R")`
- [ ] All function tests pass: `testthat::test_file("tests/testthat/test-fct_cash_secured_puts.R")`
- [ ] Test coverage >= 90%: `covr::package_coverage()`

### Integration Tests
- [ ] End-to-end backend test passes
- [ ] Full aristocrats analysis succeeds
- [ ] Parallel processing works correctly

### Manual Tests
- [ ] All backend console tests pass
- [ ] All UI tests pass (when implemented)
- [ ] Metric validation spot checks pass
- [ ] Edge cases handled correctly
- [ ] Performance meets requirements
- [ ] Cross-browser compatibility verified

### Regression Tests
- [ ] Existing covered calls functionality works
- [ ] No performance degradation
- [ ] No new errors in console/logs

### Documentation
- [ ] Function documentation complete
- [ ] Test plan document created
- [ ] Manual testing checklist completed (this document)
- [ ] README updated (if applicable)

---

## 13. Issues Log

Record any issues found during testing:

| # | Description | Severity | Status | Notes |
|---|-------------|----------|--------|-------|
| 1 |             |          |        |       |
| 2 |             |          |        |       |
| 3 |             |          |        |       |

**Severity Levels:**
- **Critical**: Blocks functionality, must fix before release
- **High**: Major issue, should fix before release
- **Medium**: Noticeable issue, fix if time permits
- **Low**: Minor cosmetic issue, can defer

---

## 14. Sign-Off

**Tester Name:** _______________________

**Date Completed:** _______________________

**Test Results:**
- [ ] All critical tests passed
- [ ] All high-priority tests passed
- [ ] Known issues documented
- [ ] Ready for production

**Notes:**
```
[Add any final notes about testing, known issues, or recommendations]
```

---

**Document Version:** 1.0
**Last Updated:** 2025-11-23
**Status:** Ready for Use
