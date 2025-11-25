# ETF Screener Test Plan

## Manual Testing Checklist

### 1. Configuration Validation
```r
# Test config accessor
investR::get_etf_screener_config()
investR::get_etf_screener_config("expense_ratio_max")

# Test validation
investR:::validate_etf_screener_config()
```

**Expected Results**:
- Config list returned with all parameters
- Individual values accessible by key
- Validation passes without errors

### 2. yfscreen Integration
```r
# Test basic ETF fetch
tickers <- investR::fetch_yfscreen_etfs()

# Test with custom parameters
tickers <- investR::fetch_yfscreen_etfs(
  expense_ratio_max = 0.3,
  dividend_yield_min = 3,
  dividend_yield_max = 8,
  min_net_assets = 1000000000
)
```

**Expected Results**:
- Returns character vector of ticker symbols
- Logging shows filter parameters and API calls
- Error handling works when API fails
- Empty vector returned when no matches

### 3. Analysis Function
```r
# Test with default parameters
results <- investR::analyze_etf_covered_calls_yfscreen()

# Test with custom parameters
results <- investR::analyze_etf_covered_calls_yfscreen(
  expense_ratio_max = 0.5,
  dividend_yield_min = 2,
  dividend_yield_max = 6,
  min_net_assets = 500000000,
  strike_threshold_pct = 0.85,
  min_days = 45,
  max_days = 120,
  max_workers = 4
)
```

**Expected Results**:
- Returns tibble with covered call opportunities
- Sorted by annualized_return descending
- Contains result flags: is_etf = TRUE, is_yfscreen = TRUE
- Logging shows progress through analysis pipeline

### 4. UI/UX Testing

#### Navigate to Page
1. Start app: `investR::run_app()`
2. Navigate to home page
3. Find "ETF Screener Covered Calls" card in Covered Call Strategies section
4. Click "Analyze ETF Screener" button

**Expected Results**:
- Page loads at `/etf-covered-calls`
- Title: "ETF Screener - Deep ITM Covered Calls"
- Sidebar with all controls visible
- Main panel empty (no results yet)

#### Test Controls
1. **Quote Source Toggle**: Switch between Yahoo and Questrade
2. **Expense Ratio Slider**: Test range 0-2%, step 0.1%
3. **Dividend Yield Slider**: Test range 0-10% (2-6 default), step 0.5%
4. **Min Net Assets Dropdown**: Select different options ($100M, $500M, $1B, $5B)
5. **Strike Threshold Slider**: Test range 50-100%, step 5%
6. **Days Range Slider**: Test range 30-365 (45-120 default), step 5
7. **Max Workers Slider**: Test range 1-20, default 4

**Expected Results**:
- All controls functional and responsive
- Help text displayed below each control
- Values update when sliders move
- Dropdown selections work

#### Run Analysis
1. Click "Run Analysis" button
2. Monitor progress messages
3. Wait for results

**Expected Results**:
- Progress message: "Screening ETFs and analyzing covered calls..."
- Success message: "Analysis complete! Found X opportunities."
- Results table appears with sortable columns
- Download CSV button becomes active

#### Test Results Table
1. Verify columns displayed correctly
2. Test sorting by clicking column headers
3. Test filtering (if enabled)
4. Download results as CSV

**Expected Results**:
- All expected columns present and formatted
- Sorting works in both directions
- CSV download contains all results
- Formatting preserved (%, $, dates)

### 5. Edge Cases

#### Empty Results
```r
# Test with impossible criteria
results <- investR::analyze_etf_covered_calls_yfscreen(
  expense_ratio_max = 0.01,  # Very low
  dividend_yield_min = 15,   # Very high
  dividend_yield_max = 20,   # Very high
  min_net_assets = 100000000000  # $100B (very high)
)
```

**Expected Results**:
- No results message: "No opportunities found..."
- Empty tibble returned
- No errors or crashes
- Helpful message suggesting parameter adjustments

#### API Failure Simulation
- Disconnect internet or block yfscreen API
- Run analysis
- Verify error handling

**Expected Results**:
- Clear error message about API failure
- No app crash
- Logging shows error details
- User can try again

#### Invalid Parameters
```r
# Test validation catches issues
ETF_SCREENER_CONFIG$expense_ratio_max <- -1
investR:::validate_etf_screener_config()
```

**Expected Results**:
- Validation error thrown
- Descriptive error message
- App doesn't start with invalid config

### 6. Integration Testing

#### Home Page Integration
1. Navigate to home page
2. Find "Covered Call Strategies" accordion
3. Verify "ETF Screener Covered Calls" card present
4. Check description, href, button text

**Expected Results**:
- Card visible in correct accordion section
- Description accurate and helpful
- Link works: `/etf-covered-calls`
- Button text: "Analyze ETF Screener"

#### Page Routing
1. Navigate to `/etf-covered-calls` directly
2. Navigate from home page
3. Navigate back to home
4. Test browser back button

**Expected Results**:
- Direct URL navigation works
- Click navigation works
- Home link works
- Browser back button works
- No Shiny session conflicts

#### Quote Source Persistence
1. Set quote source to Questrade
2. Run analysis
3. Navigate away and back
4. Check quote source setting

**Expected Results**:
- Setting persists within page session
- Analysis uses correct quote source
- Toggle state matches actual source used
- Logging confirms source used

### 7. Performance Testing

#### Small ETF Set
```r
# Test with restrictive filters (< 10 ETFs)
results <- investR::analyze_etf_covered_calls_yfscreen(
  expense_ratio_max = 0.1,
  dividend_yield_min = 5,
  dividend_yield_max = 10,
  min_net_assets = 5000000000
)
```

**Expected Results**:
- Fast execution (< 30 seconds)
- Minimal memory usage
- Clean logging output

#### Large ETF Set
```r
# Test with permissive filters (> 100 ETFs)
results <- investR::analyze_etf_covered_calls_yfscreen(
  expense_ratio_max = 2.0,
  dividend_yield_min = 0,
  dividend_yield_max = 10,
  min_net_assets = 100000000,
  max_workers = 8
)
```

**Expected Results**:
- Reasonable execution time (< 5 minutes)
- Parallel processing utilized
- Memory usage acceptable
- Progress visible in logs

#### Concurrent Users
- Open app in multiple browser tabs
- Run analysis simultaneously
- Check for session conflicts

**Expected Results**:
- Independent sessions work correctly
- No cross-contamination
- Each session maintains own state
- No performance degradation

### 8. Code Quality Checks

#### Linting
```bash
# Run lintr on new files
Rscript -e "lintr::lint_dir('R', pattern = '(etf_covered_calls|yfscreen)')"
```

#### Documentation
```bash
# Verify all exports documented
Rscript -e "roxygen2::roxygenise()"
# Check for warnings about undocumented exports
```

#### Style Guide
- Check tidyverse style compliance
- Verify no base R loops (use purrr)
- Check proper use of %>% pipe
- Verify consistent naming conventions

### 9. Regression Testing

#### Existing Strategies
1. Test Dividend Aristocrats strategy
2. Test Zero-Dividend strategy
3. Test Dynamic Covered Calls
4. Test Collar strategy

**Expected Results**:
- All existing strategies still work
- No conflicts or interference
- Shared functions still work correctly
- No performance degradation

#### Shared Utilities
```r
# Test shared functions still work
investR::analyze_covered_calls_generic(
  stock_universe = c("SPY", "QQQ", "IWM"),
  strategy_name = "Test",
  strike_threshold_pct = 0.85,
  min_days = 45,
  max_days = 120
)
```

**Expected Results**:
- Generic analysis function unchanged
- Results table module works with new data
- Quote source toggle works correctly
- Analysis controls helper works

### 10. Deployment Readiness

#### Package Build
```bash
# Check package builds cleanly
R CMD build investR
R CMD check investR_*.tar.gz
```

#### Dependencies
```bash
# Verify yfscreen package available
Rscript -e "if (!requireNamespace('yfscreen')) install.packages('yfscreen')"
```

#### Production Config
- Verify default parameters sensible for production
- Check max_workers set appropriately
- Validate error handling production-ready
- Review logging verbosity

## Success Criteria

✅ All unit tests pass
✅ All manual tests pass
✅ No regressions in existing features
✅ Performance acceptable (< 5 min for large sets)
✅ Error handling comprehensive
✅ Documentation complete
✅ Code style compliant
✅ Package builds cleanly
✅ UI/UX intuitive and responsive
✅ Integration seamless with existing app

## Known Limitations

1. **yfscreen API**: Requires working internet connection
2. **Rate Limiting**: May need retry logic for high-volume usage
3. **ETF Universe**: Limited to US region currently
4. **Filter Options**: Limited to 4 main criteria (could expand)
5. **Caching**: No results caching yet (every run fetches fresh)

## Future Enhancements

1. **Additional Filters**: Beta, tracking error, liquidity metrics
2. **Preset Strategies**: Quick buttons for common filter combinations
3. **Results Caching**: Cache yfscreen results with TTL
4. **Comparative Analysis**: Show filter funnel (how many at each stage)
5. **Export Improvements**: Additional export formats (Excel, PDF)
6. **Visualization**: Charts showing ETF distribution by filters
