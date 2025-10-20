# Risk Management - Testing Guide

## ✅ Implementation Complete!

**What's ready:**
- ✅ Monte Carlo simulation engine (Jump Diffusion + Heston models)
- ✅ RQuantLib integration for Greeks with discrete dividends
- ✅ Early exercise probability calculations
- ✅ Stress testing (5 pre-built scenarios)
- ✅ Risk scoring (0-100)
- ✅ UI integration in **TWO places**:
  1. **Strategy Results** (Aristocrats) - for researching new opportunities
  2. **Portfolio Groups** - for analyzing existing positions ⚡ **FASTEST TO TEST**

## 🚀 Quick Test (Start Here)

### Step 1: Verify Libraries
```bash
Rscript -e "library(RQuantLib); library(PerformanceAnalytics); library(MASS); cat('All libraries OK\n')"
```

If error, install:
```bash
Rscript -e "install.packages(c('RQuantLib', 'PerformanceAnalytics', 'MASS'), repos='https://cran.rstudio.com/', dependencies=TRUE)"
```

### Step 2: Start App
```r
library(investR)
run_app()
```

### Step 3: Test with Existing Portfolio Position (FASTEST)

1. **Navigate to "Portfolio Groups" page** (sidebar)

2. **Find any OPEN position card** that has:
   - A covered call (short option)
   - Strike price and expiration date visible

3. **Look for "Analyze Risk" button**:
   - Should appear as FIRST button (blue, with chart icon)
   - Only appears if position has option data
   - Located above "Close Group" and "Edit Members" buttons

4. **Click "Analyze Risk"**:
   - Loading modal should appear immediately
   - Shows spinner with "Running Monte Carlo simulation..."
   - Takes 5-10 seconds

5. **Verify Results Modal Opens** with 5 tabs:
   - **Summary**: Risk score, early exercise %, expected returns
   - **Distribution**: Return percentiles
   - **Dividend Timeline**: Projected dividends
   - **Stress Tests**: 5 scenarios (2008, COVID, etc.)
   - **Greeks & Details**: Delta, Gamma, Vega, Theta, Rho

6. **Check key metrics make sense**:
   - Risk score 0-100 (higher = riskier)
   - Early exercise probability (should be reasonable based on moneyness)
   - Stress tests show different outcomes

### Step 4: Test with Strategy Results (SLOWER - Full Strategy Run)

1. **Navigate to "Aristocrats" page**

2. **Run strategy** (use existing controls - may take 2-5 minutes for full analysis)

3. **Wait for result cards to appear**

4. **Each card should now have "Analyze Risk" button**:
   - Full-width blue button at top of card
   - Chart icon

5. **Click button on any card** → Same 5-tab modal as above

## 📊 Expected Behavior

### For In-The-Money (ITM) Covered Calls:
- **Risk Score**: 40-70 (moderate to high)
- **Early Exercise Prob**: 20-50% (depends on dividends and time value)
- **Stress Tests**: Worse in rising markets (more likely to be called away)

### For At-The-Money (ATM) Covered Calls:
- **Risk Score**: 30-50 (moderate)
- **Early Exercise Prob**: 10-30%
- **Stress Tests**: Mixed results

### For Out-of-The-Money (OTM) Covered Calls:
- **Risk Score**: 20-40 (low to moderate)
- **Early Exercise Prob**: <10%
- **Stress Tests**: Better in rising markets

## 🐛 Troubleshooting

### Button doesn't appear on portfolio cards:
**Cause**: Position doesn't have option data (strike/expiration missing)
**Solution**: This is expected - button only shows for option strategies

### Modal doesn't open when clicked:
**Check**: Browser console (F12) for JavaScript errors
**Check**: R console for error messages
**Likely cause**: Namespace issue or missing reactive trigger

### RQuantLib error in Greeks tab:
**This is OK**: Monte Carlo results will still work
**Common cause**: Edge case in option pricing (very deep ITM/OTM, or near expiration)
**Fallback**: Modal shows error message but other tabs still work

### Monte Carlo is slow (>30 seconds):
**Cause**: 10,000 paths with complex dividend schedule
**Solution**: This is normal for stocks with many dividends
**Alternative**: Can reduce paths in code if needed (edit mod_portfolio_groups_cards.R line 169)

### "No option data found for this group":
**Cause**: Group doesn't have option activity in database
**Solution**: Try different group, or verify activities table has option data

## 📝 What to Verify

### ✅ Checklist:
- [ ] Button appears on portfolio position cards (if options exist)
- [ ] Button click triggers loading modal
- [ ] Loading modal shows spinner
- [ ] Results modal opens after 5-10 seconds
- [ ] All 5 tabs are present
- [ ] Summary tab shows risk score and key metrics
- [ ] Distribution tab shows return percentiles
- [ ] Dividend Timeline tab shows projected dividends (if dividend-paying stock)
- [ ] Stress Tests tab shows 5 scenarios with P&L impacts
- [ ] Greeks tab shows Delta, Gamma, Vega, Theta, Rho
- [ ] Can close modal and click another card's button
- [ ] Button works independently for each position

### 💡 Test Scenarios:

**Scenario 1: Deep ITM Covered Call**
- Current price significantly above strike
- **Expected**: High risk score (60-80), high early exercise probability (40-70%)

**Scenario 2: ATM Covered Call**
- Current price near strike
- **Expected**: Medium risk score (40-60), moderate early exercise probability (20-40%)

**Scenario 3: OTM Covered Call**
- Current price below strike
- **Expected**: Low risk score (20-40), low early exercise probability (<20%)

**Scenario 4: LEAPS (>1 year to expiration)**
- Long time until expiration, multiple dividends
- **Expected**: Higher risk score (time = uncertainty), dividend timeline shows 4+ dividends

## 🔍 Technical Details

### Files Modified:
1. `R/mod_portfolio_groups_cards.R` - Added observeEvent for risk button clicks
2. `R/utils_group_cards.R` - Added "Analyze Risk" button to action_buttons section

### How It Works:
1. Button click sets Shiny input: `analyze_risk_group_clicked`
2. observeEvent triggers in mod_portfolio_groups_cards.R
3. Extracts position details from database (group, members, activities)
4. Parses option symbol to get strike and expiration
5. Calls `mod_position_risk_server()` with position parameters
6. Module runs async analysis using `future` (won't block UI)
7. Shows loading modal immediately
8. When analysis completes, replaces loading modal with results modal

### Data Flow:
```
Portfolio Group Card
  ↓ (user clicks "Analyze Risk")
Extract from database:
  - Ticker (from members table)
  - Strike & Expiration (from option activity symbol)
  - Premium (from option activity price × quantity)
  ↓
Pass to mod_position_risk_server()
  ↓
Call analyze_position_risk()
  ↓
Run Monte Carlo (10K paths) + RQuantLib + Stress Tests
  ↓
Return comprehensive risk profile
  ↓
Display in 5-tab modal
```

## 🎯 Success Criteria

**MVP is successful if:**
1. ✅ Button appears on portfolio cards with option data
2. ✅ Clicking button triggers analysis
3. ✅ Modal displays results within 10 seconds
4. ✅ Results are reasonable (risk scores make sense)
5. ✅ No crashes or blocking errors

**Optional enhancements (future):**
- Charts in Distribution tab (currently text-based)
- Database persistence
- Historical tracking
- Alerts for high-risk positions
- Extend to other strategy pages

## 📞 If You Find Issues

**Error messages to report:**
- Copy full error from R console
- Note which tab/button caused it
- Provide position details (ticker, strike, expiration)

**Performance issues:**
- Note how long analysis took
- Check number of dividends in timeline tab
- Check simulation paths used (shown in Summary tab footer)

## 🚀 Next Steps After Testing

**If testing successful:**
1. Decide if want same button on other strategies (Zero Dividend, Collar, Dynamic Covered Calls)
2. Consider adding histogram visualization to Distribution tab
3. Decide on database persistence strategy
4. Plan portfolio-level risk dashboard

**If issues found:**
1. Document error messages
2. Identify which component failed (Monte Carlo, RQuantLib, UI)
3. We can debug specific issues

---

## Quick Reference

**Test with portfolio positions** (fastest):
```
Portfolio Groups → Click "Analyze Risk" on any open position
```

**Test with strategy results** (slower):
```
Aristocrats → Run Strategy → Wait for cards → Click "Analyze Risk"
```

**Expected analysis time**: 5-10 seconds per position

**Libraries required**: RQuantLib, PerformanceAnalytics, MASS

**Files to check if errors**: R console, browser console (F12)
