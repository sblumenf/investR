# investR Documentation Index

**Last Updated**: October 22, 2025

---

## Quick Navigation

### For Daily Usage

📖 **[RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md)** - Start here!
- Quick function reference
- Common use cases
- Copy-paste examples
- Troubleshooting guide
- ~10 minute read

### For Deep Understanding

📚 **[RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md)** - Complete technical reference
- Full system architecture
- Detailed algorithm explanations
- Mathematical formulas
- Academic references
- Configuration guide
- ~45 minute read

### For Testing & Validation

✅ **[TEST_RESULTS.md](TEST_RESULTS.md)** - Test suite status
- Test coverage summary
- Which tests validate which features
- How to run tests
- Known issues
- ~5 minute read

### For Historical Context

📊 **[RISK_ANALYSIS_REPORT.md](RISK_ANALYSIS_REPORT.md)** - Original upgrade analysis
- A- to A+ upgrade rationale
- Feature comparison
- Implementation decisions
- Academic rigor assessment
- ~30 minute read

---

## Documentation by Topic

### Getting Started

1. **[RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md)** - Quick Start section
   - Install and load package
   - First risk analysis
   - Interpret results

### Risk Analysis Features

#### Volatility Estimation
- **Technical**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#volatility-estimation) - Section 4
- **Quick Use**: [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#volatility) - Volatility section

#### Early Exercise (LSM)
- **Technical**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#early-exercise-analysis-lsm) - Section 5
- **Quick Use**: [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#early-exercise-lsm) - LSM section
- **Academic**: [RISK_ANALYSIS_REPORT.md](RISK_ANALYSIS_REPORT.md) - LSM Implementation section

#### Market Regime Detection
- **Technical**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#market-regime-detection) - Section 6
- **Quick Use**: [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#market-regime) - Regime section
- **Regimes Table**: [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#market-regimes)

#### Portfolio Risk
- **Technical**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#portfolio-risk-analysis) - Section 7
- **Quick Use**: [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#analyze-entire-portfolio)
- **Component VaR**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#component-var-calculation)

#### Position Risk
- **Technical**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#position-risk-analysis) - Section 8
- **Quick Use**: [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#analyze-a-single-position)

#### Stress Testing
- **Technical**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#stress-testing) - Section 10
- **Scenarios**: [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#stress-test-scenarios)

### Configuration

- **Full Details**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#configuration-system) - Section 11
- **Quick Reference**: [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#configuration-quick-reference)

### Testing

- **Test Status**: [TEST_RESULTS.md](TEST_RESULTS.md)
- **Test Details**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#testing--validation) - Section 12
- **Running Tests**: [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#testing-your-changes)

---

## Documentation by User Type

### I'm a Portfolio Manager (Non-Technical)

**Start Here**: [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md)

**Key Sections**:
1. Quick Start → Analyze a Single Position
2. Market Regimes → Understanding current conditions
3. Risk Metrics Explained → What the numbers mean
4. Interpretation Guide → When to take action
5. Shiny Dashboard → Using the GUI

**Optional**: [RISK_ANALYSIS_REPORT.md](RISK_ANALYSIS_REPORT.md) - Executive Summary

### I'm a Developer/Analyst (Technical)

**Start Here**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md)

**Key Sections**:
1. System Architecture → How it all fits together
2. Core Components → Deep dive into each module
3. API Reference → Function signatures
4. Testing & Validation → Verify your changes

**Reference**: [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md) for day-to-day lookups

### I'm a Quantitative Researcher (Academic)

**Start Here**: [RISK_ANALYSIS_REPORT.md](RISK_ANALYSIS_REPORT.md)

**Key Sections**:
1. Academic Rigor Assessment → Model choices
2. LSM Implementation → Algorithm details
3. Jump-Diffusion → Model parameters

**Then Read**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md)
- Mathematical formulas in each section
- Academic References section
- Implementation details

### I'm QA/Testing

**Start Here**: [TEST_RESULTS.md](TEST_RESULTS.md)

**Key Sections**:
1. New Feature Tests → What we're validating
2. Test Coverage by Feature → Completeness check
3. How to Run Tests → Command reference

**Reference**: [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#testing--validation)

---

## Common Questions → Documentation Mapping

### "How do I analyze a position's risk?"
→ [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#analyze-a-single-position)

### "What does VaR mean?"
→ [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#risk-metrics-explained)

### "How does the LSM algorithm work?"
→ [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#early-exercise-analysis-lsm)

### "What's the current market regime?"
→ [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#check-current-market-regime)

### "How do I configure simulation paths?"
→ [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#configuration-quick-reference)

### "Which tests validate the new features?"
→ [TEST_RESULTS.md](TEST_RESULTS.md#new-feature-tests-all-passing-)

### "What changed from version 1.0 to 2.0?"
→ [RISK_ANALYSIS_REPORT.md](RISK_ANALYSIS_REPORT.md) - Feature Comparison

### "How do I interpret early exercise probability?"
→ [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#interpretation-guide)

### "What are the stress test scenarios?"
→ [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md#stress-test-scenarios)

### "How accurate is the Black-Scholes pricing?"
→ [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md#black_scholes_price)

---

## Code Examples by Task

### Analyze Single Position
```r
# See: RISK_QUICK_REFERENCE.md - "Analyze a Single Position"
result <- analyze_position_risk(
  ticker = "AAPL",
  strike = 155,
  premium_received = 5.00,
  expiration_date = as.Date("2025-12-19"),
  purchase_price = 145
)
```

### Check Market Regime
```r
# See: RISK_QUICK_REFERENCE.md - "Check Current Market Regime"
show_current_regime()
```

### Compare Multiple Positions
```r
# See: RISK_QUICK_REFERENCE.md - "Compare Positions"
positions <- c("AAPL", "MSFT", "GOOGL")
results <- purrr::map(positions, ~analyze_position_risk(...))
```

### Find Concentration Risk
```r
# See: RISK_QUICK_REFERENCE.md - "Find Concentration Risk"
portfolio <- analyze_portfolio_risk()
portfolio$concentration$alerts
```

### Run Custom Stress Test
```r
# See: RISK_MANAGEMENT_DOCUMENTATION.md - "Stress Testing"
stress <- run_stress_tests(ticker = "AAPL", ...)
```

---

## File Locations

All documentation is in the project root directory:

```
investR/
├── DOCUMENTATION_INDEX.md              ← You are here
├── RISK_QUICK_REFERENCE.md             ← Daily use
├── RISK_MANAGEMENT_DOCUMENTATION.md    ← Technical reference
├── RISK_ANALYSIS_REPORT.md             ← Upgrade rationale
└── TEST_RESULTS.md                     ← Test status
```

---

## Recommended Reading Order

### First Time Users
1. [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md) - Quick Start (5 min)
2. [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md) - Market Regimes (2 min)
3. [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md) - Risk Metrics Explained (5 min)
4. Try running your first analysis
5. [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md) - Browse as needed

### Developers Adding Features
1. [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md) - System Architecture (10 min)
2. [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md) - Relevant component section (15 min)
3. [TEST_RESULTS.md](TEST_RESULTS.md) - Test Coverage (5 min)
4. Review test files in `tests/testthat/`
5. [RISK_QUICK_REFERENCE.md](RISK_QUICK_REFERENCE.md) - Testing Your Changes (2 min)

### Code Reviewers
1. [RISK_ANALYSIS_REPORT.md](RISK_ANALYSIS_REPORT.md) - Upgrade rationale (20 min)
2. [RISK_MANAGEMENT_DOCUMENTATION.md](RISK_MANAGEMENT_DOCUMENTATION.md) - Architecture + relevant sections (30 min)
3. [TEST_RESULTS.md](TEST_RESULTS.md) - Verify test coverage (5 min)

---

## Updates and Maintenance

**Documentation Owner**: investR Development Team

**Last Major Update**: October 22, 2025 (Version 2.0 release)

**Update Frequency**:
- **RISK_QUICK_REFERENCE.md**: Updated with each feature addition
- **RISK_MANAGEMENT_DOCUMENTATION.md**: Updated with major changes
- **TEST_RESULTS.md**: Updated after test suite changes
- **RISK_ANALYSIS_REPORT.md**: Historical document (stable)

**Contributing**: If you find errors or have suggestions, please:
1. Create an issue on GitHub
2. Submit a pull request with corrections
3. Contact the development team

---

## Cheat Sheet

| I Want To...                          | Read This                                              |
|---------------------------------------|--------------------------------------------------------|
| Analyze a position NOW               | [Quick Ref - Quick Start](RISK_QUICK_REFERENCE.md#quick-start) |
| Understand what VaR means            | [Quick Ref - Risk Metrics](RISK_QUICK_REFERENCE.md#risk-metrics-explained) |
| Learn LSM algorithm details          | [Tech Docs - LSM Section](RISK_MANAGEMENT_DOCUMENTATION.md#early-exercise-analysis-lsm) |
| Find all test results                | [TEST_RESULTS.md](TEST_RESULTS.md) |
| Understand the A+ upgrade            | [RISK_ANALYSIS_REPORT.md](RISK_ANALYSIS_REPORT.md) |
| Check current market regime          | [Quick Ref - Regime](RISK_QUICK_REFERENCE.md#market-regime) |
| Configure simulation parameters      | [Tech Docs - Config](RISK_MANAGEMENT_DOCUMENTATION.md#configuration-system) |
| See usage examples                   | [Tech Docs - Examples](RISK_MANAGEMENT_DOCUMENTATION.md#usage-examples) |
| Troubleshoot errors                  | [Quick Ref - Error Messages](RISK_QUICK_REFERENCE.md#error-messages) |

---

**Welcome to the investR Risk Management System!**

Start with the [Quick Reference](RISK_QUICK_REFERENCE.md) and explore from there.
