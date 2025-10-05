#!/usr/bin/env Rscript
################################################################################
# DIAGNOSTIC SCRIPT - Run this to diagnose app issues
################################################################################

library(investR)

cat("\n")
cat("================================================================================\n")
cat("                    investR DIAGNOSTIC SCRIPT\n")
cat("================================================================================\n\n")

# Test 1: Package loading
cat("TEST 1: Package Loading\n")
cat("  ✓ investR package loaded successfully\n\n")

# Test 2: Analysis function
cat("TEST 2: Running analysis with 5 stocks (30 seconds)...\n")
results <- analyze_aristocrats(limit = 5, strike_threshold_pct = 0.8,
                                target_days = NULL, max_workers = 2)
cat("  Result: Found", nrow(results), "opportunities\n")
if (nrow(results) > 0) {
  cat("  ✓ Analysis working correctly\n\n")
} else {
  cat("  ✗ ERROR: No results from analysis\n\n")
}

# Test 3: Module availability
cat("TEST 3: Checking module functions\n")
modules_ok <- all(c(
  exists('mod_aristocrats_analysis_ui', where = asNamespace('investR')),
  exists('mod_aristocrats_analysis_server', where = asNamespace('investR')),
  exists('mod_aristocrats_results_table_ui', where = asNamespace('investR')),
  exists('mod_aristocrats_results_table_server', where = asNamespace('investR'))
))
if (modules_ok) {
  cat("  ✓ All module functions available\n\n")
} else {
  cat("  ✗ ERROR: Some modules missing\n\n")
}

# Test 4: App creation
cat("TEST 4: Creating Shiny app object\n")
app <- tryCatch({
  run_app()
}, error = function(e) {
  cat("  ✗ ERROR:", e$message, "\n\n")
  NULL
})

if (!is.null(app)) {
  cat("  ✓ App created successfully\n")
  cat("  App class:", class(app), "\n\n")
}

# Summary
cat("================================================================================\n")
cat("                             SUMMARY\n")
cat("================================================================================\n\n")

if (nrow(results) > 0 && modules_ok && !is.null(app)) {
  cat("✓ ALL TESTS PASSED\n\n")
  cat("Your app should work correctly. If you're seeing no results in the browser:\n\n")
  cat("SOLUTION:\n")
  cat("  1. Stop your Shiny app if it's running\n")
  cat("  2. In R console, run: devtools::load_all()\n")
  cat("  3. Then run: investR::run_app()\n")
  cat("  4. In the browser, click 'Run Analysis' button\n")
  cat("  5. Wait 2-3 minutes for results\n\n")
  cat("If still no results, check:\n")
  cat("  - Your internet connection (needs to fetch stock data)\n")
  cat("  - Browser console for JavaScript errors (F12)\n")
  cat("  - R console for error messages\n\n")
} else {
  cat("✗ SOME TESTS FAILED\n\n")
  cat("Please share the output above to diagnose the issue.\n\n")
}

cat("Sample results from test:\n")
if (nrow(results) > 0) {
  print(head(results[, c("ticker", "current_price", "strike", "annualized_return")], 3))
} else {
  cat("  No results to display\n")
}

cat("\n")