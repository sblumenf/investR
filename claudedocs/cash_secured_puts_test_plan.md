# Cash-Secured Puts Testing Plan

## Date: 2025-11-23
## Version: 1.0
## Status: Ready for Execution

---

## 1. Executive Summary

This document provides a comprehensive testing strategy for the cash-secured puts feature, covering unit tests, integration tests, edge cases, performance testing, and manual testing procedures.

**Test Coverage Goals:**
- **Unit Tests**: 90%+ coverage for all new functions
- **Integration Tests**: End-to-end workflow validation
- **Edge Cases**: Comprehensive boundary and error condition testing
- **Performance**: Parallel processing validation
- **Manual Testing**: UI and user experience verification

---

## 2. Critical Test Cases

### 2.1 Strike Selection Logic (HIGHEST PRIORITY)

**Why Critical**: This is the **INVERTED** logic from calls - getting it wrong means selecting completely wrong options.

**Test Scenarios:**

```r
# Scenario 1: 95% threshold with $100 stock
# Expected: strikes >= $95 (all OTM/ATM puts)
current_price = $100
threshold = 0.95
strike_threshold = $95
Expected strikes: $95, $96, $97, $98, $99, $100, $101+
NOT expected: $94, $93, $92 (too deep OTM)

# Scenario 2: 90% threshold with $150 stock (more aggressive)
current_price = $150
threshold = 0.90
strike_threshold = $135
Expected strikes: $135, $140, $145, $150+
NOT expected: $130, $125 (too deep OTM)

# Scenario 3: Edge case at exact threshold
current_price = $100
threshold = 0.95
strike_threshold = $95.00
Strike at $95.00 should be INCLUDED (>= comparison)
```

**Key Validation:**
```r
# For PUTS: higher threshold % = MORE conservative (closer to ATM)
# 95% threshold → strikes within 5% of current price
# 90% threshold → strikes within 10% of current price

# INVERTED from CALLS:
# Calls filter: Strike <= threshold (want ITM calls)
# Puts filter: Strike >= threshold (want OTM puts)
```

### 2.2 Metric Calculations (HIGH PRIORITY)

**Cash Flow Calculations:**
```r
# Test Case 1: Standard put
strike = $95
bid = $1.50
Expected:
  cash_required = $9,500 (strike × 100)
  premium_received = $150 (bid × 100)
  net_outlay = $9,350 (cash_required - premium)

# Test Case 2: Low-priced stock
strike = $25
bid = $0.75
Expected:
  cash_required = $2,500
  premium_received = $75
  net_outlay = $2,425
```

**Protection Metrics:**
```r
# Test Case 1: OTM put
current_price = $100
strike = $95
bid = $1.50
Expected:
  breakeven_price = $93.50 (strike - bid)
  downside_protection_pct = 0.065 (6.5% = (100-93.50)/100)

# Test Case 2: ATM put
current_price = $100
strike = $100
bid = $3.00
Expected:
  breakeven_price = $97.00
  downside_protection_pct = 0.03 (3%)
```

**Return Calculations:**
```r
# Test Case 1: 90-day put
strike = $95
bid = $1.50
days = 90
Expected:
  return_on_cash = 0.0158 (1.58% = 150/9500)
  annualized_return = 0.0641 (6.41% = 0.0158 × 365/90)

# Test Case 2: 45-day put (shorter duration = higher annualized)
strike = $95
bid = $1.50
days = 45
Expected:
  return_on_cash = 0.0158 (same)
  annualized_return = 0.1282 (12.82% = 0.0158 × 365/45)
```

### 2.3 Option Value Decomposition

**Intrinsic Value (for puts: ITM when strike > current_price):**
```r
# Test Case 1: OTM put (strike < current_price)
current_price = $100
strike = $95
bid = $1.50
Expected:
  intrinsic_value = $0 (max(0, strike - current_price) = max(0, -5) = 0)
  extrinsic_value = $1.50 (bid - intrinsic = 1.50 - 0)

# Test Case 2: ITM put (strike > current_price)
current_price = $100
strike = $105
bid = $6.00
Expected:
  intrinsic_value = $5.00 (max(0, 105 - 100))
  extrinsic_value = $1.00 (6.00 - 5.00)

# Test Case 3: ATM put (strike = current_price)
current_price = $100
strike = $100
bid = $3.00
Expected:
  intrinsic_value = $0 (max(0, 100 - 100))
  extrinsic_value = $3.00 (all time value)
```

---

## 3. Unit Test Specifications

### 3.1 Configuration Tests (`test-utils_cash_secured_puts_config.R`)

```r
test_that("CASH_SECURED_PUTS_CONFIG has all required fields", {
  expect_true("strike_threshold_pct" %in% names(CASH_SECURED_PUTS_CONFIG))
  expect_true("min_days" %in% names(CASH_SECURED_PUTS_CONFIG))
  expect_true("max_days" %in% names(CASH_SECURED_PUTS_CONFIG))
  expect_true("max_workers" %in% names(CASH_SECURED_PUTS_CONFIG))
  expect_true("min_option_bid" %in% names(CASH_SECURED_PUTS_CONFIG))
  expect_true("min_open_interest" %in% names(CASH_SECURED_PUTS_CONFIG))
  expect_true("max_stock_price" %in% names(CASH_SECURED_PUTS_CONFIG))
  expect_true("shares_per_contract" %in% names(CASH_SECURED_PUTS_CONFIG))
  expect_true("days_per_year" %in% names(CASH_SECURED_PUTS_CONFIG))
})

test_that("validate_puts_config accepts valid configuration", {
  expect_true(validate_puts_config(CASH_SECURED_PUTS_CONFIG))
})

test_that("validate_puts_config rejects invalid strike threshold", {
  bad_config <- CASH_SECURED_PUTS_CONFIG

  # Too high
  bad_config$strike_threshold_pct <- 1.5
  expect_error(validate_puts_config(bad_config), "strike_threshold_pct")

  # Too low
  bad_config$strike_threshold_pct <- 0.3
  expect_error(validate_puts_config(bad_config), "strike_threshold_pct")

  # Negative
  bad_config$strike_threshold_pct <- -0.5
  expect_error(validate_puts_config(bad_config), "strike_threshold_pct")
})

test_that("validate_puts_config rejects invalid days", {
  bad_config <- CASH_SECURED_PUTS_CONFIG

  # max_days < min_days
  bad_config$min_days <- 100
  bad_config$max_days <- 50
  expect_error(validate_puts_config(bad_config), "max_days")

  # Negative min_days
  bad_config$min_days <- -10
  expect_error(validate_puts_config(bad_config), "min_days")
})

test_that("validate_puts_config rejects invalid workers", {
  bad_config <- CASH_SECURED_PUTS_CONFIG

  # Too many workers
  bad_config$max_workers <- 100
  expect_error(validate_puts_config(bad_config), "max_workers")

  # Zero workers
  bad_config$max_workers <- 0
  expect_error(validate_puts_config(bad_config), "max_workers")
})

test_that("get_puts_config returns default config", {
  config <- get_puts_config()
  expect_equal(config, CASH_SECURED_PUTS_CONFIG)
})

test_that("get_puts_config accepts valid overrides", {
  config <- get_puts_config(
    strike_threshold_pct = 0.90,
    max_workers = 4
  )
  expect_equal(config$strike_threshold_pct, 0.90)
  expect_equal(config$max_workers, 4)
})

test_that("get_puts_config validates overridden config", {
  expect_error(
    get_puts_config(strike_threshold_pct = 1.5),
    "strike_threshold_pct"
  )
})

test_that("get_puts_config warns on unknown parameters", {
  expect_warning(
    get_puts_config(unknown_param = 123),
    "Unknown config parameter"
  )
})
```

### 3.2 Metric Calculation Tests (`test-fct_cash_secured_puts.R`)

```r
test_that("calculate_put_cash_flows computes correctly", {
  result <- calculate_put_cash_flows(strike = 100, bid_price = 2)

  expect_equal(result$cash_required, 10000)    # 100 * 100 shares
  expect_equal(result$premium_received, 200)   # 2 * 100 shares
  expect_equal(result$net_outlay, 9800)        # 10000 - 200
})

test_that("calculate_put_cash_flows handles decimals", {
  result <- calculate_put_cash_flows(strike = 95.50, bid_price = 1.75)

  expect_equal(result$cash_required, 9550)     # 95.50 * 100
  expect_equal(result$premium_received, 175)   # 1.75 * 100
  expect_equal(result$net_outlay, 9375)        # 9550 - 175
})

test_that("calculate_put_cash_flows rejects invalid inputs", {
  expect_error(calculate_put_cash_flows(strike = -100, bid_price = 2))
  expect_error(calculate_put_cash_flows(strike = 100, bid_price = -2))
  expect_error(calculate_put_cash_flows(strike = 0, bid_price = 2))
})

test_that("calculate_put_protection_metrics calculates breakeven correctly", {
  result <- calculate_put_protection_metrics(
    current_price = 150,
    strike = 140,
    bid_price = 2
  )

  expect_equal(result$breakeven_price, 138)    # 140 - 2
  expect_equal(result$downside_protection_pct, 0.08)  # (150-138)/150 = 8%
})

test_that("calculate_put_protection_metrics handles OTM puts", {
  result <- calculate_put_protection_metrics(
    current_price = 100,
    strike = 95,
    bid_price = 1.50
  )

  expect_equal(result$breakeven_price, 93.50)  # 95 - 1.50
  expect_equal(result$downside_protection_pct, 0.065)  # (100-93.50)/100 = 6.5%
})

test_that("calculate_put_protection_metrics handles ATM puts", {
  result <- calculate_put_protection_metrics(
    current_price = 100,
    strike = 100,
    bid_price = 3.00
  )

  expect_equal(result$breakeven_price, 97.00)  # 100 - 3
  expect_equal(result$downside_protection_pct, 0.03)  # (100-97)/100 = 3%
})

test_that("calculate_put_return_metrics calculates correctly", {
  result <- calculate_put_return_metrics(
    premium_received = 150,
    cash_required = 9500,
    days_to_expiry = 90
  )

  expect_equal(result$return_on_cash, 150 / 9500, tolerance = 0.0001)
  # Annualized: (150/9500) * (365/90) = 0.0641
  expect_equal(result$annualized_return, 0.0641, tolerance = 0.0001)
})

test_that("calculate_put_return_metrics annualizes correctly", {
  premium <- 150
  cash <- 9500
  return_on_cash <- premium / cash  # 0.0158

  # 45-day option should have higher annualized return
  result_45 <- calculate_put_return_metrics(premium, cash, 45)
  expect_equal(result_45$annualized_return, return_on_cash * (365/45), tolerance = 0.0001)

  # 90-day option should have lower annualized return
  result_90 <- calculate_put_return_metrics(premium, cash, 90)
  expect_equal(result_90$annualized_return, return_on_cash * (365/90), tolerance = 0.0001)

  # Verify 45-day > 90-day
  expect_gt(result_45$annualized_return, result_90$annualized_return)
})

test_that("calculate_put_return_metrics rejects invalid inputs", {
  expect_error(calculate_put_return_metrics(150, 0, 90))      # zero cash
  expect_error(calculate_put_return_metrics(150, -9500, 90))  # negative cash
  expect_error(calculate_put_return_metrics(150, 9500, 0))    # zero days
  expect_error(calculate_put_return_metrics(150, 9500, -90))  # negative days
})
```

### 3.3 Strike Selection Tests (CRITICAL)

```r
test_that("select_optimal_put filters strikes correctly with 95% threshold", {
  # Test data: strikes around $100 current price
  test_options <- tibble(
    Strike = c(85, 90, 95, 96, 97, 98, 99, 100, 105),
    Bid = c(0.50, 0.75, 1.50, 1.60, 1.70, 1.80, 2.00, 3.00, 5.00),
    OI = c(50, 75, 100, 100, 100, 100, 100, 150, 200),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  current_price <- 100
  threshold <- 0.95

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = current_price,
    options_df = test_options,
    strike_threshold_pct = threshold
  )

  # Should select strikes >= 95 (95, 96, 97, 98, 99, 100, 105)
  # Should NOT select: 85, 90 (too deep OTM)
  # Should select longest dated with highest OI: strike 105
  expect_false(is.null(result))
  expect_equal(result$option$Strike, 105)
})

test_that("select_optimal_put filters strikes correctly with 90% threshold", {
  test_options <- tibble(
    Strike = c(85, 90, 95, 100, 105),
    Bid = c(0.50, 1.00, 1.50, 3.00, 5.00),
    OI = c(50, 100, 150, 200, 250),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  current_price <- 100
  threshold <- 0.90  # More aggressive

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = current_price,
    options_df = test_options,
    strike_threshold_pct = threshold
  )

  # Strike threshold = 100 * 0.90 = 90
  # Should select strikes >= 90 (90, 95, 100, 105)
  # Should NOT select: 85
  expect_false(is.null(result))
  expect_gte(result$option$Strike, 90)
})

test_that("select_optimal_put handles exact threshold boundary", {
  test_options <- tibble(
    Strike = c(94.99, 95.00, 95.01),
    Bid = c(1.45, 1.50, 1.55),
    OI = c(100, 100, 100),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  current_price <- 100
  threshold <- 0.95

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = current_price,
    options_df = test_options,
    strike_threshold_pct = threshold
  )

  # Strike threshold = 100 * 0.95 = 95.00
  # With >= comparison, should select 95.00 and 95.01
  # Should NOT select 94.99
  expect_false(is.null(result))
  expect_gte(result$option$Strike, 95.00)
})

test_that("select_optimal_put selects longest expiration", {
  test_options <- tibble(
    Strike = c(95, 95, 95),
    Bid = c(1.50, 2.00, 2.50),
    OI = c(100, 100, 100),
    expiration = as.Date(c("2025-02-21", "2025-03-21", "2025-04-18")),
    days_to_expiry = c(60, 90, 120)
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  # Should select longest dated: 2025-04-18
  expect_equal(result$option$expiration, as.Date("2025-04-18"))
})

test_that("select_optimal_put filters by min/max days", {
  test_options <- tibble(
    Strike = c(95, 95, 95, 95),
    Bid = c(1.00, 1.50, 2.00, 2.50),
    OI = c(100, 100, 100, 100),
    expiration = as.Date(c("2025-01-31", "2025-02-28", "2025-03-31", "2025-05-30")),
    days_to_expiry = c(30, 60, 90, 150)
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95,
    min_days = 45,
    max_days = 120
  )

  # Should only consider: 60-day and 90-day options
  # Should select longest: 90-day
  expect_equal(result$option$days_to_expiry, 90)
})

test_that("select_optimal_put filters by minimum bid", {
  test_options <- tibble(
    Strike = c(95, 95, 95),
    Bid = c(0.005, 0.01, 1.50),  # One below min, one at min, one above
    OI = c(100, 100, 100),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  # Should filter out 0.005 bid (below min_option_bid = 0.01)
  # Should keep 0.01 and 1.50
  # Should select highest value option
  expect_gte(result$option$Bid, CASH_SECURED_PUTS_CONFIG$min_option_bid)
})

test_that("select_optimal_put filters by minimum open interest", {
  test_options <- tibble(
    Strike = c(95, 95, 95),
    Bid = c(1.50, 1.60, 1.70),
    OI = c(5, 10, 50),  # One below min, one at min, one above
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  # Should filter out OI < 10
  # Should keep OI >= 10
  expect_gte(result$option$OI, CASH_SECURED_PUTS_CONFIG$min_open_interest)
})

test_that("select_optimal_put returns NULL when no options meet criteria", {
  test_options <- tibble(
    Strike = c(85, 90),  # All too deep OTM
    Bid = c(0.50, 0.75),
    OI = c(50, 75),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95  # Needs strikes >= 95
  )

  expect_null(result)
})

test_that("select_optimal_put returns NULL for empty options", {
  test_options <- tibble(
    Strike = numeric(0),
    Bid = numeric(0),
    OI = numeric(0),
    expiration = as.Date(character(0)),
    days_to_expiry = numeric(0)
  )

  result <- select_optimal_put(
    ticker = "TEST",
    current_price = 100,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  expect_null(result)
})
```

### 3.4 Option Value Tests

```r
test_that("calculate_put_metrics computes intrinsic value correctly for OTM put", {
  # OTM put: strike < current_price
  option_row <- tibble(
    Strike = 95,
    Bid = 1.50,
    days_to_expiry = 90,
    expiration = as.Date("2025-03-21"),
    OI = 100
  )

  stock_data <- list(
    company_name = "Test Corp",
    current_price = 100,
    max_drawdown = -0.15,
    current_yield = 0.02,
    annual_dividend = 2.00
  )

  result <- calculate_put_metrics(
    ticker = "TEST",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  # OTM put: intrinsic = max(0, strike - current) = max(0, 95 - 100) = 0
  expect_equal(result$intrinsic_value, 0)
  expect_equal(result$extrinsic_value, 1.50)  # All time value
})

test_that("calculate_put_metrics computes intrinsic value correctly for ITM put", {
  # ITM put: strike > current_price
  option_row <- tibble(
    Strike = 105,
    Bid = 6.00,
    days_to_expiry = 90,
    expiration = as.Date("2025-03-21"),
    OI = 100
  )

  stock_data <- list(
    company_name = "Test Corp",
    current_price = 100,
    max_drawdown = -0.15,
    current_yield = 0.02,
    annual_dividend = 2.00
  )

  result <- calculate_put_metrics(
    ticker = "TEST",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  # ITM put: intrinsic = max(0, strike - current) = max(0, 105 - 100) = 5
  expect_equal(result$intrinsic_value, 5.00)
  expect_equal(result$extrinsic_value, 1.00)  # 6.00 - 5.00 time value
})

test_that("calculate_put_metrics computes intrinsic value correctly for ATM put", {
  # ATM put: strike = current_price
  option_row <- tibble(
    Strike = 100,
    Bid = 3.00,
    days_to_expiry = 90,
    expiration = as.Date("2025-03-21"),
    OI = 100
  )

  stock_data <- list(
    company_name = "Test Corp",
    current_price = 100,
    max_drawdown = -0.15,
    current_yield = 0.02,
    annual_dividend = 2.00
  )

  result <- calculate_put_metrics(
    ticker = "TEST",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  # ATM put: intrinsic = max(0, strike - current) = max(0, 100 - 100) = 0
  expect_equal(result$intrinsic_value, 0)
  expect_equal(result$extrinsic_value, 3.00)  # All time value
})
```

---

## 4. Integration Tests

### 4.1 End-to-End Workflow Tests

```r
test_that("analyze_cash_secured_puts works with small limit", {
  skip_on_cran()  # Requires API access
  skip_if_offline()

  # Test with minimal stocks to avoid long test times
  results <- analyze_cash_secured_puts(
    limit = 3,
    strike_threshold_pct = 0.95,
    min_days = 30,
    max_days = 90,
    max_workers = 2
  )

  # Should return a data frame (may be empty if no opportunities)
  expect_s3_class(results, "data.frame")

  # If results found, verify structure
  if (nrow(results) > 0) {
    required_cols <- c(
      "ticker", "company_name", "current_price", "strike",
      "expiration", "days_to_expiry", "bid_price", "open_interest",
      "cash_required", "premium_received", "net_outlay",
      "return_on_cash", "annualized_return",
      "max_drawdown", "current_yield",
      "breakeven_price", "downside_protection_pct",
      "intrinsic_value", "extrinsic_value", "annual_dividend",
      "warning_flag", "is_put", "is_aristocrat"
    )
    expect_true(all(required_cols %in% names(results)))

    # Verify strike selection logic (all strikes >= threshold)
    for (i in seq_len(nrow(results))) {
      expect_gte(
        results$strike[i],
        results$current_price[i] * 0.95,
        info = sprintf("Row %d: strike %.2f should be >= %.2f (95%% of %.2f)",
                      i, results$strike[i],
                      results$current_price[i] * 0.95,
                      results$current_price[i])
      )
    }

    # Verify metric calculations
    for (i in seq_len(nrow(results))) {
      # Cash required = strike × 100
      expect_equal(results$cash_required[i], results$strike[i] * 100,
                  tolerance = 0.01)

      # Premium = bid × 100
      expect_equal(results$premium_received[i], results$bid_price[i] * 100,
                  tolerance = 0.01)

      # Net outlay = cash - premium
      expect_equal(results$net_outlay[i],
                  results$cash_required[i] - results$premium_received[i],
                  tolerance = 0.01)

      # Return on cash = premium / cash
      expect_equal(results$return_on_cash[i],
                  results$premium_received[i] / results$cash_required[i],
                  tolerance = 0.0001)

      # Breakeven = strike - bid
      expect_equal(results$breakeven_price[i],
                  results$strike[i] - results$bid_price[i],
                  tolerance = 0.01)
    }
  }
})

test_that("analyze_puts_generic works with custom universe", {
  skip_on_cran()
  skip_if_offline()

  # Test with known liquid stocks
  custom_stocks <- c("AAPL", "MSFT")

  results <- analyze_puts_generic(
    stock_universe = custom_stocks,
    strategy_name = "Test Custom Puts",
    strike_threshold_pct = 0.95,
    min_days = 30,
    max_days = 90,
    max_workers = 2
  )

  expect_s3_class(results, "data.frame")

  # If results found, verify they're from our custom universe
  if (nrow(results) > 0) {
    expect_true(all(results$ticker %in% custom_stocks))
  }
})
```

### 4.2 Data Source Integration Tests

```r
test_that("get_options_chain_puts retrieves put options", {
  skip_on_cran()
  skip_if_offline()

  # Test with liquid stock
  options <- get_options_chain_puts("AAPL", 150)

  # Should return tibble (may be empty)
  expect_s3_class(options, "tbl_df")

  # If options found, verify structure
  if (nrow(options) > 0) {
    expect_true("Strike" %in% names(options))
    expect_true("Bid" %in% names(options))
    expect_true("expiration" %in% names(options))
    expect_true("days_to_expiry" %in% names(options))
    expect_true("OI" %in% names(options))

    # Verify all bids are valid
    expect_true(all(options$Bid >= CASH_SECURED_PUTS_CONFIG$min_option_bid))

    # Verify expirations are in the future
    expect_true(all(options$expiration > Sys.Date()))
  }
})

test_that("get_options_chain_puts handles quote source toggle", {
  skip_on_cran()
  skip_if_offline()

  # Test Questrade source
  options(investR.quote_source = "questrade")
  options_q <- get_options_chain_puts("AAPL", 150)
  expect_s3_class(options_q, "tbl_df")

  # Test Yahoo source
  options(investR.quote_source = "yahoo")
  options_y <- get_options_chain_puts("AAPL", 150)
  expect_s3_class(options_y, "tbl_df")
})
```

---

## 5. Edge Case Tests

### 5.1 Boundary Conditions

```r
test_that("select_optimal_put handles very low stock price", {
  test_options <- tibble(
    Strike = c(2.50, 5.00),
    Bid = c(0.05, 0.10),
    OI = c(50, 100),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "PENNY",
    current_price = 5.50,
    options_df = test_options,
    strike_threshold_pct = 0.90
  )

  # Threshold = 5.50 * 0.90 = 4.95
  # Should select strike 5.00
  expect_false(is.null(result))
  expect_equal(result$option$Strike, 5.00)
})

test_that("select_optimal_put handles very high stock price", {
  test_options <- tibble(
    Strike = c(400, 450, 500),
    Bid = c(5.00, 10.00, 15.00),
    OI = c(50, 100, 150),
    expiration = as.Date("2025-03-21"),
    days_to_expiry = 90
  )

  result <- select_optimal_put(
    ticker = "EXPENSIVE",
    current_price = 500,
    options_df = test_options,
    strike_threshold_pct = 0.95
  )

  # Threshold = 500 * 0.95 = 475
  # Should select strikes >= 475: only 500
  expect_false(is.null(result))
  expect_equal(result$option$Strike, 500)
})

test_that("calculate_put_metrics handles very small premium", {
  option_row <- tibble(
    Strike = 10,
    Bid = 0.05,  # Very small premium
    days_to_expiry = 30,
    expiration = as.Date("2025-02-21"),
    OI = 50
  )

  stock_data <- list(
    company_name = "Test Corp",
    current_price = 10.50,
    max_drawdown = -0.15,
    current_yield = 0.02,
    annual_dividend = 0.20
  )

  result <- calculate_put_metrics(
    ticker = "TEST",
    current_price = 10.50,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  # Should handle small numbers correctly
  expect_equal(result$premium_received, 5.00)  # 0.05 * 100
  expect_equal(result$cash_required, 1000)     # 10 * 100
  expect_gt(result$return_on_cash, 0)
})

test_that("calculate_put_metrics handles very short expiration", {
  option_row <- tibble(
    Strike = 95,
    Bid = 0.50,
    days_to_expiry = 7,  # Very short expiration
    expiration = as.Date(Sys.Date() + 7),
    OI = 100
  )

  stock_data <- list(
    company_name = "Test Corp",
    current_price = 100,
    max_drawdown = -0.15,
    current_yield = 0.02,
    annual_dividend = 2.00
  )

  result <- calculate_put_metrics(
    ticker = "TEST",
    current_price = 100,
    option_row = option_row,
    stock_data = stock_data,
    warning_flag = FALSE
  )

  # Short expiration should result in very high annualized return
  # but low absolute return
  expect_gt(result$annualized_return, result$return_on_cash)
  expect_gt(result$annualized_return, 1.0)  # > 100% annualized for 7 days
})
```

### 5.2 Error Handling Tests

```r
test_that("analyze_single_stock_put handles missing stock data", {
  # Invalid ticker
  result <- analyze_single_stock_put(
    ticker = "INVALID123",
    strike_threshold_pct = 0.95,
    min_days = 45,
    max_days = 120,
    return_failure_reason = TRUE
  )

  # Should return failure reason, not throw error
  expect_true(!is.null(result$failure_reason) || is.null(result))
})

test_that("analyze_single_stock_put handles stocks above max price", {
  # Mock get_stock_data to return expensive stock
  with_mock(
    get_stock_data = function(ticker) {
      list(
        company_name = "Expensive Corp",
        current_price = 500,  # Above max_stock_price = 250
        max_drawdown = -0.15,
        current_yield = 0.01,
        annual_dividend = 5.00
      )
    },
    {
      result <- analyze_single_stock_put(
        ticker = "EXPENSIVE",
        strike_threshold_pct = 0.95,
        return_failure_reason = TRUE
      )

      # Should skip due to price limit
      expect_true(!is.null(result$failure_reason) &&
                 grepl("exceeds max", result$failure_reason, ignore.case = TRUE))
    }
  )
})

test_that("analyze_single_stock_put handles no options available", {
  # Mock functions to return empty options
  with_mock(
    get_stock_data = function(ticker) {
      list(company_name = "Test", current_price = 100,
           max_drawdown = -0.15, current_yield = 0.02, annual_dividend = 2.00)
    },
    get_options_chain_puts = function(ticker, price) { tibble() },
    {
      result <- analyze_single_stock_put(
        ticker = "NOOPTS",
        strike_threshold_pct = 0.95,
        return_failure_reason = TRUE
      )

      expect_true(!is.null(result$failure_reason) &&
                 grepl("No put options", result$failure_reason, ignore.case = TRUE))
    }
  )
})

test_that("validate inputs reject invalid values", {
  # Invalid strike
  expect_error(calculate_put_cash_flows(strike = -100, bid_price = 2))
  expect_error(calculate_put_cash_flows(strike = 0, bid_price = 2))

  # Invalid bid
  expect_error(calculate_put_cash_flows(strike = 100, bid_price = -2))

  # Invalid protection metrics
  expect_error(calculate_put_protection_metrics(-100, 95, 1.50))
  expect_error(calculate_put_protection_metrics(100, -95, 1.50))
  expect_error(calculate_put_protection_metrics(100, 95, -1.50))
})
```

---

## 6. Performance Tests

### 6.1 Parallel Processing Tests

```r
test_that("parallel processing works with different worker counts", {
  skip_on_cran()
  skip_if_offline()

  test_stocks <- c("AAPL", "MSFT", "JNJ", "PG", "KO")

  # Test with 1 worker (sequential)
  start_time <- Sys.time()
  results_1 <- analyze_puts_generic(
    stock_universe = test_stocks,
    strategy_name = "Test 1 Worker",
    max_workers = 1
  )
  time_1 <- difftime(Sys.time(), start_time, units = "secs")

  # Test with 4 workers (parallel)
  start_time <- Sys.time()
  results_4 <- analyze_puts_generic(
    stock_universe = test_stocks,
    strategy_name = "Test 4 Workers",
    max_workers = 4
  )
  time_4 <- difftime(Sys.time(), start_time, units = "secs")

  # Parallel should be faster (or similar if overhead dominates)
  expect_lte(time_4, time_1 * 1.2)  # Allow 20% margin for overhead

  # Results should be identical (order may differ)
  expect_equal(nrow(results_1), nrow(results_4))
})

test_that("parallel processing handles failures gracefully", {
  skip_on_cran()

  # Mix valid and invalid tickers
  mixed_stocks <- c("AAPL", "INVALID123", "MSFT", "BADTICKER")

  results <- analyze_puts_generic(
    stock_universe = mixed_stocks,
    strategy_name = "Test Mixed Stocks",
    max_workers = 2
  )

  # Should complete without error
  expect_s3_class(results, "data.frame")

  # Should only have results from valid tickers
  if (nrow(results) > 0) {
    expect_true(all(results$ticker %in% c("AAPL", "MSFT")))
  }
})
```

### 6.2 Large Dataset Tests

```r
test_that("analyze_cash_secured_puts handles full aristocrats list", {
  skip_on_cran()
  skip("Long-running test - run manually")

  # Full aristocrats analysis (no limit)
  start_time <- Sys.time()
  results <- analyze_cash_secured_puts(
    strike_threshold_pct = 0.95,
    max_workers = 10
  )
  elapsed <- difftime(Sys.time(), start_time, units = "mins")

  # Should complete in reasonable time (<10 minutes)
  expect_lt(elapsed, 10)

  # Should have results
  expect_s3_class(results, "data.frame")

  # Should be sorted by annualized return (descending)
  if (nrow(results) > 1) {
    expect_true(all(diff(results$annualized_return) <= 0))
  }
})
```

---

## 7. Manual Testing Procedures

### 7.1 End-to-End UI Testing Checklist

**Setup:**
1. Launch application: `shiny::runApp()`
2. Navigate to Cash-Secured Puts page
3. Verify page loads without errors

**Parameter Input Testing:**
- [ ] Strike threshold slider (50-100%)
  - Test min: 50%
  - Test max: 100%
  - Test default: 95%
  - Verify label updates with value
- [ ] Days to expiry slider
  - Test range selection (30-365)
  - Test default: 45-120
  - Verify both handles work
- [ ] Parallel workers slider (1-20)
  - Test min: 1
  - Test max: 20
  - Test default: 4
- [ ] Quote source toggle
  - Toggle between Questrade and Yahoo
  - Verify selection persists during analysis

**Analysis Execution:**
- [ ] Click "Run Analysis" button
  - Verify button disables during analysis
  - Verify progress indicator appears
  - Verify analysis completes without errors
  - Verify results display after completion

**Results Display:**
- [ ] Verify results cards display correctly
  - Company name and ticker visible
  - Current price and strike price displayed
  - Expiration date formatted correctly
  - All metrics present and formatted
- [ ] Accordion sections work
  - "Quick Overview" section expands by default
  - "Risk Analysis" section toggles correctly
  - "Option Details" section toggles correctly
- [ ] Sorting and filtering
  - Verify results sorted by annualized return (descending)
  - Test filtering by ticker search (if implemented)

**Data Export:**
- [ ] Click "Download CSV" button
  - Verify CSV file downloads
  - Open CSV and verify:
    - All columns present
    - Data matches displayed results
    - Formatting is correct (no HTML artifacts)
    - Column headers are descriptive

**Error Handling:**
- [ ] Test with invalid parameters
  - Very short days range (e.g., 1-5)
  - Strike threshold at extremes (50%, 100%)
  - Verify appropriate error messages
- [ ] Test offline behavior
  - Disconnect network
  - Attempt analysis
  - Verify graceful error message
  - Reconnect and verify recovery

**Performance:**
- [ ] Measure analysis time with different parameters
  - Record time for 5 stocks
  - Record time for 10 stocks
  - Record time for full aristocrats list
  - Verify times are reasonable (<5 min for full list)

**Cross-Browser Testing:**
- [ ] Chrome: Test all functionality
- [ ] Firefox: Test all functionality
- [ ] Safari: Test all functionality
- [ ] Edge: Test all functionality

### 7.2 Metric Validation Procedure

**Select 3-5 manual test cases** from analysis results:

For each opportunity, manually verify:

1. **Strike Selection**
   - [ ] strike >= current_price × strike_threshold_pct
   - [ ] Example: price $100, threshold 95%, strike should be >= $95

2. **Cash Required**
   - [ ] cash_required = strike × 100
   - [ ] Example: strike $95 → cash = $9,500

3. **Premium Received**
   - [ ] premium_received = bid × 100
   - [ ] Example: bid $1.50 → premium = $150

4. **Breakeven Price**
   - [ ] breakeven = strike - bid
   - [ ] Example: strike $95, bid $1.50 → breakeven = $93.50

5. **Downside Protection**
   - [ ] protection % = (current - breakeven) / current
   - [ ] Example: current $100, breakeven $93.50 → 6.5% protection

6. **Return on Cash**
   - [ ] return = premium / cash_required
   - [ ] Example: premium $150, cash $9,500 → 1.58% return

7. **Annualized Return**
   - [ ] annualized = return × (365 / days)
   - [ ] Example: 1.58% return, 90 days → 6.41% annualized

### 7.3 Comparison Testing

**Compare with existing Covered Calls strategy:**

- [ ] Verify strike logic is inverted:
  - Calls: Strike <= threshold (ITM calls)
  - Puts: Strike >= threshold (OTM puts)

- [ ] Verify metric differences:
  - Calls: Use stock acquisition cost
  - Puts: Use cash collateral requirement

- [ ] Verify UI consistency:
  - Similar parameter controls
  - Similar results display format
  - Consistent card styling

---

## 8. Test Data Fixtures

### 8.1 Mock Dividend Aristocrats List

```r
mock_aristocrats <- function() {
  c("AAPL", "MSFT", "JNJ", "PG", "KO", "PEP", "WMT", "TGT", "HD", "MCD")
}
```

### 8.2 Mock Options Chain Data

```r
mock_put_options <- function(current_price = 100) {
  tibble(
    Strike = seq(current_price * 0.85, current_price * 1.05, by = 2.50),
    Bid = seq(0.50, 5.00, length.out = 9),
    Ask = Bid + 0.10,
    OI = sample(50:500, 9),
    expiration = rep(c(
      as.Date("2025-02-21"),
      as.Date("2025-03-21"),
      as.Date("2025-04-18")
    ), length.out = 9),
    days_to_expiry = as.integer(difftime(expiration, Sys.Date(), units = "days"))
  )
}
```

### 8.3 Mock Stock Data

```r
mock_stock_data <- function(ticker = "TEST", price = 100) {
  list(
    company_name = sprintf("%s Corp", ticker),
    current_price = price,
    max_drawdown = -0.15,
    current_yield = 0.02,
    annual_dividend = price * 0.02,
    history_data = tibble(
      Date = seq(Sys.Date() - 1825, Sys.Date(), by = "day"),
      Close = price + rnorm(1826, 0, 5)
    )
  )
}
```

### 8.4 Expected Calculation Outputs

```r
# Test Case 1: Standard OTM Put
expected_result_1 <- list(
  current_price = 100,
  strike = 95,
  bid = 1.50,
  days = 90,
  # Expected outputs:
  cash_required = 9500,
  premium_received = 150,
  net_outlay = 9350,
  breakeven_price = 93.50,
  downside_protection_pct = 0.065,
  return_on_cash = 0.0158,
  annualized_return = 0.0641,
  intrinsic_value = 0,
  extrinsic_value = 1.50
)

# Test Case 2: ATM Put
expected_result_2 <- list(
  current_price = 100,
  strike = 100,
  bid = 3.00,
  days = 60,
  # Expected outputs:
  cash_required = 10000,
  premium_received = 300,
  net_outlay = 9700,
  breakeven_price = 97.00,
  downside_protection_pct = 0.03,
  return_on_cash = 0.03,
  annualized_return = 0.1825,
  intrinsic_value = 0,
  extrinsic_value = 3.00
)

# Test Case 3: ITM Put
expected_result_3 <- list(
  current_price = 100,
  strike = 105,
  bid = 6.00,
  days = 45,
  # Expected outputs:
  cash_required = 10500,
  premium_received = 600,
  net_outlay = 9900,
  breakeven_price = 99.00,
  downside_protection_pct = 0.01,
  return_on_cash = 0.0571,
  annualized_return = 0.4637,
  intrinsic_value = 5.00,
  extrinsic_value = 1.00
)
```

---

## 9. Test Execution Order

### Phase 1: Foundation (Priority 1)
1. Configuration validation tests
2. Input validation tests
3. Metric calculation tests

### Phase 2: Core Logic (Priority 1)
4. Strike selection tests (CRITICAL)
5. Option value tests
6. Cash flow tests
7. Protection metrics tests

### Phase 3: Integration (Priority 2)
8. Single stock analysis tests
9. Options chain fetching tests
10. Parallel processing tests

### Phase 4: End-to-End (Priority 2)
11. Full workflow integration tests
12. Data source integration tests
13. Error handling tests

### Phase 5: Performance (Priority 3)
14. Parallel scaling tests
15. Large dataset tests

### Phase 6: Manual (Priority 2)
16. UI testing
17. Manual metric validation
18. Cross-browser testing

---

## 10. Acceptance Criteria

### Unit Tests
- [ ] All config validation tests pass
- [ ] All metric calculation tests pass
- [ ] All strike selection tests pass (CRITICAL)
- [ ] All edge case tests pass
- [ ] Test coverage >= 90%

### Integration Tests
- [ ] End-to-end workflow completes successfully
- [ ] Parallel processing works correctly
- [ ] Error handling is graceful
- [ ] Data source integration works

### Performance Tests
- [ ] Analysis completes within 5 minutes for 65 aristocrats
- [ ] Parallel processing provides speedup
- [ ] No memory leaks or resource exhaustion

### Manual Tests
- [ ] UI displays correctly
- [ ] All parameters work as expected
- [ ] Results are accurate (spot-checked)
- [ ] CSV export works
- [ ] Cross-browser compatibility verified

### Regression Tests
- [ ] Existing covered calls functionality still works
- [ ] No performance degradation in other strategies
- [ ] Shared utility functions still work correctly

---

## 11. Known Issues and Limitations

1. **API Rate Limits**: Tests that call external APIs may fail due to rate limiting
2. **Network Dependency**: Integration tests require network access
3. **Market Hours**: Some tests may behave differently during market hours vs. after hours
4. **Data Volatility**: Stock prices and options data change constantly, making exact assertions difficult

---

## 12. Test Maintenance

### Adding New Tests
- Follow existing naming conventions (`test_that("description", { ... })`)
- Use descriptive test names that explain what is being tested
- Include comments explaining WHY a test exists, not just WHAT it does
- Group related tests together

### Updating Tests
- When modifying business logic, update corresponding tests
- When tests fail, investigate root cause before updating test expectations
- Keep test data fixtures up to date

### Performance Monitoring
- Track test execution times
- Identify and optimize slow tests
- Consider mocking external dependencies for faster tests

---

## 13. Documentation

All test files should include:
- File header with description and date
- Test group descriptions
- Inline comments for complex test logic
- References to specification document
- Expected behavior documentation

---

**Document Version**: 1.0
**Created**: 2025-11-23
**Status**: Ready for Implementation
