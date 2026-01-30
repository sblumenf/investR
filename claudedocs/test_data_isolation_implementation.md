# Test Data Isolation Implementation

## Problem Solved

The test suite for the "Convert to Legacy Covered Call" feature was creating test position groups in the production database with status = "open", causing the Portfolio Risk Analysis feature to treat them as real positions and attempt to fetch market quotes for symbols the user doesn't actually own (AAPL, AMZN, MSFT, NVDA, TSLA, META, NFLX, INTC, AMD, CSCO, IBM, GOOGL).

## Solution Implemented

### 1. Database Cleanup (Immediate Fix)

Created two scripts to clean up existing test data:

- **`scripts/cleanup_test_data_dryrun.R`**: Shows what test data would be deleted
- **`scripts/cleanup_test_data_execute.R`**: Actually deletes the test data

Initial cleanup removed:
- 59 position groups with TEST_ prefix
- 80 position group members

### 2. Test Database Isolation (Permanent Solution)

Created test infrastructure to completely isolate test data from production:

#### **`tests/testthat/setup.R`**
- Creates a temporary test database for each test run: `/tmp/investR_test_*.duckdb`
- Overrides `get_portfolio_db_path()` to return test database path
- Overrides `get_portfolio_db_connection()` to connect to test database
- Assigns functions to global environment so all sourced R files use them

#### **`tests/testthat/teardown.R`**
- Cleans up temporary test database after all tests complete
- Restores original database functions if they existed

### 3. Production Safety Filter (Defense in Depth)

Modified `R/fct_portfolio_risk.R` to exclude TEST_ groups even if they somehow get into the production database:

```r
open_groups <- all_groups %>%
  filter(
    status == "open",
    !grepl("^TEST_", group_id)  # Exclude test groups
  )
```

## How It Works

1. **Before tests run**:
   - `setup.R` creates a temporary database
   - Overrides database connection functions globally

2. **During tests**:
   - All `create_position_group()` calls write to test database
   - Test data is completely isolated from production

3. **After tests complete**:
   - `teardown.R` deletes the temporary database
   - No cleanup code needed in individual tests

## Verification

After implementation:
- ✅ Production database has 0 TEST_ groups
- ✅ Test runs create data in `/tmp/investR_test_*.duckdb`
- ✅ Portfolio Risk Analysis no longer tries to fetch quotes for test symbols
- ✅ Tests are completely isolated from production data

## Benefits

1. **No Production Pollution**: Test data never enters production database
2. **Clean Tests**: Each test run starts with a fresh database
3. **No Manual Cleanup**: Temporary database is auto-deleted
4. **Safety Net**: Production filter provides defense in depth
5. **Developer Experience**: Developers don't need to worry about cleaning up test data

## Future Test Development

When writing new database tests:
- No special setup required - just write tests normally
- Use `create_position_group()` and other database functions as usual
- Test data automatically goes to isolated test database
- No cleanup code needed in tests

## Files Modified

**Created:**
- `tests/testthat/setup.R` - Test database configuration
- `tests/testthat/teardown.R` - Test cleanup
- `scripts/cleanup_test_data_dryrun.R` - Dry run cleanup script
- `scripts/cleanup_test_data_execute.R` - Execute cleanup script

**Modified:**
- `R/fct_portfolio_risk.R` - Added TEST_ group filter
