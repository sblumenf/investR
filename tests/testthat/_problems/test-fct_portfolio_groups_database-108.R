# Extracted from test-fct_portfolio_groups_database.R:108

# test -------------------------------------------------------------------------
skip_on_cran()
local_test_db()
members <- tibble::tibble(
    symbol = c("AAPL", "AAPL240119C150"),
    role = c("underlying_stock", "short_call")
  )
create_position_group(
    group_id = "TEST_CLOSE",
    group_name = "To Close",
    strategy_type = "Dividend Aristocrats",
    account_number = "12345",
    members = members
  )
result <- close_position_group("TEST_CLOSE")
expect_true(result)
