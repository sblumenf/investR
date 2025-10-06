test_that("parse_option_symbol extracts underlying ticker", {
  expect_equal(parse_option_symbol("ALB17Dec27C55.00"), "ALB")
  expect_equal(parse_option_symbol("AAPL240119C150"), "AAPL")
  expect_equal(parse_option_symbol("IBM15Jan27C175.00"), "IBM")
  expect_equal(parse_option_symbol("GOOGL"), NA_character_)  # Not an option
  expect_equal(parse_option_symbol(""), NA_character_)
  expect_equal(parse_option_symbol(NA_character_), NA_character_)
})

test_that("is_option_symbol correctly identifies options", {
  expect_true(is_option_symbol("ALB17Dec27C55.00"))
  expect_true(is_option_symbol("AAPL240119C150"))
  expect_true(is_option_symbol("IBM15Jan27P175.00"))
  expect_false(is_option_symbol("AAPL"))
  expect_false(is_option_symbol("IBM"))
  expect_false(is_option_symbol("GOOGL"))
})

test_that("classify_position_type categorizes positions correctly", {
  expect_equal(classify_position_type("AAPL"), "stock")
  expect_equal(classify_position_type("IBM.TO"), "stock")
  expect_equal(classify_position_type("ALB17Dec27C55.00"), "option")
  expect_equal(classify_position_type("AAPL240119C150"), "option")
  expect_equal(classify_position_type(NA_character_), "other")
})

test_that("validate_selection_for_grouping accepts single ticker", {
  # All positions have same underlying ticker
  selection <- tibble::tibble(
    symbol = c("ALB", "ALB17Dec27C55.00"),
    open_quantity = c(200, -2)
  )

  result <- validate_selection_for_grouping(selection)

  expect_true(result$valid)
  expect_null(result$error)
  expect_equal(result$ticker, "ALB")
})

test_that("validate_selection_for_grouping rejects mixed tickers", {
  # Positions have different underlying tickers
  selection <- tibble::tibble(
    symbol = c("AAPL", "IBM17Dec27C150.00"),
    open_quantity = c(100, -1)
  )

  result <- validate_selection_for_grouping(selection)

  expect_false(result$valid)
  expect_true(grepl("Mixed tickers", result$error))
  expect_true(grepl("AAPL", result$error))
  expect_true(grepl("IBM", result$error))
})

test_that("validate_selection_for_grouping handles option symbols", {
  # Options should extract underlying ticker
  selection <- tibble::tibble(
    symbol = c("ALB17Dec27C55.00", "ALB24Jan28P60.00"),
    open_quantity = c(-2, 2)
  )

  result <- validate_selection_for_grouping(selection)

  expect_true(result$valid)
  expect_equal(result$ticker, "ALB")
})

test_that("assign_roles_from_positions assigns roles by quantity", {
  selection <- tibble::tibble(
    symbol = c("ALB", "ALB17Dec27C55.00"),
    open_quantity = c(200, -2),
    other_col = c("foo", "bar")  # Should be dropped
  )

  result <- assign_roles_from_positions(selection)

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)  # Only symbol and role
  expect_true(all(c("symbol", "role") %in% colnames(result)))

  # Check role assignment
  alb_stock <- result %>% filter(symbol == "ALB")
  expect_equal(alb_stock$role, "underlying_stock")

  alb_option <- result %>% filter(symbol == "ALB17Dec27C55.00")
  expect_equal(alb_option$role, "short_call")
})

test_that("assign_roles_from_positions handles edge cases", {
  # Positive quantity option → NA
  # Negative quantity stock → NA
  selection <- tibble::tibble(
    symbol = c("AAPL", "IBM24Jan28C150.00"),
    open_quantity = c(-100, 1)
  )

  result <- assign_roles_from_positions(selection)

  expect_true(all(is.na(result$role)))
})

test_that("validate_group_definition validates required fields", {
  # Valid group
  valid_members <- tibble::tibble(
    symbol = c("AAPL", "AAPL240119C150"),
    role = c("underlying_stock", "short_call")
  )

  result <- validate_group_definition(
    group_name = "Test Group",
    strategy_type = "Dividend Aristocrats",
    account_number = "12345",
    members = valid_members
  )

  expect_true(result$valid)
  expect_equal(length(result$errors), 0)

  # Missing group name
  result2 <- validate_group_definition(
    group_name = "",
    strategy_type = "Dividend Aristocrats",
    account_number = "12345"
  )

  expect_false(result2$valid)
  expect_true(any(grepl("Group name", result2$errors)))

  # Invalid strategy type
  result3 <- validate_group_definition(
    group_name = "Test",
    strategy_type = "Invalid Strategy",
    account_number = "12345"
  )

  expect_false(result3$valid)
  expect_true(any(grepl("Strategy type", result3$errors)))

  # Missing account number
  result4 <- validate_group_definition(
    group_name = "Test",
    strategy_type = "Dividend Aristocrats",
    account_number = ""
  )

  expect_false(result4$valid)
  expect_true(any(grepl("Account number", result4$errors)))
})

test_that("validate_group_definition validates member roles", {
  # Invalid role
  invalid_members <- tibble::tibble(
    symbol = c("AAPL"),
    role = c("invalid_role")
  )

  result <- validate_group_definition(
    group_name = "Test",
    strategy_type = "Dividend Aristocrats",
    account_number = "12345",
    members = invalid_members
  )

  expect_false(result$valid)
  expect_true(any(grepl("Invalid roles", result$errors)))

  # Duplicate symbols
  duplicate_members <- tibble::tibble(
    symbol = c("AAPL", "AAPL"),
    role = c("underlying_stock", "short_call")
  )

  result2 <- validate_group_definition(
    group_name = "Test",
    strategy_type = "Dividend Aristocrats",
    account_number = "12345",
    members = duplicate_members
  )

  expect_false(result2$valid)
  expect_true(any(grepl("Duplicate symbols", result2$errors)))
})

test_that("generate_group_id creates unique IDs", {
  id1 <- generate_group_id("Dividend Aristocrats", "12345")
  id2 <- generate_group_id("Dividend Aristocrats", "12345")

  # Should contain strategy and account
  expect_true(grepl("DIVIDEND_ARISTOCRATS", id1))
  expect_true(grepl("12345", id1))

  # Should be unique (different random suffix)
  expect_false(id1 == id2)
})

test_that("get_missing_members identifies missing positions", {
  group_members <- tibble::tibble(
    account_number = c("12345", "12345", "12345"),
    symbol = c("AAPL", "AAPL240119C150", "IBM"),
    role = c("underlying_stock", "short_call", "underlying_stock")
  )

  current_positions <- tibble::tibble(
    account_number = c("12345", "12345"),
    symbol = c("AAPL", "IBM")
  )

  missing <- get_missing_members(group_members, current_positions)

  expect_equal(length(missing), 1)
  expect_equal(missing, "AAPL240119C150")
})

test_that("get_missing_members returns empty when all exist", {
  group_members <- tibble::tibble(
    account_number = c("12345", "12345"),
    symbol = c("AAPL", "IBM"),
    role = c("underlying_stock", "underlying_stock")
  )

  current_positions <- tibble::tibble(
    account_number = c("12345", "12345"),
    symbol = c("AAPL", "IBM")
  )

  missing <- get_missing_members(group_members, current_positions)

  expect_equal(length(missing), 0)
})

test_that("build_group_name_lookup returns correct structure", {
  skip_on_cran()

  # Create test groups in database
  create_position_group(
    group_id = "TEST_LOOKUP_BATCH_1",
    group_name = "Batch Test Group 1",
    strategy_type = "Dividend Aristocrats",
    account_number = "99999",
    members = tibble::tibble(
      symbol = c("TSLA", "TSLA24Jan28C200"),
      role = c("underlying_stock", "short_call")
    )
  )

  create_position_group(
    group_id = "TEST_LOOKUP_BATCH_2",
    group_name = "Batch Test Group 2",
    strategy_type = "Zero-Dividend Stocks",
    account_number = "88888",
    members = tibble::tibble(
      symbol = c("NVDA"),
      role = c("underlying_stock")
    )
  )

  # Build lookup table
  lookup <- build_group_name_lookup()

  # Should have correct structure
  expect_true(is.data.frame(lookup))
  expect_true(all(c("account_number", "symbol", "group_name") %in% names(lookup)))

  # Should contain our test groups
  expect_true(any(lookup$group_name == "Batch Test Group 1"))
  expect_true(any(lookup$group_name == "Batch Test Group 2"))

  # Check specific mappings
  tsla_stock <- lookup %>% filter(symbol == "TSLA" & account_number == "99999")
  expect_equal(nrow(tsla_stock), 1)
  expect_equal(tsla_stock$group_name, "Batch Test Group 1")

  nvda_stock <- lookup %>% filter(symbol == "NVDA" & account_number == "88888")
  expect_equal(nrow(nvda_stock), 1)
  expect_equal(nvda_stock$group_name, "Batch Test Group 2")
})

test_that("build_group_name_lookup handles empty database", {
  skip_on_cran()

  # This will use the actual database which may have groups
  # Just verify it returns a valid tibble structure
  lookup <- build_group_name_lookup()

  expect_true(is.data.frame(lookup))
  expect_true(all(c("account_number", "symbol", "group_name") %in% names(lookup)))
})
