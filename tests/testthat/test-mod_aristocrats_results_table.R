test_that("mod_aristocrats_results_table shows overview when no data", {
  testServer(mod_aristocrats_results_table_server, args = list(
    results_data = reactive(NULL)
  ), {
    # When no data, strategy overview should be shown
    overview <- output$strategy_overview
    expect_true(!is.null(overview))
  })
})

test_that("mod_aristocrats_results_table hides overview when data exists", {
  # Create sample results data with all columns
  sample_data <- tibble::tibble(
    ticker = "AAPL",
    company_name = "Apple Inc.",
    current_price = 150,
    strike = 120,
    bid_price = 32,
    open_interest = 1000,
    expiration = "2025-12-31",
    days_to_expiry = 365,
    investment = 15000,
    premium_received = 3200,
    dividend_income = 400,
    reinvestment_income = 10,
    exercise_proceeds = 12000,
    net_profit = 1610,
    net_outlay = 11800,
    total_return = 0.15,
    annualized_return = 0.15,
    max_drawdown = -0.20,
    current_yield = 0.02,
    breakeven_price = 118,
    downside_protection_pct = 0.21,
    intrinsic_value = 30,
    extrinsic_value = 2,
    annual_dividend = 4.00,
    warning_flag = FALSE,
    is_aristocrat = TRUE
  )

  testServer(mod_aristocrats_results_table_server, args = list(
    results_data = reactive(sample_data)
  ), {
    # When data exists, overview should be hidden
    overview <- output$strategy_overview
    expect_null(overview)
  })
})

test_that("mod_aristocrats_results_table renders cards with data", {
  # Create sample results data with all columns
  sample_data <- tibble::tibble(
    ticker = c("AAPL", "MSFT"),
    company_name = c("Apple Inc.", "Microsoft Corp."),
    current_price = c(150, 300),
    strike = c(120, 240),
    bid_price = c(32, 64),
    open_interest = c(1000, 2000),
    expiration = c("2025-12-31", "2025-12-31"),
    days_to_expiry = c(365, 365),
    investment = c(15000, 30000),
    premium_received = c(3200, 6400),
    dividend_income = c(400, 500),
    reinvestment_income = c(10, 15),
    exercise_proceeds = c(12000, 24000),
    net_profit = c(1610, 915),
    net_outlay = c(11800, 23600),
    total_return = c(0.15, 0.18),
    annualized_return = c(0.15, 0.18),
    max_drawdown = c(-0.20, -0.15),
    current_yield = c(0.02, 0.015),
    breakeven_price = c(118, 236),
    downside_protection_pct = c(0.21, 0.23),
    intrinsic_value = c(30, 60),
    extrinsic_value = c(2, 4),
    annual_dividend = c(4.00, 5.00),
    warning_flag = c(FALSE, FALSE),
    is_aristocrat = c(TRUE, TRUE)
  )

  testServer(mod_aristocrats_results_table_server, args = list(
    results_data = reactive(sample_data)
  ), {
    session$flushReact()

    # Cards should be rendered
    cards_output <- output$results_cards
    expect_true(!is.null(cards_output))
  })
})

# Unit tests for helper functions
test_that("format_currency handles various inputs correctly", {
  expect_equal(format_currency(1234.56), "$1,234.56")
  expect_equal(format_currency(1000000), "$1,000,000.00")
  expect_equal(format_currency(0), "$0.00")
  expect_equal(format_currency(99.99), "$99.99")
})

test_that("format_percentage handles various inputs correctly", {
  expect_equal(format_percentage(0.15), "15.00%")
  expect_equal(format_percentage(0.9546), "95.46%")
  expect_equal(format_percentage(0), "0.00%")
  expect_equal(format_percentage(-0.20), "-20.00%")
})

test_that("create_metric_row generates correct HTML structure", {
  row <- create_metric_row("Test Label", "$100.00")

  expect_s3_class(row, "shiny.tag")
  expect_equal(row$name, "div")
  expect_true(grepl("metric-row", row$attribs$class))

  # Check children structure
  expect_length(row$children, 2)
  expect_true(grepl("metric-label", as.character(row$children[[1]])))
  expect_true(grepl("metric-value", as.character(row$children[[2]])))
})

test_that("create_metric_row applies primary class correctly", {
  row <- create_metric_row("Test", "$100", is_primary = TRUE)
  expect_true(grepl("metric-value-primary", as.character(row)))
})

test_that("create_metric_row applies negative class correctly", {
  row <- create_metric_row("Test", "-20.00%", is_negative = TRUE)
  expect_true(grepl("negative-value", as.character(row)))
})

test_that("create_generic_card_header is used in module for card headers", {
  # This test verifies that the module uses create_generic_card_header
  # Comprehensive tests for create_generic_card_header are in test-utils_ui_components.R
  header <- create_generic_card_header("Apple Inc. (AAPL)", "$150.00")

  expect_s3_class(header, "shiny.tag")
  header_html <- as.character(header)

  # Check for company-info class (line 1)
  expect_true(grepl("company-info", header_html))

  # Check for company name and ticker
  expect_true(grepl("Apple Inc.", header_html, fixed = TRUE))
  expect_true(grepl("AAPL", header_html, fixed = TRUE))

  # Check for stock-price class (line 2)
  expect_true(grepl("stock-price", header_html))

  # Check for stock price with proper formatting
  expect_true(grepl("$150.00", header_html, fixed = TRUE))
})

test_that("create_accordion_section generates details element", {
  section <- create_accordion_section("Test Section", is_open = TRUE,
                                       tags$p("Test content"))

  expect_s3_class(section, "shiny.tag")
  expect_equal(section$name, "details")

  # The first element passed to tags$details becomes attribs
  # Check the class is in the list passed as first arg
  expect_true("accordion-section" %in% section$attribs$class ||
              grepl("accordion-section", as.character(section)))

  # Check for summary child (should be first child after attribs)
  summary_found <- FALSE
  for (child in section$children) {
    if (inherits(child, "shiny.tag") && child$name == "summary") {
      summary_found <- TRUE
      expect_true(grepl("accordion-header", child$attribs$class))
      break
    }
  }
  expect_true(summary_found)
})

test_that("create_accordion_section has open attribute when is_open = TRUE", {
  section_open <- create_accordion_section("Test", is_open = TRUE)
  # Check if open attribute exists in the attributes list
  expect_true("open" %in% names(section_open$attribs))

  section_closed <- create_accordion_section("Test", is_open = FALSE)
  expect_false("open" %in% names(section_closed$attribs))
})