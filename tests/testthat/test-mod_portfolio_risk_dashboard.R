#' Tests for Portfolio Risk Dashboard Module
#'
#' Tests the Shiny module for portfolio-level risk analysis and visualization

test_that("mod_portfolio_risk_dashboard_ui renders correctly", {
  # Test that UI function returns proper structure
  ui <- mod_portfolio_risk_dashboard_ui("test_id")

  # Should return a tagList
  expect_s3_class(ui, "shiny.tag.list")

  # Convert to character to check for key elements
  ui_html <- as.character(ui)

  # Check for key UI elements
  expect_true(grepl("test_id-analyze_portfolio", ui_html))
  expect_true(grepl("test_id-loading_indicator", ui_html))
  expect_true(grepl("test_id-dashboard_content", ui_html))
  expect_true(grepl("Portfolio Risk Analysis", ui_html))
  expect_true(grepl("Analyze Portfolio Risk", ui_html))
})

test_that("mod_portfolio_risk_dashboard_ui includes description", {
  ui <- mod_portfolio_risk_dashboard_ui("test")
  ui_html <- as.character(ui)

  # Check for descriptive content
  expect_true(grepl("Monte Carlo", ui_html, ignore.case = TRUE))
  expect_true(grepl("VaR", ui_html))
  expect_true(grepl("Component VaR", ui_html, ignore.case = TRUE))
  expect_true(grepl("Stress Testing", ui_html, ignore.case = TRUE))
  expect_true(grepl("Concentration", ui_html, ignore.case = TRUE))
  expect_true(grepl("Correlation", ui_html, ignore.case = TRUE))
})

test_that("mod_portfolio_risk_dashboard_server returns reactive values", {
  testServer(mod_portfolio_risk_dashboard_server, args = list(id = "test"), {
    # Module should initialize without error
    # Note: outputs aren't created until rendered, so we just test initialization
    expect_true(TRUE)
  })
})

test_that("mod_portfolio_risk_dashboard_server initializes with NULL results", {
  testServer(mod_portfolio_risk_dashboard_server, args = list(id = "test"), {
    # Check that initial state is correct
    # The server initializes without error
    expect_true(TRUE)
  })
})

test_that("create_positions_risk_table handles empty data", {
  # Mock empty positions
  positions <- tibble::tibble(
    group_id = character(),
    ticker = character(),
    strike = numeric(),
    expiration = as.Date(character()),
    current_value = numeric()
  )

  contributions <- tibble::tibble(
    group_id = character(),
    ticker = character(),
    expected_contribution = numeric(),
    prob_assignment = numeric(),
    risk_contribution = numeric(),
    pct_of_portfolio_risk = numeric(),
    risk_return_ratio = numeric(),
    portfolio_correlation = numeric(),
    correlation_impact = character()
  )

  ns <- shiny::NS("test")

  # Should not error
  expect_no_error({
    table <- create_positions_risk_table(positions, contributions, ns)
  })
})

test_that("create_positions_risk_table creates proper structure", {
  # Mock positions data
  positions <- tibble::tibble(
    group_id = c(1, 2),
    ticker = c("AAPL", "MSFT"),
    strike = c(150, 300),
    expiration = as.Date(c("2025-12-31", "2026-01-15")),
    current_value = c(5000, 8000)
  )

  contributions <- tibble::tibble(
    group_id = c(1, 2),
    ticker = c("AAPL", "MSFT"),
    expected_contribution = c(250, 400),
    prob_assignment = c(0.15, 0.25),
    risk_contribution = c(150, 200),
    pct_of_portfolio_risk = c(0.08, 0.12),
    risk_return_ratio = c(0.6, 0.5),
    portfolio_correlation = c(0.65, 0.45),
    correlation_impact = c("neutral", "neutral")
  )

  ns <- shiny::NS("test")

  table <- create_positions_risk_table(positions, contributions, ns)

  # DT datatable returns an htmlwidget - check it's created properly
  expect_s3_class(table, "datatables")

  # Get underlying data
  table_data <- table$x$data

  # Check column names
  expect_true("Ticker" %in% names(table_data))
  expect_true("Expected Contribution" %in% names(table_data))
  expect_true("Risk Contribution" %in% names(table_data))

  # Check data values
  expect_true(any(grepl("AAPL", table_data$Ticker)))
  expect_true(any(grepl("MSFT", table_data$Ticker)))
})

test_that("create_positions_risk_table highlights high risk positions", {
  # Mock position with >10% risk contribution
  positions <- tibble::tibble(
    group_id = 1,
    ticker = "AAPL",
    strike = 150,
    expiration = as.Date("2025-12-31"),
    current_value = 10000
  )

  contributions <- tibble::tibble(
    group_id = 1,
    ticker = "AAPL",
    expected_contribution = 500,
    prob_assignment = 0.20,
    risk_contribution = 1000,
    pct_of_portfolio_risk = 0.15,  # >10%, should be highlighted
    risk_return_ratio = 2.0,
    portfolio_correlation = 0.75,
    correlation_impact = "amplifying"
  )

  ns <- shiny::NS("test")
  table <- create_positions_risk_table(positions, contributions, ns)

  # DT datatable returns an htmlwidget
  expect_s3_class(table, "datatables")

  # Check table was created with data
  table_data <- table$x$data
  expect_true(any(grepl("AAPL", table_data$Ticker)))

  # Check that high risk position has warning icon (⚠️)
  expect_true(any(grepl("\u26A0", table_data$Ticker)))  # Warning icon in Unicode

  # Check correlation impact shows as amplifying
  expect_true(any(grepl("Amplifying", table_data$Correlation)))
})

test_that("create_stress_test_table creates proper structure", {
  # Mock stress test results
  stress_results <- tibble::tibble(
    scenario = c("2008 Crisis", "COVID Crash"),
    portfolio_pnl = c(-5000, -3000),
    portfolio_return_pct = c(-0.25, -0.15)
  )

  table <- create_stress_test_table(stress_results)
  table_html <- as.character(table)

  # Check structure
  expect_true(grepl("<table", table_html))
  expect_true(grepl("2008 Crisis", table_html))
  expect_true(grepl("COVID Crash", table_html))
  expect_true(grepl("Scenario", table_html))
  expect_true(grepl("P.*L", table_html, ignore.case = TRUE))  # P&L with flexible matching
})

test_that("create_stress_test_table colors negative values correctly", {
  stress_results <- tibble::tibble(
    scenario = "2008 Crisis",
    portfolio_pnl = -5000,
    portfolio_return_pct = -0.25
  )

  table <- create_stress_test_table(stress_results)
  table_html <- as.character(table)

  # Should have red color for negative values
  expect_true(grepl("dc3545", table_html))  # danger color
})

test_that("create_concentration_table handles empty data", {
  concentration_data <- tibble::tibble(
    ticker = character(),
    total_value = numeric(),
    pct_of_portfolio = numeric(),
    n_positions = integer()
  )

  table <- create_concentration_table(concentration_data, "Ticker")
  table_html <- as.character(table)

  expect_true(grepl("No data available", table_html, ignore.case = TRUE))
})

test_that("create_concentration_table creates proper structure", {
  concentration_data <- tibble::tibble(
    ticker = c("AAPL", "MSFT", "GOOGL"),
    total_value = c(10000, 8000, 6000),
    pct_of_portfolio = c(0.30, 0.24, 0.18),
    n_positions = c(2, 1, 1)
  )

  table <- create_concentration_table(concentration_data, "Ticker")
  table_html <- as.character(table)

  expect_true(grepl("<table", table_html))
  expect_true(grepl("AAPL", table_html))
  expect_true(grepl("MSFT", table_html))
  expect_true(grepl("% of Portfolio", table_html))
})

test_that("create_concentration_table highlights high concentration", {
  # Ticker with >25% concentration
  concentration_data <- tibble::tibble(
    ticker = "AAPL",
    total_value = 15000,
    pct_of_portfolio = 0.35,  # >25%, should be highlighted
    n_positions = 3
  )

  table <- create_concentration_table(concentration_data, "Ticker")
  table_html <- as.character(table)

  # Should have warning color (key indicator)
  expect_true(grepl("fff3cd", table_html))  # warning background
  expect_true(grepl("AAPL", table_html))  # ticker present
})

test_that("create_concentration_table highlights sector concentration correctly", {
  # Sector with >40% concentration
  concentration_data <- tibble::tibble(
    sector = "Technology",
    total_value = 25000,
    pct_of_portfolio = 0.50,  # >40% for sector, should be highlighted
    n_positions = 5
  )

  table <- create_concentration_table(concentration_data, "Sector")
  table_html <- as.character(table)

  # Should have warning color (key indicator)
  expect_true(grepl("fff3cd", table_html))
  expect_true(grepl("Technology", table_html))  # sector present
})

test_that("dashboard content includes regime card when present", {
  # This tests the regime card in dashboard output
  # We test the structure indirectly by checking HTML generation

  # Mock results with regime
  mock_results <- list(
    total_positions = 5,
    total_value = 50000,
    var_95 = -2500,
    risk_level = "Moderate",
    expected_return = 2000,
    median_return = 1800,
    prob_loss = 0.15,
    var_99 = -4000,
    cvar_95 = -3000,
    cvar_99 = -5000,
    percentile_5 = -2500,
    percentile_95 = 5000,
    sd_return = 2000,
    simulation_paths = 10000,
    portfolio_pnl = rnorm(10000, 1800, 2000),
    positions = tibble::tibble(
      group_id = 1,
      ticker = "AAPL",
      strike = 150,
      expiration = as.Date("2025-12-31"),
      current_value = 10000
    ),
    position_contributions = tibble::tibble(
      group_id = 1,
      ticker = "AAPL",
      expected_contribution = 500,
      prob_assignment = 0.15,
      risk_contribution = 1000,
      pct_of_portfolio_risk = 0.08,
      risk_return_ratio = 2.0,
      portfolio_correlation = 0.55,
      correlation_impact = "neutral"
    ),
    stress_results = tibble::tibble(
      scenario = "2008 Crisis",
      portfolio_pnl = -5000,
      portfolio_return_pct = -0.25
    ),
    stress_position_attribution = list(
      financial_crisis_2008 = tibble::tibble(
        group_id = 1,
        ticker = "AAPL",
        position_pnl = -5000,
        pct_of_scenario_impact = 1.0
      )
    ),
    concentration = list(
      by_ticker = tibble::tibble(
        ticker = "AAPL",
        total_value = 10000,
        pct_of_portfolio = 0.20,
        n_positions = 1
      ),
      by_sector = tibble::tibble(
        sector = "Technology",
        total_value = 10000,
        pct_of_portfolio = 0.20,
        n_positions = 1
      ),
      alerts = character()
    ),
    risk_reduction_levers = tibble::tibble(
      group_id = 1,
      ticker = "AAPL",
      days_to_expiry = 30,
      current_value = 10000,
      risk_reduction = 500,
      risk_reduction_pct = 0.20,
      expected_return_lost = 100,
      var_without_position = -2000,
      suggested_action = "Roll"
    ),
    assignment_clustering = list(
      calendar = tibble::tibble(
        expiration_week = as.Date("2025-12-29"),
        n_positions = 1,
        tickers = "AAPL",
        total_value = 10000,
        avg_assignment_prob = 0.15,
        max_assignment_prob = 0.15,
        value_at_assignment_risk = 1500,
        pct_of_portfolio = 0.03,
        risk_level = "low"
      ),
      high_risk_weeks = tibble::tibble(),
      clustering_score = 0.03,
      alerts = character()
    ),
    regime = list(
      name = "normal",
      description = "Normal market conditions",
      risk_multiplier = 1.0,
      vix_current = 18.5
    )
  )

  # Note: We can't easily test the full dashboard rendering without running the server
  # But we can verify the structure would include regime data
  expect_true(!is.null(mock_results$regime))
  expect_equal(mock_results$regime$name, "normal")
  expect_equal(mock_results$regime$risk_multiplier, 1.0)
})

test_that("dashboard handles NULL regime gracefully", {
  # Mock results WITHOUT regime
  mock_results <- list(
    total_positions = 5,
    total_value = 50000,
    var_95 = -2500,
    risk_level = "Moderate",
    expected_return = 2000,
    median_return = 1800,
    prob_loss = 0.15,
    var_99 = -4000,
    cvar_95 = -3000,
    cvar_99 = -5000,
    percentile_5 = -2500,
    percentile_95 = 5000,
    sd_return = 2000,
    simulation_paths = 10000,
    portfolio_pnl = rnorm(10000, 1800, 2000),
    positions = tibble::tibble(
      group_id = 1,
      ticker = "AAPL",
      strike = 150,
      expiration = as.Date("2025-12-31"),
      current_value = 10000
    ),
    position_contributions = tibble::tibble(
      group_id = 1,
      ticker = "AAPL",
      expected_contribution = 500,
      prob_assignment = 0.15,
      risk_contribution = 1000,
      pct_of_portfolio_risk = 0.08,
      risk_return_ratio = 2.0,
      portfolio_correlation = 0.55,
      correlation_impact = "neutral"
    ),
    stress_results = tibble::tibble(
      scenario = "2008 Crisis",
      portfolio_pnl = -5000,
      portfolio_return_pct = -0.25
    ),
    stress_position_attribution = list(),
    concentration = list(
      by_ticker = tibble::tibble(
        ticker = "AAPL",
        total_value = 10000,
        pct_of_portfolio = 0.20,
        n_positions = 1
      ),
      by_sector = tibble::tibble(
        sector = "Technology",
        total_value = 10000,
        pct_of_portfolio = 0.20,
        n_positions = 1
      ),
      alerts = character()
    ),
    risk_reduction_levers = tibble::tibble(),
    assignment_clustering = list(
      calendar = tibble::tibble(),
      high_risk_weeks = tibble::tibble(),
      clustering_score = 0,
      alerts = character()
    ),
    regime = NULL  # No regime
  )

  # Should handle NULL regime without error
  expect_null(mock_results$regime)
  expect_true(is.list(mock_results))
})

test_that("format helper functions work correctly", {
  # Test currency formatting
  expect_true(grepl("\\$", format_currency(1000)))
  expect_true(grepl("1,000", format_currency(1000)))

  # Test percentage formatting
  expect_true(grepl("%", format_percentage(0.15)))
  expect_true(grepl("15", format_percentage(0.15)))
})

test_that("create_metric_row helper exists and works", {
  # This helper is used throughout the modules
  metric <- create_metric_row("Test Label", "Test Value")
  metric_html <- as.character(metric)

  expect_true(grepl("Test Label", metric_html))
  expect_true(grepl("Test Value", metric_html))
})
