# Extracted from test-mod_portfolio_risk_dashboard.R:157

# test -------------------------------------------------------------------------
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
expect_s3_class(table, "datatables")
table_data <- table$x$data
expect_true(any(grepl("AAPL", table_data$Ticker)))
expect_true(any(grepl("\u26A0", table_data$Ticker)))
