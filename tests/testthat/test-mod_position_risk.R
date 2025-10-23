#' Tests for Position Risk Analysis Module
#'
#' Tests the Shiny module for position risk analysis modal and results display

test_that("mod_position_risk_ui returns empty tagList", {
  # Position risk module has no static UI (modal-based)
  ui <- mod_position_risk_ui("test_id")

  # Should return a tagList
  expect_s3_class(ui, "shiny.tag.list")

  # Should be empty (no static UI, everything in modal)
  expect_equal(length(ui), 0)
})

test_that("mod_position_risk_server returns reactive structure", {
  testServer(mod_position_risk_server, args = list(
    trigger = reactive(0),
    ticker = reactive("AAPL"),
    strike = reactive(150),
    expiration = reactive(as.Date("2025-12-31")),
    premium_received = reactive(5.00)
  ), {
    # Module should return a reactive
    returned <- session$getReturned()

    expect_true(is.reactive(returned))

    # Initial value should be NULL
    expect_null(returned())
  })
})

test_that("mod_position_risk_server initializes with NULL results", {
  testServer(mod_position_risk_server, args = list(
    trigger = reactive(0),
    ticker = reactive("AAPL"),
    strike = reactive(150),
    expiration = reactive(as.Date("2025-12-31")),
    premium_received = reactive(5.00)
  ), {
    # Get returned reactive
    results <- session$getReturned()

    # Should start as NULL (no analysis run yet)
    expect_null(results())
  })
})

test_that("create_summary_tab includes market regime section when present", {
  # Mock results with regime data
  mock_results <- list(
    ticker = "AAPL",
    current_price = 150,
    purchase_price = 145,
    strike = 155,
    premium_received = 5.00,
    days_to_expiry = 30,
    expiration = "2025-12-31",
    is_aristocrat = TRUE,
    simulation_paths = 10000,
    risk_adjusted_return_annualized = 0.15,
    monte_carlo = list(
      median_return = 0.08,
      prob_profit = 0.75,
      percentile_95 = 0.20,
      percentile_5 = -0.05,
      early_exercise_prob = 0.15,
      implied_volatility = 0.25,
      model = "jump_diffusion",
      regime = list(
        name = "normal",
        description = "Normal market conditions",
        risk_multiplier = 1.0,
        vix_current = 18.5
      )
    )
  )

  # Create summary tab
  summary_html <- as.character(create_summary_tab(mock_results))

  # Check for regime section
  expect_true(grepl("Market Regime", summary_html, ignore.case = TRUE))
  expect_true(grepl("normal", summary_html, ignore.case = TRUE))
  expect_true(grepl("Normal market conditions", summary_html, ignore.case = TRUE))
  expect_true(grepl("1\\.0x", summary_html))  # risk multiplier
  expect_true(grepl("18\\.5", summary_html))  # VIX
})

test_that("create_summary_tab handles NULL regime gracefully", {
  # Mock results WITHOUT regime data
  mock_results <- list(
    ticker = "AAPL",
    current_price = 150,
    purchase_price = 145,
    strike = 155,
    premium_received = 5.00,
    days_to_expiry = 30,
    expiration = "2025-12-31",
    is_aristocrat = FALSE,
    simulation_paths = 10000,
    risk_adjusted_return_annualized = 0.15,
    monte_carlo = list(
      median_return = 0.08,
      prob_profit = 0.75,
      percentile_95 = 0.20,
      percentile_5 = -0.05,
      early_exercise_prob = 0.15,
      implied_volatility = 0.25,
      model = "jump_diffusion",
      regime = NULL  # No regime
    )
  )

  # Should not error
  expect_no_error({
    summary_html <- as.character(create_summary_tab(mock_results))
  })

  # Create summary tab
  summary_html <- as.character(create_summary_tab(mock_results))

  # Should not have regime section
  expect_false(grepl("Market Regime", summary_html, ignore.case = TRUE))
})

test_that("create_summary_tab shows volatility source indicator", {
  # Mock results with regime (implies implied volatility used)
  mock_results_implied <- list(
    ticker = "AAPL",
    current_price = 150,
    purchase_price = 145,
    strike = 155,
    premium_received = 5.00,
    days_to_expiry = 30,
    expiration = "2025-12-31",
    is_aristocrat = TRUE,
    simulation_paths = 10000,
    risk_adjusted_return_annualized = 0.15,
    monte_carlo = list(
      median_return = 0.08,
      prob_profit = 0.75,
      percentile_95 = 0.20,
      percentile_5 = -0.05,
      early_exercise_prob = 0.15,
      implied_volatility = 0.25,
      model = "jump_diffusion",
      regime = list(name = "normal", description = "Normal", risk_multiplier = 1.0)
    )
  )

  summary_html <- as.character(create_summary_tab(mock_results_implied))

  # Should show implied volatility source
  expect_true(grepl("Market-based.*implied.*historical.*blend", summary_html, ignore.case = TRUE))
})

test_that("create_summary_tab shows historical volatility when no regime", {
  # Mock results without regime (implies historical volatility)
  mock_results_historical <- list(
    ticker = "AAPL",
    current_price = 150,
    purchase_price = 145,
    strike = 155,
    premium_received = 5.00,
    days_to_expiry = 30,
    expiration = "2025-12-31",
    is_aristocrat = FALSE,
    simulation_paths = 10000,
    risk_adjusted_return_annualized = 0.15,
    monte_carlo = list(
      median_return = 0.08,
      prob_profit = 0.75,
      percentile_95 = 0.20,
      percentile_5 = -0.05,
      early_exercise_prob = 0.15,
      implied_volatility = 0.22,
      model = "jump_diffusion",
      regime = NULL
    )
  )

  summary_html <- as.character(create_summary_tab(mock_results_historical))

  # Should show historical volatility source
  expect_true(grepl("Historical adaptive", summary_html, ignore.case = TRUE))
})

test_that("create_risk_results_modal creates modal with tabs", {
  # Mock minimal results
  mock_results <- list(
    ticker = "AAPL",
    current_price = 150,
    purchase_price = 145,
    strike = 155,
    premium_received = 5.00,
    days_to_expiry = 30,
    expiration = "2025-12-31",
    is_aristocrat = TRUE,
    simulation_paths = 10000,
    risk_adjusted_return_annualized = 0.15,
    monte_carlo = list(
      median_return = 0.08,
      prob_profit = 0.75,
      percentile_95 = 0.20,
      percentile_5 = -0.05,
      early_exercise_prob = 0.15,
      implied_volatility = 0.25,
      model = "jump_diffusion",
      regime = NULL,
      returns = rnorm(100, 0.08, 0.05),  # Mock returns for plotly
      n_paths = 100,
      avg_payoff_if_held = 520,
      avg_payoff_if_exercised = 500
    ),
    dividend_schedule = data.frame(
      dividend_date = as.Date("2025-11-01"),
      dividend_amount = 0.25,
      days_until = 15,
      confidence = "high"
    ),
    stress_tests = data.frame(
      scenario = "2008 Crisis",
      stock_price_change_pct = -0.45,
      stressed_stock_price = 82.5,
      position_pnl = -500,
      position_return_pct = -0.30,
      early_exercise_impact = "High"
    ),
    rquantlib = list(
      success = TRUE,
      delta = 0.5,
      gamma = 0.02,
      vega = 0.15,
      theta = -0.05,
      rho = 0.03,
      value = 5.25
    )
  )

  ns <- shiny::NS("test")
  modal <- create_risk_results_modal(mock_results, ns)

  # Should be a modal dialog
  expect_s3_class(modal, "shiny.tag")

  # Convert to HTML and check for tabs
  modal_html <- as.character(modal)
  expect_true(grepl("Summary", modal_html))
  expect_true(grepl("Distribution", modal_html))
  expect_true(grepl("Dividend Timeline", modal_html))
  expect_true(grepl("Stress Tests", modal_html))
  expect_true(grepl("Greeks.*Details", modal_html, ignore.case = TRUE))
})

test_that("create_summary_tab handles NULL monte_carlo gracefully", {
  # Mock results with NULL monte_carlo
  mock_results <- list(
    ticker = "AAPL",
    current_price = 150,
    purchase_price = 145,
    strike = 155,
    premium_received = 5.00,
    days_to_expiry = 30,
    expiration = "2025-12-31",
    is_aristocrat = FALSE,
    simulation_paths = 10000,
    risk_adjusted_return_annualized = NULL,
    monte_carlo = NULL
  )

  # Should not error
  expect_no_error({
    summary_html <- as.character(create_summary_tab(mock_results))
  })

  summary_html <- as.character(create_summary_tab(mock_results))

  # Should show "not available" messages
  expect_true(grepl("not available", summary_html, ignore.case = TRUE))
})

test_that("create_distribution_tab handles NULL monte_carlo", {
  mock_results <- list(monte_carlo = NULL)

  dist_html <- as.character(create_distribution_tab(mock_results))

  expect_true(grepl("not available", dist_html, ignore.case = TRUE))
})

test_that("create_timeline_tab handles empty dividend schedule", {
  mock_results <- list(
    ticker = "AAPL",
    dividend_schedule = data.frame()
  )

  timeline_html <- as.character(create_timeline_tab(mock_results))

  expect_true(grepl("No dividends", timeline_html, ignore.case = TRUE))
})

test_that("create_stress_tab handles NULL stress tests", {
  mock_results <- list(stress_tests = NULL)

  stress_html <- as.character(create_stress_tab(mock_results))

  expect_true(grepl("not available", stress_html, ignore.case = TRUE))
})

test_that("create_details_tab handles RQuantLib failure", {
  mock_results <- list(
    monte_carlo = list(
      model = "jump_diffusion",
      implied_volatility = 0.25
    ),
    simulation_paths = 10000,
    rquantlib = list(
      success = FALSE,
      error = "Calculation failed"
    )
  )

  details_html <- as.character(create_details_tab(mock_results))

  expect_true(grepl("Greeks calculation failed", details_html, ignore.case = TRUE))
})
