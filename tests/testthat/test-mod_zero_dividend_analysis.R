test_that("mod_zero_dividend_analysis_ui creates correct UI elements", {
  ui <- mod_zero_dividend_analysis_ui("test")

  # Should create a tagList
  expect_s3_class(ui, "shiny.tag.list")

  # Convert to character for inspection
  ui_html <- as.character(ui)

  # Should contain strike threshold slider
  expect_true(grepl("strike_threshold", ui_html))

  # Should contain target days slider
  expect_true(grepl("target_days", ui_html))

  # Should contain max workers slider
  expect_true(grepl("max_workers", ui_html))

  # Should contain run button
  expect_true(grepl("run_analysis", ui_html))

  # Should contain download button
  expect_true(grepl("download_results", ui_html))
})

test_that("strike_threshold slider has correct range", {
  ui <- mod_zero_dividend_analysis_ui("test")
  ui_html <- as.character(ui)

  # Should have min=50, max=100
  expect_true(grepl('data-min="50"', ui_html))
  expect_true(grepl('data-max="100"', ui_html))

  # Should have default value=85
  expect_true(grepl('data-from="85"', ui_html) ||
             grepl('value="85"', ui_html))
})

test_that("target_days slider has correct range", {
  ui <- mod_zero_dividend_analysis_ui("test")
  ui_html <- as.character(ui)

  # Should have min=30, max=1000
  expect_true(grepl('data-min="30"', ui_html))
  expect_true(grepl('data-max="1000"', ui_html))

  # Should have default value=1000
  expect_true(grepl('data-from="1000"', ui_html) ||
             grepl('value="1000"', ui_html))
})

test_that("analysis_params reactive converts slider values correctly", {
  # Mock module for testing
  testServer(
    mod_zero_dividend_analysis_server,
    {
      # Set slider inputs
      session$setInputs(
        strike_threshold = 85,
        target_days = 1000,
        max_workers = 10
      )

      # Get analysis params
      params <- analysis_params()

      # Strike threshold should be converted to decimal
      expect_equal(params$strike_threshold_pct, 0.85)

      # Target days = 1000 should become NULL
      expect_null(params$target_days)

      # Max workers should pass through
      expect_equal(params$max_workers, 10)
    }
  )
})

test_that("analysis_params reactive handles non-1000 target_days", {
  testServer(
    mod_zero_dividend_analysis_server,
    {
      # Set slider inputs with specific target days
      session$setInputs(
        strike_threshold = 80,
        target_days = 60,
        max_workers = 5
      )

      params <- analysis_params()

      # Strike threshold converted
      expect_equal(params$strike_threshold_pct, 0.80)

      # Target days should NOT be NULL
      expect_equal(params$target_days, 60)

      # Max workers
      expect_equal(params$max_workers, 5)
    }
  )
})

test_that("module returns list with expected reactive values", {
  testServer(
    mod_zero_dividend_analysis_server,
    {
      # Module should return list
      expect_type(session$returned, "list")

      # Should have results_data reactive
      expect_true("results_data" %in% names(session$returned))

      # Should have status_ui reactive
      expect_true("status_ui" %in% names(session$returned))

      # results_data should be a reactive
      expect_true(is.reactive(session$returned$results_data))

      # status_ui should be a reactive
      expect_true(is.reactive(session$returned$status_ui))
    }
  )
})

test_that("slider help text is present", {
  ui <- mod_zero_dividend_analysis_ui("test")
  ui_html <- as.character(ui)

  # Should contain help text about deeper ITM
  expect_true(grepl("Deeper ITM", ui_html) ||
             grepl("Lower risk", ui_html))

  # Should contain help text about longest expiration
  expect_true(grepl("1000 days", ui_html) ||
             grepl("longest", ui_html))
})

test_that("UI includes navigation button", {
  ui <- mod_zero_dividend_analysis_ui("test")
  ui_html <- as.character(ui)

  # Should have home button/link
  expect_true(grepl("Home", ui_html))
  expect_true(grepl('href="/"', ui_html))
})
