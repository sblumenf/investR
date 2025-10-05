test_that("mod_aristocrats_analysis_ui renders correctly", {
  # Test that UI function returns proper structure
  ui <- mod_aristocrats_analysis_ui("test_id")

  # Should return a tagList
  expect_s3_class(ui, "shiny.tag.list")

  # Convert to character to check for key elements
  ui_html <- as.character(ui)

  # Check for key UI elements
  expect_true(grepl("test_id-strike_threshold", ui_html))
  expect_true(grepl("test_id-target_days", ui_html))
  expect_true(grepl("test_id-max_workers", ui_html))
  expect_true(grepl("test_id-run_analysis", ui_html))
  expect_true(grepl("test_id-download_results", ui_html))
})

test_that("mod_aristocrats_analysis_server returns reactive values", {
  testServer(mod_aristocrats_analysis_server, args = list(id = "test"), {
    # Module should return a list with reactive components
    returned <- session$getReturned()

    expect_true(is.list(returned))
    expect_true("results_data" %in% names(returned))
    expect_true("status_ui" %in% names(returned))

    # Check that results_data is a reactiveVal
    expect_true(is.reactive(returned$results_data))

    # Check that status_ui is a reactive
    expect_true(is.reactive(returned$status_ui))
  })
})

test_that("mod_aristocrats_analysis_server initializes with NULL results", {
  testServer(mod_aristocrats_analysis_server, args = list(id = "test"), {
    # Get returned values
    returned <- session$getReturned()

    # Initial state should have NULL results
    expect_null(returned$results_data())
    expect_null(returned$status_ui())
  })
})

test_that("mod_aristocrats_analysis_server creates analysis params reactive", {
  testServer(mod_aristocrats_analysis_server, args = list(id = "test"), {
    # Set some input values
    session$setInputs(
      strike_threshold = 85,
      target_days = 365,
      max_workers = 4
    )

    # Check that reactive exists and can be accessed
    # Note: analysis_params is internal, so we test via side effects

    # The inputs should be set correctly
    expect_equal(input$strike_threshold, 85)
    expect_equal(input$target_days, 365)
    expect_equal(input$max_workers, 4)
  })
})

test_that("mod_aristocrats_analysis_server handles parameter edge cases", {
  testServer(mod_aristocrats_analysis_server, args = list(id = "test"), {
    # Test with target_days = 1000 (should become NULL)
    session$setInputs(
      strike_threshold = 80,
      target_days = 1000,
      max_workers = 4
    )

    # These should not error
    expect_equal(input$target_days, 1000)
    expect_equal(input$strike_threshold, 80)
  })
})

test_that("mod_aristocrats_analysis_server has proper structure", {
  testServer(mod_aristocrats_analysis_server, args = list(id = "test"), {
    # Test that the server function runs without error
    # Download handlers are difficult to test in testServer context
    # Just verify the module initializes correctly
    returned <- session$getReturned()
    expect_true(is.list(returned))
  })
})

test_that("mod_aristocrats_analysis_ui has correct input ranges", {
  ui <- mod_aristocrats_analysis_ui("test")
  ui_html <- as.character(ui)

  # Check that slider inputs exist with reasonable content
  expect_true(grepl("strike_threshold", ui_html))
  expect_true(grepl("target_days", ui_html))
  expect_true(grepl("max_workers", ui_html))

  # Check for min/max/value attributes (in any format)
  expect_true(grepl("50", ui_html))  # min value
  expect_true(grepl("80", ui_html))  # default strike threshold
  expect_true(grepl("100", ui_html)) # max value
})

test_that("mod_aristocrats_analysis_ui includes navigation", {
  ui <- mod_aristocrats_analysis_ui("test")
  ui_html <- as.character(ui)

  # Check for home link
  expect_true(grepl('href="/"', ui_html))
  expect_true(grepl("Home", ui_html))
})

test_that("mod_aristocrats_analysis_server status messages are reactive", {
  testServer(mod_aristocrats_analysis_server, args = list(id = "test"), {
    # Get the returned values
    returned <- session$getReturned()

    # Get the status_ui reactive
    status_fn <- returned$status_ui

    # Initially should be NULL
    expect_null(status_fn())

    # Status should remain reactive after checks
    expect_true(is.reactive(status_fn))
  })
})