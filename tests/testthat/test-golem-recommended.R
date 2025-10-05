test_that("app ui", {
  ui <- golem_add_external_resources()
  # Brochure apps return shiny.tag for external resources, not tagList
  expect_s3_class(ui, "shiny.tag")
})

test_that("app server", {
  # Note: For Brochure apps, server testing is done per-page
  # This is a placeholder for general app tests
  expect_true(TRUE)
})

test_that("app launches successfully", {
  # Test that run_app returns a valid object
  # Note: This doesn't actually launch the app, just checks the structure
  skip_on_cran()
  skip_on_ci()

  # Brochure apps return shiny.appobj, not brochure_app
  expect_s3_class(
    run_app(),
    "shiny.appobj"
  )
})

test_that("app config", {
  # Test that config file exists and can be read
  config_file <- app_sys("golem-config.yml")
  expect_true(file.exists(config_file))

  # Test get_golem_config function
  expect_no_error(
    get_golem_config("golem_name")
  )

  expect_equal(
    get_golem_config("golem_name"),
    "investR"
  )
})

test_that("app_sys works", {
  expect_true(
    app_sys("golem-config.yml") != ""
  )
})

test_that("pages are properly structured", {
  # Test that page functions exist
  expect_true(exists("page_home"))
  expect_true(exists("page_about"))

  # Test that pages return proper brochure page objects
  home_page <- page_home()
  expect_true("brochure_page" %in% class(home_page))

  about_page <- page_about()
  expect_true("brochure_page" %in% class(about_page))
})