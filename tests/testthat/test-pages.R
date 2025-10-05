test_that("page_home creates valid brochure page", {
  page <- page_home()

  # Check that it's a brochure page object
  expect_s3_class(page, "brochure_page")

  # Check href
  expect_equal(page$href, "/")

  # Check that ui is a function
  expect_true(is.function(page$ui))
})

test_that("page_home ui function returns valid HTML", {
  page <- page_home()

  # Call the ui function (Brochure pages pass request object)
  ui_result <- page$ui(request = list())

  # Should return a tagList
  expect_s3_class(ui_result, "shiny.tag.list")

  # Convert to character and check content
  ui_html <- as.character(ui_result)

  # Check for key content
  expect_true(grepl("investR", ui_html))
  expect_true(grepl("Investment Research", ui_html))
  expect_true(grepl("Dividend Aristocrats", ui_html))
  expect_true(grepl("/aristocrats", ui_html))
})

test_that("page_aristocrats creates valid brochure page", {
  page <- page_aristocrats()

  # Check that it's a brochure page object
  expect_s3_class(page, "brochure_page")

  # Check href
  expect_equal(page$href, "/aristocrats")

  # Check that ui and server are functions
  expect_true(is.function(page$ui))
  expect_true(is.function(page$server))
})

test_that("page_aristocrats ui function returns valid HTML", {
  page <- page_aristocrats()

  # Call the ui function
  ui_result <- page$ui(request = list())

  # Should return a tagList
  expect_s3_class(ui_result, "shiny.tag.list")

  # Convert to character and check content
  ui_html <- as.character(ui_result)

  # Check for key content
  expect_true(grepl("Dividend Aristocrats", ui_html))
  expect_true(grepl("Deep ITM Covered Calls", ui_html))

  # Check for module UI
  expect_true(grepl("analysis", ui_html))
  expect_true(grepl("results", ui_html))
})

test_that("page_aristocrats includes both analysis and results modules", {
  page <- page_aristocrats()
  ui_result <- page$ui(request = list())
  ui_html <- as.character(ui_result)

  # Should include analysis module
  expect_true(grepl("analysis-strike_threshold", ui_html) ||
              grepl("analysis", ui_html))

  # Should include results module
  expect_true(grepl("results", ui_html))
})

test_that("page_about creates valid brochure page", {
  page <- page_about()

  # Check that it's a brochure page object
  expect_s3_class(page, "brochure_page")

  # Check href
  expect_equal(page$href, "/about")

  # Check that ui and server are functions
  expect_true(is.function(page$ui))
  expect_true(is.function(page$server))
})

test_that("page_about ui function returns valid HTML", {
  page <- page_about()

  # Call the ui function
  ui_result <- page$ui(request = list())

  # Should return a tagList
  expect_s3_class(ui_result, "shiny.tag.list")

  # Convert to character and check content
  ui_html <- as.character(ui_result)

  # Check for key content
  expect_true(grepl("About", ui_html))
  expect_true(grepl("investR", ui_html))
  expect_true(grepl("golem", ui_html))
  expect_true(grepl("Brochure", ui_html))
})

test_that("page_about includes navigation back to home", {
  page <- page_about()
  ui_result <- page$ui(request = list())
  ui_html <- as.character(ui_result)

  # Should have link back to home
  expect_true(grepl('href="/"', ui_html))
  expect_true(grepl("Home", ui_html) || grepl("Back", ui_html))
})

test_that("all pages have unique hrefs", {
  home <- page_home()
  aristocrats <- page_aristocrats()
  about <- page_about()

  hrefs <- c(home$href, aristocrats$href, about$href)

  # All hrefs should be unique
  expect_equal(length(hrefs), length(unique(hrefs)))

  # Check expected values
  expect_true("/" %in% hrefs)
  expect_true("/aristocrats" %in% hrefs)
  expect_true("/about" %in% hrefs)
})

test_that("page_home structure is valid", {
  page <- page_home()

  # Home page should be a simple brochure page
  # Check it has the basic structure
  expect_s3_class(page, "brochure_page")
  expect_equal(page$href, "/")
  expect_true(is.function(page$ui))
})

test_that("page_aristocrats server function is valid", {
  page <- page_aristocrats()

  # Server should be a function
  expect_true(is.function(page$server))

  # Server function should accept input, output, session
  server_args <- names(formals(page$server))
  expect_true("input" %in% server_args)
  expect_true("output" %in% server_args)
  expect_true("session" %in% server_args)
})

test_that("page_about server function is valid", {
  page <- page_about()

  # Server should be a function
  expect_true(is.function(page$server))

  # Server function should accept input, output, session
  server_args <- names(formals(page$server))
  expect_true("input" %in% server_args)
  expect_true("output" %in% server_args)
  expect_true("session" %in% server_args)
})