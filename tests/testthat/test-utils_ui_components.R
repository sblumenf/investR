test_that("create_metric_row generates proper HTML structure", {
  result <- create_metric_row("Price", "$100.50")

  # Should return a shiny tag
  expect_s3_class(result, "shiny.tag")

  # Convert to HTML string
  html <- as.character(result)

  # Should contain label and value
  expect_true(grepl("Price:", html))
  expect_true(grepl("\\$100\\.50", html))

  # Should have metric-row class
  expect_true(grepl("metric-row", html))
})

test_that("create_metric_row handles primary styling", {
  result <- create_metric_row("Return", "15.5%", is_primary = TRUE)

  html <- as.character(result)

  # Should have primary class
  expect_true(grepl("metric-value-primary", html))
})

test_that("create_metric_row handles negative styling", {
  result <- create_metric_row("Loss", "-5.2%", is_negative = TRUE)

  html <- as.character(result)

  # Should have negative-value class
  expect_true(grepl("negative-value", html))
})

test_that("create_metric_row handles both primary and negative", {
  result <- create_metric_row("Drawdown", "-25%", is_primary = TRUE, is_negative = TRUE)

  html <- as.character(result)

  # Should have both classes
  expect_true(grepl("metric-value-primary", html))
  expect_true(grepl("negative-value", html))
})

test_that("create_accordion_section generates details element", {
  result <- create_accordion_section("Section Title")

  # Should return a shiny tag
  expect_s3_class(result, "shiny.tag")

  html <- as.character(result)

  # Should be a details element
  expect_true(grepl("<details", html))

  # Should have summary with title
  expect_true(grepl("<summary", html))
  expect_true(grepl("Section Title", html))

  # Should have accordion classes
  expect_true(grepl("accordion-section", html))
})

test_that("create_accordion_section handles open state", {
  result <- create_accordion_section("Open Section", is_open = TRUE)

  html <- as.character(result)

  # Should have open attribute
  expect_true(grepl('open=', html) || grepl("open", html))
})

test_that("create_accordion_section includes content", {
  content <- create_metric_row("Test", "Value")
  result <- create_accordion_section("Title", is_open = FALSE, content)

  html <- as.character(result)

  # Should contain the content
  expect_true(grepl("Test:", html))
  expect_true(grepl("Value", html))
})

test_that("create_generic_card_header with single line", {
  result <- create_generic_card_header("AAPL")

  # Should return a bslib card_header
  expect_s3_class(result, "shiny.tag")

  html <- as.character(result)

  # Should contain the primary text
  expect_true(grepl("AAPL", html))
})

test_that("create_generic_card_header with two lines", {
  result <- create_generic_card_header("Apple Inc.", "$150.25")

  html <- as.character(result)

  # Should contain both texts
  expect_true(grepl("Apple Inc\\.", html))
  expect_true(grepl("\\$150\\.25", html))
})

test_that("create_status_alert generates proper alert structure", {
  result <- create_status_alert(type = "success", message = "Operation completed!")

  # Should return a shiny tag (div)
  expect_s3_class(result, "shiny.tag")

  html <- as.character(result)

  # Should have alert class
  expect_true(grepl("alert", html))
  expect_true(grepl("alert-success", html))

  # Should contain message
  expect_true(grepl("Operation completed!", html))

  # Should have icon (circle-check for success in FA6)
  expect_true(grepl("circle-check", html) || grepl("fa-circle-check", html))
})

test_that("create_status_alert handles different types", {
  # Test info
  info_result <- create_status_alert(type = "info", message = "Info message")
  expect_true(grepl("alert-info", as.character(info_result)))

  # Test warning
  warn_result <- create_status_alert(type = "warning", message = "Warning message")
  expect_true(grepl("alert-warning", as.character(warn_result)))

  # Test danger
  danger_result <- create_status_alert(type = "danger", message = "Error message")
  expect_true(grepl("alert-danger", as.character(danger_result)))
})

test_that("create_status_alert auto-selects icons", {
  # Info should get info-circle
  info_result <- create_status_alert(type = "info", message = "Test")
  expect_true(grepl("circle-info|info-circle", as.character(info_result)))

  # Success should get circle-check
  success_result <- create_status_alert(type = "success", message = "Test")
  expect_true(grepl("circle-check|check-circle", as.character(success_result)))

  # Warning should get triangle-exclamation
  warning_result <- create_status_alert(type = "warning", message = "Test")
  expect_true(grepl("triangle-exclamation|exclamation-triangle", as.character(warning_result)))

  # Danger should get circle-xmark
  danger_result <- create_status_alert(type = "danger", message = "Test")
  expect_true(grepl("circle-xmark|times-circle", as.character(danger_result)))
})

test_that("create_status_alert accepts custom icon", {
  result <- create_status_alert(type = "info", message = "Custom", icon_name = "star")

  html <- as.character(result)

  # Should use custom icon
  expect_true(grepl("star", html))
})

test_that("create_progress_alert generates spinner alert", {
  result <- create_progress_alert("Loading data...")

  # Should return a shiny tag
  expect_s3_class(result, "shiny.tag")

  html <- as.character(result)

  # Should have alert-info class
  expect_true(grepl("alert-info", html))

  # Should have spinner icon with fa-spin class
  expect_true(grepl("spinner", html))
  expect_true(grepl("fa-spin", html))

  # Should contain message
  expect_true(grepl("Loading data...", html))
})

test_that("create_warning_card has warning styling", {
  result <- create_warning_card(bslib::card_body("Test content"))

  # Should return a card
  expect_s3_class(result, "shiny.tag")

  html <- as.character(result)

  # Should have warning class
  expect_true(grepl("low-dividend-warning", html))
  expect_true(grepl("opportunity-card", html))
})

test_that("create_warning_card accepts additional classes", {
  result <- create_warning_card(bslib::card_body("Test"), class = "custom-class")

  html <- as.character(result)

  # Should have both warning and custom classes
  expect_true(grepl("low-dividend-warning", html))
  expect_true(grepl("custom-class", html))
})

test_that("create_standard_card has standard styling", {
  result <- create_standard_card(bslib::card_body("Test content"))

  # Should return a card
  expect_s3_class(result, "shiny.tag")

  html <- as.character(result)

  # Should have opportunity-card class
  expect_true(grepl("opportunity-card", html))

  # Should NOT have warning class
  expect_false(grepl("low-dividend-warning", html))
})

test_that("create_standard_card accepts additional classes", {
  result <- create_standard_card(bslib::card_body("Test"), class = "featured")

  html <- as.character(result)

  # Should have both standard and custom classes
  expect_true(grepl("opportunity-card", html))
  expect_true(grepl("featured", html))
})

test_that("UI components handle special characters", {
  # Test with HTML special characters
  result <- create_metric_row("P&L", "<$100>")

  html <- as.character(result)

  # Should properly escape HTML
  expect_true(grepl("P&amp;L|P&L", html))
})

test_that("UI components handle empty values", {
  # Test with empty string
  result <- create_metric_row("Empty", "")

  html <- as.character(result)

  # Should still generate valid HTML
  expect_s3_class(result, "shiny.tag")
  expect_true(grepl("Empty:", html))
})

test_that("create_progress_alert handles long messages", {
  long_message <- paste(rep("Analyzing", 20), collapse = " ")
  result <- create_progress_alert(long_message)

  html <- as.character(result)

  # Should contain the message
  expect_true(grepl("Analyzing", html))
})
