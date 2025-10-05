#' UI Component Builders
#'
#' Reusable UI component functions for creating consistent card-based layouts
#' across all strategies. These components use bslib cards and HTML5 details
#' elements for native accordions.
#'
#' @name ui-components
#' @importFrom shiny tags div icon
#' @importFrom bslib card card_header card_body
NULL

#' Create a metric row with label and value
#'
#' Generates a standardized metric display row with consistent styling.
#'
#' @param label Metric label string
#' @param value Formatted value string
#' @param is_primary Logical, if TRUE uses larger/bolder font for key metrics
#' @param is_negative Logical, if TRUE applies negative (red) styling
#' @return HTML div element with metric row
#' @export
create_metric_row <- function(label,
                              value,
                              is_primary = FALSE,
                              is_negative = FALSE) {
  value_class <- paste(
    if (is_primary) "metric-value-primary" else "metric-value",
    if (is_negative) "negative-value" else ""
  )

  tags$div(
    class = "metric-row",
    tags$span(class = "metric-label", paste0(label, ":")),
    tags$span(class = value_class, value)
  )
}

#' Create an accordion section using HTML5 details element
#'
#' Creates a collapsible section with title and content.
#'
#' @param title Section title string
#' @param is_open Logical, should section be open by default
#' @param ... Metric rows or other content to include in section body
#' @return HTML details element with summary and body
#' @export
create_accordion_section <- function(title, is_open = FALSE, ...) {
  if (is_open) {
    tags$details(
      class = "accordion-section",
      open = NA,
      tags$summary(class = "accordion-header", title),
      tags$div(class = "accordion-body", ...)
    )
  } else {
    tags$details(
      class = "accordion-section",
      tags$summary(class = "accordion-header", title),
      tags$div(class = "accordion-body", ...)
    )
  }
}

#' Create a generic card header
#'
#' Creates a card header with primary and secondary text.
#' Flexible for different strategies (stocks, ETFs, etc.)
#'
#' @param primary_text Main title text (e.g., company name, ETF ticker)
#' @param secondary_text Subtitle text (e.g., ticker, price)
#' @return bslib card_header element
#' @export
create_generic_card_header <- function(primary_text, secondary_text = NULL) {
  if (is.null(secondary_text)) {
    # Single line header
    card_header(
      tags$div(class = "company-info", primary_text)
    )
  } else {
    # Two-line header
    card_header(
      tags$div(class = "company-info", primary_text),
      tags$div(class = "stock-price", secondary_text)
    )
  }
}

#' Create a status alert message
#'
#' Creates a Bootstrap-styled alert box for status messages.
#'
#' @param type Alert type: "info", "success", "warning", "danger"
#' @param message Alert message text
#' @param icon_name Font Awesome icon name (optional, auto-selects based on type if NULL)
#' @return HTML div element with alert styling
#' @export
create_status_alert <- function(type = "info", message, icon_name = NULL) {
  # Auto-select icon based on type if not provided
  if (is.null(icon_name)) {
    icon_name <- switch(type,
      "info" = "info-circle",
      "success" = "check-circle",
      "warning" = "exclamation-triangle",
      "danger" = "times-circle",
      "info-circle"  # default
    )
  }

  alert_class <- paste0("alert alert-", type)

  div(
    class = alert_class,
    icon(icon_name),
    " ",
    message
  )
}

#' Create a spinning progress alert
#'
#' Convenience function for displaying progress/loading state.
#'
#' @param message Progress message text
#' @return HTML div element with spinner and info alert styling
#' @export
create_progress_alert <- function(message) {
  div(
    class = "alert alert-info",
    icon("spinner", class = "fa-spin"),
    " ",
    message
  )
}

#' Create a card with warning styling
#'
#' Wrapper around bslib::card that adds warning class for flagged items.
#'
#' @param class Additional CSS classes
#' @param ... Card content (header, body, etc.)
#' @return bslib card with warning styling
#' @export
create_warning_card <- function(..., class = NULL) {
  combined_class <- if (is.null(class)) {
    "opportunity-card low-dividend-warning"
  } else {
    paste("opportunity-card low-dividend-warning", class)
  }

  card(class = combined_class, ...)
}

#' Create a standard card
#'
#' Wrapper around bslib::card with standard opportunity card styling.
#'
#' @param class Additional CSS classes
#' @param ... Card content (header, body, etc.)
#' @return bslib card element
#' @export
create_standard_card <- function(..., class = NULL) {
  combined_class <- if (is.null(class)) {
    "opportunity-card"
  } else {
    paste("opportunity-card", class)
  }

  card(class = combined_class, ...)
}

#' Create a strategy card for home page
#'
#' Generates a standardized strategy card (well component) with title,
#' description, and navigation button. Used for organizing strategies
#' on the home page.
#'
#' @param title Strategy title string
#' @param description Character vector of description paragraphs
#' @param href Navigation link (e.g., "/aristocrats")
#' @param button_text Text for the navigation button
#' @return HTML div element with well styling
#' @export
create_strategy_card <- function(title, description, href, button_text) {
  # Build description paragraphs
  description_tags <- purrr::map(description, ~ tags$p(.x))

  div(
    class = "well",
    h4(title),
    description_tags,
    tags$a(
      href = href,
      class = "btn btn-primary btn-lg",
      button_text
    )
  )
}

#' Create a home page accordion section with strategy cards
#'
#' Generates an accordion section containing strategy cards arranged in
#' a two-column layout. Uses HTML5 details/summary for native accordion
#' behavior.
#'
#' @param title Section title (e.g., "Covered Call Strategies")
#' @param strategies List of strategy metadata (each with title, description, href, button_text)
#' @param is_open Logical, should section be open by default
#' @return HTML details element with strategy cards
#' @export
create_home_accordion_section <- function(title, strategies, is_open = FALSE) {
  # Split strategies into pairs for 2-column layout
  strategy_pairs <- split(strategies, ceiling(seq_along(strategies) / 2))

  # Create rows of strategy cards
  card_rows <- purrr::map(strategy_pairs, function(pair) {
    # Create columns for each strategy in the pair
    columns <- purrr::map(pair, function(strategy) {
      column(
        width = 6,
        create_strategy_card(
          title = strategy$title,
          description = strategy$description,
          href = strategy$href,
          button_text = strategy$button_text
        )
      )
    })

    # Return a fluidRow with the columns
    do.call(fluidRow, columns)
  })

  # Wrap in accordion section
  create_accordion_section(
    title = title,
    is_open = is_open,
    card_rows
  )
}
