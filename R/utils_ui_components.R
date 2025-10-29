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
create_status_alert <- function(type = "info", message, icon_name = NULL, action_button = NULL) {
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

  # Add classes for flexbox layout if a button is present
  if (!is.null(action_button)) {
    alert_class <- paste(alert_class, "d-flex align-items-center justify-content-between")
  }

  # Main message content
  message_content <- div(
    icon(icon_name),
    " ",
    message
  )

  # If an action button is provided, wrap message and button in a flex container
  if (!is.null(action_button)) {
    div(
      class = alert_class,
      message_content,
      action_button
    )
  } else {
    div(
      class = alert_class,
      message_content
    )
  }
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

################################################################################
# POSITION GROUPS UI COMPONENTS
################################################################################

#' Create a status badge for position groups
#'
#' Creates a colored badge indicating group status (active, incomplete, broken)
#'
#' @param status Character: "active", "incomplete", or "broken"
#' @return HTML span element with badge styling
#' @export
create_group_status_badge <- function(status) {
  # Determine badge class and icon based on status
  badge_info <- switch(status,
    "active" = list(class = "success", icon = "check-circle", text = "Active"),
    "incomplete" = list(class = "warning", icon = "exclamation-triangle", text = "Incomplete"),
    "broken" = list(class = "danger", icon = "times-circle", text = "Broken"),
    list(class = "secondary", icon = "question-circle", text = "Unknown")
  )

  tags$span(
    class = sprintf("badge badge-%s", badge_info$class),
    style = "font-size: 12px;",
    icon(badge_info$icon),
    " ",
    badge_info$text
  )
}

#' Create a summary alert for broken groups
#'
#' Creates a dismissible alert showing count of groups needing attention
#'
#' @param broken_count Number of broken groups
#' @param incomplete_count Number of incomplete groups
#' @return HTML div element with alert styling
#' @export
create_groups_summary_alert <- function(broken_count = 0, incomplete_count = 0) {
  total_count <- broken_count + incomplete_count

  if (total_count == 0) {
    return(NULL)
  }

  message_parts <- c()
  if (incomplete_count > 0) {
    message_parts <- c(message_parts,
                       sprintf("%d incomplete group%s", incomplete_count,
                               if (incomplete_count > 1) "s" else ""))
  }
  if (broken_count > 0) {
    message_parts <- c(message_parts,
                       sprintf("%d broken group%s", broken_count,
                               if (broken_count > 1) "s" else ""))
  }

  div(
    class = "alert alert-warning alert-dismissible fade in",
    role = "alert",
    style = "margin-bottom: 20px;",
    tags$button(
      type = "button",
      class = "close",
      `data-dismiss` = "alert",
      `aria-label` = "Close",
      tags$span(`aria-hidden` = "true", HTML("&times;"))
    ),
    tags$h5(
      icon("exclamation-triangle"),
      sprintf(" %d Position Group%s Need%s Attention",
              total_count,
              if (total_count > 1) "s" else "",
              if (total_count == 1) "s" else "")
    ),
    tags$p(paste(message_parts, collapse = " and ")),
    tags$a(
      href = "/portfolio/groups",
      class = "btn btn-sm btn-warning",
      icon("cog"),
      " Manage Groups"
    )
  )
}

################################################################################
# GENERIC CARD BUILDER
################################################################################

#' Create a generic strategy opportunity card
#'
#' Reusable card builder for all strategy opportunity displays. Provides
#' consistent structure across strategies while allowing strategy-specific
#' content through section configuration.
#'
#' @param row Single-row data frame with opportunity data
#' @param primary_text Card header primary text (e.g., "Company Name (TICKER)")
#' @param secondary_text Card header secondary text (e.g., "$XX.XX")
#' @param sections List of section configurations. Each section is a list with:
#'   - title: Section title string
#'   - is_open: Logical, should section be open by default
#'   - metrics: List of metric specifications, each with:
#'     - label: Metric label string
#'     - value: Formatted value string
#'     - is_primary: Logical, use primary styling (default FALSE)
#'     - is_negative: Logical, use negative/red styling (default FALSE)
#' @param ns Optional namespace function for risk button (from module)
#' @param risk_id Optional risk module ID for risk analysis button
#' @param include_risk_button Logical, whether to include risk analysis button (default TRUE)
#' @param card_class CSS class for card styling (default "opportunity-card")
#'
#' @return A bslib card component with standardized structure
#' @export
#'
#' @examples
#' \dontrun{
#'   sections <- list(
#'     list(
#'       title = "Quick Overview",
#'       is_open = TRUE,
#'       metrics = list(
#'         list(label = "Return", value = "10.5%", is_primary = TRUE),
#'         list(label = "Days", value = "45")
#'       )
#'     )
#'   )
#'   create_strategy_opportunity_card(
#'     row = opportunity_data,
#'     primary_text = "Apple Inc. (AAPL)",
#'     secondary_text = "$150.00",
#'     sections = sections
#'   )
#' }
create_strategy_opportunity_card <- function(
  row,
  primary_text,
  secondary_text,
  sections,
  ns = NULL,
  risk_id = NULL,
  include_risk_button = TRUE,
  card_class = "opportunity-card"
) {
  # Create card header using generic header builder
  header <- create_generic_card_header(
    primary_text = primary_text,
    secondary_text = secondary_text
  )

  # Build card body content
  body_content <- list()

  # Add risk analysis button if requested
  if (include_risk_button && !is.null(ns) && !is.null(risk_id)) {
    # Extract row index from risk_id (e.g., "risk_1" -> 1)
    idx <- as.integer(sub("risk_", "", risk_id))

    body_content[[length(body_content) + 1]] <- tags$div(
      style = "margin-bottom: 15px;",
      actionButton(
        inputId = ns(paste0("analyze_risk_btn_", idx)),
        label = "Analyze Risk",
        icon = icon("chart-line"),
        class = "btn btn-primary btn-sm",
        style = "width: 100%;"
      )
    )
  }

  # Build accordion sections
  for (section in sections) {
    # Build metric rows for this section
    metric_rows <- purrr::map(section$metrics, function(metric) {
      create_metric_row(
        label = metric$label,
        value = metric$value,
        is_primary = metric$is_primary %||% FALSE,
        is_negative = metric$is_negative %||% FALSE
      )
    })

    # Create accordion section using do.call to unpack metric_rows list
    accordion <- do.call(
      create_accordion_section,
      c(
        list(title = section$title, is_open = section$is_open),
        metric_rows
      )
    )

    body_content[[length(body_content) + 1]] <- accordion
  }

  # Create card body with all content
  body <- do.call(bslib::card_body, body_content)

  # Combine into final card
  bslib::card(
    header,
    body,
    class = card_class
  )
}
