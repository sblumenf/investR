#' Quote Source Toggle Components
#'
#' Reusable UI and server components for selecting stock quote data source
#' (Yahoo Finance vs Questrade API) within strategy modules.
#'
#' @name quote-source-toggle
#' @importFrom shiny NS radioButtons observeEvent reactive
#' @importFrom logger log_info
NULL

#' Create quote source toggle UI
#'
#' Generates a radio button group for selecting quote data source.
#' Designed to be embedded in strategy module sidebars.
#'
#' @param ns Namespacing function from module (e.g., NS(id))
#' @return Shiny radio buttons input
#' @export
quote_source_toggle_ui <- function(ns) {
  radioButtons(
    inputId = ns("quote_source"),
    label = "Stock Quote Source:",
    choices = c(
      "Yahoo Finance" = "yahoo",
      "Questrade API" = "questrade"
    ),
    selected = "yahoo",
    inline = FALSE
  )
}

#' Setup quote source toggle server logic
#'
#' Handles quote source selection and sets the appropriate option
#' for the quote fetching infrastructure. Call this within your
#' module server function.
#'
#' @param input Shiny input object from parent moduleServer
#' @param session Shiny session object from parent moduleServer
#' @param strategy_name Character name of strategy for logging
#' @return A reactive expression returning current quote source ("yahoo" or "questrade")
#' @export
quote_source_toggle_server <- function(input, session, strategy_name) {
  # Reactive value to track current source
  current_source <- reactive({
    input$quote_source
  })

  # Observer to set global option when source changes
  observeEvent(input$quote_source, {
    log_info("{strategy_name}: Quote source changed to {input$quote_source}")
    options(investR.quote_source = input$quote_source)
  })

  # Return reactive for potential use by parent module
  return(current_source)
}

#' Set quote source option programmatically
#'
#' Helper function to set the quote source option directly.
#' Useful for ensuring correct source is set before running analysis.
#'
#' @param source Character: "yahoo" or "questrade"
#' @noRd
set_quote_source <- function(source) {
  if (!source %in% c("yahoo", "questrade")) {
    stop("Quote source must be 'yahoo' or 'questrade'")
  }
  options(investR.quote_source = source)
}

#' Get current quote source setting
#'
#' Helper function to retrieve current quote source option.
#'
#' @return Character: "yahoo" (default) or "questrade"
#' @export
get_quote_source <- function() {
  getOption("investR.quote_source", default = "yahoo")
}

#' Check and notify about Questrade fallbacks
#'
#' Checks if any Questrade quote fetches fell back to Yahoo Finance
#' during the analysis run. If fallbacks occurred and Questrade was
#' selected, shows a warning notification to the user.
#'
#' Call this after an analysis completes to inform users about fallbacks.
#'
#' @return NULL (invisibly), side effect is showing notification
#' @importFrom shiny showNotification
#' @export
check_and_notify_fallbacks <- function() {
  # Only check if Questrade was selected
  if (get_quote_source() != "questrade") {
    return(invisible(NULL))
  }

  # Get fallback summary
  summary <- get_fallback_summary()

  # Show notification if fallbacks occurred
  if (summary$count > 0) {
    showNotification(
      summary$message,
      type = "warning",
      duration = 10
    )
  }

  invisible(NULL)
}
