#' dividend_capture_weekly_controls UI Function
#'
#' @description Shiny Module for weekly dividend capture analysis controls.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList sidebarPanel sliderInput actionButton downloadButton hr tags icon br
mod_dividend_capture_weekly_controls_ui <- function(id) {
  ns <- NS(id)

  config <- get_dividend_capture_config()

  sidebarPanel(
    width = 3,
    h3("Weekly Dividend ETFs"),

    # Info text
    p("Analyzes all 32 weekly dividend ETFs for dividend capture opportunities."),
    p(
      strong("Strategy:"),
      "Buy at close before ex-dividend, sell at open on ex-dividend day."
    ),

    hr(),

    # Day of week filter
    h5("Filter Options"),
    checkboxGroupInput(
      ns("day_filter"),
      "Buy Day",
      choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
      selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
      inline = FALSE
    ),

    hr(),

    # Run analysis button
    actionButton(
      ns("run_analysis"),
      "Run Analysis",
      class = "btn-primary btn-lg btn-block",
      icon = icon("chart-line")
    ),

    br(),

    # Download CSV button
    downloadButton(
      ns("download_csv"),
      "Download CSV",
      class = "btn-success btn-block"
    ),

    hr(),

    # Home navigation
    tags$a(
      href = "/",
      class = "btn btn-default btn-block",
      icon("home"),
      " Home"
    )
  )
}

#' dividend_capture_weekly_controls Server Functions
#'
#' @description Server logic for dividend capture controls module.
#'   Returns reactive values for results data and status UI.
#'   Uses the analysis controls helper function to eliminate code duplication.
#'
#' @param id Module ID
#'
#' @return A list with reactive values: results_data, day_filter, status_ui
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive
mod_dividend_capture_weekly_controls_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Use analysis controls helper with weekly-specific configuration
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = batch_analyze_weekly_etfs,
      progress_message = "Analyzing 32 weekly dividend ETFs... This may take 2-5 minutes.",
      success_message_template = "Analysis complete! Analyzed %d ETFs successfully.",
      no_results_message = "No ETFs could be analyzed. Check logs for details.",
      download_filename_prefix = "weekly_dividend_capture",
      additional_return_values = list(
        day_filter = reactive({ input$day_filter })
      )
    )

    # Return all values from helper plus day_filter
    return(result)
  })
}
