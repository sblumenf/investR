#' dividend_capture_monthly_controls UI Function
#'
#' @description Shiny Module for monthly dividend capture analysis controls.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList sidebarPanel sliderInput actionButton downloadButton hr tags icon br p strong h3 h5 checkboxGroupInput
mod_dividend_capture_monthly_controls_ui <- function(id) {
  ns <- NS(id)

  config <- get_dividend_capture_monthly_config()

  sidebarPanel(
    width = 3,
    h3("Monthly Dividend ETFs"),

    # Quote source toggle
    quote_source_toggle_ui(ns),

    hr(),

    # Info text
    p("Analyzes 76+ monthly dividend ETFs for dividend capture opportunities."),
    p(
      strong("Strategy:"),
      "Buy at close before ex-dividend, sell at open on ex-dividend day."
    ),

    hr(),

    # Days since last dividend filter
    h5("Filter Options"),
    checkboxGroupInput(
      ns("days_since_filter"),
      "Days Since Last Ex-Dividend",
      choices = config$days_since_dividend_ranges,
      selected = config$days_since_dividend_ranges,
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

#' dividend_capture_monthly_controls Server Functions
#'
#' @description Server logic for dividend capture controls module.
#'   Returns reactive values for results data and status UI.
#'   Uses the analysis controls helper function to eliminate code duplication.
#'
#' @param id Module ID
#'
#' @return A list with reactive values: results_data, days_since_filter, status_ui
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive
mod_dividend_capture_monthly_controls_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Setup quote source toggle
    quote_source_toggle_server(input, session, "Monthly Dividend Capture")

    # Use analysis controls helper with monthly-specific configuration
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = batch_analyze_monthly_etfs,
      progress_message = "Analyzing 76+ monthly dividend ETFs... This may take 3-8 minutes.",
      success_message_template = "Analysis complete! Analyzed %d ETFs successfully.",
      no_results_message = "No ETFs could be analyzed. Check logs for details.",
      download_filename_prefix = "monthly_dividend_capture",
      additional_return_values = list(
        days_since_filter = reactive({ input$days_since_filter })
      )
    )

    # Return all values from helper plus days_since_filter
    return(result)
  })
}
