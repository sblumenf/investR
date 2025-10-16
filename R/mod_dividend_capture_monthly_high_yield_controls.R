#' dividend_capture_monthly_high_yield_controls UI Function
#'
#' @description Shiny Module for monthly high-yield dividend capture analysis controls.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList sidebarPanel actionButton downloadButton hr tags icon br p h3 strong
mod_dividend_capture_monthly_high_yield_controls_ui <- function(id) {
  ns <- NS(id)

  config <- get_monthly_high_yield_config()

  sidebarPanel(
    width = 3,
    h3("Monthly High-Yield ETFs"),

    # Quote source toggle
    quote_source_toggle_ui(ns),

    hr(),

    # Info text
    p("Analyzes 100+ monthly dividend ETFs with yields above 8%."),
    p("Ticker list is fetched dynamically from stockanalysis.com."),
    p(
      strong("Strategy:"),
      "Buy at close before ex-dividend, sell at open on ex-dividend day."
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

#' dividend_capture_monthly_high_yield_controls Server Functions
#'
#' @description Server logic for dividend capture controls module.
#'   Returns reactive values for results data and status UI.
#'   Uses the analysis controls helper function to eliminate code duplication.
#'
#' @param id Module ID
#'
#' @return A list with reactive values: results_data, min_yield, status_ui
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive
mod_dividend_capture_monthly_high_yield_controls_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Setup quote source toggle
    quote_source_toggle_server(input, session, "Monthly High-Yield Dividend Capture")

    # Use analysis controls helper with monthly high-yield-specific configuration
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = function() {
        # Use default min_yield from config (8%)
        batch_analyze_monthly_high_yield_etfs()
      },
      progress_message = "Analyzing monthly high-yield dividend ETFs... This may take 1-3 minutes.",
      success_message_template = "Analysis complete! Analyzed %d ETFs successfully.",
      no_results_message = "No ETFs could be analyzed. Check logs for details.",
      download_filename_prefix = "monthly_high_yield_dividend_capture"
    )

    # Return values from helper
    return(result)
  })
}
