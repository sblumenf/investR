#' put_calendar_spread UI Function
#'
#' @description A shiny Module for put calendar spread parameters and execution.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_put_calendar_spread_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h3("Calendar Spread Parameters"),

      # Quote source toggle
      quote_source_toggle_ui(ns),

      hr(),

      # Strike threshold slider (80-105%, default 95% = ATM)
      sliderInput(
        ns("strike_pct"),
        "Strike (% of current price)",
        min = 80,
        max = 105,
        value = 95,
        step = 5,
        post = "%"
      ),
      helpText("95% = ATM (recommended). Lower = more OTM."),

      hr(),

      h4("Short Leg (Sell Put)"),
      # Short leg days range (front month)
      sliderInput(
        ns("short_days_range"),
        "Short Leg Expiry (days)",
        min = 7,
        max = 60,
        value = c(20, 45),
        step = 5
      ),
      helpText("Front month: 20-45 DTE optimal for theta decay"),

      hr(),

      h4("Long Leg (Buy Put)"),
      # Long leg days range (back month)
      sliderInput(
        ns("long_days_range"),
        "Long Leg Expiry (days)",
        min = 45,
        max = 180,
        value = c(50, 120),
        step = 5
      ),
      helpText("Back month: 60-90 DTE for 2:1 ratio"),

      hr(),

      # Parallel workers slider
      sliderInput(
        ns("max_workers"),
        "Parallel Workers",
        min = 1,
        max = 20,
        value = 4,
        step = 1
      ),
      helpText("Note: Use 1 worker during development"),

      hr(),

      # Run analysis button
      actionButton(
        ns("run_analysis"),
        "Run Analysis",
        class = "btn-primary btn-lg btn-block",
        icon = icon("calendar-alt")
      ),

      br(),

      # Download results button
      downloadButton(
        ns("download_results"),
        "Download CSV",
        class = "btn-success btn-block"
      ),

      hr(),

      # Navigation
      tags$a(
        href = "/",
        class = "btn btn-default btn-block",
        icon("home"),
        " Home"
      )
    )
  )
}

#' put_calendar_spread Server Functions
#'
#' @description Server logic for put calendar spread module.
#'   Returns a reactive containing analysis results data.
#'   Uses the analysis controls helper function to eliminate code duplication.
#'
#' @param id Module ID
#'
#' @return A list with reactive values: results_data, status_ui
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive
mod_put_calendar_spread_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Setup quote source toggle
    quote_source_toggle_server(input, session, "Put Calendar Spread")

    # Create analysis function
    analysis_function <- function() {
      analyze_put_calendar_spread(
        limit = NULL,
        strike_pct = input$strike_pct / 100,
        short_min_days = input$short_days_range[1],
        short_max_days = input$short_days_range[2],
        long_min_days = input$long_days_range[1],
        long_max_days = input$long_days_range[2],
        max_workers = input$max_workers
      )
    }

    # Use analysis controls helper function (DRY!)
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = analysis_function,
      progress_message = "Analyzing dividend aristocrats for put calendar spread opportunities... This may take several minutes on first run.",
      success_message_template = "Analysis complete! Found %d calendar spread opportunities.",
      no_results_message = "No opportunities found. Try adjusting strike or expiration parameters.",
      download_filename_prefix = "put_calendar_spread_analysis"
    )

    # Return reactive values
    return(result)
  })
}

## To be copied in the UI
# mod_put_calendar_spread_ui("put_calendar_spread_1")

## To be copied in the server
# mod_put_calendar_spread_server("put_calendar_spread_1")
