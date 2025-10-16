#' zero_dividend_analysis UI Function
#'
#' @description A shiny Module for zero-dividend analysis parameters and execution.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_zero_dividend_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h3("Strategy Parameters"),

      # Quote source toggle
      quote_source_toggle_ui(ns),

      hr(),

      # Strike threshold slider (50-100%, default 85%)
      sliderInput(
        ns("strike_threshold"),
        "Strike Threshold (% of current price)",
        min = 50,
        max = 100,
        value = 85,
        step = 5,
        post = "%"
      ),
      helpText("Deeper ITM = Lower risk, lower return"),

      # Days to expiry range slider (30-365, default 45-120)
      sliderInput(
        ns("days_range"),
        "Days to Expiry Range",
        min = 30,
        max = 365,
        value = c(45, 120),  # Range selector
        step = 5
      ),
      helpText("Select minimum and maximum days to expiration"),

      hr(),

      # Expiry month filter (client-side filtering)
      h5("Filter Options"),
      checkboxGroupInput(
        ns("expiry_month_filter"),
        "Expiry Month",
        choices = c(
          "January" = "1",
          "February" = "2",
          "March" = "3",
          "April" = "4",
          "May" = "5",
          "June" = "6",
          "July" = "7",
          "August" = "8",
          "September" = "9",
          "October" = "10",
          "November" = "11",
          "December" = "12"
        ),
        selected = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
        inline = FALSE
      ),
      helpText("Check/uncheck months to filter results instantly"),

      # Parallel workers slider
      sliderInput(
        ns("max_workers"),
        "Parallel Workers",
        min = 1,
        max = 20,
        value = 4,  # Default to 4 for production
        step = 1
      ),
      helpText("Note: Use 1 worker during development with devtools::load_all()"),

      hr(),

      # Run analysis button
      actionButton(
        ns("run_analysis"),
        "Run Analysis",
        class = "btn-primary btn-lg btn-block",
        icon = icon("chart-line")
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

#' zero_dividend_analysis Server Functions
#'
#' @description Server logic for zero-dividend analysis module.
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
mod_zero_dividend_analysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Setup quote source toggle
    quote_source_toggle_server(input, session, "Zero Dividend Analysis")

    # Create analysis function using parameters from sliders
    # Month filtering happens client-side after results are returned
    analysis_function <- function() {
      analyze_zero_dividend(
        limit = NULL,
        strike_threshold_pct = input$strike_threshold / 100,
        min_days = input$days_range[1],
        max_days = input$days_range[2],
        expiry_month = NULL,  # Always NULL - filter client-side with checkboxes
        max_workers = input$max_workers
      )
    }

    # Use analysis controls helper function (DRY!)
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = analysis_function,
      progress_message = "Analyzing zero-dividend stocks... This may take several minutes on first run (building cache).",
      success_message_template = "Analysis complete! Found %d opportunities. Use month checkboxes to filter.",
      no_results_message = "No opportunities found with current parameters. Try adjusting strike threshold or expiration range.",
      download_filename_prefix = "zero_dividend_analysis",
      additional_return_values = list(
        expiry_month_filter = reactive({ input$expiry_month_filter })
      )
    )

    # Return all values including month filter
    return(result)
  })
}

## To be copied in the UI
# mod_zero_dividend_analysis_ui("zero_dividend_analysis_1")

## To be copied in the server
# mod_zero_dividend_analysis_server("zero_dividend_analysis_1")
