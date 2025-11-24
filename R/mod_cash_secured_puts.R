#' cash_secured_puts UI Function
#'
#' @description A shiny Module for cash-secured puts parameters and execution.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cash_secured_puts_ui <- function(id){
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
      helpText("Lower % = More OTM, more downside protection. 85% = 15% OTM"),

      # Days to expiry range slider (7-90, default 30-45)
      sliderInput(
        ns("days_range"),
        "Days to Expiry Range",
        min = 7,
        max = 90,
        value = c(30, 45),  # Range selector
        step = 5
      ),
      helpText("Select minimum and maximum days to expiration"),

      hr(),

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

#' cash_secured_puts Server Functions
#'
#' @description Server logic for cash-secured puts module.
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
mod_cash_secured_puts_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Setup quote source toggle
    quote_source_toggle_server(input, session, "Cash-Secured Puts")

    # Create analysis function
    analysis_function <- function() {
      analyze_cash_secured_puts(
        limit = NULL,
        strike_threshold_pct = input$strike_threshold / 100,
        min_days = input$days_range[1],
        max_days = input$days_range[2],
        expiry_month = NULL,
        max_workers = input$max_workers
      )
    }

    # Use analysis controls helper function (DRY!)
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = analysis_function,
      progress_message = "Analyzing dividend aristocrats for cash-secured put opportunities... This may take several minutes on first run (building cache).",
      success_message_template = "Analysis complete! Found %d cash-secured put opportunities.",
      no_results_message = "No opportunities found with current parameters. Try adjusting strike threshold or expiration range.",
      download_filename_prefix = "cash_secured_puts_analysis"
    )

    # Return reactive values
    return(result)
  })
}

## To be copied in the UI
# mod_cash_secured_puts_ui("cash_secured_puts_1")

## To be copied in the server
# mod_cash_secured_puts_server("cash_secured_puts_1")
