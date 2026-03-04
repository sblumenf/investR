#' etf_collar_controls UI Function
#'
#' @description A shiny Module for ETF collar strategy parameters and execution.
#'   Uses yfscreen-sourced ETFs (dividend-paying or zero-dividend).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_etf_collar_controls_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h3("ETF Collar Strategy"),
      helpText("Synthetic bond: Long ETF + Short ATM call + Long ATM put"),

      # Quote source toggle
      quote_source_toggle_ui(ns),

      hr(),

      # ETF variant selector
      h4("Select ETF Variant"),
      selectInput(
        ns("collar_variant"),
        NULL,
        choices = c(
          "Dividend-paying ETFs" = "dividend_paying",
          "Zero-dividend ETFs" = "zero_dividend"
        ),
        selected = "dividend_paying"
      ),

      hr(),

      # Strategy parameters
      h5("Strategy Parameters"),
      sliderInput(
        ns("target_days"),
        "Target Days to Expiry",
        min = 45,
        max = 850,
        value = 300,
        step = 5
      ),

      sliderInput(
        ns("strike_adjustment"),
        "Strike Adjustment",
        min = -20,
        max = 20,
        value = 0,
        step = 5,
        post = "%"
      ),

      hr(),

      # Parallel workers slider
      h5("Performance"),
      sliderInput(
        ns("max_workers"),
        "Parallel Workers",
        min = 1,
        max = 20,
        value = ETF_COLLAR_CONFIG$max_workers,
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

#' etf_collar_controls Server Functions
#'
#' @description Server logic for ETF collar controls module.
#'   Returns a reactive containing analysis results data.
#'
#' @param id Module ID
#'
#' @return A list with reactive values: results_data, status_ui
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive
mod_etf_collar_controls_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Setup quote source toggle
    quote_source_toggle_server(input, session, "ETF Collar Strategy")

    # Create analysis function
    analysis_function <- function() {
      analyze_etf_collar_yfscreen(
        dividend_filter = input$collar_variant,
        target_days = input$target_days,
        strike_adjustment_pct = input$strike_adjustment / 100,
        max_workers = input$max_workers
      )
    }

    # Get variant label for messages
    variant_label <- reactive({
      switch(input$collar_variant,
        "dividend_paying" = "dividend-paying ETFs",
        "zero_dividend" = "zero-dividend ETFs"
      )
    })

    # Use analysis controls helper function
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = analysis_function,
      progress_message = reactive({
        sprintf("Fetching and analyzing %s from Yahoo Finance screener... This may take several minutes.", variant_label())
      }),
      success_message_template = "Analysis complete! Found %d collar opportunities (net credit > 0).",
      no_results_message = "No collar opportunities found. All positions had net debit (call bid < put ask).",
      download_filename_prefix = "etf_collar_analysis"
    )

    return(result)
  })
}

## To be copied in the UI
# mod_etf_collar_controls_ui("etf_collar_controls_1")

## To be copied in the server
# mod_etf_collar_controls_server("etf_collar_controls_1")
