#' aristocrats_analysis UI Function
#'
#' @description A shiny Module for aristocrats analysis parameters and execution.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_aristocrats_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h3("Strategy Parameters"),

      selectInput(
        ns("aristocrats_variant"),
        NULL,
        choices = c(
          "Dividend Aristocrats" = "aristocrats",
          "Dividend Kings (50+ yrs)" = "kings",
          "Dividend Champions (25+ yrs)" = "champions",
          "Dividend Contenders (10-24 yrs)" = "contenders",
          "Dividend Challengers (5-9 yrs)" = "challengers"
        ),
        selected = "aristocrats"
      ),

      # Quote source toggle
      quote_source_toggle_ui(ns),

      hr(),

      # Strike threshold
      sliderInput(
        ns("strike_threshold"),
        "Strike Threshold (% of current price)",
        min = 50,
        max = 100,
        value = 80,
        step = 5,
        post = "%"
      ),

      # Target days to expiry
      sliderInput(
        ns("target_days"),
        "Target Days to Expiry",
        min = 0,
        max = 1000,
        value = 1000,
        step = 1
      ),

      # Parallel workers
      sliderInput(
        ns("max_workers"),
        "Parallel Workers",
        min = 1,
        max = 20,
        value = 4,
        step = 1
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

#' aristocrats_analysis Server Functions
#'
#' @description Server logic for aristocrats analysis module.
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
mod_aristocrats_analysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Setup quote source toggle
    quote_source_toggle_server(input, session, "Dividend Aristocrats")

    # Reactive: Analysis parameters
    analysis_params <- reactive({
      list(
        strike_threshold_pct = input$strike_threshold / 100,
        target_days = if (input$target_days == 1000) NULL else input$target_days,
        max_workers = input$max_workers
      )
    })

    variant_label <- reactive({
      switch(input$aristocrats_variant,
        "aristocrats" = "Dividend Aristocrats",
        "kings"       = "Dividend Kings (50+ yrs)",
        "champions"   = "Dividend Champions (25+ yrs)",
        "contenders"  = "Dividend Contenders (10-24 yrs)",
        "challengers" = "Dividend Challengers (5-9 yrs)"
      )
    })

    # Create analysis function that captures current parameters
    analysis_function <- function() {
      params <- analysis_params()
      tickers <- if (input$aristocrats_variant == "aristocrats") {
        NULL
      } else if (input$aristocrats_variant == "kings") {
        get_drip_tickers("King")
      } else if (input$aristocrats_variant == "champions") {
        get_drip_tickers("Champion")
      } else if (input$aristocrats_variant == "contenders") {
        get_drip_tickers("Contender")
      } else if (input$aristocrats_variant == "challengers") {
        get_drip_tickers("Challenger")
      }
      analyze_aristocrats(
        limit = NULL,
        strike_threshold_pct = params$strike_threshold_pct,
        target_days = params$target_days,
        max_workers = params$max_workers,
        tickers = tickers,
        strategy_name = variant_label()
      )
    }

    # Use analysis controls helper function
    setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = analysis_function,
      progress_message = reactive({
        sprintf("Analyzing %s... This may take several minutes.", variant_label())
      }),
      success_message_template = "Analysis complete! Found %d opportunities.",
      no_results_message = "No opportunities found with current parameters.",
      download_filename_prefix = "aristocrats_analysis"
    )
  })
}

## To be copied in the UI
# mod_aristocrats_analysis_ui("aristocrats_analysis_1")

## To be copied in the server
# mod_aristocrats_analysis_server("aristocrats_analysis_1")
