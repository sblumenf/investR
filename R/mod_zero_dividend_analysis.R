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

      # Variant selector
      h4("Select Variant"),
      selectInput(
        ns("zero_dividend_variant"),
        NULL,
        choices = c(
          "S&P 500 Zero-Dividend Stocks" = "sp500_zero",
          "Overbought Stocks" = "overbought",
          "Oversold Stocks" = "oversold",
          "Most Shorted Stocks" = "most_shorted",
          "2x Leveraged ETFs" = "leveraged_2x",
          "3x Leveraged ETFs" = "leveraged_3x"
        ),
        selected = "sp500_zero"
      ),

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

    # Create analysis function based on selected variant
    # Month filtering happens client-side after results are returned
    analysis_function <- function() {
      switch(input$zero_dividend_variant,
        "sp500_zero" = analyze_zero_dividend(
          limit = NULL,
          strike_threshold_pct = input$strike_threshold / 100,
          min_days = input$days_range[1],
          max_days = input$days_range[2],
          expiry_month = NULL,  # Always NULL - filter client-side with checkboxes
          max_workers = input$max_workers
        ),
        "overbought" = analyze_zero_dividend_custom_list(
          list_type = "overbought",
          strike_threshold_pct = input$strike_threshold / 100,
          min_days = input$days_range[1],
          max_days = input$days_range[2],
          expiry_month = NULL,
          max_workers = input$max_workers
        ),
        "oversold" = analyze_zero_dividend_custom_list(
          list_type = "oversold",
          strike_threshold_pct = input$strike_threshold / 100,
          min_days = input$days_range[1],
          max_days = input$days_range[2],
          expiry_month = NULL,
          max_workers = input$max_workers
        ),
        "most_shorted" = analyze_zero_dividend_custom_list(
          list_type = "most_shorted",
          strike_threshold_pct = input$strike_threshold / 100,
          min_days = input$days_range[1],
          max_days = input$days_range[2],
          expiry_month = NULL,
          max_workers = input$max_workers
        ),
        "leveraged_2x" = analyze_zero_dividend_custom_list(
          list_type = "leveraged_2x",
          strike_threshold_pct = input$strike_threshold / 100,
          min_days = input$days_range[1],
          max_days = input$days_range[2],
          expiry_month = NULL,
          max_workers = input$max_workers
        ),
        "leveraged_3x" = analyze_zero_dividend_custom_list(
          list_type = "leveraged_3x",
          strike_threshold_pct = input$strike_threshold / 100,
          min_days = input$days_range[1],
          max_days = input$days_range[2],
          expiry_month = NULL,
          max_workers = input$max_workers
        )
      )
    }

    # Get variant label for messages
    variant_label <- reactive({
      switch(input$zero_dividend_variant,
        "sp500_zero" = "S&P 500 zero-dividend stocks",
        "overbought" = "overbought stocks",
        "oversold" = "oversold stocks",
        "most_shorted" = "most shorted stocks",
        "leveraged_2x" = "2x leveraged ETFs",
        "leveraged_3x" = "3x leveraged ETFs"
      )
    })

    # Use analysis controls helper function (DRY!)
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = analysis_function,
      progress_message = reactive({
        sprintf("Analyzing %s... This may take several minutes on first run (building cache).", variant_label())
      }),
      success_message_template = "Analysis complete! Found %d opportunities.",
      no_results_message = "No opportunities found with current parameters. Try adjusting strike threshold or expiration range.",
      download_filename_prefix = "zero_dividend_analysis"
    )

    # Return reactive values
    return(result)
  })
}

## To be copied in the UI
# mod_zero_dividend_analysis_ui("zero_dividend_analysis_1")

## To be copied in the server
# mod_zero_dividend_analysis_server("zero_dividend_analysis_1")
