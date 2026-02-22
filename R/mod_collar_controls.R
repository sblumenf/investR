#' collar_controls UI Function
#'
#' @description A shiny Module for collar strategy parameters and execution.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_collar_controls_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h3("Collar Strategy"),
      helpText("Synthetic bond: Long stock + Short ATM call + Long ATM put"),

      # Quote source toggle
      quote_source_toggle_ui(ns),

      hr(),

      # Collar variant selector
      h4("Select Collar Variant"),
      selectInput(
        ns("collar_variant"),
        NULL,
        choices = c(
          "S&P 500 Dividend Payers" = "sp500_dividend",
          "S&P 500 Non-Dividend Payers" = "sp500_zero",
          "High Liquidity ETFs" = "liquid_etfs",
          "Overbought Stocks" = "overbought",
          "Oversold Stocks" = "oversold",
          "Most Shorted Stocks" = "most_shorted",
          "2x Leveraged ETFs" = "leveraged_2x",
          "3x Leveraged ETFs" = "leveraged_3x"
        ),
        selected = "sp500_dividend"
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
        value = 10,  # Default to 10 for production
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

#' collar_controls Server Functions
#'
#' @description Server logic for collar controls module.
#'   Returns a reactive containing analysis results data.
#'
#' @param id Module ID
#'
#' @return A list with reactive values: results_data, status_ui
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive
mod_collar_controls_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Setup quote source toggle
    quote_source_toggle_server(input, session, "Collar Strategy")

    # Create analysis function based on selected variant
    analysis_function <- function() {
      switch(input$collar_variant,
        "sp500_dividend" = analyze_collar_stocks(
          dividend_filter = "dividend",
          target_days = input$target_days,
          strike_adjustment_pct = input$strike_adjustment / 100,
          limit = NULL,
          max_workers = input$max_workers
        ),
        "sp500_zero" = analyze_collar_stocks(
          dividend_filter = "zero",
          target_days = input$target_days,
          strike_adjustment_pct = input$strike_adjustment / 100,
          limit = NULL,
          max_workers = input$max_workers
        ),
        "liquid_etfs" = analyze_collar_etfs(
          min_market_cap = COLLAR_CONFIG$min_market_cap,
          min_avg_volume = COLLAR_CONFIG$min_avg_volume,
          target_days = input$target_days,
          strike_adjustment_pct = input$strike_adjustment / 100,
          limit = NULL,
          max_workers = input$max_workers
        ),
        "overbought" = analyze_collar_custom_list(
          list_type = "overbought",
          target_days = input$target_days,
          strike_adjustment_pct = input$strike_adjustment / 100,
          max_workers = input$max_workers
        ),
        "oversold" = analyze_collar_custom_list(
          list_type = "oversold",
          target_days = input$target_days,
          strike_adjustment_pct = input$strike_adjustment / 100,
          max_workers = input$max_workers
        ),
        "most_shorted" = analyze_collar_custom_list(
          list_type = "most_shorted",
          target_days = input$target_days,
          strike_adjustment_pct = input$strike_adjustment / 100,
          max_workers = input$max_workers
        ),
        "leveraged_2x" = analyze_collar_custom_list(
          list_type = "leveraged_2x",
          target_days = input$target_days,
          strike_adjustment_pct = input$strike_adjustment / 100,
          max_workers = input$max_workers
        ),
        "leveraged_3x" = analyze_collar_custom_list(
          list_type = "leveraged_3x",
          target_days = input$target_days,
          strike_adjustment_pct = input$strike_adjustment / 100,
          max_workers = input$max_workers
        )
      )
    }

    # Get variant label for messages
    variant_label <- reactive({
      switch(input$collar_variant,
        "sp500_dividend" = "S&P 500 dividend-paying stocks",
        "sp500_zero" = "S&P 500 non-dividend stocks",
        "liquid_etfs" = "high liquidity ETFs",
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
        sprintf("Analyzing %s for collar opportunities... This may take several minutes.", variant_label())
      }),
      success_message_template = "Analysis complete! Found %d collar opportunities (net credit > 0).",
      no_results_message = "No collar opportunities found. All positions had net debit (call bid < put ask).",
      download_filename_prefix = "collar_analysis"
    )

    # Return reactive values
    return(result)
  })
}

## To be copied in the UI
# mod_collar_controls_ui("collar_controls_1")

## To be copied in the server
# mod_collar_controls_server("collar_controls_1")
