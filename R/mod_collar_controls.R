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

      # Universe selector
      h4("Select Universe"),
      radioButtons(
        ns("universe"),
        NULL,
        choices = c(
          "S&P 500 Stocks" = "stocks",
          "Liquid ETFs" = "etfs"
        ),
        selected = "stocks"
      ),

      hr(),

      # Conditional filters based on universe
      # For S&P 500: dividend filter
      conditionalPanel(
        condition = sprintf("input['%s'] == 'stocks'", ns("universe")),
        h5("Stock Filters"),
        selectInput(
          ns("dividend_filter"),
          "Dividend Status",
          choices = c(
            "All Stocks" = "all",
            "Dividend-Paying Only" = "dividend",
            "Non-Dividend Only" = "zero"
          ),
          selected = "all"
        )
      ),

      # For ETFs: market cap and volume filters
      conditionalPanel(
        condition = sprintf("input['%s'] == 'etfs'", ns("universe")),
        h5("ETF Filters"),
        sliderInput(
          ns("min_market_cap"),
          "Minimum Market Cap",
          min = 100,  # $100M
          max = 10000,  # $10B
          value = 1000,  # $1B default
          step = 100,
          post = "M"
        ),

        sliderInput(
          ns("min_avg_volume"),
          "Minimum Avg Volume",
          min = 100000,  # 100K
          max = 10000000,  # 10M
          value = 1000000,  # 1M default
          step = 100000
        )
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

    # Create analysis function based on selected universe
    analysis_function <- function() {
      if (input$universe == "stocks") {
        # Analyze S&P 500 stocks
        analyze_collar_stocks(
          dividend_filter = input$dividend_filter,
          target_days = input$target_days,
          strike_adjustment_pct = input$strike_adjustment / 100,
          limit = NULL,
          max_workers = input$max_workers
        )
      } else {
        # Analyze ETFs
        analyze_collar_etfs(
          min_market_cap = input$min_market_cap * 1e6,  # Convert millions to actual value
          min_avg_volume = input$min_avg_volume,
          target_days = input$target_days,
          strike_adjustment_pct = input$strike_adjustment / 100,
          limit = NULL,
          max_workers = input$max_workers
        )
      }
    }

    # Get universe label for messages
    universe_label <- reactive({
      if (input$universe == "stocks") {
        switch(input$dividend_filter,
          "dividend" = "dividend-paying S&P 500 stocks",
          "zero" = "non-dividend S&P 500 stocks",
          "S&P 500 stocks"
        )
      } else {
        "liquid ETFs"
      }
    })

    # Use analysis controls helper function (DRY!)
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = analysis_function,
      progress_message = reactive({
        sprintf("Analyzing %s for collar opportunities... This may take several minutes.", universe_label())
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
