#' etf_covered_calls_analysis UI Function
#'
#' @description A shiny Module for ETF screener covered calls analysis parameters and execution.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_etf_covered_calls_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h3("Strategy Parameters"),

      # Quote source toggle
      quote_source_toggle_ui(ns),

      hr(),

      h4("ETF Screening Criteria"),

      # Dividend filter toggle
      selectInput(
        ns("dividend_filter"),
        "Dividend Filter",
        choices = c(
          "All ETFs (any dividend status)" = "all",
          "Dividend-paying ETFs only" = "dividend_paying",
          "Zero-dividend ETFs only" = "zero_dividend"
        ),
        selected = "all"
      ),
      helpText("Filter by dividend payment status"),

      # Dividend yield range slider (0-10%, default 2-6%)
      # Conditionally shown based on dividend_filter selection
      conditionalPanel(
        condition = "input.dividend_filter == 'dividend_paying'",
        ns = ns,
        sliderInput(
          ns("dividend_yield_range"),
          "Dividend Yield Range (%)",
          min = 0,
          max = 10,
          value = c(2, 6),
          step = 0.5,
          post = "%"
        ),
        helpText("Filter ETFs by dividend yield range")
      ),

      # ETF size (AUM) minimum selector
      selectInput(
        ns("min_market_cap"),
        "Minimum ETF Size",
        choices = c(
          "No minimum (include all ETFs)" = "0",
          "$50 Million" = "50000000",
          "$100 Million" = "100000000",
          "$250 Million" = "250000000",
          "$500 Million" = "500000000",
          "$1 Billion" = "1000000000",
          "$2 Billion" = "2000000000",
          "$5 Billion" = "5000000000",
          "$10 Billion" = "10000000000",
          "$25 Billion" = "25000000000",
          "$50 Billion" = "50000000000",
          "$100 Billion" = "100000000000"
        ),
        selected = "0"
      ),
      helpText("Filter out smaller ETFs. Smaller ETFs may have less liquid options."),

      # Top N ETFs by volume
      sliderInput(
        ns("top_n_etfs"),
        "Maximum Number of ETFs to Analyze",
        min = 10,
        max = 200,
        value = 50,
        step = 10
      ),
      helpText("Select top N most traded ETFs (sorted by average volume). Use lower values for faster analysis."),

      hr(),

      h4("Covered Call Parameters"),

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
        value = c(45, 120),
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
        value = 4,
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

#' etf_covered_calls_analysis Server Functions
#'
#' @description Server logic for ETF screener covered calls analysis module.
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
mod_etf_covered_calls_analysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Setup quote source toggle
    quote_source_toggle_server(input, session, "ETF Screener Covered Calls Analysis")

    # Create analysis function based on UI parameters
    analysis_function <- function() {
      # Handle dividend filter logic
      dividend_yield_min <- NULL
      dividend_yield_max <- NULL

      if (input$dividend_filter == "dividend_paying") {
        # Use the slider values if dividend-paying is selected
        dividend_yield_min <- input$dividend_yield_range[1]
        dividend_yield_max <- input$dividend_yield_range[2]
      } else if (input$dividend_filter == "zero_dividend") {
        # For zero-dividend, set both to 0
        dividend_yield_min <- 0
        dividend_yield_max <- 0
      }
      # else "all" - leave both NULL to skip dividend filter

      analyze_etf_covered_calls_yfscreen(
        dividend_yield_min = dividend_yield_min,
        dividend_yield_max = dividend_yield_max,
        dividend_filter = input$dividend_filter,
        market_cap_min = as.numeric(input$min_market_cap),
        market_cap_max = NULL,  # No maximum filter - include all large ETFs
        top_n = input$top_n_etfs,
        strike_threshold_pct = input$strike_threshold / 100,
        min_days = input$days_range[1],
        max_days = input$days_range[2],
        expiry_month = NULL,  # Always NULL - filter client-side if needed
        max_workers = input$max_workers
      )
    }

    # Use analysis controls helper function (DRY!)
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = analysis_function,
      progress_message = reactive({
        sprintf("Screening ETFs and analyzing covered calls... This may take several minutes.")
      }),
      success_message_template = "Analysis complete! Found %d opportunities.",
      no_results_message = "No opportunities found with current parameters. Try adjusting screening criteria or covered call parameters.",
      download_filename_prefix = "etf_screener_covered_calls"
    )

    # Return reactive values
    return(result)
  })
}

## To be copied in the UI
# mod_etf_covered_calls_analysis_ui("etf_covered_calls_analysis_1")

## To be copied in the server
# mod_etf_covered_calls_analysis_server("etf_covered_calls_analysis_1")
