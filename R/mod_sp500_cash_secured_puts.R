#' sp500_cash_secured_puts UI Function
#'
#' @description A shiny Module for S&P 500 cash-secured puts analysis parameters and execution.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sp500_cash_secured_puts_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h3("Strategy Parameters"),

      # Quote source toggle
      quote_source_toggle_ui(ns),

      hr(),

      h4("Stock Selection Criteria"),

      # Dividend filter dropdown
      selectInput(
        ns("dividend_filter"),
        "Dividend Filter",
        choices = c(
          "All S&P 500 stocks" = "all",
          "Dividend-paying stocks only" = "dividend_paying",
          "Zero-dividend stocks only" = "zero_dividend"
        ),
        selected = "all"
      ),
      helpText("Filter stocks by dividend payment status"),

      # Stock limit input (conditionally shown for filtered lists)
      conditionalPanel(
        condition = "input.dividend_filter != 'all'",
        ns = ns,
        numericInput(
          ns("stock_limit"),
          "Maximum Stocks to Analyze",
          value = 50,
          min = 10,
          max = 500,
          step = 10
        ),
        helpText("Limit analysis to top N stocks by market cap for faster results")
      ),

      hr(),

      h4("Cash-Secured Put Parameters"),

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
        value = c(30, 45),
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

#' sp500_cash_secured_puts Server Functions
#'
#' @description Server logic for S&P 500 cash-secured puts module.
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
mod_sp500_cash_secured_puts_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Setup quote source toggle
    quote_source_toggle_server(input, session, "S&P 500 Cash-Secured Puts")

    # Create analysis function
    analysis_function <- function() {
      # Determine stock limit based on dividend filter
      limit_value <- if (input$dividend_filter == "all") NULL else input$stock_limit

      # Call S&P 500 cash-secured puts analysis
      analyze_sp500_cash_secured_puts(
        dividend_filter = input$dividend_filter,
        limit = limit_value,
        strike_threshold_pct = input$strike_threshold / 100,
        min_days = input$days_range[1],
        max_days = input$days_range[2],
        expiry_month = NULL,
        max_workers = input$max_workers
      )
    }

    # Get dividend filter label for messages
    filter_label <- reactive({
      switch(input$dividend_filter,
        "all" = "all S&P 500 stocks",
        "dividend_paying" = "dividend-paying S&P 500 stocks",
        "zero_dividend" = "zero-dividend S&P 500 stocks"
      )
    })

    # Use analysis controls helper function (DRY!)
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = analysis_function,
      progress_message = reactive({
        sprintf("Analyzing cash-secured puts for %s... This may take several minutes.", filter_label())
      }),
      success_message_template = "Analysis complete! Found %d cash-secured put opportunities.",
      no_results_message = "No opportunities found. Try adjusting dividend filter or put parameters.",
      download_filename_prefix = "sp500_cash_secured_puts"
    )

    # Return reactive values
    return(result)
  })
}

## To be copied in the UI
# mod_sp500_cash_secured_puts_ui("sp500_cash_secured_puts_1")

## To be copied in the server
# mod_sp500_cash_secured_puts_server("sp500_cash_secured_puts_1")
