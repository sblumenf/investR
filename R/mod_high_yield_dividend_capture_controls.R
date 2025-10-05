#' high_yield_dividend_capture_controls UI Function
#'
#' @description Shiny Module for high-yield dividend capture analysis controls.
#'   Universe-agnostic module that accepts configuration parameter.
#'
#' @param id Module identifier
#' @param universe_config Universe configuration object
#'
#' @noRd
#'
#' @importFrom shiny NS tagList sidebarPanel sliderInput actionButton downloadButton hr tags icon br p strong h3 h5
mod_high_yield_dividend_capture_controls_ui <- function(id, universe_config) {
  ns <- NS(id)

  sidebarPanel(
    width = 3,
    h3(paste(universe_config$universe_name, "High-Yield")),

    # Info text
    p(paste0("Screens ", universe_config$universe_name, " stocks for high-yield dividend capture opportunities with announced ex-dividend dates in the next 1-2 business days.")),
    p(
      strong("Strategy:"),
      "Buy at close before ex-dividend, sell at open on ex-dividend day."
    ),
    p(
      strong("Efficient two-phase filtering:"),
      "Phase 1 screens all stocks for yield and upcoming ex-dates. Phase 2 backtests only qualifying candidates."
    ),

    hr(),

    # Filter controls
    h5("Filter Options"),

    # Yield threshold slider
    sliderInput(
      ns("yield_threshold"),
      "Minimum Yield (%)",
      min = universe_config$min_yield_threshold,
      max = universe_config$max_yield_threshold,
      value = universe_config$default_yield_threshold,
      step = 0.5
    ),

    # Max workers slider
    sliderInput(
      ns("max_workers"),
      "Parallel Workers",
      min = universe_config$min_workers,
      max = universe_config$max_workers_limit,
      value = universe_config$max_workers,
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

    # Download CSV button
    downloadButton(
      ns("download_csv"),
      "Download CSV",
      class = "btn-success btn-block"
    ),

    hr(),

    # Home navigation
    tags$a(
      href = "/",
      class = "btn btn-default btn-block",
      icon("home"),
      " Home"
    )
  )
}

#' high_yield_dividend_capture_controls Server Functions
#'
#' @description Server logic for high-yield dividend capture controls module.
#'   Returns reactive values for results data and status UI.
#'   Uses the analysis controls helper function to eliminate code duplication.
#'
#' @param id Module ID
#' @param universe_config Universe configuration object
#'
#' @return A list with reactive values: results_data, yield_filter, max_workers, status_ui
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive
mod_high_yield_dividend_capture_controls_server <- function(id, universe_config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create wrapper function that passes parameters to batch analysis
    analysis_wrapper <- function() {
      batch_analyze_high_yield_stocks(
        universe_config = universe_config,
        min_yield = input$yield_threshold,
        max_workers = input$max_workers,
        stock_limit = NULL
      )
    }

    # Simple progress message - efficient two-phase approach is fast
    progress_msg <- reactive({
      "Screening and analyzing stocks..."
    })

    # Use analysis controls helper with universe-specific configuration
    result <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = analysis_wrapper,
      progress_message = paste0("Analyzing ", universe_config$universe_name, " stocks for high-yield opportunities..."),
      success_message_template = "Analysis complete! Found %d high-yield opportunities.",
      no_results_message = "No stocks meet the criteria. Try lowering the yield threshold.",
      download_filename_prefix = paste0("high_yield_", universe_config$universe_key, "_dividend_capture"),
      additional_return_values = list(
        yield_filter = reactive({ input$yield_threshold }),
        max_workers = reactive({ input$max_workers })
      )
    )

    # Return all values from helper plus yield_filter and max_workers
    return(result)
  })
}
