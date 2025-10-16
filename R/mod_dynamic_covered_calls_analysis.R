#' dynamic_covered_calls_analysis UI Function
#'
#' @description A shiny Module for dynamic covered calls analysis parameters and execution.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dynamic_covered_calls_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h3("Strategy Parameters"),

      # Quote source toggle
      quote_source_toggle_ui(ns),

      hr(),

      # Price threshold slider
      sliderInput(
        ns("max_price"),
        "Max Stock Price ($)",
        min = 50,
        max = 500,
        value = get_dynamic_config("default_max_price"),
        step = 10
      ),
      helpText("Filter S&P 500 stocks by maximum price"),

      hr(),

      # Lookback period slider
      sliderInput(
        ns("lookback_years"),
        "Historical Lookback (Years)",
        min = 1,
        max = 10,
        value = get_dynamic_config("default_lookback_years"),
        step = 1
      ),
      helpText("Years of price history for drawdown calculation"),

      hr(),

      h4("Strike Bounds"),
      helpText("Bounds for dynamically calculated strike thresholds"),

      # Min strike percentage
      numericInput(
        ns("min_strike_pct"),
        "Min Strike (%)",
        value = get_dynamic_config("default_min_strike_pct") * 100,
        min = 30,
        max = 95,
        step = 5
      ),

      # Max strike percentage
      numericInput(
        ns("max_strike_pct"),
        "Max Strike (%)",
        value = get_dynamic_config("default_max_strike_pct") * 100,
        min = 50,
        max = 100,
        step = 5
      ),

      hr(),

      # Advanced options (collapsible)
      tags$details(
        tags$summary(
          style = "cursor: pointer; font-weight: bold;",
          "Advanced Options"
        ),

        br(),

        # Parallel workers
        sliderInput(
          ns("max_workers"),
          "Parallel Workers",
          min = 1,
          max = 20,
          value = get_dynamic_config("max_workers"),
          step = 1
        ),

        # Limit for testing
        numericInput(
          ns("limit"),
          "Limit Stocks (Testing)",
          value = NA,
          min = 1,
          max = 500,
          step = 1
        ),
        helpText("Leave empty to analyze all stocks")
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

#' dynamic_covered_calls_analysis Server Functions
#'
#' @description Server logic for dynamic covered calls analysis module.
#'   Returns a reactive containing analysis results data.
#'   Uses async execution with promises for non-blocking UI.
#'
#' @param id Module ID
#'
#' @return A list with reactive values: results_data, status_ui
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive showNotification reactiveVal observeEvent downloadHandler req withProgress incProgress
#' @importFrom promises future_promise %...>% %...!%
#' @importFrom future plan multisession
#' @importFrom readr write_csv
mod_dynamic_covered_calls_analysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Setup quote source toggle
    quote_source_toggle_server(input, session, "Dynamic Covered Calls")

    # Reactive values
    results_data <- reactiveVal(NULL)
    status_message <- reactiveVal(NULL)

    # Reactive: Analysis parameters
    analysis_params <- reactive({
      # Validate strike bounds
      min_strike <- input$min_strike_pct / 100
      max_strike <- input$max_strike_pct / 100

      # Ensure min < max
      if (min_strike >= max_strike) {
        showNotification(
          "Min strike must be less than max strike",
          type = "error",
          duration = 5
        )
        return(NULL)
      }

      list(
        max_price = input$max_price,
        lookback_years = input$lookback_years,
        min_strike_pct = min_strike,
        max_strike_pct = max_strike,
        max_workers = input$max_workers,
        limit = if (is.na(input$limit)) NULL else input$limit
      )
    })

    # Run analysis when button clicked (ASYNC VERSION)
    observeEvent(input$run_analysis, {
      params <- analysis_params()

      # If validation failed, stop
      if (is.null(params)) {
        return()
      }

      # Reset fallback tracker before starting analysis
      reset_fallback_tracker()

      # Show progress message
      status_message(create_progress_alert(
        "Analyzing stocks with dynamic parameters... This will take several minutes. The UI remains responsive."
      ))

      # Check if async is enabled
      enable_async <- get_dynamic_config("enable_async")

      if (enable_async) {
        # ASYNC EXECUTION with promises
        future_promise({
          # This runs in a background R process
          analyze_dynamic_covered_calls(
            limit = params$limit,
            max_price = params$max_price,
            lookback_years = params$lookback_years,
            min_strike_pct = params$min_strike_pct,
            max_strike_pct = params$max_strike_pct,
            max_workers = params$max_workers
          )
        }) %...>% {
          # On success - this runs in main thread when background process completes
          results <- .

          results_data(results)

          if (nrow(results) > 0) {
            status_message(create_status_alert(
              type = "success",
              message = sprintf("Analysis complete! Found %d opportunities.", nrow(results))
            ))

            showNotification(
              sprintf("Found %d opportunities!", nrow(results)),
              type = "message",
              duration = 5
            )
          } else {
            status_message(create_status_alert(
              type = "warning",
              message = "No opportunities found with current parameters."
            ))
          }

          # Check for Questrade fallbacks and notify user
          check_and_notify_fallbacks()
        } %...!% {
          # On error
          error_msg <- .

          status_message(create_status_alert(
            type = "danger",
            message = paste("Error:", error_msg$message)
          ))

          showNotification(
            paste("Analysis failed:", error_msg$message),
            type = "error",
            duration = 10
          )
        }

      } else {
        # SYNCHRONOUS EXECUTION (fallback)
        results <- tryCatch({
          withProgress(message = "Analyzing stocks...", value = 0, {
            analyze_dynamic_covered_calls(
              limit = params$limit,
              max_price = params$max_price,
              lookback_years = params$lookback_years,
              min_strike_pct = params$min_strike_pct,
              max_strike_pct = params$max_strike_pct,
              max_workers = params$max_workers
            )
          })
        }, error = function(e) {
          status_message(create_status_alert(
            type = "danger",
            message = paste("Error:", e$message)
          ))
          return(NULL)
        })

        results_data(results)

        if (!is.null(results)) {
          if (nrow(results) > 0) {
            status_message(create_status_alert(
              type = "success",
              message = sprintf("Analysis complete! Found %d opportunities.", nrow(results))
            ))
          } else {
            status_message(create_status_alert(
              type = "warning",
              message = "No opportunities found with current parameters."
            ))
          }
        }

        # Check for Questrade fallbacks and notify user
        check_and_notify_fallbacks()
      }
    })

    # Download handler
    output$download_results <- downloadHandler(
      filename = function() {
        paste0("dynamic_covered_calls_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(results_data())
        write_csv(results_data(), file)
      }
    )

    # Return reactive values
    list(
      results_data = results_data,
      status_ui = reactive({ status_message() })
    )
  })
}

## To be copied in the UI
# mod_dynamic_covered_calls_analysis_ui("dynamic_covered_calls_analysis_1")

## To be copied in the server
# mod_dynamic_covered_calls_analysis_server("dynamic_covered_calls_analysis_1")
