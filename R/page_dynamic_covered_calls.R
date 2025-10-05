#' Dynamic Covered Calls Page
#'
#' This function creates the dynamic covered calls strategy page.
#' Uses modular architecture with separate modules for analysis controls and results display.
#' Strategy calculates unique strike and expiration parameters for each stock based on
#' historical maximum drawdown.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_dynamic_covered_calls <- function() {
  brochure::page(
    href = "/dynamic-covered-calls",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("Dynamic Covered Calls - Drawdown-Based Strikes"),

          # Strategy description
          fluidRow(
            column(
              width = 12,
              wellPanel(
                style = "background-color: #f8f9fa; border-left: 4px solid #007bff;",
                h4(icon("chart-line"), " Strategy Overview"),
                p(
                  "This strategy dynamically calculates strike prices and expiration targets for each stock ",
                  "based on its historical maximum drawdown. Stocks with higher volatility get deeper ITM strikes ",
                  "and longer expirations for better downside protection."
                ),
                tags$ul(
                  tags$li(
                    strong("Strike Calculation:"), " Strike = 100% + Max Drawdown (e.g., -30% drawdown → 70% strike)"
                  ),
                  tags$li(
                    strong("Expiration Target:"), " Based on time between peak and trough during max drawdown"
                  ),
                  tags$li(
                    strong("Stock Universe:"), " S&P 500 stocks under user-defined price threshold"
                  )
                )
              )
            )
          ),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Analysis controls module
            mod_dynamic_covered_calls_analysis_ui("analysis"),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress
              uiOutput("analysis_status"),

              # Results table module (reuse from aristocrats)
              mod_aristocrats_results_table_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Call analysis module and get reactive results
      analysis <- mod_dynamic_covered_calls_analysis_server("analysis")

      # Pass results to table module
      mod_aristocrats_results_table_server("results", analysis$results_data)

      # Display status messages from analysis module
      output$analysis_status <- renderUI({
        analysis$status_ui()
      })
    }
  )
}
