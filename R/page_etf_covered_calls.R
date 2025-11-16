#' ETF Screener Covered Calls Page
#'
#' This function creates the ETF screener covered call strategy page.
#' Uses modular architecture with separate modules for analysis controls and results display.
#' Leverages yfscreen package to dynamically filter ETFs by expense ratio, dividend yield,
#' and net assets before analyzing for covered call opportunities.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_etf_covered_calls <- function() {
  brochure::page(
    href = "/etf-covered-calls",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("ETF Screener - Deep ITM Covered Calls"),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Analysis controls module
            mod_etf_covered_calls_analysis_ui("analysis"),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress
              uiOutput("analysis_status"),

              # Results table module (reuse from zero-dividend)
              mod_zero_dividend_results_table_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Call analysis module and get reactive results
      analysis <- mod_etf_covered_calls_analysis_server("analysis")

      # Pass results to table module
      mod_zero_dividend_results_table_server("results",
                                            analysis$results_data)

      # Display status messages from analysis module
      output$analysis_status <- renderUI({
        analysis$status_ui()
      })
    }
  )
}
