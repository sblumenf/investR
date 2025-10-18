#' Zero-Dividend Stocks Page
#'
#' This function creates the zero-dividend stocks covered call strategy page.
#' Uses modular architecture with separate modules for analysis controls and results display.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_zero_dividend <- function() {
  brochure::page(
    href = "/zero-dividend",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("Zero-Dividend Stocks - Deep ITM Covered Calls"),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Analysis controls module
            mod_zero_dividend_analysis_ui("analysis"),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress
              uiOutput("analysis_status"),

              # Results table module
              mod_zero_dividend_results_table_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Call analysis module and get reactive results + month filter
      analysis <- mod_zero_dividend_analysis_server("analysis")

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
