#' Dividend Aristocrats Page
#'
#' This function creates the dividend aristocrats covered call strategy page.
#' Uses modular architecture with separate modules for analysis controls and results display.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_aristocrats <- function() {
  brochure::page(
    href = "/aristocrats",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("Dividend Aristocrats - Deep ITM Covered Calls"),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Analysis controls module
            mod_aristocrats_analysis_ui("analysis"),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress
              uiOutput("analysis_status"),

              # Results table module
              mod_aristocrats_results_table_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Call analysis module and get reactive results
      analysis <- mod_aristocrats_analysis_server("analysis")

      # Pass results to table module
      mod_aristocrats_results_table_server("results", analysis$results_data)

      # Display status messages from analysis module
      output$analysis_status <- renderUI({
        analysis$status_ui()
      })
    }
  )
}