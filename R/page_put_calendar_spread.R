#' Put Calendar Spread Page
#'
#' This function creates the put calendar spread strategy page.
#' Uses modular architecture with separate modules for analysis controls and results display.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_put_calendar_spread <- function() {
  brochure::page(
    href = "/put-calendar-spread",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("Put Calendar Spread - Dividend Aristocrats"),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Analysis controls module
            mod_put_calendar_spread_ui("analysis"),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress
              uiOutput("analysis_status"),

              # Results table module
              mod_put_calendar_spread_results_table_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Call analysis module and get reactive results
      analysis <- mod_put_calendar_spread_server("analysis")

      # Pass results to table module
      mod_put_calendar_spread_results_table_server("results",
                                                   analysis$results_data)

      # Display status messages from analysis module
      output$analysis_status <- renderUI({
        analysis$status_ui()
      })
    }
  )
}
