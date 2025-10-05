#' Weekly Dividend Capture Page
#'
#' This function creates the weekly dividend capture strategy page.
#' Uses modular architecture with separate modules for controls and results display.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_dividend_capture_weekly <- function() {
  brochure::page(
    href = "/dividend-capture-weekly",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("Weekly Dividend Capture Strategy"),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Controls module
            mod_dividend_capture_weekly_controls_ui("controls"),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress
              uiOutput("analysis_status"),

              # Results display module
              mod_dividend_capture_weekly_results_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Call controls module and get reactive results
      controls <- mod_dividend_capture_weekly_controls_server("controls")

      # Pass results and day filter to display module
      mod_dividend_capture_weekly_results_server("results",
                                                 controls$results_data,
                                                 controls$day_filter)

      # Display status messages from controls module
      output$analysis_status <- renderUI({
        controls$status_ui()
      })
    }
  )
}
