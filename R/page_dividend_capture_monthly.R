#' Monthly Dividend Capture Page
#'
#' This function creates the monthly dividend capture strategy page.
#' Uses modular architecture with separate modules for controls and results display.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_dividend_capture_monthly <- function() {
  brochure::page(
    href = "/dividend-capture-monthly",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("Monthly Dividend Capture Strategy"),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Controls module
            mod_dividend_capture_monthly_controls_ui("controls"),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress
              uiOutput("analysis_status"),

              # Results display module
              mod_dividend_capture_monthly_results_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Call controls module and get reactive results
      controls <- mod_dividend_capture_monthly_controls_server("controls")

      # Pass results and days since filter to display module
      mod_dividend_capture_monthly_results_server("results",
                                                   controls$results_data,
                                                   controls$days_since_filter)

      # Display status messages from controls module
      output$analysis_status <- renderUI({
        controls$status_ui()
      })
    }
  )
}
