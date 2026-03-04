#' ETF Collar Strategy Page
#'
#' This function creates the ETF collar (synthetic bond) strategy page.
#' Uses yfscreen to dynamically source ETFs, then runs collar analysis.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_etf_collar <- function() {
  brochure::page(
    href = "/etf-collar",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("ETF Collar Strategy (Synthetic Bonds)"),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Control module
            mod_etf_collar_controls_ui("controls"),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress
              uiOutput("controls_status"),

              # Results module
              mod_collar_results_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Call controls module and get reactive results
      controls <- mod_etf_collar_controls_server("controls")

      # Pass results to results module
      mod_collar_results_server("results", controls$results_data)

      # Display status messages from controls module
      output$controls_status <- renderUI({
        controls$status_ui()
      })
    }
  )
}
