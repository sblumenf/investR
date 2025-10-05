#' Collar Strategy Page
#'
#' This function creates the collar (synthetic bond) strategy page.
#' Uses modular architecture with separate modules for controls and results display.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_collar <- function() {
  brochure::page(
    href = "/collar",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("Collar Strategy (Synthetic Bonds)"),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Control module
            mod_collar_controls_ui("controls"),

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
      controls <- mod_collar_controls_server("controls")

      # Pass results to results module
      mod_collar_results_server("results", controls$results_data)

      # Display status messages from controls module
      output$controls_status <- renderUI({
        controls$status_ui()
      })
    }
  )
}
