#' Cash-Secured Puts Page
#'
#' This function creates the cash-secured puts strategy page.
#' Uses modular architecture with separate modules for analysis controls and results display.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_cash_secured_puts <- function() {
  brochure::page(
    href = "/cash-secured-puts",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("Cash-Secured Puts - Dividend Aristocrats"),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Analysis controls module
            mod_cash_secured_puts_ui("analysis"),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress
              uiOutput("analysis_status"),

              # Results table module
              mod_cash_secured_puts_results_table_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Call analysis module and get reactive results
      analysis <- mod_cash_secured_puts_server("analysis")

      # Pass results to table module
      mod_cash_secured_puts_results_table_server("results",
                                                analysis$results_data)

      # Display status messages from analysis module
      output$analysis_status <- renderUI({
        analysis$status_ui()
      })
    }
  )
}
