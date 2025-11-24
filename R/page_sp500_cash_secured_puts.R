#' S&P 500 Cash-Secured Puts Page
#'
#' @description Brochure page for S&P 500 cash-secured puts analysis
#'
#' @importFrom brochure page
#' @importFrom shiny fluidPage titlePanel sidebarLayout mainPanel uiOutput renderUI
#' @noRd
page_sp500_cash_secured_puts <- function() {
  brochure::page(
    href = "/sp500-cash-secured-puts",
    ui = function(request) {
      fluidPage(
        titlePanel("S&P 500 - Cash-Secured Puts"),
        sidebarLayout(
          mod_sp500_cash_secured_puts_ui("analysis"),
          mainPanel(
            width = 9,
            uiOutput("analysis_status"),
            mod_cash_secured_puts_results_table_ui("results")
          )
        )
      )
    },
    server = function(input, output, session) {
      # Run analysis and get reactive results
      analysis <- mod_sp500_cash_secured_puts_server("analysis")

      # Wire up results table
      mod_cash_secured_puts_results_table_server("results", analysis$results_data)

      # Render status UI
      output$analysis_status <- renderUI({
        analysis$status_ui()
      })
    }
  )
}
