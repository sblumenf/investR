#' ETF Cash-Secured Puts Screener Page
#'
#' @description Brochure page for ETF screener cash-secured puts analysis
#'
#' @importFrom brochure page
#' @importFrom shiny fluidPage titlePanel sidebarLayout mainPanel uiOutput renderUI
#' @noRd
page_etf_cash_secured_puts_yfscreen <- function() {
  brochure::page(
    href = "/etf-cash-secured-puts",
    ui = function(request) {
      fluidPage(
        titlePanel("ETF Screener - Cash-Secured Puts"),
        sidebarLayout(
          mod_etf_cash_secured_puts_yfscreen_ui("analysis"),
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
      analysis <- mod_etf_cash_secured_puts_yfscreen_server("analysis")

      # Wire up results table
      mod_cash_secured_puts_results_table_server("results", analysis$results_data)

      # Render status UI
      output$analysis_status <- renderUI({
        analysis$status_ui()
      })
    }
  )
}
