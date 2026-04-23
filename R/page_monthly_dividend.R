#' Monthly Dividend Stocks Page
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @export
page_monthly_dividend <- function() {
  brochure::page(
    href = "/monthly-dividend",
    ui = function(request) {
      tagList(
        fluidPage(
          titlePanel("Monthly Dividend Stocks"),

          sidebarLayout(
            mod_monthly_dividend_analysis_ui("analysis"),

            mainPanel(
              width = 9,

              uiOutput("analysis_status"),

              mod_monthly_dividend_results_table_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      analysis <- mod_monthly_dividend_analysis_server("analysis")

      mod_monthly_dividend_results_table_server("results", analysis$results_data)

      output$analysis_status <- renderUI({
        analysis$status_ui()
      })
    }
  )
}
