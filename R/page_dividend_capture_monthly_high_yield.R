#' Monthly High-Yield Dividend Capture Page
#'
#' @description Brochure page for monthly high-yield dividend capture strategy analysis.
#'   Analyzes 100+ monthly dividend ETFs with yields >8% for dividend capture opportunities.
#'
#' @return Brochure page object
#' @noRd
#'
#' @importFrom shiny tagList fluidPage titlePanel sidebarLayout mainPanel uiOutput renderUI
#' @importFrom brochure page
page_dividend_capture_monthly_high_yield <- function() {
  brochure::page(
    href = "/dividend-capture-monthly-high-yield",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("Monthly High-Yield Dividend Capture Strategy"),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Controls module
            mod_dividend_capture_monthly_high_yield_controls_ui("controls"),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress
              uiOutput("analysis_status"),

              # Results display module
              mod_dividend_capture_monthly_high_yield_results_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Call controls module and get reactive results
      controls <- mod_dividend_capture_monthly_high_yield_controls_server("controls")

      # Pass results to display module
      mod_dividend_capture_monthly_high_yield_results_server(
        "results",
        controls$results_data
      )

      # Display status messages from controls module
      output$analysis_status <- renderUI({
        controls$status_ui()
      })
    }
  )
}
