#' Portfolio Risk Page
#'
#' Brochure page for portfolio-level risk analysis.
#' Each Brochure page has its own independent Shiny session.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_portfolio_risk <- function() {
  brochure::page(
    href = "/portfolio/risk",
    ui = function(request) {
      tagList(
        fluidPage(
          mod_portfolio_risk_dashboard_ui("portfolio_risk")
        )
      )
    },
    server = function(input, output, session) {
      mod_portfolio_risk_dashboard_server("portfolio_risk")
    }
  )
}
