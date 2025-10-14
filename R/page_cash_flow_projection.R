#' Cash Flow Projection Page
#'
#' Brochure page for viewing projected and actual cash flows across
#' all position groups. Shows monthly aggregation with visualization
#' and transaction-level detail.
#' Each Brochure page has its own independent Shiny session.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_cash_flow_projection <- function() {
  brochure::page(
    href = "/portfolio/cash-flow-projection",
    ui = function(request) {
      tagList(
        fluidPage(
          mod_cash_flow_projection_ui("cash_flow_projection")
        )
      )
    },
    server = function(input, output, session) {
      mod_cash_flow_projection_server("cash_flow_projection")
    }
  )
}
