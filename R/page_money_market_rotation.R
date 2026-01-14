#' Money Market Rotation Page
#'
#' Brochure page for tracking the money market dividend capture rotation strategy.
#' Shows current position, performance metrics, rotation history, and dividends.
#' Each Brochure page has its own independent Shiny session.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_money_market_rotation <- function() {
  brochure::page(
    href = "/portfolio/money-market-rotation",
    ui = function(request) {
      tagList(
        fluidPage(
          mod_money_market_rotation_ui("money_market_rotation")
        )
      )
    },
    server = function(input, output, session) {
      mod_money_market_rotation_server("money_market_rotation")
    }
  )
}
