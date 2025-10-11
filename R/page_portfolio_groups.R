#' Portfolio Groups Page
#'
#' Brochure page for viewing and managing position groups.
#' Each Brochure page has its own independent Shiny session.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_portfolio_groups <- function() {
  brochure::page(
    href = "/portfolio/groups",
    ui = function(request) {
      tagList(
        fluidPage(
          mod_portfolio_groups_ui("portfolio_groups")
        )
      )
    },
    server = function(input, output, session) {
      mod_portfolio_groups_server("portfolio_groups")
    }
  )
}
