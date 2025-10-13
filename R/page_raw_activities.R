#' Raw Activities Page
#'
#' Brochure page for viewing all raw activities from the database.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_raw_activities <- function() {
  brochure::page(
    href = "/portfolio/raw-activities",
    ui = function(request) {
      tagList(
        fluidPage(
          mod_raw_activities_ui("raw_activities_ui_1")
        )
      )
    },
    server = function(input, output, session) {
      mod_raw_activities_server("raw_activities_ui_1")
    }
  )
}
