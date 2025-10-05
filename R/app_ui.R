#' Add external Resources to the Application
#'
#' This function is internally used to add external resources (CSS, JS, favicon)
#' inside the Shiny application. It is a standard Golem function for resource
#' management.
#'
#' @section Brochure Integration:
#' In a standard Golem app, this would be called from `app_ui()`. However, since
#' this app uses Brochure for multi-page routing, there is no traditional `app_ui()`
#' function. Instead, this function is called directly in `run_app()` at the
#' brochureApp level, ensuring resources are loaded for all pages.
#'
#' @section File Location:
#' This file (`app_ui.R`) exists to maintain Golem convention, even though it
#' doesn't contain a full UI function. The resource management function is still
#' critical for proper app styling and functionality.
#'
#' @return A shiny.tag.list with resource links
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "investR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}