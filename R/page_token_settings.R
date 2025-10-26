#' Token Settings Page
#'
#' Simple page for updating Questrade refresh token
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_token_settings <- function() {
  brochure::page(
    href = "/settings/token",
    ui = function(request) {
      fluidPage(
        titlePanel("Questrade Token Settings"),

        # Navigation
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            "back_home",
            "← Back to Home",
            onclick = "window.location.href='/';",
            class = "btn-default"
          )
        ),

        # Token settings module
        mod_token_settings_ui("token_manager")
      )
    },
    server = function(input, output, session) {
      mod_token_settings_server("token_manager")
    }
  )
}
