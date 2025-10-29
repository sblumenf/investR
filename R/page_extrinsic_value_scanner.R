
#' Extrinsic Value Scanner Page
#'
#' This function creates the Extrinsic Value Scanner page.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_extrinsic_value_scanner <- function() {
  brochure::page(
    href = "/extrinsic_value_scanner",
    ui = function(request) {
      tagList(
        mod_extrinsic_value_scanner_ui("extrinsic_value_scanner_1")
      )
    },
    server = function(input, output, session) {
      mod_extrinsic_value_scanner_server("extrinsic_value_scanner_1")
    }
  )
}
