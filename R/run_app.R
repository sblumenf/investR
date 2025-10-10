#' Run the Shiny Application
#'
#' This function launches the investR Shiny app using a Golem + Brochure
#' architecture for multi-page functionality.
#'
#' @section Golem + Brochure Integration:
#' This app uses an advanced pattern combining two frameworks:
#'
#' **Golem Framework** provides:
#' - Package structure for production-ready Shiny apps
#' - Resource management (CSS, JS, favicon) via `golem_add_external_resources()`
#' - Configuration management via `golem-config.yml`
#' - Development workflow and deployment tools
#'
#' **Brochure Framework** provides:
#' - Multi-page routing with independent URLs
#' - Separate Shiny sessions per page (better performance)
#' - Native page navigation without complex routing logic
#'
#' Unlike standard Golem apps that use `shinyApp(ui = app_ui, server = app_server)`,
#' this app uses `brochureApp()` and passes individual page objects created with
#' `brochure::page()`. Each page (defined in `page_*.R` files) has its own
#' independent UI and server function.
#'
#' @section Architecture:
#' - `run_app()` - App launcher (this function)
#' - `page_home()` - Home page at "/"
#' - `page_aristocrats()` - Analysis page at "/aristocrats"
#' - `page_about()` - About page at "/about"
#' - `mod_*.R` - Shiny modules used within pages
#' - `fct_*.R` - Business logic functions
#' - `utils_*.R` - Utility helper functions
#'
#' @param onStart A function that will be called before the app is actually run.
#'     This is typically used for initialization or setting up environment.
#' @param options Named options that should be passed to the `runApp` call
#'     (these can be any of the options listed in `?shiny::runApp`)
#' @param enableBookmarking Can be one of "url", "server", or "disable".
#' @param uiPattern A regular expression that will be applied to each GET
#'     request to determine whether the `ui` should be used to handle the
#'     request.
#' @param ... arguments to pass to golem_opts.
#'     See `?golem::get_golem_options` for more details.
#'
#' @return A Brochure app object ready to be run
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom brochure brochureApp
#' @examples
#' \dontrun{
#'   # Run the app locally
#'   investR::run_app()
#'
#'   # Run with custom options
#'   investR::run_app(options = list(port = 3838))
#' }
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  # Validate all strategy configurations on app startup
  validate_config()  # Aristocrats
  validate_zero_dividend_config()  # Zero Dividend
  validate_dynamic_config()  # Dynamic Covered Calls
  validate_collar_config()  # Collar Strategy

  # Questrade API now uses lazy refresh - no background ping needed
  # Tokens are cached in ~/.investr_questrade_tokens.json and automatically
  # refreshed when expired. Access tokens are reused for up to 30 minutes.

  with_golem_options(
    app = brochureApp(
      # Add external resources (CSS, JS, favicon)
      golem_add_external_resources(),

      # Define pages
      page_home(),
      page_aristocrats(),
      page_zero_dividend(),
      page_dynamic_covered_calls(),
      page_collar(),
      page_dividend_capture_weekly(),
      page_dividend_capture_monthly(),
      page_dividend_capture_russell_2000(),
      page_portfolio_positions(),
      page_about(),

      # Brochure configuration
      onStart = onStart,
      uiPattern = uiPattern,

      # Additional options
      wrapped = shiny::tagList
    ),
    golem_opts = list(...)
  )
}