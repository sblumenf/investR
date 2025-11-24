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
  validate_etf_screener_config()  # ETF Screener
  validate_puts_config()  # Cash-Secured Puts

  # --- Background Data Refresh ---
  # Sequential refresh chain to prevent token race conditions
  # Activities completes first, then positions, then schedule next cycle

  # 1. Initial refresh on app startup (sequential execution)
  log_info("App Startup: Starting sequential Questrade data refresh (activities -> positions).")
  refresh_questrade_activities() %...>% {
    log_info("App Startup: Activities refresh complete, starting positions refresh.")
    refresh_questrade_positions()
  } %...>% {
    log_info("App Startup: Initial refresh complete, scheduling hourly updates.")
  } %...!% (function(error) {
    log_error("App Startup: Refresh chain failed - {error$message}")
  })

  # 2. Define the recursive function for the hourly refresh (sequential)
  hourly_refresh <- function() {
    log_info("Hourly Timer: Starting sequential Questrade data refresh.")
    refresh_questrade_activities() %...>% {
      log_info("Hourly Timer: Activities refresh complete, starting positions refresh.")
      refresh_questrade_positions()
    } %...>% {
      log_info("Hourly Timer: Refresh cycle complete, scheduling next cycle in 1 hour.")
      # Schedule the next run in 3600 seconds (1 hour)
      later::later(hourly_refresh, 3600)
    } %...!% (function(error) {
      log_error("Hourly Timer: Refresh chain failed - {error$message}")
      # Still schedule next cycle even if this one failed
      later::later(hourly_refresh, 3600)
    })
  }

  # 3. Schedule the first hourly refresh to start in 1 hour
  log_info("App Startup: Scheduling hourly background refresh of Questrade data.")
  later::later(hourly_refresh, 3600)
  # --- End Background Data Refresh ---

  with_golem_options(
    app = brochureApp(
      # Add external resources (CSS, JS, favicon)
      golem_add_external_resources(),

      # Define pages
      page_home(),
      page_aristocrats(),
      page_zero_dividend(),
      page_dynamic_covered_calls(),
      page_etf_covered_calls(),
      page_collar(),
      page_cash_secured_puts(),
      page_etf_cash_secured_puts_yfscreen(),
      page_dividend_capture_weekly(),
      page_dividend_capture_monthly(),
      page_dividend_capture_monthly_high_yield(),
      page_dividend_capture_russell_2000(),
      page_portfolio_groups(),
      page_portfolio_risk(),
      page_cash_flow_projection(),
      page_raw_activities(),
      page_token_settings(),
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