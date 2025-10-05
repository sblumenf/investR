#' Russell 2000 High-Yield Dividend Capture Page
#'
#' This function creates the Russell 2000 high-yield dividend capture strategy page.
#' Uses modular architecture with parameterized modules and universe-specific configuration.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_dividend_capture_russell_2000 <- function() {
  # Get Russell 2000-specific configuration
  universe_config <- get_russell_2000_high_yield_config()

  brochure::page(
    href = paste0("/", universe_config$route),
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel(paste(universe_config$universe_name, "High-Yield Dividend Capture Strategy")),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Controls module (parameterized with universe config)
            mod_high_yield_dividend_capture_controls_ui("controls", universe_config),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress
              uiOutput("analysis_status"),

              # Results display module
              mod_high_yield_dividend_capture_results_ui("results")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Call controls module with universe config and get reactive results
      controls <- mod_high_yield_dividend_capture_controls_server("controls", universe_config)

      # Pass results, yield filter, and universe config to display module
      mod_high_yield_dividend_capture_results_server("results",
                                                     controls$results_data,
                                                     controls$yield_filter,
                                                     universe_config)

      # Display status messages from controls module
      output$analysis_status <- renderUI({
        controls$status_ui()
      })
    }
  )
}
