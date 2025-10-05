#' Portfolio Positions Page
#'
#' This function creates the portfolio positions page for viewing current holdings
#' from all Questrade accounts. Uses modular architecture with separate modules
#' for controls and table display.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @importFrom logger log_info log_debug
#' @noRd
page_portfolio_positions <- function() {
  brochure::page(
    href = "/portfolio/positions",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("Portfolio Positions"),

          # Sidebar layout
          sidebarLayout(
            # Sidebar: Controls module
            mod_portfolio_positions_controls_ui("controls"),

            # Main panel
            mainPanel(
              width = 9,

              # Status/Progress messages
              uiOutput("controls_status"),

              # Results table module
              mod_portfolio_positions_table_ui("table")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Initialize data with existing DB data or trigger smart refresh
      existing_data <- reactiveVal(NULL)

      # On session start, check for existing data and smart refresh
      observe({
        log_info("Portfolio Positions: Page loaded, checking for existing data")

        # Get latest snapshot timestamp
        latest_timestamp <- get_latest_snapshot_timestamp()

        if (is.null(latest_timestamp)) {
          # No data exists, leave empty (user will click Refresh)
          log_info("Portfolio Positions: No existing data found")
          return(NULL)
        }

        # Check if data is stale
        if (is_data_stale(latest_timestamp)) {
          log_info("Portfolio Positions: Data is stale (last updated {latest_timestamp}), triggering refresh")

          # Trigger auto-refresh
          tryCatch({
            positions <- fetch_all_positions_sequential()
            if (nrow(positions) > 0) {
              snapshot_timestamp <- Sys.time()
              save_positions_snapshot(positions, snapshot_timestamp)
              existing_data(positions)
              log_info("Portfolio Positions: Auto-refreshed {nrow(positions)} positions")
            }
          }, error = function(e) {
            log_warn("Portfolio Positions: Auto-refresh failed, loading stale data - {e$message}")
            # Fall back to stale data
            stale_positions <- get_latest_positions()
            existing_data(stale_positions)
          })
        } else {
          # Data is fresh, load from DB
          log_info("Portfolio Positions: Loading fresh data from database (last updated {latest_timestamp})")
          positions <- get_latest_positions()
          existing_data(positions)
        }
      }) %>%
        bindEvent(session$clientData, once = TRUE, ignoreNULL = FALSE)

      # Call controls module
      controls <- mod_portfolio_positions_controls_server("controls")

      # Merge existing data with controls data
      merged_data <- reactive({
        # If controls have new data, use that
        if (!is.null(controls$results_data()) && nrow(controls$results_data()) > 0) {
          return(controls$results_data())
        }

        # Otherwise use existing data from DB
        return(existing_data())
      })

      # Pass merged data to table module
      mod_portfolio_positions_table_server("table", merged_data)

      # Display status messages from controls module
      output$controls_status <- renderUI({
        controls$status_ui()
      })
    }
  )
}
