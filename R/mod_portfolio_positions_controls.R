#' portfolio_positions_controls UI Function
#'
#' @description A shiny Module for portfolio positions controls
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList sidebarPanel h3 actionButton downloadButton tags icon hr
mod_portfolio_positions_controls_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h3("Portfolio Positions"),

      # Refresh button
      actionButton(
        ns("run_analysis"),
        "Refresh Positions",
        class = "btn-primary btn-lg btn-block",
        icon = icon("sync")
      ),

      br(),
      br(),

      # Last updated display
      uiOutput(ns("last_updated")),

      hr(),

      # Navigation
      tags$a(
        href = "/",
        class = "btn btn-default btn-block",
        icon("home"),
        " Home"
      )
    )
  )
}

#' portfolio_positions_controls Server Functions
#'
#' @description Server logic for portfolio positions controls module
#'   Handles fetching positions from Questrade and saving to database
#'
#' @param id Module ID
#'
#' @return A list with reactive values: results_data, status_ui, last_updated
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive reactiveVal renderUI req observeEvent
mod_portfolio_positions_controls_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Reactive value to store last update timestamp
    last_updated_time <- reactiveVal(NULL)

    # Analysis function: fetch positions and save to database
    fetch_and_save_positions <- function() {
      # Fetch from Questrade API
      positions <- fetch_all_positions_sequential()

      if (nrow(positions) == 0) {
        return(tibble::tibble())
      }

      # Save to database with current timestamp
      snapshot_timestamp <- Sys.time()
      save_success <- save_positions_snapshot(positions, snapshot_timestamp)

      if (save_success) {
        last_updated_time(snapshot_timestamp)
      }

      return(positions)
    }

    # Use the standard analysis controls helper
    controls <- setup_analysis_controls(
      input = input,
      output = output,
      session = session,
      analysis_func = fetch_and_save_positions,
      progress_message = "Fetching positions from Questrade API...",
      success_message_template = "Successfully refreshed %d positions",
      no_results_message = "No positions found in any account",
      download_filename_prefix = "portfolio_positions"
    )

    # Render last updated timestamp
    output$last_updated <- renderUI({
      timestamp <- last_updated_time()

      if (is.null(timestamp)) {
        tags$p(
          class = "text-muted",
          tags$small("Not yet updated")
        )
      } else {
        tags$p(
          class = "text-muted",
          tags$small(
            tags$strong("Last updated:"),
            br(),
            format(timestamp, "%Y-%m-%d %H:%M:%S")
          )
        )
      }
    })

    # Return controls plus last_updated_time
    list(
      results_data = controls$results_data,
      status_ui = controls$status_ui,
      last_updated_time = last_updated_time
    )
  })
}

## To be copied in the UI
# mod_portfolio_positions_controls_ui("portfolio_positions_controls_1")

## To be copied in the server
# mod_portfolio_positions_controls_server("portfolio_positions_controls_1")
