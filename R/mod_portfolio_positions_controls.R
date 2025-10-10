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

      # Fetch activities button
      actionButton(
        ns("fetch_activities"),
        "Fetch Activities",
        class = "btn-info btn-lg btn-block",
        icon = icon("download")
      ),

      br(),

      # Run pattern matching button
      actionButton(
        ns("run_pattern_matching"),
        "Run Pattern Matching",
        class = "btn-secondary btn-lg btn-block",
        icon = icon("search")
      ),

      br(),
      br(),

      # Create group button
      uiOutput(ns("create_group_button")),

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
#' @param has_selection Reactive returning TRUE if rows are selected
#'
#' @return A list with reactive values: results_data, status_ui, last_updated, create_group_trigger
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive reactiveVal renderUI req observeEvent
mod_portfolio_positions_controls_server <- function(id, has_selection = reactive(FALSE)){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Reactive value to store last update timestamp
    last_updated_time <- reactiveVal(NULL)
    last_activities_time <- reactiveVal(NULL)

    # Reactive value to store save status for custom messaging
    save_status <- reactiveVal(NULL)

    # Analysis function: fetch positions and save to database
    fetch_and_save_positions <- function() {
      # Fetch from Questrade API
      positions <- fetch_all_positions_sequential()

      if (nrow(positions) == 0) {
        save_status("error")
        return(tibble::tibble())
      }

      # Save to database with current timestamp
      snapshot_timestamp <- Sys.time()
      status <- save_positions_snapshot(positions, snapshot_timestamp)

      # Auto-close stale groups (groups with missing positions)
      if (status %in% c("saved", "unchanged")) {
        auto_close_stale_groups(positions)
      }

      # Store status for custom messaging
      save_status(status)

      # Update timestamp on successful save or unchanged (both mean data is fresh)
      if (status %in% c("saved", "unchanged")) {
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

    # Custom status UI that shows save status information
    custom_status_ui <- reactive({
      base_ui <- controls$status_ui()

      # Add save status information if available
      status <- save_status()
      if (!is.null(status) && !is.null(controls$results_data()) &&
          nrow(controls$results_data()) > 0) {

        if (status == "unchanged") {
          # Override success message to indicate no changes
          return(
            tags$div(
              class = "alert alert-info alert-dismissible fade in",
              role = "alert",
              tags$button(
                type = "button",
                class = "close",
                `data-dismiss` = "alert",
                `aria-label` = "Close",
                tags$span(`aria-hidden` = "true", HTML("&times;"))
              ),
              tags$strong(icon("info-circle"), " Info: "),
              sprintf("Refreshed %d positions - no changes since last snapshot",
                      nrow(controls$results_data()))
            )
          )
        } else if (status == "saved") {
          # Standard success message is fine, but we could customize if needed
          return(base_ui)
        }
      }

      return(base_ui)
    })

    # Render create group button (enabled/disabled based on selection)
    output$create_group_button <- renderUI({
      if (has_selection()) {
        actionButton(
          ns("create_group"),
          "Create Group from Selection",
          class = "btn-success btn-block",
          icon = icon("object-group")
        )
      } else {
        tags$button(
          class = "btn btn-secondary btn-block disabled",
          disabled = "disabled",
          icon("object-group"),
          " Create Group from Selection"
        )
      }
    })

    # Render last updated timestamp
    output$last_updated <- renderUI({
      pos_timestamp <- last_updated_time()
      act_timestamp <- last_activities_time()

      tags$div(
        class = "text-muted",
        tags$small(
          tags$strong("Last updated:"),
          br(),
          # Positions timestamp
          if (!is.null(pos_timestamp)) {
            tags$span(
              title = format(pos_timestamp, "%Y-%m-%d %H:%M:%S"),
              sprintf("Positions: %s", format_time_ago(pos_timestamp))
            )
          } else {
            "Positions: Never"
          },
          br(),
          # Activities timestamp
          if (!is.null(act_timestamp)) {
            tags$span(
              title = format(act_timestamp, "%Y-%m-%d %H:%M:%S"),
              sprintf("Activities: %s", format_time_ago(act_timestamp))
            )
          } else {
            "Activities: Never"
          }
        )
      )
    })

    # Return controls with custom status UI
    list(
      results_data = controls$results_data,
      status_ui = custom_status_ui,
      last_updated_time = last_updated_time,
      last_activities_time = last_activities_time,
      create_group_trigger = reactive(input$create_group),
      fetch_activities_trigger = reactive(input$fetch_activities),
      run_pattern_matching_trigger = reactive(input$run_pattern_matching)
    )
  })
}

## To be copied in the UI
# mod_portfolio_positions_controls_ui("portfolio_positions_controls_1")

## To be copied in the server
# mod_portfolio_positions_controls_server("portfolio_positions_controls_1")
