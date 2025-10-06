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

              # Position groups alert
              uiOutput("groups_alert"),

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
              save_status <- save_positions_snapshot(positions, snapshot_timestamp)

              if (save_status == "saved") {
                log_info("Portfolio Positions: Auto-refreshed and saved {nrow(positions)} positions")
              } else if (save_status == "unchanged") {
                log_info("Portfolio Positions: Auto-refreshed {nrow(positions)} positions (no changes)")
              }

              existing_data(positions)
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

      # Reactive for whether there's a selection (will be set after table is created)
      has_selection <- reactive(FALSE)  # Default to FALSE

      # Call controls module with selection state
      controls <- mod_portfolio_positions_controls_server("controls", has_selection)

      # Merge existing data with controls data
      merged_data <- reactive({
        # If controls have new data, use that
        if (!is.null(controls$results_data()) && nrow(controls$results_data()) > 0) {
          return(controls$results_data())
        }

        # Otherwise use existing data from DB
        return(existing_data())
      })

      # Call table module and get selected rows
      table <- mod_portfolio_positions_table_server("table", merged_data)

      # Update has_selection reactive based on table selection
      has_selection <- reactive({
        !is.null(table$selected_rows()) && nrow(table$selected_rows()) > 0
      })

      # Display status messages from controls module
      output$controls_status <- renderUI({
        controls$status_ui()
      })

      # Check position groups integrity and display alert
      output$groups_alert <- renderUI({
        positions <- merged_data()

        if (is.null(positions) || nrow(positions) == 0) {
          return(NULL)
        }

        # Check group integrity
        summary <- get_broken_groups_summary(positions)

        # Display alert if there are broken/incomplete groups
        create_groups_summary_alert(
          broken_count = summary$broken_count,
          incomplete_count = summary$incomplete_count
        )
      })

      # Handle create group button click
      observeEvent(controls$create_group_trigger(), {
        req(table$selected_rows())

        # Validate selection
        validation <- validate_selection_for_grouping(table$selected_rows())

        if (!validation$valid) {
          showNotification(validation$error, type = "error", duration = 5)
          return()
        }

        # Show modal
        show_create_group_modal(validation$ticker)
      })

      # Handle save group
      observeEvent(input$save_group, {
        req(table$selected_rows())

        # Auto-assign roles
        members <- assign_roles_from_positions(table$selected_rows())

        # Get account from selection (all should be same after validation)
        account <- unique(table$selected_rows()$account_number)[1]

        # Validate group definition
        validation <- validate_group_definition(
          group_name = input$group_name,
          strategy_type = input$group_strategy,
          account_number = account,
          members = members
        )

        if (!validation$valid) {
          showNotification(
            paste("Validation errors:", paste(validation$errors, collapse = "; ")),
            type = "error",
            duration = 10
          )
          return()
        }

        # Generate ID and create
        group_id <- generate_group_id(input$group_strategy, account)

        success <- create_position_group(
          group_id = group_id,
          group_name = input$group_name,
          strategy_type = input$group_strategy,
          account_number = account,
          members = members
        )

        if (success) {
          showNotification(
            sprintf("Created group '%s'", input$group_name),
            type = "message",
            duration = 3
          )
          removeModal()

          # Clear row selection to prevent accidental duplicate group creation
          table$clear_selection()

          # Force table refresh to show new group highlighting immediately
          table$refresh()
        } else {
          showNotification("Failed to create group", type = "error", duration = 5)
        }
      })

      # Handle cancel group
      observeEvent(input$cancel_group, {
        removeModal()
      })
    }
  )
}

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Show create group modal
#' @noRd
show_create_group_modal <- function(default_name) {
  showModal(
    modalDialog(
      title = "Create Position Group",
      size = "m",

      textInput("group_name", "Name", value = default_name),
      selectInput("group_strategy", "Strategy", choices = get_strategy_types()),

      footer = tagList(
        actionButton("cancel_group", "Cancel"),
        actionButton("save_group", "Create Group", class = "btn-primary")
      )
    )
  )
}
