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

              # Grouping suggestions alert
              uiOutput("suggestions_alert"),

              # Position groups alert
              uiOutput("groups_alert"),

              # Results table module
              mod_portfolio_positions_table_ui("table"),

              # Suggestions modal (rendered programmatically)
              mod_suggestions_modal_ui("suggestions")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Initialize data with existing DB data or trigger smart refresh
      existing_data <- reactiveVal(NULL)

      # Store multi-group detection data for multi-group creation
      multi_group_data <- reactiveVal(NULL)

      # On session start, load existing data from DB (no auto-fetch)
      observe({
        log_info("Portfolio Positions: Page loaded, loading data from database")

        # Always load from DB, never auto-fetch
        positions <- get_latest_positions()

        if (nrow(positions) > 0) {
          log_info("Portfolio Positions: Loaded {nrow(positions)} positions from database")
          existing_data(positions)
        } else {
          log_info("Portfolio Positions: No existing data found")
        }
      }) %>%
        bindEvent(session$clientData, once = TRUE, ignoreNULL = FALSE)

      # Load activities timestamp on page load (no auto-fetch)
      observe({
        latest_timestamp <- get_last_activities_update()
        if (!is.null(latest_timestamp)) {
          controls$last_activities_time(latest_timestamp)
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

      # Reactive trigger for showing suggestions modal
      review_suggestions_trigger <- reactiveVal(0)

      # Call suggestions modal module
      suggestions_modal <- mod_suggestions_modal_server(
        "suggestions",
        reactive(review_suggestions_trigger())
      )

      # Display grouping suggestions alert
      output$suggestions_alert <- renderUI({
        # Re-run when suggestions are processed
        suggestions_modal()

        # Get pending suggestion count
        pending <- get_pending_suggestions()

        if (nrow(pending) == 0) {
          return(NULL)
        }

        # Create alert banner
        div(
          class = "alert alert-info",
          style = "margin-bottom: 15px;",
          icon("lightbulb"),
          sprintf(" You have %d pending grouping suggestion%s.",
                  nrow(pending),
                  if (nrow(pending) == 1) "" else "s"),
          actionButton(
            "review_suggestions_btn",
            "Review",
            class = "btn-primary btn-sm",
            style = "margin-left: 10px;"
          )
        )
      })

      # Trigger modal when Review button clicked
      observeEvent(input$review_suggestions_btn, {
        review_suggestions_trigger(review_suggestions_trigger() + 1)
      })

      # Refresh table when suggestions are processed
      observeEvent(suggestions_modal(), {
        table$refresh()
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

        # Check for multi-group scenario
        multi_group_check <- detect_multi_group_scenario(table$selected_rows())

        if (multi_group_check$is_multi_group) {
          # Store multi-group data for save handler
          multi_group_data(multi_group_check)
          # Show multi-group modal
          show_multi_group_modal(multi_group_check)
        } else {
          # Show single group modal
          show_create_group_modal(validation$ticker)
        }
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
          # Generate income projections for the new group
          generate_initial_projections(group_id, members, account)

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

      # Handle cancel multi-group
      observeEvent(input$cancel_multi_group, {
        removeModal()
        multi_group_data(NULL)
      })

      # Handle save multi-group
      observeEvent(input$save_multi_group, {
        req(multi_group_data())

        multi_check <- multi_group_data()
        num_groups <- length(multi_check$suggested_groups)

        # Collect inputs for each group
        groups_to_create <- list()

        for (i in seq_len(num_groups)) {
          group_name <- input[[sprintf("multi_group_name_%d", i)]]
          strategy <- input[[sprintf("multi_group_strategy_%d", i)]]
          allocated_shares <- input[[sprintf("multi_group_shares_%d", i)]]

          if (is.null(group_name) || is.null(strategy) || is.null(allocated_shares)) {
            showNotification(
              sprintf("Group %d has missing inputs", i),
              type = "error",
              duration = 5
            )
            return()
          }

          groups_to_create[[i]] <- list(
            name = group_name,
            strategy = strategy,
            allocated_shares = allocated_shares,
            suggested_group = multi_check$suggested_groups[[i]]
          )
        }

        # Validate total allocation
        total_allocated <- sum(map_dbl(groups_to_create, ~ .$allocated_shares))
        stock_qty <- multi_check$stock_quantity

        if (total_allocated != stock_qty) {
          showNotification(
            sprintf("Total allocated shares (%d) must equal stock quantity (%d)", total_allocated, stock_qty),
            type = "error",
            duration = 10
          )
          return()
        }

        # Get account number from first suggested group
        account <- unique(table$selected_rows()$account_number)[1]

        # Create each group
        success_count <- 0

        for (i in seq_len(num_groups)) {
          group_def <- groups_to_create[[i]]
          suggested <- group_def$suggested_group

          # Generate group ID
          group_id <- generate_group_id(group_def$strategy, account)

          # Build members with allocated quantity for stock
          members <- tibble::tibble(
            symbol = c(suggested$stock_symbol, suggested$option_symbols),
            role = c("underlying_stock", rep("short_call", length(suggested$option_symbols))),
            allocated_quantity = c(group_def$allocated_shares, rep(NA_real_, length(suggested$option_symbols)))
          )

          # Validate group
          validation <- validate_group_definition(
            group_name = group_def$name,
            strategy_type = group_def$strategy,
            account_number = account,
            members = members
          )

          if (!validation$valid) {
            showNotification(
              sprintf("Group %d validation failed: %s", i, paste(validation$errors, collapse = "; ")),
              type = "error",
              duration = 10
            )
            next
          }

          # Create group
          success <- create_position_group(
            group_id = group_id,
            group_name = group_def$name,
            strategy_type = group_def$strategy,
            account_number = account,
            members = members
          )

          if (success) {
            # Generate income projections
            generate_initial_projections(group_id, members, account)
            success_count <- success_count + 1
          }
        }

        if (success_count > 0) {
          showNotification(
            sprintf("Created %d group%s", success_count, if (success_count > 1) "s" else ""),
            type = "message",
            duration = 3
          )
          removeModal()
          multi_group_data(NULL)

          # Clear selection and refresh table
          table$clear_selection()
          table$refresh()
        } else {
          showNotification("Failed to create any groups", type = "error", duration = 5)
        }
      })

      # Handle fetch activities button
      observeEvent(controls$fetch_activities_trigger(), {
        log_info("Portfolio Positions: Manual fetch activities triggered")

        tryCatch({
          suggestion_count <- fetch_all_activities()
          controls$last_activities_time(Sys.time())

          showNotification(
            sprintf("Fetched activities and generated %d suggestion%s",
                    suggestion_count,
                    if (suggestion_count == 1) "" else "s"),
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          log_error("Portfolio Positions: Failed to fetch activities - {e$message}")
          showNotification(
            sprintf("Failed to fetch activities: %s", e$message),
            type = "error",
            duration = 10
          )
        })
      })

      # Handle run pattern matching button
      observeEvent(controls$run_pattern_matching_trigger(), {
        log_info("Portfolio Positions: Manual pattern matching triggered")

        tryCatch({
          suggestion_count <- run_pattern_matching()

          showNotification(
            sprintf("Pattern matching complete: %d suggestion%s generated",
                    suggestion_count,
                    if (suggestion_count == 1) "" else "s"),
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          log_error("Portfolio Positions: Failed to run pattern matching - {e$message}")
          showNotification(
            sprintf("Pattern matching failed: %s", e$message),
            type = "error",
            duration = 10
          )
        })
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

#' Show multi-group modal for position splitting
#' @noRd
show_multi_group_modal <- function(multi_group_check) {
  suggested_groups <- multi_group_check$suggested_groups
  stock_qty <- multi_group_check$stock_quantity

  # Build table rows for each suggested group
  group_inputs <- map(seq_along(suggested_groups), function(i) {
    group <- suggested_groups[[i]]

    tagList(
      tags$tr(
        tags$td(
          tags$strong(sprintf("Group %d", i)),
          br(),
          tags$small(sprintf("%d options", length(group$option_symbols)))
        ),
        tags$td(
          textInput(
            sprintf("multi_group_name_%d", i),
            NULL,
            value = group$group_name,
            width = "100%"
          )
        ),
        tags$td(
          selectInput(
            sprintf("multi_group_strategy_%d", i),
            NULL,
            choices = get_strategy_types(),
            width = "100%"
          )
        ),
        tags$td(
          numericInput(
            sprintf("multi_group_shares_%d", i),
            NULL,
            value = group$allocated_shares,
            min = 100,
            max = stock_qty,
            step = 100,
            width = "100%"
          )
        )
      )
    )
  })

  showModal(
    modalDialog(
      title = sprintf("Create %d Position Groups", length(suggested_groups)),
      size = "l",

      p(sprintf("Detected %d groups for %d shares. Adjust quantities as needed:",
                length(suggested_groups), stock_qty)),

      tags$table(
        class = "table table-sm",
        tags$thead(
          tags$tr(
            tags$th("Group"),
            tags$th("Name"),
            tags$th("Strategy"),
            tags$th("Shares")
          )
        ),
        tags$tbody(group_inputs)
      ),

      # Hidden field to store number of groups
      tags$input(
        type = "hidden",
        id = "multi_group_count",
        value = length(suggested_groups)
      ),

      footer = tagList(
        actionButton("cancel_multi_group", "Cancel"),
        actionButton("save_multi_group", sprintf("Create %d Groups", length(suggested_groups)), class = "btn-primary")
      )
    )
  )
}
