#' portfolio_groups_cards UI Function
#'
#' @description Sub-module for displaying position group cards stream.
#' Renders filtered and sorted cards, handles user interactions (Close Group, etc.).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_group_cards_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards_container"))
  )
}

#' portfolio_groups_cards Server Functions
#'
#' @description Server logic for rendering cards and handling actions.
#'
#' @param id Module ID
#' @param filtered_groups Reactive containing filtered/sorted tibble of groups
#' @param metrics Reactive containing pre-calculated metrics (from main orchestrator)
#'
#' @noRd
#'
#' @importFrom shiny moduleServer renderUI req tags observeEvent showModal modalDialog removeModal showNotification reactiveValues textInput selectInput uiOutput
#' @importFrom dplyr %>% arrange desc filter left_join anti_join bind_rows mutate select
#' @importFrom purrr map
mod_group_cards_server <- function(id, filtered_groups, metrics = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Reactive to track card regeneration
    card_version <- reactiveVal(0)

    # Render cards container
    output$cards_container <- renderUI({
      req(filtered_groups())

      # Trigger dependency on card_version to force re-render after actions
      card_version()

      groups <- filtered_groups()

      if (nrow(groups) == 0) {
        return(tags$div(
          class = "alert alert-info",
          style = "margin-top: 20px;",
          icon("info-circle"),
          " No groups found matching the current filters."
        ))
      }

      # Batch-fetch all data for all groups at once (instead of per-card queries)
      group_ids <- groups$group_id
      all_members <- get_members_for_groups(group_ids)
      all_activities <- get_activities_for_groups(group_ids)
      all_cash_flows <- get_cash_flows_for_groups(group_ids)
      latest_positions <- get_latest_positions()  # Fetch once for market data enrichment

      # Get pre-calculated metrics if available (DRY principle)
      metrics_lookup <- if (!is.null(metrics)) {
        metrics_data <- metrics()
        # Create lookup table: group_id -> metrics
        bind_rows(
          metrics_data$open_groups_detail,
          metrics_data$closed_groups_detail
        )
      } else {
        NULL
      }

      # Generate cards with pre-fetched data
      cards <- purrr::map(seq_len(nrow(groups)), function(i) {
        group <- groups[i, ]
        gid <- group$group_id

        # Filter pre-fetched data for this group
        group_members <- all_members %>% filter(group_id == gid)
        group_activities <- all_activities %>% filter(group_id == gid)
        group_cash_flows <- all_cash_flows %>% filter(group_id == gid)

        # Get pre-calculated metrics for this group (if available)
        group_metrics <- if (!is.null(metrics_lookup)) {
          metrics_lookup %>% filter(group_id == gid)
        } else {
          NULL
        }

        # Pass all data to card creation
        # Note: ns() must be passed for onclick JavaScript to set namespaced inputs
        create_group_card(
          group_id = gid,
          group_data = group,
          members = group_members,
          activities = group_activities,
          cash_flows = group_cash_flows,
          metrics = group_metrics,
          latest_positions = latest_positions,
          ns = ns
        )
      })

      # Return cards container
      tags$div(
        class = "opportunity-cards-container",
        style = "margin-top: 20px;",
        cards
      )
    })

    # Handle Analyze Risk button clicks for portfolio groups
    observeEvent(input$analyze_risk_group_clicked, {
      group_id <- input$analyze_risk_group_clicked

      log_info("Analyze Risk clicked for group: {group_id}")

      # Get group data to extract position details
      group_data <- get_group_by_id(group_id)
      members <- get_group_members(group_id)
      activities <- get_activities_by_group(group_id)

      if (nrow(group_data) == 0) {
        log_warn("Group {group_id} not found for risk analysis")
        return()
      }

      # Extract ticker
      ticker <- if (nrow(members) > 0) {
        underlying <- members %>% filter(role == "underlying_stock")
        if (nrow(underlying) > 0) {
          underlying$symbol[1]
        } else {
          members$symbol[1]
        }
      } else {
        NULL
      }

      # Extract option details from activities
      option_activity <- activities %>%
        filter(type == "Trades") %>%
        filter(purrr::map_lgl(symbol, is_option_symbol)) %>%
        arrange(desc(trade_date)) %>%
        slice(1)

      if (nrow(option_activity) == 0) {
        showNotification("No option data found for this group", type = "warning")
        return()
      }

      # Parse option symbol to get strike and expiration
      option_details <- parse_option_details(option_activity$symbol)

      # Extract cost basis from stock purchase activities
      stock_purchases <- activities %>%
        filter(type == "Trades", action == "Buy") %>%
        filter(!purrr::map_lgl(symbol, is_option_symbol))

      cost_basis <- if (nrow(stock_purchases) > 0) {
        # Weighted average cost basis
        total_cost <- sum(abs(stock_purchases$net_amount), na.rm = TRUE)
        total_shares <- sum(abs(stock_purchases$quantity), na.rm = TRUE)
        total_cost / total_shares
      } else {
        NULL  # No stock purchases found, will use current price
      }

      # Trigger risk analysis module
      mod_position_risk_server(
        id = paste0("risk_group_", group_id),
        trigger = reactive({ input$analyze_risk_group_clicked }),
        ticker = reactive(ticker),
        strike = reactive(option_details$strike),
        expiration = reactive(option_details$expiry),
        premium_received = reactive({
          # Premium per contract (100 shares)
          # net_amount is total for all contracts, quantity is number of contracts
          total_premium <- abs(option_activity$net_amount)
          num_contracts <- abs(option_activity$quantity)
          total_premium / num_contracts
        }),
        current_price = reactive(NULL),  # Will fetch live
        cost_basis = reactive(cost_basis),  # Actual entry price
        is_aristocrat = reactive(FALSE),  # Unknown for portfolio positions
        simulation_paths = reactive(10000)
      )
    })

    # Handle Close Group button clicks
    observeEvent(input$close_group_clicked, {
      group_id <- input$close_group_clicked

      cat(sprintf("\n=== [TRACE] Close Group Button Clicked ===\n"))
      cat(sprintf("Timestamp: %s\n", Sys.time()))
      cat(sprintf("Group ID: %s\n", group_id))
      cat(sprintf("Input value: %s\n", input$close_group_clicked))
      cat(sprintf("==========================================\n\n"))

      log_info("Close Group clicked - group_id: {group_id}")

      # Show confirmation modal
      showModal(modalDialog(
        title = "Close Position Group",
        tags$div(
          tags$p(sprintf("Are you sure you want to close group: %s?", group_id)),
          tags$p("This will:"),
          tags$ul(
            tags$li("Calculate final P&L from all activities"),
            tags$li("Mark the group as closed"),
            tags$li("Remove projected cash flow events")
          ),
          tags$hr(),
          # Show P&L preview
          tags$div(
            id = "pnl_preview",
            tags$h5("P&L Preview:"),
            renderPnLPreview(group_id)
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          tags$button(
            id = ns(sprintf("confirm_close_btn_%s", group_id)),
            type = "button",
            class = "btn btn-warning",
            onclick = sprintf("console.log('Confirm close: %s'); Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                              group_id, ns("close_group_confirmed"), group_id),
            icon("check"),
            " Confirm Close"
          )
        ),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)

    # Handle confirmation of close action
    observeEvent(input$close_group_confirmed, {
      group_id <- input$close_group_confirmed

      cat(sprintf("\n=== [TRACE] Close Group Confirmed ===\n"))
      cat(sprintf("Timestamp: %s\n", Sys.time()))
      cat(sprintf("Group ID: %s\n", group_id))
      cat(sprintf("======================================\n\n"))

      log_info("Close Group confirmed - group_id: {group_id}")

      # Close the group
      result <- close_position_group(group_id)

      if (nrow(result) > 0) {
        # Success
        showNotification(
          sprintf("Group closed successfully. Net P&L: %s",
                 format_currency(result$net_pnl)),
          type = "message",
          duration = 5
        )

        # Increment card version to force re-render
        card_version(card_version() + 1)
      } else {
        # Failure
        showNotification(
          "Failed to close group. Please check logs.",
          type = "error",
          duration = 5
        )
      }

      # Remove modal
      removeModal()
    }, ignoreInit = TRUE)

    # Handle Reopen Group button clicks
    observeEvent(input$reopen_group_clicked, {
      group_id <- input$reopen_group_clicked

      log_info("Reopen Group clicked - group_id: {group_id}")

      # Reopen the group
      result <- reopen_position_group(group_id)

      if (result) {
        showNotification(
          sprintf("Group %s reopened successfully.", group_id),
          type = "message",
          duration = 5
        )

        # Force re-render
        card_version(card_version() + 1)
      } else {
        showNotification(
          "Failed to reopen group. Please check logs.",
          type = "error",
          duration = 5
        )
      }
    }, ignoreInit = TRUE)

    # Handle Edit Members button clicks
    observeEvent(input$edit_group_clicked, {
      group_id <- input$edit_group_clicked

      log_info("Edit Members clicked - group_id: {group_id}")

      # Fetch group data and members
      group_data <- get_group_by_id(group_id)
      if (nrow(group_data) == 0) {
        showNotification(
          "Group not found. Please refresh the page.",
          type = "error",
          duration = 5
        )
        return()
      }

      current_members <- get_group_members(group_id)
      all_positions <- get_latest_positions()

      # Reactive values to track pending changes
      edit_state <- reactiveValues(
        to_remove = character(0),
        to_add = tibble::tibble(symbol = character(0), role = character(0)),
        group_name = group_data$group_name
      )

      # Compute available positions (excluding current members)
      available_positions <- get_available_positions_for_group(
        group_data$account_number,
        current_members,
        all_positions
      )

      # Role choices for dropdown
      role_choices <- c(
        "Underlying Stock" = "underlying_stock",
        "Short Call" = "short_call",
        "Short Put" = "short_put",
        "Long Call" = "long_call",
        "Long Put" = "long_put"
      )

      # Show modal
      showModal(modalDialog(
        title = sprintf("Edit Group: %s", group_id),
        size = "m",
        tags$div(
          # Group Name Section
          tags$h5("Group Name"),
          shiny::textInput(
            ns("edit_group_name"),
            label = NULL,
            value = group_data$group_name,
            width = "100%",
            placeholder = "Enter group name..."
          ),
          tags$hr(),

          # Current Members Section
          tags$h5("Members"),
          uiOutput(ns("edit_members_list")),
          tags$hr(),

          # Add Member Section
          tags$h5("Add Member (Optional)"),
          tags$div(
            class = "row",
            tags$div(
              class = "col-md-6",
              shiny::selectInput(
                ns("edit_add_symbol"),
                label = "Position:",
                choices = c("Select position..." = "", setNames(available_positions$symbol, available_positions$symbol)),
                width = "100%"
              )
            ),
            tags$div(
              class = "col-md-4",
              shiny::selectInput(
                ns("edit_add_role"),
                label = "Role:",
                choices = c("Select role..." = "", role_choices),
                width = "100%"
              )
            ),
            tags$div(
              class = "col-md-2",
              tags$label(HTML("&nbsp;")),
              tags$button(
                id = ns("edit_add_btn"),
                type = "button",
                class = "btn btn-sm btn-success btn-block",
                onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
                                 ns("add_member_clicked")),
                icon("plus"),
                " Add"
              )
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          tags$button(
            id = ns(sprintf("save_edit_btn_%s", group_id)),
            type = "button",
            class = "btn btn-primary",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                             ns("save_edit_clicked"), group_id),
            icon("save"),
            " Save Changes"
          )
        ),
        easyClose = FALSE
      ))

      # Render members list
      output$edit_members_list <- renderUI({
        # Combine current members with pending changes
        # Start with current members
        display_members <- current_members %>%
          mutate(
            is_pending = FALSE,
            is_removed = symbol %in% edit_state$to_remove
          )

        # Add pending additions
        if (nrow(edit_state$to_add) > 0) {
          pending_members <- edit_state$to_add %>%
            mutate(
              is_pending = TRUE,
              is_removed = FALSE
            )

          display_members <- bind_rows(display_members, pending_members)
        }

        # Render member rows
        if (nrow(display_members) == 0) {
          tags$p(class = "text-muted", "No members in this group")
        } else {
          member_rows <- purrr::map(seq_len(nrow(display_members)), function(i) {
            member <- display_members[i, ]
            render_member_row(
              symbol = member$symbol,
              role = member$role,
              is_pending = member$is_pending,
              is_removed = member$is_removed,
              ns = ns
            )
          })

          tags$div(member_rows)
        }
      })

      # Handle Remove Member clicks
      observeEvent(input$remove_member_clicked, {
        symbol_to_remove <- input$remove_member_clicked

        # Show mini-confirmation
        showModal(modalDialog(
          title = "Remove Member",
          tags$p(sprintf("Are you sure you want to remove %s from this group?", symbol_to_remove)),
          footer = tagList(
            modalButton("Cancel"),
            tags$button(
              type = "button",
              class = "btn btn-danger",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); $('#%s').modal('hide');",
                               ns("confirm_remove_member"), symbol_to_remove, ns("shiny-modal")),
              icon("trash"),
              " Remove"
            )
          ),
          size = "s",
          easyClose = TRUE
        ))
      }, ignoreInit = TRUE)

      # Handle confirmed removal
      observeEvent(input$confirm_remove_member, {
        symbol_to_remove <- input$confirm_remove_member

        # Check if this is a pending addition (remove from to_add instead)
        if (symbol_to_remove %in% edit_state$to_add$symbol) {
          edit_state$to_add <- edit_state$to_add %>%
            filter(symbol != symbol_to_remove)
        } else {
          # Add to removal list
          edit_state$to_remove <- c(edit_state$to_remove, symbol_to_remove)
        }

        # Re-render will happen automatically via reactive output
      }, ignoreInit = TRUE)

      # Handle Add Member clicks
      observeEvent(input$add_member_clicked, {
        req(input$edit_add_symbol, input$edit_add_role)

        new_symbol <- input$edit_add_symbol
        new_role <- input$edit_add_role

        # Validate inputs
        if (new_symbol == "" || new_role == "") {
          showNotification(
            "Please select both a position and a role.",
            type = "warning",
            duration = 3
          )
          return()
        }

        # Check if already in current members or pending additions
        if (new_symbol %in% current_members$symbol || new_symbol %in% edit_state$to_add$symbol) {
          showNotification(
            sprintf("%s is already a member of this group.", new_symbol),
            type = "warning",
            duration = 3
          )
          return()
        }

        # Add to pending additions
        edit_state$to_add <- bind_rows(
          edit_state$to_add,
          tibble::tibble(symbol = new_symbol, role = new_role)
        )

        # Clear dropdowns (using updateSelectInput would be ideal, but we'll let it refresh)
        showNotification(
          sprintf("Added %s as pending member.", new_symbol),
          type = "message",
          duration = 2
        )
      }, ignoreInit = TRUE)

      # Handle Save Changes
      observeEvent(input$save_edit_clicked, {
        save_group_id <- input$save_edit_clicked

        log_info("Saving edits for group: {save_group_id}")

        # Track if any changes were made
        changes_made <- FALSE

        # Update group name if changed
        new_name <- input$edit_group_name
        if (!is.null(new_name) && new_name != group_data$group_name) {
          result <- update_position_group(save_group_id, group_name = new_name)
          if (result) {
            log_info("Updated group name to: {new_name}")
            changes_made <- TRUE
          } else {
            showNotification(
              "Failed to update group name.",
              type = "error",
              duration = 5
            )
          }
        }

        # Remove members
        if (length(edit_state$to_remove) > 0) {
          for (symbol in edit_state$to_remove) {
            result <- remove_group_member(save_group_id, symbol)
            if (result) {
              log_info("Removed member: {symbol}")
              changes_made <- TRUE
            } else {
              showNotification(
                sprintf("Failed to remove member: %s", symbol),
                type = "error",
                duration = 5
              )
            }
          }
        }

        # Add members
        if (nrow(edit_state$to_add) > 0) {
          for (i in seq_len(nrow(edit_state$to_add))) {
            new_member <- edit_state$to_add[i, ]
            result <- add_group_member(
              group_id = save_group_id,
              account_number = group_data$account_number,
              symbol = new_member$symbol,
              role = new_member$role
            )
            if (result) {
              log_info("Added member: {new_member$symbol} as {new_member$role}")
              changes_made <- TRUE
            } else {
              showNotification(
                sprintf("Failed to add member: %s", new_member$symbol),
                type = "error",
                duration = 5
              )
            }
          }
        }

        # Show success notification
        if (changes_made) {
          showNotification(
            "Group updated successfully.",
            type = "message",
            duration = 5
          )

          # Force card re-render
          card_version(card_version() + 1)
        } else {
          showNotification(
            "No changes were made.",
            type = "message",
            duration = 3
          )
        }

        # Close modal
        removeModal()
      }, ignoreInit = TRUE)

    }, ignoreInit = TRUE)
  })
}

################################################################################
# HELPER FUNCTIONS FOR EDIT MEMBERS MODAL
################################################################################

#' Format role code to human-readable label
#'
#' @param role Character role code (e.g., "underlying_stock")
#' @return Character formatted label (e.g., "Underlying Stock")
#' @noRd
format_role_label <- function(role) {
  role_map <- c(
    "underlying_stock" = "Underlying Stock",
    "short_call" = "Short Call",
    "short_put" = "Short Put",
    "long_call" = "Long Call",
    "long_put" = "Long Put"
  )

  role_map[role] %||% tools::toTitleCase(gsub("_", " ", role))
}

#' Get available positions for adding to a group
#'
#' Filters positions to same account and excludes current members.
#'
#' @param group_account_number Character account number for the group
#' @param current_members Tibble with current member symbols
#' @param all_positions Tibble with all available positions
#' @return Tibble with available positions
#' @noRd
get_available_positions_for_group <- function(group_account_number, current_members, all_positions) {
  # Filter to same account
  available <- all_positions %>%
    filter(account_number == group_account_number)

  # Exclude current members
  if (nrow(current_members) > 0) {
    available <- available %>%
      anti_join(current_members, by = "symbol")
  }

  available %>%
    select(symbol, current_price) %>%
    arrange(symbol)
}

#' Render a single member row for the edit modal
#'
#' @param symbol Character position symbol
#' @param role Character role code
#' @param is_pending Logical, is this a pending addition?
#' @param is_removed Logical, is this marked for removal?
#' @param ns Namespace function
#' @return HTML div for member row
#' @noRd
render_member_row <- function(symbol, role, is_pending = FALSE, is_removed = FALSE, ns) {
  # Determine styling
  style <- if (is_removed) {
    "padding: 8px; margin-bottom: 5px; background: #f8d7da; border-left: 3px solid #dc3545; text-decoration: line-through; opacity: 0.6;"
  } else if (is_pending) {
    "padding: 8px; margin-bottom: 5px; background: #d1ecf1; border-left: 3px solid #0c5460;"
  } else {
    "padding: 8px; margin-bottom: 5px; background: #f8f9fa; border-left: 3px solid #6c757d;"
  }

  # Determine badge color
  badge_class <- if (is_removed) {
    "badge badge-danger"
  } else if (is_pending) {
    "badge badge-info"
  } else {
    "badge badge-secondary"
  }

  tags$div(
    style = style,
    tags$span(
      class = badge_class,
      style = "font-family: monospace; margin-right: 10px;",
      symbol
    ),
    tags$span(
      format_role_label(role),
      style = "color: #6c757d;"
    ),
    if (!is_removed) {
      tags$button(
        type = "button",
        class = "btn btn-xs btn-danger pull-right",
        style = "padding: 2px 6px; font-size: 11px;",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                         ns("remove_member_clicked"), symbol),
        icon("times")
      )
    },
    if (is_pending) {
      tags$span(
        class = "label label-info pull-right",
        style = "margin-right: 5px; font-size: 10px;",
        "PENDING"
      )
    }
  )
}

################################################################################
# P&L PREVIEW HELPER
################################################################################

#' Render P&L preview for close confirmation modal
#'
#' @param group_id Group identifier
#' @return HTML div with P&L metrics
#' @noRd
renderPnLPreview <- function(group_id) {
  # Calculate P&L
  pnl <- calculate_group_pnl(group_id)

  if (nrow(pnl) == 0) {
    return(tags$p(class = "text-muted", "Unable to calculate P&L preview"))
  }

  # Format preview
  tags$div(
    style = "background: #f8f9fa; padding: 10px; border-radius: 4px;",
    tags$table(
      class = "table table-condensed",
      style = "margin-bottom: 0;",
      tags$tr(
        tags$th("Metric"),
        tags$th("Value", style = "text-align: right;")
      ),
      tags$tr(
        tags$td("Net P&L"),
        tags$td(format_currency(pnl$net_pnl), style = "text-align: right;")
      ),
      tags$tr(
        tags$td("Total Return"),
        tags$td(format_percentage(pnl$total_return_pct / 100), style = "text-align: right;")
      ),
      tags$tr(
        tags$td("Annualized Return"),
        tags$td(format_percentage(pnl$annualized_return_pct / 100), style = "text-align: right;")
      ),
      tags$tr(
        tags$td("Hold Period"),
        tags$td(sprintf("%d days", pnl$hold_days), style = "text-align: right;")
      )
    )
  )
}
