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
#' @importFrom shiny moduleServer renderUI req tags observeEvent showModal modalDialog removeModal showNotification
#' @importFrom dplyr %>% arrange desc filter left_join
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
        create_group_card(
          group_id = gid,
          group_data = group,
          members = group_members,
          activities = group_activities,
          cash_flows = group_cash_flows,
          metrics = group_metrics
        )
      })

      # Return cards container
      tags$div(
        class = "opportunity-cards-container",
        style = "margin-top: 20px;",
        cards
      )
    })

    # Handle Close Group button clicks
    # Use observeEvent with a pattern to catch all close buttons
    observeEvent(input, {
      # Get all input IDs that match close_group_*
      input_ids <- names(input)
      close_button_ids <- grep("^close_group_", input_ids, value = TRUE)

      for (button_id in close_button_ids) {
        if (!is.null(input[[button_id]]) && input[[button_id]] > 0) {
          # Extract group_id from button ID
          group_id <- sub("^close_group_", "", button_id)

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
              actionButton(
                ns(sprintf("confirm_close_%s", group_id)),
                "Confirm Close",
                class = "btn-warning",
                icon = icon("check")
              )
            ),
            easyClose = TRUE
          ))
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle confirmation of close action
    observeEvent(input, {
      # Get all input IDs that match confirm_close_*
      input_ids <- names(input)
      confirm_ids <- grep("^confirm_close_", input_ids, value = TRUE)

      for (confirm_id in confirm_ids) {
        if (!is.null(input[[confirm_id]]) && input[[confirm_id]] > 0) {
          # Extract group_id
          group_id <- sub("^confirm_close_", "", confirm_id)

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
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle Reopen Group button clicks
    observeEvent(input, {
      input_ids <- names(input)
      reopen_button_ids <- grep("^reopen_group_", input_ids, value = TRUE)

      for (button_id in reopen_button_ids) {
        if (!is.null(input[[button_id]]) && input[[button_id]] > 0) {
          group_id <- sub("^reopen_group_", "", button_id)

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
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle Edit Members button clicks
    observeEvent(input, {
      input_ids <- names(input)
      edit_button_ids <- grep("^edit_group_", input_ids, value = TRUE)

      for (button_id in edit_button_ids) {
        if (!is.null(input[[button_id]]) && input[[button_id]] > 0) {
          group_id <- sub("^edit_group_", "", button_id)

          # Show info notification (feature not implemented yet)
          showNotification(
            "Edit Members feature coming soon!",
            type = "message",
            duration = 3
          )
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })
}

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
        tags$td(format_percentage(pnl$total_return_pct), style = "text-align: right;")
      ),
      tags$tr(
        tags$td("Annualized Return"),
        tags$td(format_percentage(pnl$annualized_return_pct), style = "text-align: right;")
      ),
      tags$tr(
        tags$td("Hold Period"),
        tags$td(sprintf("%d days", pnl$hold_days), style = "text-align: right;")
      )
    )
  )
}
