#' Suggestions Modal Module
#'
#' Shiny module for displaying and managing grouping suggestions.
#' Shows pending suggestions in a modal dialog with approve/reject actions.
#'
#' @name mod_suggestions_modal
#' @import shiny
#' @import dplyr
#' @importFrom purrr map
#' @importFrom logger log_info log_warn log_debug
NULL

################################################################################
# UI
################################################################################

#' Suggestions Modal UI
#'
#' @param id Module ID
#' @return Shiny UI elements
#' @noRd
mod_suggestions_modal_ui <- function(id) {
  ns <- NS(id)

  # Modal is shown programmatically, no static UI needed
  tagList()
}

################################################################################
# SERVER
################################################################################

#' Suggestions Modal Server
#'
#' @param id Module ID
#' @param trigger Reactive trigger to show modal (e.g., when Review button clicked)
#' @return Reactive value indicating when suggestions were processed
#' @noRd
mod_suggestions_modal_server <- function(id, trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to track when suggestions are processed (for refreshing other modules)
    suggestions_processed <- reactiveVal(0)

    # Show modal when triggered (ignoreInit prevents auto-open on page load)
    observeEvent(trigger(), ignoreInit = TRUE, {
      # Get pending suggestions
      suggestions <- get_pending_suggestions()

      if (nrow(suggestions) == 0) {
        showNotification("No pending suggestions", type = "message")
        return()
      }

      log_info("Suggestions Modal: Showing {nrow(suggestions)} pending suggestions")

      # Show modal with suggestions
      showModal(modalDialog(
        title = h3(icon("lightbulb"), "Grouping Suggestions"),
        size = "l",
        easyClose = FALSE,

        # Suggestions list
        div(
          class = "suggestions-container",
          style = "max-height: 500px; overflow-y: auto;",

          map(seq_len(nrow(suggestions)), function(i) {
            suggestion <- suggestions[i, ]
            render_suggestion_card(suggestion, ns)
          })
        ),

        footer = tagList(
          actionButton(ns("close_modal"), "Close", class = "btn-secondary")
        )
      ))
    })

    # Close modal handler
    observeEvent(input$close_modal, {
      removeModal()
    })

    # Handle approve button clicks
    observeEvent(input$approve_suggestion, {
      suggestion_id <- input$approve_suggestion

      # Get suggestion details
      suggestion <- get_suggestion_by_id(suggestion_id)

      if (nrow(suggestion) == 0) {
        showNotification("Suggestion not found", type = "error")
        return()
      }

      # Create position group
      success <- approve_and_create_group(suggestion)

      if (success) {
        showNotification("Group created successfully", type = "message")
        suggestions_processed(suggestions_processed() + 1)

        # Refresh modal with updated suggestions
        trigger()
      } else {
        showNotification("Failed to create group", type = "error")
      }
    })

    # Handle reject button clicks
    observeEvent(input$reject_suggestion, {
      suggestion_id <- input$reject_suggestion

      success <- reject_suggestion(suggestion_id)

      if (success) {
        showNotification("Suggestion rejected", type = "message")
        suggestions_processed(suggestions_processed() + 1)

        # Refresh modal
        trigger()
      } else {
        showNotification("Failed to reject suggestion", type = "error")
      }
    })

    # Handle edit button clicks
    observeEvent(input$edit_suggestion, {
      suggestion_id <- input$edit_suggestion

      # Get suggestion details
      suggestion <- get_suggestion_by_id(suggestion_id)

      if (nrow(suggestion) == 0) {
        showNotification("Suggestion not found", type = "error")
        return()
      }

      # Show edit form modal
      show_edit_modal(suggestion, ns)
    })

    # Handle save edited group
    observeEvent(input$save_edited_group, {
      suggestion_id <- input$edit_suggestion_id
      new_name <- input$edit_group_name
      new_strategy <- input$edit_strategy_type

      # Get suggestion
      suggestion <- get_suggestion_by_id(suggestion_id)

      # Update suggestion with new name/strategy
      suggestion$suggested_group_name <- new_name
      suggestion$suggested_strategy_type <- new_strategy

      # Create group
      success <- approve_and_create_group(suggestion)

      if (success) {
        showNotification("Group created successfully", type = "message")
        suggestions_processed(suggestions_processed() + 1)
        removeModal()  # Close edit modal
        trigger()  # Refresh suggestions modal
      } else {
        showNotification("Failed to create group", type = "error")
      }
    })

    return(suggestions_processed)
  })
}

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Render a suggestion card
#'
#' @param suggestion Single suggestion row
#' @param ns Namespace function
#' @return Shiny tag with suggestion card
#' @noRd
render_suggestion_card <- function(suggestion, ns) {
  # Get activity details
  activity_ids <- suggestion$involved_activity_ids[[1]]
  activities <- get_activities() %>%
    filter(activity_id %in% activity_ids)

  # Different rendering based on pattern type
  if (suggestion$pattern_type == "ambiguous_match") {
    render_ambiguous_card(suggestion, activities, ns)
  } else if (suggestion$pattern_type == "late_dividend") {
    render_late_dividend_card(suggestion, activities, ns)
  } else {
    render_standard_card(suggestion, activities, ns)
  }
}

#' Render standard suggestion card
#'
#' @param suggestion Suggestion row
#' @param activities Related activities
#' @param ns Namespace function
#' @return Shiny tag
#' @noRd
render_standard_card <- function(suggestion, activities, ns) {
  div(
    class = "suggestion-card",
    style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 15px; border-radius: 5px; background-color: #f9f9f9;",

    # Header
    div(
      style = "margin-bottom: 10px;",
      h4(
        style = "margin: 0;",
        icon("check-circle", class = "text-success"),
        suggestion$suggested_group_name
      ),
      div(
        style = "font-size: 0.9em; color: #666;",
        paste("Strategy:", suggestion$suggested_strategy_type)
      )
    ),

    # Reasoning
    div(
      style = "margin-bottom: 10px; padding: 10px; background-color: white; border-left: 3px solid #007bff;",
      suggestion$reasoning
    ),

    # Transaction details
    div(
      style = "margin-bottom: 10px;",
      h5("Transactions:", style = "margin-top: 0;"),
      tags$ul(
        style = "margin: 0; padding-left: 20px;",
        map(seq_len(nrow(activities)), function(i) {
          act <- activities[i, ]
          tags$li(
            sprintf("%s: %s %s - %g shares @ $%.2f = $%.2f",
                   format(as.Date(act$trade_date), "%b %d"),
                   act$action,
                   act$symbol,
                   act$quantity,
                   act$price,
                   act$net_amount)
          )
        })
      )
    ),

    # Action buttons
    div(
      style = "text-align: right;",
      actionButton(
        inputId = paste0("approve_", suggestion$suggestion_id),
        label = "Approve",
        class = "btn-success btn-sm",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                         ns("approve_suggestion"), suggestion$suggestion_id)
      ),
      actionButton(
        inputId = paste0("edit_", suggestion$suggestion_id),
        label = "Edit",
        class = "btn-warning btn-sm",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                         ns("edit_suggestion"), suggestion$suggestion_id)
      ),
      actionButton(
        inputId = paste0("reject_", suggestion$suggestion_id),
        label = "Reject",
        class = "btn-danger btn-sm",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                         ns("reject_suggestion"), suggestion$suggestion_id)
      )
    )
  )
}

#' Render ambiguous match card
#'
#' @param suggestion Suggestion row
#' @param activities Related activities
#' @param ns Namespace function
#' @return Shiny tag
#' @noRd
render_ambiguous_card <- function(suggestion, activities, ns) {
  div(
    class = "suggestion-card-ambiguous",
    style = "border: 2px solid #ff9800; padding: 15px; margin-bottom: 15px; border-radius: 5px; background-color: #fff3e0;",

    h4(
      icon("question-circle", class = "text-warning"),
      "Needs Clarification"
    ),

    p(suggestion$reasoning),

    # Show all transactions with checkboxes
    p(strong("Select transactions to group together:")),

    # Note: Full ambiguous handling would need more complex UI
    # For now, show message
    p(
      style = "font-style: italic; color: #666;",
      "Please reject this suggestion and manually create groups for these transactions."
    ),

    div(
      style = "text-align: right;",
      actionButton(
        inputId = paste0("reject_", suggestion$suggestion_id),
        label = "Reject",
        class = "btn-danger btn-sm",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                         ns("reject_suggestion"), suggestion$suggestion_id)
      )
    )
  )
}

#' Render late dividend card
#'
#' @param suggestion Suggestion row
#' @param activities Related activities
#' @param ns Namespace function
#' @return Shiny tag
#' @noRd
render_late_dividend_card <- function(suggestion, activities, ns) {
  div(
    class = "suggestion-card-dividend",
    style = "border: 1px solid #28a745; padding: 15px; margin-bottom: 15px; border-radius: 5px; background-color: #d4edda;",

    h4(
      icon("dollar-sign", class = "text-success"),
      "Dividend Received"
    ),

    p(suggestion$reasoning),

    div(
      style = "text-align: right;",
      actionButton(
        inputId = paste0("approve_", suggestion$suggestion_id),
        label = "Link to Group",
        class = "btn-success btn-sm",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                         ns("approve_suggestion"), suggestion$suggestion_id)
      ),
      actionButton(
        inputId = paste0("reject_", suggestion$suggestion_id),
        label = "Ignore",
        class = "btn-secondary btn-sm",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                         ns("reject_suggestion"), suggestion$suggestion_id)
      )
    )
  )
}

#' Show edit modal for suggestion
#'
#' @param suggestion Suggestion row
#' @param ns Namespace function
#' @noRd
show_edit_modal <- function(suggestion, ns) {
  showModal(modalDialog(
    title = "Edit Group Details",
    size = "m",

    textInput(
      inputId = ns("edit_group_name"),
      label = "Group Name",
      value = suggestion$suggested_group_name
    ),

    selectInput(
      inputId = ns("edit_strategy_type"),
      label = "Strategy Type",
      choices = get_strategy_types(),
      selected = suggestion$suggested_strategy_type
    ),

    # Hidden input to store suggestion ID
    tags$input(
      type = "hidden",
      id = ns("edit_suggestion_id"),
      value = suggestion$suggestion_id
    ),

    footer = tagList(
      actionButton(ns("save_edited_group"), "Create Group", class = "btn-primary"),
      modalButton("Cancel")
    )
  ))
}

#' Approve suggestion and create group
#'
#' @param suggestion Suggestion row
#' @return Logical TRUE if successful
#' @noRd
approve_and_create_group <- function(suggestion) {
  tryCatch({
    # Handle different pattern types
    if (suggestion$pattern_type == "late_dividend") {
      # Link dividend to existing group
      activity_ids <- suggestion$involved_activity_ids[[1]]
      group_id <- suggestion$suggested_group_name  # Contains group ID for late dividends

      for (activity_id in activity_ids) {
        link_activity_to_group(activity_id, group_id)
      }

      # Recalculate P&L for the group
      update_group_pnl(group_id)

      # Approve suggestion
      approve_suggestion(suggestion$suggestion_id, group_id)

      return(TRUE)
    }

    # For other patterns, create new group
    # Generate group ID
    group_id <- generate_group_id(
      suggestion$suggested_strategy_type,
      "AUTO"  # Account will be determined from activities
    )

    # Get activities
    activity_ids <- suggestion$involved_activity_ids[[1]]
    activities <- get_activities() %>%
      filter(activity_id %in% activity_ids)

    if (nrow(activities) == 0) {
      log_warn("Approve Suggestion: No activities found for suggestion {suggestion$suggestion_id}")
      return(FALSE)
    }

    # Get account number from first activity
    account_number <- activities$account_number[1]

    # Assign roles based on activity types (vectorized)
    members <- activities %>%
      mutate(
        role = case_when(
          is_stock_purchase(.) ~ "underlying_stock",  # Now vectorized - returns logical vector
          is_option_sale(.) ~ "short_call",
          TRUE ~ "other"
        )
      ) %>%
      select(symbol, role)

    # Create position group
    success <- create_position_group(
      group_id = group_id,
      group_name = suggestion$suggested_group_name,
      strategy_type = suggestion$suggested_strategy_type,
      account_number = account_number,
      members = members
    )

    if (!success) {
      log_warn("Approve Suggestion: Failed to create group for suggestion {suggestion$suggestion_id}")
      return(FALSE)
    }

    # Link activities to group
    for (activity_id in activity_ids) {
      link_activity_to_group(activity_id, group_id)
    }

    # Approve suggestion
    approve_suggestion(suggestion$suggestion_id, group_id)

    # Generate income projections (reuse existing function)
    generate_initial_projections(group_id, members, account_number)

    log_info("Approve Suggestion: Created group {group_id} from suggestion {suggestion$suggestion_id}")

    return(TRUE)

  }, error = function(e) {
    log_error("Approve Suggestion: Failed - {e$message}")
    return(FALSE)
  })
}
