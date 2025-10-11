#' Position Group Card UI Components
#'
#' Pure functions for generating position group cards. Follows the pattern
#' established in mod_aristocrats_results_table.R with create_opportunity_card().
#' These are pure functions that take data and return bslib cards.
#'
#' @name group-cards
#' @importFrom shiny tags div icon
#' @importFrom bslib card card_header card_body
#' @importFrom dplyr %>% filter select mutate
#' @importFrom purrr map
NULL

################################################################################
# FORMATTING HELPERS
################################################################################

#' Format currency for display
#' @param value Numeric value
#' @return Character formatted as currency
#' @noRd
format_currency <- function(value) {
  if (is.na(value) || is.null(value)) return("$0.00")
  sprintf("$%s", format(round(value, 2), big.mark = ",", nsmall = 2))
}

#' Format percentage for display
#' @param value Numeric percentage value
#' @return Character formatted as percentage
#' @noRd
format_percentage <- function(value) {
  if (is.na(value) || is.null(value)) return("0.0%")
  sprintf("%.1f%%", value)
}

################################################################################
# CARD COMPONENTS
################################################################################

#' Create cash flow section accordion
#'
#' Displays projected and actual cash flow events in separate subsections.
#'
#' @param cash_flows Tibble from get_group_cash_flows()
#' @param is_open Logical, is this an open group (shows projected) or closed (actual only)
#' @return HTML accordion section
#' @noRd
create_cash_flow_section <- function(cash_flows, is_open = TRUE) {
  if (is.null(cash_flows) || nrow(cash_flows) == 0) {
    return(create_accordion_section(
      title = "Cash Flows",
      is_open = FALSE,
      tags$p(class = "text-muted", "No cash flow events recorded")
    ))
  }

  # Split into projected and actual
  projected <- cash_flows %>% filter(status == "projected")
  actual <- cash_flows %>% filter(status == "actual")

  # Build content
  content <- list()

  # Projected events (only for open groups)
  if (is_open && nrow(projected) > 0) {
    projected_total <- sum(projected$amount, na.rm = TRUE)

    content <- c(content, list(
      tags$h5(
        sprintf("Projected Events (%d upcoming)", nrow(projected)),
        tags$span(class = "pull-right", format_currency(projected_total))
      ),
      tags$hr()
    ))

    # Add each projected event
    for (i in seq_len(nrow(projected))) {
      event <- projected[i, ]
      content <- c(content, list(
        create_metric_row(
          label = sprintf("%s  |  %s", event$event_date, event$event_type),
          value = format_currency(event$amount)
        )
      ))
    }

    content <- c(content, list(tags$br()))
  }

  # Actual events
  if (nrow(actual) > 0) {
    actual_total <- sum(actual$amount, na.rm = TRUE)

    content <- c(content, list(
      tags$h5(
        sprintf("Actual Events (%d realized)", nrow(actual)),
        tags$span(class = "pull-right", format_currency(actual_total))
      ),
      tags$hr()
    ))

    # Add each actual event
    for (i in seq_len(nrow(actual))) {
      event <- actual[i, ]
      content <- c(content, list(
        create_metric_row(
          label = sprintf("%s  |  %s", event$event_date, event$event_type),
          value = format_currency(event$amount)
        )
      ))
    }
  }

  create_accordion_section(
    title = "Cash Flows",
    is_open = FALSE,
    content
  )
}

#' Create transaction history section accordion
#'
#' Displays all activities for the group in chronological order.
#'
#' @param activities Tibble from get_activities_by_group()
#' @return HTML accordion section
#' @noRd
create_transaction_history_section <- function(activities) {
  if (is.null(activities) || nrow(activities) == 0) {
    return(create_accordion_section(
      title = "Transaction History",
      is_open = FALSE,
      tags$p(class = "text-muted", "No transactions recorded")
    ))
  }

  # Sort by trade date
  activities <- activities %>%
    arrange(trade_date)

  # Build transaction rows
  transaction_rows <- purrr::map(seq_len(nrow(activities)), function(i) {
    activity <- activities[i, ]

    # Format action and symbol
    action_str <- sprintf("%s %s", activity$action, activity$type)
    symbol_str <- activity$symbol

    # Format quantity and price
    qty_price <- if (!is.na(activity$quantity) && !is.na(activity$price)) {
      sprintf("%s @ %s",
              format(abs(activity$quantity), big.mark = ","),
              format_currency(activity$price))
    } else {
      ""
    }

    # Format net amount
    net_amount <- format_currency(activity$net_amount)

    create_metric_row(
      label = sprintf("%s  |  %s  |  %s  |  %s",
                     as.character(activity$trade_date),
                     action_str,
                     symbol_str,
                     qty_price),
      value = net_amount
    )
  })

  create_accordion_section(
    title = "Transaction History",
    is_open = FALSE,
    transaction_rows
  )
}

#' Create position details section
#'
#' Lists all members of the group with their roles and quantities.
#'
#' @param members Tibble from get_group_members()
#' @return HTML accordion section
#' @noRd
create_position_details_section <- function(members) {
  if (is.null(members) || nrow(members) == 0) {
    return(create_accordion_section(
      title = "Position Details",
      is_open = TRUE,
      tags$p(class = "text-muted", "No members in this group")
    ))
  }

  # Build member rows
  member_rows <- purrr::map(seq_len(nrow(members)), function(i) {
    member <- members[i, ]

    # Format role
    role_display <- switch(member$role,
      "underlying_stock" = "Stock",
      "short_call" = "Short Call",
      "long_put" = "Long Put",
      "short_put" = "Short Put",
      member$role  # fallback
    )

    # Format allocated quantity if present
    qty_str <- if (!is.null(member$allocated_quantity) && !is.na(member$allocated_quantity)) {
      sprintf(" (%s allocated)", format(member$allocated_quantity, big.mark = ","))
    } else {
      ""
    }

    create_metric_row(
      label = sprintf("%s (%s)%s", member$symbol, role_display, qty_str),
      value = ""
    )
  })

  create_accordion_section(
    title = "Position Details",
    is_open = TRUE,
    tags$p(tags$strong("Members in this group:")),
    member_rows
  )
}

################################################################################
# MAIN CARD CONSTRUCTORS
################################################################################

#' Create card for an open position group
#'
#' @param group_data Single-row tibble from position_groups table
#' @param metrics Tibble from calculate_open_group_metrics()
#' @param members Tibble from get_group_members()
#' @param cash_flows Tibble from get_group_cash_flows()
#' @param activities Tibble from get_activities_by_group()
#' @return bslib card component
#' @noRd
create_open_group_card <- function(group_data, metrics, members, cash_flows, activities) {
  # Card header
  header_primary <- sprintf("%s", group_data$group_name)
  header_secondary <- sprintf("%s | %d days held | %s projected",
                              group_data$strategy_type,
                              if (nrow(metrics) > 0) metrics$days_held else 0,
                              if (nrow(metrics) > 0) format_percentage(metrics$projected_annualized_return_pct) else "0%")

  header <- create_generic_card_header(
    primary_text = tags$div(
      header_primary,
      tags$span(
        class = "badge badge-success pull-right",
        style = "font-size: 14px; margin-left: 10px;",
        icon("check-circle"),
        " OPEN"
      )
    ),
    secondary_text = header_secondary
  )

  # Quick Overview section
  quick_overview <- if (nrow(metrics) > 0) {
    create_accordion_section(
      title = "Quick Overview",
      is_open = TRUE,
      create_metric_row("Days in Position", as.character(metrics$days_held)),
      create_metric_row("Capital Deployed", format_currency(metrics$cost_basis), is_primary = TRUE),
      create_metric_row("Cash Collected", sprintf("%s (%.1f%% recovered)",
                                                   format_currency(metrics$cash_collected),
                                                   metrics$pct_recovered)),
      create_metric_row("Projected Income", format_currency(metrics$projected_income)),
      create_metric_row("Target Total Return", sprintf("%s (%s)",
                                                       format_currency(metrics$target_total_return),
                                                       format_percentage((metrics$target_total_return / metrics$cost_basis) * 100)),
                       is_primary = TRUE)
    )
  } else {
    create_accordion_section(
      title = "Quick Overview",
      is_open = TRUE,
      tags$p(class = "text-muted", "No metrics available - no activities recorded")
    )
  }

  # Position details
  position_details <- create_position_details_section(members)

  # Cash flows
  cash_flow_section <- create_cash_flow_section(cash_flows, is_open = TRUE)

  # Transaction history
  transaction_section <- create_transaction_history_section(activities)

  # Action buttons
  action_buttons <- tags$div(
    class = "card-actions",
    style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #ddd;",
    tags$button(
      type = "button",
      class = "btn btn-sm btn-warning",
      id = sprintf("close_group_%s", group_data$group_id),
      `data-group-id` = group_data$group_id,
      icon("times-circle"),
      " Close Group"
    ),
    tags$button(
      type = "button",
      class = "btn btn-sm btn-default",
      style = "margin-left: 10px;",
      id = sprintf("edit_group_%s", group_data$group_id),
      `data-group-id` = group_data$group_id,
      icon("edit"),
      " Edit Members"
    )
  )

  # Assemble card body
  body <- bslib::card_body(
    quick_overview,
    position_details,
    cash_flow_section,
    transaction_section,
    action_buttons
  )

  # Return card
  create_standard_card(header, body)
}

#' Create card for a closed position group
#'
#' @param group_data Single-row tibble from position_groups table
#' @param pnl Tibble from calculate_group_pnl()
#' @param members Tibble from get_group_members()
#' @param cash_flows Tibble from get_group_cash_flows()
#' @param activities Tibble from get_activities_by_group()
#' @return bslib card component
#' @noRd
create_closed_group_card <- function(group_data, pnl, members, cash_flows, activities) {
  # Card header
  header_primary <- sprintf("%s", group_data$group_name)
  header_secondary <- sprintf("%s | %d days | %s annualized",
                              group_data$strategy_type,
                              if (nrow(pnl) > 0) pnl$hold_days else 0,
                              if (nrow(pnl) > 0) format_percentage(pnl$annualized_return_pct) else "0%")

  header <- create_generic_card_header(
    primary_text = tags$div(
      header_primary,
      tags$span(
        class = "badge badge-info pull-right",
        style = "font-size: 14px; margin-left: 10px;",
        icon("check"),
        " CLOSED"
      )
    ),
    secondary_text = header_secondary
  )

  # Quick Overview section
  quick_overview <- if (nrow(pnl) > 0) {
    create_accordion_section(
      title = "Quick Overview",
      is_open = TRUE,
      create_metric_row("Annualized Return", format_percentage(pnl$annualized_return_pct), is_primary = TRUE),
      create_metric_row("Net P&L", sprintf("%s (%s)",
                                           format_currency(pnl$net_pnl),
                                           format_percentage(pnl$total_return_pct))),
      create_metric_row("Hold Period", sprintf("%d days", pnl$hold_days)),
      create_metric_row("Strategy", group_data$strategy_type)
    )
  } else {
    create_accordion_section(
      title = "Quick Overview",
      is_open = TRUE,
      tags$p(class = "text-muted", "No P&L data available")
    )
  }

  # P&L Breakdown section
  pnl_breakdown <- if (nrow(pnl) > 0) {
    create_accordion_section(
      title = "P&L Breakdown",
      is_open = TRUE,
      tags$h5("Costs"),
      create_metric_row("Stock Purchases", format_currency(pnl$stock_purchases)),
      create_metric_row("Commissions", format_currency(pnl$total_commissions)),
      create_metric_row("Total Cost", format_currency(pnl$total_cost), is_primary = TRUE),
      tags$br(),
      tags$h5("Proceeds"),
      create_metric_row("Stock Sales", format_currency(pnl$stock_sales)),
      create_metric_row("Option Premiums", format_currency(pnl$option_premiums)),
      create_metric_row("Dividends", format_currency(pnl$total_dividends)),
      create_metric_row("Total Proceeds", format_currency(pnl$total_proceeds), is_primary = TRUE),
      tags$br(),
      tags$h5("Results"),
      create_metric_row("Net P&L", format_currency(pnl$net_pnl), is_primary = TRUE),
      create_metric_row("Total Return", format_percentage(pnl$total_return_pct)),
      create_metric_row("Annualized Return", format_percentage(pnl$annualized_return_pct), is_primary = TRUE)
    )
  } else {
    create_accordion_section(
      title = "P&L Breakdown",
      is_open = TRUE,
      tags$p(class = "text-muted", "No P&L breakdown available")
    )
  }

  # Position details
  position_details <- create_position_details_section(members)

  # Cash flows (actual only for closed groups)
  cash_flow_section <- create_cash_flow_section(cash_flows, is_open = FALSE)

  # Transaction history
  transaction_section <- create_transaction_history_section(activities)

  # Action buttons
  action_buttons <- tags$div(
    class = "card-actions",
    style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #ddd;",
    tags$button(
      type = "button",
      class = "btn btn-sm btn-success",
      id = sprintf("reopen_group_%s", group_data$group_id),
      `data-group-id` = group_data$group_id,
      icon("undo"),
      " Reopen Group"
    )
  )

  # Assemble card body
  body <- bslib::card_body(
    quick_overview,
    pnl_breakdown,
    position_details,
    cash_flow_section,
    transaction_section,
    action_buttons
  )

  # Return card
  create_standard_card(header, body)
}

################################################################################
# PUBLIC API
################################################################################

#' Create a position group card (public wrapper)
#'
#' Determines group status and routes to appropriate card constructor.
#' Accepts optional pre-fetched data for performance optimization. If data is
#' not provided, fetches it individually (for standalone use).
#'
#' @param group_id Group identifier
#' @param group_data Optional pre-fetched group data (from get_group_by_id)
#' @param members Optional pre-fetched members (from get_group_members)
#' @param activities Optional pre-fetched activities (from get_activities_by_group)
#' @param cash_flows Optional pre-fetched cash flows (from get_group_cash_flows)
#' @param metrics Optional pre-calculated metrics (from calculate_dashboard_metrics)
#' @return bslib card component or NULL if group not found
#' @export
create_group_card <- function(group_id, group_data = NULL, members = NULL,
                               activities = NULL, cash_flows = NULL, metrics = NULL) {
  # Fetch data if not provided (allows function to work standalone)
  if (is.null(group_data)) {
    group_data <- get_group_by_id(group_id)
  }

  if (nrow(group_data) == 0) {
    log_warn("Group Card: Group {group_id} not found")
    return(NULL)
  }

  if (is.null(members)) {
    members <- get_group_members(group_id)
  }

  if (is.null(cash_flows)) {
    cash_flows <- get_group_cash_flows(group_id)
  }

  if (is.null(activities)) {
    activities <- get_activities_by_group(group_id)
  }

  # Route based on status
  if (group_data$status == "closed") {
    # For closed groups: always calculate full P&L breakdown
    # Closed groups are few (typically <10) and data doesn't change,
    # so the performance impact is negligible
    pnl <- calculate_group_pnl(group_id)

    create_closed_group_card(
      group_data = group_data,
      pnl = pnl,
      members = members,
      cash_flows = cash_flows,
      activities = activities
    )
  } else {
    # Get metrics (use pre-calculated if available, otherwise calculate)
    if (!is.null(metrics) && nrow(metrics) > 0) {
      # Metrics already calculated - reuse them! (DRY)
      open_metrics <- metrics
    } else {
      # Fallback: calculate if not provided
      open_metrics <- calculate_open_group_metrics(group_id)
    }

    create_open_group_card(
      group_data = group_data,
      metrics = open_metrics,
      members = members,
      cash_flows = cash_flows,
      activities = activities
    )
  }
}
