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

#' Format days to expiration as countdown
#'
#' Displays days remaining in human-readable format
#'
#' @param days Numeric days until expiration
#' @param expiration_date Date object (optional, for display)
#' @return Character formatted countdown
#' @noRd
format_days_to_expiration <- function(days, expiration_date = NULL) {
  if (is.null(days) || is.na(days)) {
    return("N/A")
  }

  days_text <- if (days == 0) {
    "today"
  } else if (days == 1) {
    "1 day"
  } else if (days < 0) {
    sprintf("%d days ago", abs(days))
  } else {
    sprintf("%d days", days)
  }

  if (!is.null(expiration_date) && !is.na(expiration_date)) {
    sprintf("%s (%s)", days_text, format(expiration_date, "%b %d, %Y"))
  } else {
    days_text
  }
}

#' Format strike price relationship
#'
#' Shows ITM/OTM status for options (e.g., "in-the-money by $5.45")
#'
#' @param current_price Numeric current stock price
#' @param strike_price Numeric option strike price
#' @param option_type Character "call" or "put" (default "call")
#' @return Character formatted ITM/OTM status
#' @noRd
format_strike_relationship <- function(current_price, strike_price, option_type = "call") {
  if (is.null(current_price) || is.na(current_price) ||
      is.null(strike_price) || is.na(strike_price)) {
    return("")
  }

  diff <- current_price - strike_price

  # For calls: ITM if stock > strike
  # For puts: ITM if stock < strike
  is_itm <- if (option_type == "call") {
    diff > 0
  } else {
    diff < 0
  }

  status <- if (is_itm) "in-the-money" else "out-of-the-money"

  sprintf("(%s by %s)", status, format_currency(abs(diff)))
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

    # Add each projected event with improved labeling
    for (i in seq_len(nrow(projected))) {
      event <- projected[i, ]

      # Format event type for better readability
      event_label <- switch(event$event_type,
        "option_gain" = "Profit at Expiration",
        "dividend" = "Dividend Payment",
        tools::toTitleCase(gsub("_", " ", event$event_type))  # fallback
      )

      content <- c(content, list(
        create_metric_row(
          label = sprintf("%s  |  %s", event$event_date, event_label),
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

    # Add each actual event with improved labeling
    for (i in seq_len(nrow(actual))) {
      event <- actual[i, ]

      # Format event type for better readability
      event_label <- switch(event$event_type,
        "option_gain" = "Profit at Expiration",
        "dividend" = "Dividend Payment",
        tools::toTitleCase(gsub("_", " ", event$event_type))  # fallback
      )

      content <- c(content, list(
        create_metric_row(
          label = sprintf("%s  |  %s", event$event_date, event_label),
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
#' @param latest_positions Tibble from get_latest_positions() (optional)
#' @return bslib card component
#' @noRd
create_open_group_card <- function(group_data, metrics, members, cash_flows, activities, latest_positions = NULL) {
  # Enrich with market data
  market_data <- enrich_group_with_market_data(group_data$group_id, members, activities, latest_positions)

  # Extract ticker from group name (e.g., "CZR OPEN" -> "CZR")
  ticker <- strsplit(group_data$group_name, " ")[[1]][1]

  # Build header with current price if available
  header_primary <- if (!is.null(market_data$current_stock_price)) {
    sprintf("%s %s", ticker, format_currency(market_data$current_stock_price))
  } else {
    ticker
  }

  # Build secondary line: strategy + days to expiration + annualized return
  header_secondary_parts <- c(group_data$strategy_type)

  if (!is.null(market_data$days_to_expiration) && !is.na(market_data$days_to_expiration)) {
    expiration_text <- format_days_to_expiration(
      market_data$days_to_expiration,
      market_data$expiration_date
    )
    header_secondary_parts <- c(header_secondary_parts, sprintf("%s to expiration", expiration_text))
  } else if (nrow(metrics) > 0) {
    # Fallback to days held if no expiration
    header_secondary_parts <- c(header_secondary_parts, sprintf("%d days held", metrics$days_held))
  }

  # Add annualized return if available
  if (nrow(metrics) > 0 && !is.na(metrics$projected_annualized_return_pct)) {
    header_secondary_parts <- c(
      header_secondary_parts,
      sprintf("%s annualized", format_percentage(metrics$projected_annualized_return_pct))
    )
  }

  header_secondary <- paste(header_secondary_parts, collapse = " | ")

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

  # Quick Overview section with new structure
  quick_overview <- if (nrow(metrics) > 0) {
    # Build Position Status subsection
    position_status_rows <- list(
      tags$h5("Position Status", style = "margin-top: 10px; margin-bottom: 10px;")
    )

    # Current stock price
    if (!is.null(market_data$current_stock_price)) {
      price_display <- format_currency(market_data$current_stock_price)
      position_status_rows <- c(position_status_rows, list(
        create_metric_row("Current Stock Price", price_display)
      ))
    }

    # Strike price with ITM/OTM status
    if (!is.null(market_data$strike_price)) {
      strike_display <- format_currency(market_data$strike_price)
      if (!is.null(market_data$itm_otm_amount)) {
        itm_str <- format_strike_relationship(
          market_data$current_stock_price,
          market_data$strike_price,
          "call"  # Assume short call for now
        )
        strike_display <- sprintf("%s %s", strike_display, itm_str)
      }
      position_status_rows <- c(position_status_rows, list(
        create_metric_row("Strike Price", strike_display)
      ))
    }

    # Days to expiration
    if (!is.null(market_data$days_to_expiration)) {
      expiration_display <- format_days_to_expiration(
        market_data$days_to_expiration,
        market_data$expiration_date
      )
      position_status_rows <- c(position_status_rows, list(
        create_metric_row("Days to Expiration", expiration_display)
      ))
    }

    # Build Capital & Returns subsection
    capital_rows <- list(
      tags$h5("Capital & Returns", style = "margin-top: 15px; margin-bottom: 10px;")
    )

    # Get premium collected from activities for display
    option_premium_collected <- activities %>%
      filter(
        type == "Trades",
        action == "Sell",
        purrr::map_lgl(symbol, is_option_symbol)
      ) %>%
      summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
      pull(total)
    option_premium_collected <- if (length(option_premium_collected) == 0) 0 else option_premium_collected

    # Capital deployed with context
    capital_display <- if (option_premium_collected > 0 && group_data$strategy_type != "Other") {
      sprintf("%s (net after %s premium collected)",
              format_currency(metrics$cost_basis),
              format_currency(option_premium_collected))
    } else {
      format_currency(metrics$cost_basis)
    }

    capital_rows <- c(capital_rows, list(
      create_metric_row("Capital Deployed", capital_display, is_primary = TRUE)
    ))

    # Expected profit with clear labeling
    if (metrics$projected_income > 0) {
      return_pct <- (metrics$projected_income / metrics$cost_basis) * 100
      annualized_pct <- metrics$projected_annualized_return_pct

      profit_display <- sprintf("%s (%s return | %s annualized)",
                               format_currency(metrics$projected_income),
                               format_percentage(return_pct),
                               format_percentage(annualized_pct))

      capital_rows <- c(capital_rows, list(
        create_metric_row("Expected Profit", profit_display, is_primary = TRUE)
      ))
    }

    # Build Income Tracking subsection
    income_rows <- list(
      tags$h5("Income Tracking", style = "margin-top: 15px; margin-bottom: 10px;")
    )

    # Dividends collected
    dividend_display <- if (metrics$cash_collected > 0) {
      sprintf("%s collected", format_currency(metrics$cash_collected))
    } else {
      # Check if this is a zero-dividend stock
      if (grepl("Zero-Dividend", group_data$strategy_type, ignore.case = TRUE)) {
        "$0.00 (Zero-Dividend Stock)"
      } else {
        "$0.00 (no dividends yet)"
      }
    }

    income_rows <- c(income_rows, list(
      create_metric_row("Dividends Collected", dividend_display)
    ))

    # Combine all subsections
    create_accordion_section(
      title = "Quick Overview",
      is_open = TRUE,
      position_status_rows,
      capital_rows,
      income_rows
    )
  } else {
    create_accordion_section(
      title = "Quick Overview",
      is_open = TRUE,
      tags$p(class = "text-muted", "No metrics available - no activities recorded")
    )
  }

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
    # Check strategy type for display formatting
    strategy_type <- group_data$strategy_type

    # Build costs section based on strategy type
    if (strategy_type != "Other" && pnl$option_premiums > 0) {
      # For covered call strategies: show premiums as cost reduction
      costs_section <- list(
        tags$h5("Costs"),
        create_metric_row("Stock Purchases", format_currency(pnl$stock_purchases)),
        create_metric_row("Option Premiums (Credit)",
                         sprintf("-%s", format_currency(pnl$option_premiums))),
        create_metric_row("Commissions", format_currency(pnl$total_commissions)),
        create_metric_row("Net Cost Basis", format_currency(pnl$total_cost), is_primary = TRUE)
      )

      # Proceeds section without premiums
      proceeds_section <- list(
        tags$h5("Proceeds"),
        create_metric_row("Stock Sales", format_currency(pnl$stock_sales)),
        create_metric_row("Dividends", format_currency(pnl$total_dividends)),
        create_metric_row("Total Proceeds", format_currency(pnl$total_proceeds), is_primary = TRUE)
      )
    } else {
      # For "Other" strategy: legacy display (premiums as proceeds)
      costs_section <- list(
        tags$h5("Costs"),
        create_metric_row("Stock Purchases", format_currency(pnl$stock_purchases)),
        create_metric_row("Commissions", format_currency(pnl$total_commissions)),
        create_metric_row("Total Cost", format_currency(pnl$total_cost), is_primary = TRUE)
      )

      proceeds_section <- list(
        tags$h5("Proceeds"),
        create_metric_row("Stock Sales", format_currency(pnl$stock_sales)),
        create_metric_row("Option Premiums", format_currency(pnl$option_premiums)),
        create_metric_row("Dividends", format_currency(pnl$total_dividends)),
        create_metric_row("Total Proceeds", format_currency(pnl$total_proceeds), is_primary = TRUE)
      )
    }

    # Combine all sections
    create_accordion_section(
      title = "P&L Breakdown",
      is_open = TRUE,
      costs_section,
      tags$br(),
      proceeds_section,
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
#' @param latest_positions Optional pre-fetched latest positions (from get_latest_positions)
#' @return bslib card component or NULL if group not found
#' @export
create_group_card <- function(group_id, group_data = NULL, members = NULL,
                               activities = NULL, cash_flows = NULL, metrics = NULL,
                               latest_positions = NULL) {
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
      activities = activities,
      latest_positions = latest_positions
    )
  }
}
