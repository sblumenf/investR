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
# NOTE: Formatting functions (format_currency, format_percentage, etc.) are
# imported from utils_formatting.R to ensure consistency across the codebase.
################################################################################

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
        "option_premium" = "Option Premium",
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
        "option_premium" = "Option Premium",
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
#' @param ns Namespace function for Shiny modules (optional)
#' @return bslib card component
#' @noRd
create_open_group_card <- function(group_data, metrics, members, cash_flows, activities, latest_positions = NULL, ns = NULL) {
  # Enrich with market data
  market_data <- enrich_group_with_market_data(group_data$group_id, members, activities, latest_positions)

  # Extract ticker - prefer from members (most reliable), fallback to parsing name
  ticker <- if (!is.null(members) && nrow(members) > 0) {
    # Get the underlying stock symbol from members
    underlying <- members %>% filter(role == "underlying_stock")
    if (nrow(underlying) > 0) {
      underlying$symbol[1]
    } else {
      # No underlying stock, use first member (might be option)
      base_symbol <- members$symbol[1]
      # If it's an option, extract underlying ticker
      if (is_option_symbol(base_symbol)) {
        parse_option_symbol(base_symbol)
      } else {
        base_symbol
      }
    }
  } else {
    # Fallback: parse from group name
    # Handles standardized format: "{STRATEGY} - {TICKER} - {EXPIRY} @ ${STRIKE}"
    if (grepl(" - ", group_data$group_name)) {
      parts <- strsplit(group_data$group_name, " - ", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        parts[2]  # Ticker is second segment in standardized format
      } else {
        parts[1]
      }
    } else {
      # Simple format - take first word
      strsplit(group_data$group_name, " ")[[1]][1]
    }
  }

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
    header_secondary_parts <- c(header_secondary_parts, sprintf("%d days held", metrics$days_held[1]))
  }

  # Add annualized return if available
  if (nrow(metrics) > 0 && !is.na(metrics$projected_annualized_return_pct[1])) {
    header_secondary_parts <- c(
      header_secondary_parts,
      sprintf("%s annualized", format_percentage(metrics$projected_annualized_return_pct[1] / 100))
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
  # Ensure metrics is a single row (take first if multiple)
  if (nrow(metrics) > 1) {
    log_warn("Group Card: Multiple metrics rows for group {group_data$group_id}, using first")
    metrics <- metrics[1, ]
  }

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
        # Determine option type based on strategy
        option_type <- if (grepl("Cash-Secured Put", group_data$strategy_type, ignore.case = TRUE)) {
          "put"
        } else {
          "call"
        }
        itm_str <- format_strike_relationship(
          market_data$current_stock_price,
          market_data$strike_price,
          option_type
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

    # Cash-Secured Put specific: Show cash collateral instead of capital deployed
    is_csp <- grepl("Cash-Secured Put", group_data$strategy_type, ignore.case = TRUE)

    if (is_csp) {
      # For CSP: Calculate cash collateral (strike × 100)
      if (!is.null(market_data$strike_price)) {
        # Get number of contracts from activities
        num_contracts <- activities %>%
          filter(
            type == "Trades",
            action == "Sell",
            purrr::map_lgl(symbol, is_option_symbol)
          ) %>%
          summarise(total = sum(abs(quantity), na.rm = TRUE)) %>%
          pull(total)
        num_contracts <- if (length(num_contracts) == 0) 1 else num_contracts

        cash_collateral <- market_data$strike_price * 100 * num_contracts
        collateral_display <- sprintf("%s (%d shares × %s strike)",
                                      format_currency(cash_collateral),
                                      as.integer(num_contracts * 100),
                                      format_currency(market_data$strike_price))

        capital_rows <- c(capital_rows, list(
          create_metric_row("Cash Collateral", collateral_display, is_primary = TRUE)
        ))

        # Premium received
        if (option_premium_collected > 0) {
          return_on_collateral <- option_premium_collected / cash_collateral
          premium_display <- sprintf("%s (%s of collateral)",
                                    format_currency(option_premium_collected),
                                    format_percentage(return_on_collateral))
          capital_rows <- c(capital_rows, list(
            create_metric_row("Premium Received", premium_display)
          ))
        }
      }
    } else {
      # Standard covered call display
      capital_display <- if (option_premium_collected > 0 && group_data$strategy_type != "Other") {
        sprintf("%s (net after %s premium collected)",
                format_currency(metrics$cost_basis[1]),
                format_currency(option_premium_collected))
      } else {
        format_currency(metrics$cost_basis[1])
      }

      capital_rows <- c(capital_rows, list(
        create_metric_row("Capital Deployed", capital_display, is_primary = TRUE)
      ))
    }

    # Expected profit with clear labeling
    if (metrics$projected_income[1] > 0) {
      return_pct <- metrics$projected_income[1] / metrics$cost_basis[1]
      annualized_pct <- metrics$projected_annualized_return_pct[1] / 100

      profit_display <- sprintf("%s (%s return | %s annualized)",
                               format_currency(metrics$projected_income[1]),
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
    dividend_display <- if (metrics$cash_collected[1] > 0) {
      sprintf("%s collected", format_currency(metrics$cash_collected[1]))
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
  # Button IDs and Shiny.setInputValue names must be namespaced
  close_btn_id <- sprintf("close_group_%s", group_data$group_id)
  edit_btn_id <- sprintf("edit_group_%s", group_data$group_id)

  # Get namespaced input names for JavaScript
  close_input_name <- if (!is.null(ns)) ns("close_group_clicked") else "close_group_clicked"
  edit_input_name <- if (!is.null(ns)) ns("edit_group_clicked") else "edit_group_clicked"

  # Risk analysis button (only for strategies with options)
  risk_btn_id <- if (!is.null(ns)) {
    ns(paste0("analyze_risk_btn_group_", group_data$group_id))
  } else {
    paste0("analyze_risk_btn_group_", group_data$group_id)
  }

  # Determine if this group has option data needed for risk analysis
  has_option_data <- !is.null(market_data$strike_price) &&
                     !is.null(market_data$expiration_date) &&
                     !is.na(market_data$strike_price) &&
                     !is.na(market_data$expiration_date)

  # Determine if this group can be converted to legacy (any strategy with projected cash flows)
  can_convert_to_legacy <- nrow(cash_flows %>% filter(status == "projected")) > 0

  action_buttons <- tags$div(
    class = "card-actions",
    style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #ddd; display: flex; justify-content: flex-end; flex-wrap: wrap; gap: 8px; row-gap: 8px;",

    # Add Analyze Risk button if option data exists
    if (has_option_data) {
      tags$button(
        id = risk_btn_id,
        type = "button",
        class = "btn btn-sm btn-primary",
        style = "white-space: nowrap;",
        onclick = sprintf("console.log('Analyze Risk clicked: %s'); Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                          group_data$group_id,
                          if (!is.null(ns)) ns("analyze_risk_group_clicked") else "analyze_risk_group_clicked",
                          group_data$group_id),
        icon("chart-line"),
        " Risk"
      )
    },

    # Add Convert to Legacy button if eligible
    if (can_convert_to_legacy) {
      tags$button(
        id = sprintf("convert_legacy_%s", group_data$group_id),
        type = "button",
        class = "btn btn-sm btn-info",
        style = "white-space: nowrap;",
        onclick = sprintf("console.log('Convert to Legacy clicked: %s'); Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                          group_data$group_id,
                          if (!is.null(ns)) ns("convert_legacy_clicked") else "convert_legacy_clicked",
                          group_data$group_id),
        icon("archive"),
        " Legacy"
      )
    },

    tags$button(
      id = close_btn_id,
      type = "button",
      class = "btn btn-sm btn-warning",
      style = "white-space: nowrap;",
      onclick = sprintf("console.log('Close clicked: %s'); Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                        group_data$group_id, close_input_name, group_data$group_id),
      icon("times-circle"),
      " Close"
    ),
    tags$button(
      id = edit_btn_id,
      type = "button",
      class = "btn btn-sm btn-default",
      style = "white-space: nowrap;",
      onclick = sprintf("console.log('Edit clicked: %s'); Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                        group_data$group_id, edit_input_name, group_data$group_id),
      icon("edit"),
      " Edit"
    ),
    tags$button(
      id = sprintf("delete_group_%s", group_data$group_id),
      type = "button",
      class = "btn btn-sm btn-danger",
      style = "white-space: nowrap;",
      onclick = sprintf("console.log('Delete clicked: %s'); Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                        group_data$group_id,
                        if (!is.null(ns)) ns("delete_group_clicked") else "delete_group_clicked",
                        group_data$group_id),
      icon("trash"),
      " Delete"
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
#' @param ns Namespace function for Shiny modules (optional)
#' @return bslib card component
#' @noRd
create_closed_group_card <- function(group_data, pnl, members, cash_flows, activities, ns = NULL) {
  # Card header
  header_primary <- sprintf("%s", group_data$group_name)
  header_secondary <- sprintf("%s | %d days | %s annualized",
                              group_data$strategy_type,
                              if (nrow(pnl) > 0) pnl$hold_days else 0,
                              if (nrow(pnl) > 0) format_percentage(pnl$annualized_return_pct / 100) else "0%")

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
      create_metric_row("Annualized Return", format_percentage(pnl$annualized_return_pct / 100), is_primary = TRUE),
      create_metric_row("Net P&L", sprintf("%s (%s)",
                                           format_currency(pnl$net_pnl),
                                           format_percentage(pnl$total_return_pct / 100))),
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
    if (strategy_type != "Other" && pnl$option_net_premium > 0) {
      # For covered call strategies: show premiums as cost reduction
      costs_section <- list(
        tags$h5("Costs"),
        create_metric_row("Stock Purchases", format_currency(pnl$stock_purchases)),
        create_metric_row("Option Premiums (Credit)",
                         sprintf("-%s", format_currency(pnl$option_net_premium))),
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
        create_metric_row("Option Premiums", format_currency(pnl$option_net_premium)),
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
      create_metric_row("Total Return", format_percentage(pnl$total_return_pct / 100)),
      create_metric_row("Annualized Return", format_percentage(pnl$annualized_return_pct / 100), is_primary = TRUE)
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
  # Button ID and Shiny.setInputValue name must be namespaced
  reopen_btn_id <- sprintf("reopen_group_%s", group_data$group_id)

  # Get namespaced input name for JavaScript
  reopen_input_name <- if (!is.null(ns)) ns("reopen_group_clicked") else "reopen_group_clicked"

  action_buttons <- tags$div(
    class = "card-actions",
    style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #ddd; display: flex; justify-content: flex-end; flex-wrap: wrap; gap: 8px; row-gap: 8px;",
    tags$button(
      id = reopen_btn_id,
      type = "button",
      class = "btn btn-sm btn-success",
      style = "white-space: nowrap;",
      onclick = sprintf("console.log('Reopen clicked: %s'); Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                        group_data$group_id, reopen_input_name, group_data$group_id),
      icon("undo"),
      " Reopen"
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
#' @param ns Namespace function for Shiny modules (optional, required for button interactions)
#' @return bslib card component or NULL if group not found
#' @export
create_group_card <- function(group_id, group_data = NULL, members = NULL,
                               activities = NULL, cash_flows = NULL, metrics = NULL,
                               latest_positions = NULL, ns = NULL) {
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
      activities = activities,
      ns = ns
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
      latest_positions = latest_positions,
      ns = ns
    )
  }
}
