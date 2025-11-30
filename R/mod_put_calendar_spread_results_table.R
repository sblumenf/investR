#' put_calendar_spread_results_table UI Function
#'
#' @description A shiny Module for displaying put calendar spread analysis results.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_put_calendar_spread_results_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Strategy overview (shown when no results)
    uiOutput(ns("strategy_overview")),

    # Results cards
    uiOutput(ns("results_cards"))
  )
}

#' put_calendar_spread_results_table Server Functions
#'
#' @description Server logic for displaying calendar spread results as cards.
#'
#' @param id Module ID
#' @param results_data Reactive containing results data frame
#'
#' @noRd
#'
#' @importFrom shiny moduleServer renderUI req tags wellPanel h4 p
#' @importFrom bslib card card_header card_body
#' @importFrom dplyr %>%
#' @importFrom purrr map
mod_put_calendar_spread_results_table_server <- function(id, results_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Render strategy overview (hide when results exist)
    output$strategy_overview <- renderUI({
      if (is.null(results_data()) || nrow(results_data()) == 0) {
        wellPanel(
          h4("Put Calendar Spread Strategy"),
          p("This strategy profits from the differential time decay between near-term
            and longer-dated put options at the same strike price."),
          tags$ul(
            tags$li(strong("Short Leg:"), " Sell near-term put (20-45 DTE) - collects premium from fast theta decay"),
            tags$li(strong("Long Leg:"), " Buy longer-dated put (60-90 DTE) - slower decay, provides protection"),
            tags$li(strong("Same Strike:"), " Both options at ATM or slightly OTM strike"),
            tags$li(strong("Net Debit:"), " Pay to enter (long put costs more than short put credit)"),
            tags$li(strong("Max Loss:"), " Limited to net debit paid"),
            tags$li(strong("Max Profit:"), " Occurs when stock is at strike at front-month expiration")
          ),
          tags$hr(),
          h5("Ideal Conditions (from research):"),
          tags$ul(
            tags$li("IV Rank below 30% (options are cheap)"),
            tags$li("Stock in consolidation/range-bound"),
            tags$li("No earnings during front-month period"),
            tags$li("Ex-dividend date after front-month expiration")
          ),
          p(strong("Click 'Run Analysis' to find calendar spread opportunities.")),
          tags$hr(),
          p(tags$em("Note: Target 15-20% profit on debit. Close 5-10 days before front expiration."))
        )
      } else {
        NULL
      }
    })

    # Render results as cards
    output$results_cards <- renderUI({
      req(results_data())

      # Show message if no results
      if (nrow(results_data()) == 0) {
        return(wellPanel(
          h4("No opportunities found"),
          p("Try adjusting the strike percentage or expiration ranges.")
        ))
      }

      results <- results_data()

      # Create cards
      cards <- lapply(seq_len(nrow(results)), function(i) {
        row <- results[i, ]
        create_calendar_spread_card(row, ns, i)
      })

      tags$div(class = "opportunity-cards-container", cards)
    })
  })
}

################################################################################
# CARD CONSTRUCTOR (Strategy-Specific)
################################################################################

#' Create put calendar spread opportunity card
#'
#' @param row Single-row tibble with all opportunity data
#' @param ns Namespace function
#' @param idx Card index
#' @return A bslib card component
#' @noRd
create_calendar_spread_card <- function(row, ns, idx) {
  # Card header with score badge
  score_class <- if (row$opportunity_score >= 85) {
    "badge bg-success"
  } else if (row$opportunity_score >= 70) {
    "badge bg-primary"
  } else if (row$opportunity_score >= 55) {
    "badge bg-warning"
  } else {
    "badge bg-secondary"
  }

  header <- create_generic_card_header(
    primary_text = paste0(row$company_name, " (", row$ticker, ")"),
    secondary_text = paste0(
      format_currency(row$current_price),
      " | Score: ",
      tags$span(class = score_class, sprintf("%.0f", row$opportunity_score))
    )
  )

  # Card body with sections
  body <- bslib::card_body(
    # Section 1: Quick Overview (OPEN by default)
    create_accordion_section(
      title = "Quick Overview",
      is_open = TRUE,
      create_metric_row("Strike Price", format_currency(row$strike)),
      create_metric_row("Net Debit", format_currency(row$net_debit), is_primary = TRUE),
      create_metric_row("Max Loss", format_currency(row$max_loss)),
      create_metric_row("Est. Max Profit", format_currency(row$estimated_max_profit)),
      create_metric_row("Annualized Return", format_percentage(row$annualized_return), is_primary = TRUE),
      create_metric_row("Opportunity Score", sprintf("%.0f / 100", row$opportunity_score))
    ),

    # Section 2: Spread Details (OPEN by default)
    create_accordion_section(
      title = "Spread Details",
      is_open = TRUE,
      tags$div(
        style = "display: flex; gap: 20px;",
        tags$div(
          style = "flex: 1;",
          tags$h6("Short Leg (Sell)", style = "color: #dc3545;"),
          create_metric_row("Expiration", row$short_expiration),
          create_metric_row("Days to Expiry", as.character(row$short_days_to_expiry)),
          create_metric_row("Bid (Premium)", format_currency(row$short_bid)),
          create_metric_row("Open Interest", format(row$short_oi, big.mark = ","))
        ),
        tags$div(
          style = "flex: 1;",
          tags$h6("Long Leg (Buy)", style = "color: #198754;"),
          create_metric_row("Expiration", row$long_expiration),
          create_metric_row("Days to Expiry", as.character(row$long_days_to_expiry)),
          create_metric_row("Ask (Cost)", format_currency(row$long_ask)),
          create_metric_row("Open Interest", format(row$long_oi, big.mark = ","))
        )
      ),
      tags$hr(),
      create_metric_row("Days Spread", as.character(row$days_spread)),
      create_metric_row("Expiry Ratio", sprintf("%.1fx", row$expiry_ratio))
    ),

    # Section 3: Profit Targets (collapsed)
    create_accordion_section(
      title = "Profit Targets",
      is_open = FALSE,
      create_metric_row("15% Target (Conservative)", format_currency(row$profit_target_15pct)),
      create_metric_row("20% Target (Standard)", format_currency(row$profit_target_20pct)),
      create_metric_row("25% Target (Aggressive)", format_currency(row$profit_target_25pct)),
      create_metric_row("ROI (if max profit)", format_percentage(row$roi))
    ),

    # Section 4: Greeks (collapsed)
    create_accordion_section(
      title = "Position Greeks",
      is_open = FALSE,
      create_metric_row("Net Theta",
                       if (!is.na(row$net_theta)) sprintf("$%.3f/day", row$net_theta) else "N/A",
                       is_primary = !is.na(row$net_theta) && row$net_theta > 0),
      create_metric_row("Net Vega",
                       if (!is.na(row$net_vega)) sprintf("%.3f", row$net_vega) else "N/A"),
      create_metric_row("Net Delta",
                       if (!is.na(row$net_delta)) sprintf("%.3f", row$net_delta) else "N/A"),
      create_metric_row("IV Ratio (Short/Long)",
                       if (!is.na(row$iv_ratio)) sprintf("%.2f", row$iv_ratio) else "N/A"),
      tags$hr(),
      tags$p(
        tags$small(
          style = "color: #6c757d;",
          "Ideal: Net Theta > 0 (time decay works for you), Net Vega > 0 (IV expansion helps), Net Delta ~ 0 (directionally neutral)"
        )
      )
    ),

    # Section 5: Stock Context (collapsed)
    create_accordion_section(
      title = "Stock Context",
      is_open = FALSE,
      create_metric_row("Current Price", format_currency(row$current_price)),
      create_metric_row("Strike / Price", format_percentage(row$strike / row$current_price)),
      create_metric_row("Current Yield", format_percentage(row$current_yield)),
      create_metric_row("Max Drawdown (5yr)", format_percentage(row$max_drawdown), is_negative = TRUE),
      create_metric_row("Min Open Interest", format(row$min_open_interest, big.mark = ","))
    ),

    # Section 6: Management Guidelines (collapsed)
    create_accordion_section(
      title = "Management Guidelines",
      is_open = FALSE,
      tags$div(
        style = "font-size: 0.9em;",
        tags$p(tags$strong("Entry:"), " Enter as single spread order at mid-price or better"),
        tags$p(tags$strong("Profit Target:"), " Close at 15-20% of debit paid"),
        tags$p(tags$strong("Time Exit:"), " Close 5-10 days before front-month expiration"),
        tags$p(tags$strong("Stop Loss:"), " Consider closing if loss reaches 50% of debit"),
        tags$p(tags$strong("Adjustment:"), " If stock moves away, can roll short leg to new strike")
      )
    )
  )

  # Use standard card styling
  create_standard_card(header, body)
}

## To be copied in the UI
# mod_put_calendar_spread_results_table_ui("put_calendar_spread_results_table_1")

## To be copied in the server
# mod_put_calendar_spread_results_table_server("put_calendar_spread_results_table_1", results_data)
