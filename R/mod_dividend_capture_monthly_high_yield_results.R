#' dividend_capture_monthly_high_yield_results UI Function
#'
#' @description Shiny Module for displaying monthly high-yield dividend capture results.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput wellPanel h4 p tags strong
mod_dividend_capture_monthly_high_yield_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Strategy overview (shown when no results)
    uiOutput(ns("strategy_overview")),

    # Results cards
    uiOutput(ns("results_cards"))
  )
}

#' dividend_capture_monthly_high_yield_results Server Functions
#'
#' @description Server logic for displaying results as cards.
#'
#' @param id Module ID
#' @param results_data Reactive containing results data frame
#'
#' @noRd
#'
#' @importFrom shiny moduleServer renderUI req tags wellPanel h4 p
#' @importFrom bslib card_body
#' @importFrom dplyr %>%
#' @importFrom purrr map
mod_dividend_capture_monthly_high_yield_results_server <- function(id, results_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render strategy overview (hide when results exist)
    output$strategy_overview <- renderUI({
      if (is.null(results_data()) || nrow(results_data()) == 0) {
        wellPanel(
          h4("Strategy Overview"),
          p("This strategy analyzes dividend capture opportunities for monthly high-yield ETFs. It backtests:"),
          tags$ul(
            tags$li("Buy at close on day before ex-dividend"),
            tags$li("Sell at open on ex-dividend day"),
            tags$li("Collect dividend + price movement"),
            tags$li("Historical success rates and returns")
          ),
          p(strong("Results are sorted by upcoming ex-dividend date (soonest first), then by risk-adjusted returns."))
        )
      } else {
        NULL
      }
    })

    # Render results as cards
    output$results_cards <- renderUI({
      req(results_data())

      if (nrow(results_data()) == 0) {
        return(wellPanel(
          h4("No ETFs found"),
          p("No ETFs met the analysis criteria. This may be due to insufficient historical data.")
        ))
      }

      # Create cards for each ETF
      results_data() %>%
        split(seq_len(nrow(.))) %>%
        purrr::map(create_monthly_high_yield_etf_card) %>%
        tags$div(class = "opportunity-cards-container", .)
    })
  })
}

################################################################################
# CARD CONSTRUCTOR (Strategy-Specific)
################################################################################

#' Create ETF dividend capture card for monthly high-yield strategy
#'
#' @param row Single-row tibble with ETF analysis data
#' @return A bslib card component
#' @noRd
create_monthly_high_yield_etf_card <- function(row) {
  # Card header
  header <- create_generic_card_header(
    primary_text = paste0(row$ticker, " - Monthly High-Yield Capture"),
    secondary_text = format_currency(row$current_price)
  )

  # Format next ex-div date display
  next_ex_div_display <- if (!is.na(row$next_ex_div_date)) {
    if (row$days_until_next_ex_div < 0) {
      sprintf("%s (%d days ago)", format(row$next_ex_div_date, "%Y-%m-%d"), abs(row$days_until_next_ex_div))
    } else if (row$days_until_next_ex_div == 0) {
      sprintf("%s (TODAY)", format(row$next_ex_div_date, "%Y-%m-%d"))
    } else if (row$days_until_next_ex_div == 1) {
      sprintf("%s (TOMORROW)", format(row$next_ex_div_date, "%Y-%m-%d"))
    } else {
      sprintf("%s (in %d days)", format(row$next_ex_div_date, "%Y-%m-%d"), row$days_until_next_ex_div)
    }
  } else {
    "Unknown (insufficient data)"
  }

  # Card body
  body <- card_body(
    # Section 1: Quick Overview (OPEN)
    create_accordion_section(
      title = "Quick Overview",
      is_open = TRUE,
      create_metric_row("Current Yield", row$yield_pct, is_primary = TRUE),
      create_metric_row("Success Rate", format_percentage(row$success_rate / 100, digits = 1), is_primary = TRUE),
      create_metric_row("Avg Return per Trade", format_percentage(row$avg_return / 100, digits = 3)),
      create_metric_row("Events Analyzed", as.character(row$total_events)),
      create_metric_row("Date Range", row$date_range)
    ),

    # Section 2: Next Dividend (OPEN)
    create_accordion_section(
      title = "Next Dividend (Estimated)",
      is_open = TRUE,
      create_metric_row("Next Ex-Dividend Date", next_ex_div_display, is_primary = TRUE),
      create_metric_row("Expected Dividend", format_currency(row$avg_dividend, digits = 4)),
      create_metric_row("Hold Period", "Overnight (1 day)")
    ),

    # Section 3: Returns Analysis (CLOSED)
    create_accordion_section(
      title = "Returns Analysis",
      is_open = FALSE,
      create_metric_row("Simple Annual Return", format_percentage(row$simple_annual_return / 100, digits = 1)),
      create_metric_row("Compound Annual Return", format_percentage(row$compound_annual_return / 100, digits = 1), is_primary = TRUE),
      create_metric_row("Income per $10k", format_currency(row$annual_income_per_10k, digits = 0)),
      create_metric_row("Median Return", format_percentage(row$median_return / 100, digits = 3))
    ),

    # Section 4: Risk Metrics (CLOSED)
    create_accordion_section(
      title = "Risk Metrics",
      is_open = FALSE,
      create_metric_row("Standard Deviation", format_percentage(row$std_deviation / 100, digits = 3)),
      create_metric_row("Sharpe Ratio", sprintf("%.2f", row$sharpe_ratio)),
      create_metric_row("Annual Sharpe", sprintf("%.2f", row$annual_sharpe)),
      create_metric_row("Sortino Ratio", if (is.infinite(row$sortino_ratio)) "Inf" else sprintf("%.2f", row$sortino_ratio)),
      create_metric_row("Annual Sortino", if (is.infinite(row$annual_sortino)) "Inf" else sprintf("%.2f", row$annual_sortino)),
      create_metric_row("Best Trade", format_percentage(row$best_return / 100, digits = 2)),
      create_metric_row("Worst Trade", format_percentage(row$worst_return / 100, digits = 2), is_negative = TRUE)
    ),

    # Section 5: Historical Performance (CLOSED)
    create_accordion_section(
      title = "Historical Performance",
      is_open = FALSE,
      create_metric_row("Profitable Trades", as.character(row$profitable_trades)),
      create_metric_row("Losing Trades", as.character(row$losing_trades)),
      create_metric_row("Recent Success Rate (10)", format_percentage(row$recent_success_rate / 100, digits = 1)),
      create_metric_row("Recent Avg Return (10)", format_percentage(row$recent_avg_return / 100, digits = 3)),
      create_metric_row("Avg Dividend", format_currency(row$avg_dividend, digits = 4)),
      create_metric_row("Avg Overnight Move", format_currency(row$avg_overnight_move, digits = 4)),
      create_metric_row("Drop Ratio", format_percentage(row$drop_ratio / 100, digits = 1))
    )
  )

  # Return standard card
  create_standard_card(header, body)
}
