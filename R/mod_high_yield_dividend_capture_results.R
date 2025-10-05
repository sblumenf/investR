#' high_yield_dividend_capture_results UI Function
#'
#' @description Shiny Module for displaying high-yield dividend capture results.
#'   Universe-agnostic module.
#'
#' @param id Module identifier
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput wellPanel h4 p tags strong
mod_high_yield_dividend_capture_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Strategy overview (shown when no results)
    uiOutput(ns("strategy_overview")),

    # Results cards
    uiOutput(ns("results_cards"))
  )
}

#' high_yield_dividend_capture_results Server Functions
#'
#' @description Server logic for displaying results as cards.
#'
#' @param id Module ID
#' @param results_data Reactive containing results data frame
#' @param yield_filter Reactive containing yield threshold value
#' @param universe_config Universe configuration object
#'
#' @noRd
#'
#' @importFrom shiny moduleServer renderUI req tags wellPanel h4 p
#' @importFrom bslib card_body
#' @importFrom dplyr %>%
#' @importFrom purrr map
mod_high_yield_dividend_capture_results_server <- function(id, results_data, yield_filter, universe_config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render strategy overview (hide when results exist)
    output$strategy_overview <- renderUI({
      if (is.null(results_data()) || nrow(results_data()) == 0) {
        wellPanel(
          h4("Strategy Overview"),
          p(paste0("This strategy screens ", universe_config$universe_name, " stocks for high-yield dividend capture opportunities. It:")),
          tags$ul(
            tags$li("Filters for stocks with yield above threshold (adjustable)"),
            tags$li("Identifies stocks with ex-dividend dates in the next 1-2 business days"),
            tags$li("Backtests historical performance: buy at close before ex-dividend, sell at open on ex-dividend day"),
            tags$li("Calculates success rates, returns, and risk metrics")
          ),
          p(strong("Results are sorted by Annual Sortino Ratio (risk-adjusted returns)."))
        )
      } else {
        NULL
      }
    })

    # Render results as cards
    output$results_cards <- renderUI({
      req(results_data())

      # No additional filtering needed - yield filter already applied in analysis
      filtered_data <- results_data()

      if (nrow(filtered_data) == 0) {
        return(wellPanel(
          h4("No stocks match criteria"),
          p("Try lowering the yield threshold or run analysis at a different time when more ex-dividend dates align.")
        ))
      }

      filtered_data %>%
        split(seq_len(nrow(.))) %>%
        purrr::map(create_high_yield_stock_card) %>%
        tags$div(class = "opportunity-cards-container", .)
    })
  })
}

################################################################################
# CARD CONSTRUCTOR (Strategy-Specific)
################################################################################

#' Create high-yield stock dividend capture card
#'
#' @param row Single-row tibble with stock analysis data
#' @return A bslib card component
#' @noRd
create_high_yield_stock_card <- function(row) {
  # Card header
  header <- create_generic_card_header(
    primary_text = paste0(row$ticker, " - High-Yield Dividend Capture"),
    secondary_text = format_currency(row$current_price)
  )

  # Card body
  body <- card_body(
    # Section 1: Opportunity Details (OPEN)
    create_accordion_section(
      title = "Opportunity Details",
      is_open = TRUE,
      create_metric_row("Current Yield", format_percentage(row$current_yield / 100, digits = 2), is_primary = TRUE),
      create_metric_row("Predicted Ex-Div Date", row$predicted_ex_date),
      create_metric_row("Days Until Ex-Div", as.character(row$days_until_ex_div)),
      create_metric_row("Prediction Confidence", row$prediction_confidence)
    ),

    # Section 2: Historical Performance (OPEN)
    create_accordion_section(
      title = "Historical Performance",
      is_open = TRUE,
      create_metric_row("Success Rate", format_percentage(row$success_rate / 100, digits = 1), is_primary = TRUE),
      create_metric_row("Avg Return per Trade", format_percentage(row$avg_return / 100, digits = 3)),
      create_metric_row("Events Analyzed", as.character(row$total_events)),
      create_metric_row("Date Range", row$date_range)
    ),

    # Section 3: Returns Analysis (CLOSED)
    create_accordion_section(
      title = "Returns Analysis",
      is_open = FALSE,
      create_metric_row("Payment Frequency", sprintf("%.1f per year", row$payments_per_year)),
      create_metric_row("Simple Annual Return", format_percentage(row$simple_annual_return / 100, digits = 1)),
      create_metric_row("Compound Annual Return", format_percentage(row$compound_annual_return / 100, digits = 1), is_primary = TRUE),
      create_metric_row("Income per $10k", format_currency(row$annual_income_per_10k, digits = 0)),
      create_metric_row("Median Return", format_percentage(row$median_return / 100, digits = 3)),
      create_metric_row("Avg Profit per $10k", format_currency(row$avg_profit_per_10k, digits = 2))
    ),

    # Section 4: Risk Metrics (CLOSED)
    create_accordion_section(
      title = "Risk Metrics",
      is_open = FALSE,
      create_metric_row("Standard Deviation", format_percentage(row$std_deviation / 100, digits = 3)),
      create_metric_row("Sharpe Ratio", sprintf("%.2f", row$sharpe_ratio)),
      create_metric_row("Annual Sharpe", sprintf("%.2f", row$annual_sharpe)),
      create_metric_row("Sortino Ratio", if (is.infinite(row$sortino_ratio)) "Inf" else sprintf("%.2f", row$sortino_ratio)),
      create_metric_row("Annual Sortino", if (is.infinite(row$annual_sortino)) "Inf" else sprintf("%.2f", row$annual_sortino), is_primary = TRUE),
      create_metric_row("Best Trade", format_percentage(row$best_return / 100, digits = 2)),
      create_metric_row("Worst Trade", format_percentage(row$worst_return / 100, digits = 2), is_negative = TRUE)
    ),

    # Section 5: Trade Details (CLOSED)
    create_accordion_section(
      title = "Trade Details",
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
