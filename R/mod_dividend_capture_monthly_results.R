#' dividend_capture_monthly_results UI Function
#'
#' @description Shiny Module for displaying monthly dividend capture results.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput wellPanel h4 p tags strong
mod_dividend_capture_monthly_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Strategy overview (shown when no results)
    uiOutput(ns("strategy_overview")),

    # Results cards
    uiOutput(ns("results_cards"))
  )
}

#' dividend_capture_monthly_results Server Functions
#'
#' @description Server logic for displaying results as cards.
#'
#' @param id Module ID
#' @param results_data Reactive containing results data frame
#' @param days_since_filter Reactive containing selected days since filter values
#'
#' @noRd
#'
#' @importFrom shiny moduleServer renderUI req tags wellPanel h4 p
#' @importFrom bslib card_body
#' @importFrom dplyr %>% filter
#' @importFrom purrr map
#' @importFrom rlang .data
mod_dividend_capture_monthly_results_server <- function(id, results_data, days_since_filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render strategy overview (hide when results exist)
    output$strategy_overview <- renderUI({
      if (is.null(results_data()) || nrow(results_data()) == 0) {
        wellPanel(
          h4("Strategy Overview"),
          p("This strategy analyzes dividend capture opportunities for monthly dividend ETFs. It backtests:"),
          tags$ul(
            tags$li("Buy at close on day before ex-dividend"),
            tags$li("Sell at open on ex-dividend day"),
            tags$li("Collect dividend + price movement"),
            tags$li("Historical success rates and returns")
          ),
          p(strong("Includes 76+ ETFs: covered calls, REITs, BDCs, and Canadian securities")),
          p(strong("Results are sorted by annual Sharpe ratio."))
        )
      } else {
        NULL
      }
    })

    # Render results as cards
    output$results_cards <- renderUI({
      req(results_data())
      req(days_since_filter())

      # Filter by selected days since ranges
      selected_ranges <- days_since_filter()
      filtered_data <- results_data() %>%
        filter(
          # Check if row matches any selected range
          sapply(.data$days_since_last_dividend, function(days) {
            any(sapply(selected_ranges, function(range) {
              match_days_range(days, range)
            }))
          })
        )

      if (nrow(filtered_data) == 0) {
        return(wellPanel(
          h4("No ETFs match selected filters"),
          p("Try selecting more time ranges.")
        ))
      }

      filtered_data %>%
        split(seq_len(nrow(.))) %>%
        purrr::map(create_monthly_etf_card) %>%
        tags$div(class = "opportunity-cards-container", .)
    })
  })
}

#' Match days value to a range string
#'
#' @param days Numeric days since last dividend
#' @param range Character range string (e.g., "0-7 days ago")
#' @return Logical TRUE if days matches range
#' @noRd
match_days_range <- function(days, range) {
  if (is.na(days)) {
    return(FALSE)
  }

  if (range == "0-7 days ago") {
    return(days >= 0 && days <= 7)
  } else if (range == "8-14 days ago") {
    return(days >= 8 && days <= 14)
  } else if (range == "15-21 days ago") {
    return(days >= 15 && days <= 21)
  } else if (range == "22-30 days ago") {
    return(days >= 22 && days <= 30)
  } else if (range == "31+ days ago") {
    return(days >= 31)
  }

  return(FALSE)
}

################################################################################
# CARD CONSTRUCTOR (Strategy-Specific)
################################################################################

#' Create ETF dividend capture card for monthly strategy
#'
#' @param row Single-row tibble with ETF analysis data
#' @return A bslib card component
#' @noRd
create_monthly_etf_card <- function(row) {
  # Card header
  header <- create_generic_card_header(
    primary_text = paste0(row$ticker, " - Monthly Dividend Capture"),
    secondary_text = format_currency(row$current_price)
  )

  # Display last ex-dividend date info
  last_ex_info <- if (!is.na(row$last_ex_date)) {
    ex_date <- as.Date(row$last_ex_date)
    days_since <- as.numeric(difftime(Sys.Date(), ex_date, units = "days"))
    date_str <- format(ex_date, "%b %d, %Y")

    if (days_since == 0) {
      paste0(date_str, " (Today!)")
    } else if (days_since == 1) {
      paste0(date_str, " (1 day ago)")
    } else {
      paste0(date_str, " (", days_since, " days ago)")
    }
  } else {
    "Not available"
  }

  # Show last dividend amount
  last_div_info <- if (!is.na(row$last_dividend_amount)) {
    format_currency(row$last_dividend_amount, digits = 4)
  } else {
    "Not available"
  }

  # Card body
  body <- card_body(
    # Section 1: Quick Overview (OPEN)
    create_accordion_section(
      title = "Quick Overview",
      is_open = TRUE,
      create_metric_row("Success Rate", format_percentage(row$success_rate / 100, digits = 1), is_primary = TRUE),
      create_metric_row("Avg Return per Trade", format_percentage(row$avg_return / 100, digits = 3)),
      create_metric_row("Events Analyzed", as.character(row$total_events)),
      create_metric_row("Date Range", row$date_range)
    ),

    # Section 2: Trading Strategy (OPEN)
    create_accordion_section(
      title = "Trading Strategy",
      is_open = TRUE,
      create_metric_row("Schedule Type", row$schedule_type),
      create_metric_row("Observed Pattern", row$observed_schedule),
      create_metric_row("Last Ex-Div Date", last_ex_info, is_primary = TRUE),
      create_metric_row("Last Dividend Amount", last_div_info),
      create_metric_row("Total Dividends Paid", paste0(row$dividend_count, " historical payments")),
      create_metric_row("Avg Dividend (Historical)", format_currency(row$avg_dividend, digits = 4)),
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
