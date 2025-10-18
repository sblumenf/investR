#' zero_dividend_results_table UI Function
#'
#' @description A shiny Module for displaying zero-dividend analysis results.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_zero_dividend_results_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Strategy overview (shown when no results)
    uiOutput(ns("strategy_overview")),

    # Results cards
    uiOutput(ns("results_cards"))
  )
}

#' zero_dividend_results_table Server Functions
#'
#' @description Server logic for displaying the results as cards.
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
mod_zero_dividend_results_table_server <- function(id, results_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Render strategy overview (hide when results exist)
    output$strategy_overview <- renderUI({
      if (is.null(results_data()) || nrow(results_data()) == 0) {
        wellPanel(
          h4("Strategy Overview"),
          p("This strategy analyzes zero-dividend stocks for deep in-the-money (ITM)
            covered call opportunities. It targets growth stocks that reinvest profits rather
            than paying dividends."),
          tags$ul(
            tags$li(strong("Variants available:"), " S&P 500 zero-dividend, overbought, oversold, most shorted, 2x/3x leveraged ETFs"),
            tags$li(strong("Default strike:"), " 85% of current price (deeper ITM than aristocrats)"),
            tags$li(strong("Adjustable parameters:"), " Strike threshold (50-100%) and expiration (30-365+ days)"),
            tags$li(strong("No dividend income:"), " Returns come purely from option premiums + capital appreciation"),
            tags$li(strong("Higher volatility:"), " Growth stocks typically have higher option premiums")
          ),
          p(strong("Select a variant and click 'Run Analysis' to fetch opportunities.")),
          tags$hr(),
          p(tags$em("Note: First run may take several minutes to scan stocks.
                    Results are cached for efficiency."))
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
          p("Try adjusting the strike threshold or expiration range parameters.")
        ))
      }

      # Create cards for results
      results_data() %>%
        split(seq_len(nrow(.))) %>%
        purrr::map(create_zero_dividend_opportunity_card) %>%
        tags$div(class = "opportunity-cards-container", .)
    })
  })
}

################################################################################
# CARD CONSTRUCTOR (Strategy-Specific)
################################################################################

#' Create a complete opportunity card for zero-dividend stock
#'
#' @param row Single-row tibble with all opportunity data
#' @return A bslib card component with HTML5 details accordions
#' @noRd
create_zero_dividend_opportunity_card <- function(row) {
  # Card header
  header <- create_generic_card_header(
    primary_text = paste0(row$company_name, " (", row$ticker, ")"),
    secondary_text = format_currency(row$current_price)
  )

  # Card body with sections
  body <- bslib::card_body(
    # Section 1: Quick Overview (OPEN by default)
    create_accordion_section(
      title = "Quick Overview",
      is_open = TRUE,
      create_metric_row("Annualized Return", format_percentage(row$annualized_return), is_primary = TRUE),
      create_metric_row("Total Return", format_percentage(row$total_return)),
      create_metric_row("Net Profit", format_currency(row$net_profit / 100), is_primary = TRUE),
      create_metric_row("Days to Expiry", as.character(row$days_to_expiry))
    ),

    # Section 2: Transaction (OPEN by default)
    create_accordion_section(
      title = "Transaction",
      is_open = TRUE,
      create_metric_row("Expiration Date", as.character(row$expiration)),
      create_metric_row("Strike Price", format_currency(row$strike)),
      create_metric_row("Strike / Current", format_percentage(row$strike / row$current_price)),
      create_metric_row("Open Interest", format(row$open_interest, big.mark = ",")),
      create_metric_row("Net Outlay", format_currency(row$net_outlay / 100))
    ),

    # Section 3: Cash Flow Details (collapsed)
    create_accordion_section(
      title = "Cash Flow Details",
      is_open = FALSE,
      create_metric_row("Investment", format_currency(row$investment / 100)),
      create_metric_row("Premium Received", format_currency(row$premium_received / 100)),
      create_metric_row("Dividend Income", format_currency(row$dividend_income / 100)),
      create_metric_row("Reinvestment Income", format_currency(row$reinvestment_income / 100)),
      create_metric_row("Exercise Proceeds", format_currency(row$exercise_proceeds / 100))
    ),

    # Section 4: Option Details (collapsed)
    create_accordion_section(
      title = "Option Details",
      is_open = FALSE,
      create_metric_row("Bid Price", format_currency(row$bid_price)),
      create_metric_row("Intrinsic Value", format_currency(row$intrinsic_value)),
      create_metric_row("Extrinsic Value", format_currency(row$extrinsic_value)),
      create_metric_row("Time Value %", format_percentage(row$extrinsic_value / row$bid_price))
    ),

    # Section 5: Risk & Protection (collapsed)
    create_accordion_section(
      title = "Risk & Protection",
      is_open = FALSE,
      create_metric_row("Downside Protection", format_percentage(row$downside_protection_pct)),
      create_metric_row("Breakeven Price", format_currency(row$breakeven_price)),
      # Max Drawdown is negative - apply red styling
      create_metric_row("Max Drawdown (5yr)", format_percentage(row$max_drawdown), is_negative = TRUE),
      create_metric_row("Current Yield", format_percentage(row$current_yield))
    )
  )

  # Always use standard card (no dividend warnings for zero-div stocks)
  create_standard_card(header, body)
}

## To be copied in the UI
# mod_zero_dividend_results_table_ui("zero_dividend_results_table_1")

## To be copied in the server
# mod_zero_dividend_results_table_server("zero_dividend_results_table_1", results_data)
