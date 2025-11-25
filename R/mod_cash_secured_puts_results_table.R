#' cash_secured_puts_results_table UI Function
#'
#' @description A shiny Module for displaying cash-secured puts analysis results.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cash_secured_puts_results_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Strategy overview (shown when no results)
    uiOutput(ns("strategy_overview")),

    # Results cards
    uiOutput(ns("results_cards"))
  )
}

#' cash_secured_puts_results_table Server Functions
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
mod_cash_secured_puts_results_table_server <- function(id, results_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Render strategy overview (hide when results exist)
    output$strategy_overview <- renderUI({
      if (is.null(results_data()) || nrow(results_data()) == 0) {
        wellPanel(
          h4("Strategy Overview"),
          p("This strategy analyzes dividend aristocrats for cash-secured put opportunities.
            By selling puts on high-quality dividend stocks, you collect premium income and
            potentially acquire stocks at a discount if assigned."),
          tags$ul(
            tags$li(strong("Target:"), " Dividend aristocrats (25+ years of dividend increases)"),
            tags$li(strong("Default strike:"), " 95% of current price (ATM/slightly OTM)"),
            tags$li(strong("Adjustable parameters:"), " Strike threshold (50-100%) and expiration (7-90 days)"),
            tags$li(strong("Premium income:"), " Collect option premium upfront"),
            tags$li(strong("Assignment scenario:"), " Acquire quality stocks at effective discount (strike - premium)"),
            tags$li(strong("Downside protection:"), " Premium provides buffer against stock decline")
          ),
          p(strong("Click 'Run Analysis' to fetch cash-secured put opportunities.")),
          tags$hr(),
          p(tags$em("Note: First run may take several minutes to scan stocks.
                    Results are cached for efficiency."))
        )
      } else {
        NULL
      }
    })

    # Render results as cards with risk analysis integration
    output$results_cards <- renderUI({
      req(results_data())

      # Show message if no results
      if (nrow(results_data()) == 0) {
        return(wellPanel(
          h4("No opportunities found"),
          p("Try adjusting the strike threshold or expiration range parameters.")
        ))
      }

      results <- results_data()

      # Create cards with risk analysis integration
      cards <- lapply(seq_len(nrow(results)), function(i) {
        row <- results[i, ]
        create_cash_secured_put_card_with_risk(row, ns, paste0("risk_", i))
      })

      tags$div(class = "opportunity-cards-container", cards)
    })

    # Setup risk analysis modules for each card
    observe({
      req(results_data())
      results <- results_data()

      lapply(seq_len(nrow(results)), function(i) {
        row <- results[i, ]
        risk_id <- paste0("risk_", i)

        # Create reactive trigger for this card's button
        trigger <- reactive({
          input[[paste0("analyze_risk_btn_", i)]]
        })

        # Call risk analysis module
        # For cash-secured puts, specify option_type = "put"
        mod_position_risk_server(
          id = risk_id,
          trigger = trigger,
          ticker = reactive(row$ticker),
          strike = reactive(row$strike),
          expiration = reactive(row$expiration),
          premium_received = reactive(row$premium_received),
          current_price = reactive(row$current_price),
          is_aristocrat = reactive(TRUE),  # Cash-secured puts on aristocrats
          simulation_paths = reactive(10000),
          option_type = reactive("put")  # CRITICAL: Must specify "put" for cash-secured puts
        )
      })
    })
  })
}

################################################################################
# CARD CONSTRUCTOR (Strategy-Specific)
################################################################################

#' Create cash-secured put opportunity card with risk analysis button
#'
#' @param row Single-row tibble with all opportunity data
#' @param ns Namespace function
#' @param risk_id Risk module ID
#' @return A bslib card component with risk analysis button
#' @noRd
create_cash_secured_put_card_with_risk <- function(row, ns, risk_id) {
  # Extract row index from risk_id (e.g., "risk_1" -> 1)
  idx <- as.integer(sub("risk_", "", risk_id))

  # Card header
  header <- create_generic_card_header(
    primary_text = paste0(row$company_name, " (", row$ticker, ")"),
    secondary_text = format_currency(row$current_price)
  )

  # Card body with sections
  body <- bslib::card_body(
    # Risk Analysis Button (at top)
    tags$div(
      style = "margin-bottom: 15px;",
      actionButton(
        inputId = ns(paste0("analyze_risk_btn_", idx)),
        label = "Analyze Risk",
        icon = icon("chart-line"),
        class = "btn btn-primary btn-sm",
        style = "width: 100%;"
      )
    ),

    # Section 1: Quick Overview (OPEN by default)
    create_accordion_section(
      title = "Quick Overview",
      is_open = TRUE,
      create_metric_row("Cash Required", format_currency(row$cash_required / 100), is_primary = TRUE),
      create_metric_row("Premium Received", format_currency(row$premium_received / 100)),
      create_metric_row("Return on Cash", format_percentage(row$return_on_cash)),
      create_metric_row("Annualized Return", format_percentage(row$annualized_return), is_primary = TRUE)
    ),

    # Section 2: Risk Metrics (OPEN by default)
    create_accordion_section(
      title = "Risk Metrics",
      is_open = TRUE,
      create_metric_row("Downside Protection", format_percentage(row$downside_protection_pct)),
      create_metric_row("Breakeven Price", format_currency(row$breakeven_price)),
      create_metric_row("Current Price", format_currency(row$current_price)),
      create_metric_row("Strike Price", format_currency(row$strike)),
      create_metric_row("Open Interest", format(row$open_interest, big.mark = ","))
    ),

    # Section 3: Assignment Scenario (collapsed)
    create_accordion_section(
      title = "Assignment Scenario",
      is_open = FALSE,
      create_metric_row("Effective Purchase Price", format_currency(row$breakeven_price)),
      create_metric_row("Net Outlay (if assigned)", format_currency(row$net_outlay / 100)),
      create_metric_row("Annual Dividend (if assigned)", format_currency(row$annual_dividend)),
      create_metric_row("Dividend Yield (on breakeven)",
                       format_percentage(row$annual_dividend / row$breakeven_price))
    ),

    # Section 4: Transaction Details (collapsed)
    create_accordion_section(
      title = "Transaction Details",
      is_open = FALSE,
      create_metric_row("Expiration Date", as.character(row$expiration)),
      create_metric_row("Days to Expiry", as.character(row$days_to_expiry)),
      create_metric_row("Bid Price", format_currency(row$bid_price)),
      create_metric_row("Strike / Current", format_percentage(row$strike / row$current_price)),
      create_metric_row("Open Interest", format(row$open_interest, big.mark = ","))
    ),

    # Section 5: Option Value Decomposition (collapsed)
    create_accordion_section(
      title = "Option Value Decomposition",
      is_open = FALSE,
      create_metric_row("Total Premium", format_currency(row$bid_price)),
      create_metric_row("Intrinsic Value", format_currency(row$intrinsic_value)),
      create_metric_row("Extrinsic Value", format_currency(row$extrinsic_value)),
      create_metric_row("Time Value %", format_percentage(row$extrinsic_value / row$bid_price))
    ),

    # Section 6: Risk Context (collapsed)
    create_accordion_section(
      title = "Risk Context",
      is_open = FALSE,
      # Max Drawdown is negative - apply red styling
      create_metric_row("Max Drawdown (5yr)", format_percentage(row$max_drawdown), is_negative = TRUE),
      create_metric_row("Current Yield", format_percentage(row$current_yield))
    )
  )

  # Use standard card styling
  create_standard_card(header, body)
}

## To be copied in the UI
# mod_cash_secured_puts_results_table_ui("cash_secured_puts_results_table_1")

## To be copied in the server
# mod_cash_secured_puts_results_table_server("cash_secured_puts_results_table_1", results_data)
