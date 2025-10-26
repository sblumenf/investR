#' aristocrats_results_table UI Function
#'
#' @description A shiny Module for displaying aristocrats analysis results.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_aristocrats_results_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Strategy overview (shown when no results)
    uiOutput(ns("strategy_overview")),

    # Results cards
    uiOutput(ns("results_cards"))
  )
}

#' aristocrats_results_table Server Functions
#'
#' @description Server logic for displaying the results as cards.
#'
#' @param id Module ID
#' @param results_data Reactive containing results data frame
#'
#' @noRd
#'
#' @importFrom shiny moduleServer renderUI req tags actionButton observeEvent
#' @importFrom bslib card card_header card_body
#' @importFrom dplyr %>%
#' @importFrom purrr map
mod_aristocrats_results_table_server <- function(id, results_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Render strategy overview (hide when results exist)
    output$strategy_overview <- renderUI({
      if (is.null(results_data()) || nrow(results_data()) == 0) {
        wellPanel(
          h4("Strategy Overview"),
          p("This strategy analyzes Dividend Aristocrats for deep in-the-money (ITM)
            covered call opportunities. It considers:"),
          tags$ul(
            tags$li("Option premiums and time value"),
            tags$li("Projected dividend income during holding period"),
            tags$li("Downside protection from premium received"),
            tags$li("Historical drawdown and current yield")
          ),
          p(strong("Results are sorted by annualized return."))
        )
      } else {
        NULL
      }
    })

    # Render results as cards with risk analysis buttons
    output$results_cards <- renderUI({
      req(results_data())

      results <- results_data()

      # Create cards with risk analysis integration
      cards <- lapply(seq_len(nrow(results)), function(i) {
        row <- results[i, ]
        create_opportunity_card_with_risk(row, ns, paste0("risk_", i))
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
        mod_position_risk_server(
          id = risk_id,
          trigger = trigger,
          ticker = reactive(row$ticker),
          strike = reactive(row$strike),
          expiration = reactive(row$expiration),
          premium_received = reactive(row$premium_received / 100),
          current_price = reactive(row$current_price),
          is_aristocrat = reactive(TRUE),  # Aristocrats strategy
          simulation_paths = reactive(10000)
        )
      })
    })
  })
}

################################################################################
# CARD CONSTRUCTOR (Strategy-Specific)
################################################################################

#' Create opportunity card with risk analysis button
#'
#' @param row Single-row tibble with all opportunity data
#' @param ns Namespace function
#' @param risk_id Risk module ID
#' @return A bslib card component with risk analysis button
#' @noRd
create_opportunity_card_with_risk <- function(row, ns, risk_id) {
  # Calculate low dividend flag
  low_div <- if ("dividend_income" %in% names(row) && "net_profit" %in% names(row)) {
    (row$dividend_income / row$net_profit) < ARISTOCRATS_CONFIG$low_dividend_threshold
  } else {
    FALSE
  }

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
      create_metric_row("Current Price", format_currency(row$current_price))
    ),

    # Section 5: Risk & Protection (collapsed)
    create_accordion_section(
      title = "Risk & Protection",
      is_open = FALSE,
      create_metric_row("Downside Protection", format_percentage(row$downside_protection_pct)),
      create_metric_row("Breakeven Price", format_currency(row$breakeven_price)),
      # Max Drawdown is negative - apply red styling
      create_metric_row("Max Drawdown", format_percentage(row$max_drawdown), is_negative = TRUE),
      create_metric_row("Current Yield", format_percentage(row$current_yield)),
      create_metric_row("Annual Dividend", format_currency(row$annual_dividend))
    )
  )

  # Return appropriate card type
  if (low_div) {
    create_warning_card(header, body)
  } else {
    create_standard_card(header, body)
  }
}

## To be copied in the UI
# mod_aristocrats_results_table_ui("aristocrats_results_table_1")

## To be copied in the server
# mod_aristocrats_results_table_server("aristocrats_results_table_1", results_data)