#' collar_results UI Function
#'
#' @description A shiny Module for displaying collar strategy analysis results.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_collar_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Strategy overview (shown when no results)
    uiOutput(ns("strategy_overview")),

    # Results cards
    uiOutput(ns("results_cards"))
  )
}

#' collar_results Server Functions
#'
#' @description Server logic for displaying the collar results as cards.
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
mod_collar_results_server <- function(id, results_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Render strategy overview (hide when results exist)
    output$strategy_overview <- renderUI({
      if (is.null(results_data()) || nrow(results_data()) == 0) {
        wellPanel(
          h4("Collar Strategy (Synthetic Bonds)"),
          p("This strategy creates a 'synthetic bond' position with locked-in returns regardless of stock movement."),
          p(strong("Position Structure:")),
          tags$ul(
            tags$li("Buy 100 shares of stock or ETF"),
            tags$li("Sell 1 ATM call option (at current price)"),
            tags$li("Buy 1 ATM put option (at same strike)")
          ),
          p(strong("How It Works:")),
          tags$ul(
            tags$li(strong("If stock rises:"), " Shares called away at strike price"),
            tags$li(strong("If stock falls:"), " Put exercised to sell at strike price"),
            tags$li(strong("Result:"), " You receive exactly the strike price at expiration")
          ),
          p(strong("Key Concepts:")),
          tags$ul(
            tags$li(strong("Net Credit:"), " Call bid - Put ask (must be positive)"),
            tags$li(strong("Net Investment:"), " Stock cost - Net credit received"),
            tags$li(strong("Final Value:"), " Strike price × 100 shares (locked in)"),
            tags$li(strong("Return:"), " (Final value - Net investment + Dividends) / Net investment")
          ),
          p(strong("Universe Options:")),
          tags$ul(
            tags$li(strong("S&P 500 Stocks:"), " All stocks, dividend-paying only, or non-dividend only"),
            tags$li(strong("Liquid ETFs:"), " Major ETFs with active options markets")
          ),
          tags$hr(),
          p(strong("Select a universe and click 'Run Analysis' to find collar opportunities."))
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
          h4("No collar opportunities found"),
          p("All analyzed securities had net debit positions (put ask > call bid)."),
          p("Try a different universe or wait for market conditions to change.")
        ))
      }

      results <- results_data()

      # Create cards with risk analysis integration
      cards <- lapply(seq_len(nrow(results)), function(i) {
        row <- results[i, ]
        create_collar_card_with_risk(row, ns, paste0("risk_", i))
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
          premium_received = reactive(row$premium_received),
          current_price = reactive(row$current_price),
          is_aristocrat = reactive(FALSE),  # Collar strategy
          simulation_paths = reactive(10000)
        )
      })
    })
  })
}

################################################################################
# CARD CONSTRUCTOR (Strategy-Specific)
################################################################################

#' Create collar opportunity card with risk analysis button
#'
#' @param row Single-row tibble with all opportunity data
#' @param ns Namespace function
#' @param risk_id Risk module ID
#' @return A bslib card component with risk analysis button
#' @noRd
create_collar_card_with_risk <- function(row, ns, risk_id) {
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

    # Section 2: Position Details (OPEN by default)
    create_accordion_section(
      title = "Position Details",
      is_open = TRUE,
      create_metric_row("Expiration Date", as.character(row$expiration)),
      create_metric_row("Strike Price (ATM)", format_currency(row$strike)),
      create_metric_row("Net Credit Received", format_currency(row$net_credit), is_primary = TRUE),
      create_metric_row("Premium Received", format_currency(row$premium_received / 100)),
      create_metric_row("Net Investment", format_currency(row$net_investment / 100))
    ),

    # Section 3: Cash Flow Breakdown (collapsed)
    create_accordion_section(
      title = "Cash Flow Breakdown",
      is_open = FALSE,
      create_metric_row("Stock Purchase", format_currency(row$capital_required / 100)),
      create_metric_row("Net Credit", format_currency(row$premium_received / 100)),
      create_metric_row("Net Outlay", format_currency(row$net_outlay / 100)),
      tags$hr(),
      create_metric_row("Dividend Income", format_currency(row$dividend_income / 100)),
      create_metric_row("Reinvestment Income", format_currency(row$reinvestment_income / 100)),
      tags$hr(),
      create_metric_row("Final Value at Strike", format_currency(row$final_value / 100), is_primary = TRUE),
      create_metric_row("Max Profit", format_currency(row$max_profit / 100))
    ),

    # Section 4: Call Option Details (collapsed)
    create_accordion_section(
      title = "Call Option (Sold)",
      is_open = FALSE,
      create_metric_row("Strike", format_currency(row$strike)),
      create_metric_row("Bid (Received)", format_currency(row$call_bid)),
      create_metric_row("Ask", format_currency(row$call_ask)),
      create_metric_row("Volume", format(row$call_volume, big.mark = ",")),
      create_metric_row("Open Interest", format(row$call_oi, big.mark = ","))
    ),

    # Section 5: Put Option Details (collapsed)
    create_accordion_section(
      title = "Put Option (Bought)",
      is_open = FALSE,
      create_metric_row("Strike", format_currency(row$strike)),
      create_metric_row("Bid", format_currency(row$put_bid)),
      create_metric_row("Ask (Paid)", format_currency(row$put_ask)),
      create_metric_row("Volume", format(row$put_volume, big.mark = ",")),
      create_metric_row("Open Interest", format(row$put_oi, big.mark = ","))
    ),

    # Section 6: Trade Mechanics (collapsed)
    create_accordion_section(
      title = "Trade Mechanics",
      is_open = FALSE,
      tags$p(tags$strong("Position:")),
      tags$ul(
        tags$li("Long 100 shares at ", format_currency(row$current_price)),
        tags$li("Short 1 call at strike ", format_currency(row$strike)),
        tags$li("Long 1 put at strike ", format_currency(row$strike))
      ),
      tags$p(tags$strong("At Expiration:")),
      tags$ul(
        tags$li("Stock above strike → Shares called away"),
        tags$li("Stock below strike → Put exercised to sell"),
        tags$li("Either way → Receive ", format_currency(row$strike), " per share")
      ),
      tags$p(tags$strong("Locked Return:")),
      tags$p(sprintf("You are guaranteed to receive $%.2f per share regardless of stock movement.",
                    row$strike))
    )
  )

  # Combine into card
  bslib::card(
    header,
    body,
    class = "opportunity-card"
  )
}

#' Create a complete opportunity card for collar position (legacy, no risk button)
#'
#' @param row Single-row tibble with all opportunity data
#' @return A bslib card component with HTML5 details accordions
#' @noRd
create_collar_opportunity_card <- function(row) {
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

    # Section 2: Position Details (OPEN by default)
    create_accordion_section(
      title = "Position Details",
      is_open = TRUE,
      create_metric_row("Expiration Date", as.character(row$expiration)),
      create_metric_row("Strike Price (ATM)", format_currency(row$strike)),
      create_metric_row("Net Credit Received", format_currency(row$net_credit), is_primary = TRUE),
      create_metric_row("Premium Received", format_currency(row$premium_received / 100)),
      create_metric_row("Net Investment", format_currency(row$net_investment / 100))
    ),

    # Section 3: Cash Flow Breakdown (collapsed)
    create_accordion_section(
      title = "Cash Flow Breakdown",
      is_open = FALSE,
      create_metric_row("Stock Purchase", format_currency(row$capital_required / 100)),
      create_metric_row("Net Credit", format_currency(row$premium_received / 100)),
      create_metric_row("Net Outlay", format_currency(row$net_outlay / 100)),
      tags$hr(),
      create_metric_row("Dividend Income", format_currency(row$dividend_income / 100)),
      create_metric_row("Reinvestment Income", format_currency(row$reinvestment_income / 100)),
      tags$hr(),
      create_metric_row("Final Value at Strike", format_currency(row$final_value / 100), is_primary = TRUE),
      create_metric_row("Max Profit", format_currency(row$max_profit / 100))
    ),

    # Section 4: Call Option Details (collapsed)
    create_accordion_section(
      title = "Call Option (Sold)",
      is_open = FALSE,
      create_metric_row("Strike", format_currency(row$strike)),
      create_metric_row("Bid (Received)", format_currency(row$call_bid)),
      create_metric_row("Ask", format_currency(row$call_ask)),
      create_metric_row("Volume", format(row$call_volume, big.mark = ",")),
      create_metric_row("Open Interest", format(row$call_oi, big.mark = ","))
    ),

    # Section 5: Put Option Details (collapsed)
    create_accordion_section(
      title = "Put Option (Bought)",
      is_open = FALSE,
      create_metric_row("Strike", format_currency(row$strike)),
      create_metric_row("Bid", format_currency(row$put_bid)),
      create_metric_row("Ask (Paid)", format_currency(row$put_ask)),
      create_metric_row("Volume", format(row$put_volume, big.mark = ",")),
      create_metric_row("Open Interest", format(row$put_oi, big.mark = ","))
    ),

    # Section 6: Trade Mechanics (collapsed)
    create_accordion_section(
      title = "Trade Mechanics",
      is_open = FALSE,
      tags$p(tags$strong("Position:")),
      tags$ul(
        tags$li("Long 100 shares at ", format_currency(row$current_price)),
        tags$li("Short 1 call at strike ", format_currency(row$strike)),
        tags$li("Long 1 put at strike ", format_currency(row$strike))
      ),
      tags$p(tags$strong("At Expiration:")),
      tags$ul(
        tags$li("Stock above strike → Shares called away"),
        tags$li("Stock below strike → Put exercised to sell"),
        tags$li("Either way → Receive ", format_currency(row$strike), " per share")
      ),
      tags$p(tags$strong("Locked Return:")),
      tags$p(sprintf("You are guaranteed to receive $%.2f per share regardless of stock movement.",
                    row$strike))
    )
  )

  # Combine into card
  bslib::card(
    header,
    body,
    class = "opportunity-card"
  )
}

## To be copied in the UI
# mod_collar_results_ui("collar_results_1")

## To be copied in the server
# mod_collar_results_server("collar_results_1")
