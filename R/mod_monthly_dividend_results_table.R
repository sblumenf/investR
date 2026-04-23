#' monthly_dividend_results_table UI Function
#'
#' @description A shiny Module for displaying monthly dividend analysis results.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_monthly_dividend_results_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Strategy overview (shown when no results)
    uiOutput(ns("strategy_overview")),

    # Results cards
    uiOutput(ns("results_cards"))
  )
}

#' monthly_dividend_results_table Server Functions
#'
#' @description Server logic for displaying the results as cards.
#'
#' @param id Module ID
#' @param results_data Reactive containing results data frame
#'
#' @noRd
#'
#' @importFrom shiny moduleServer renderUI req tags actionButton observeEvent showModal
#' @importFrom bslib card card_header card_body
#' @importFrom dplyr %>%
#' @importFrom purrr map
mod_monthly_dividend_results_table_server <- function(id, results_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Render strategy overview (hide when results exist)
    output$strategy_overview <- renderUI({
      if (is.null(results_data()) || nrow(results_data()) == 0) {
        wellPanel(
          h4("Strategy Overview"),
          p("This strategy analyzes Monthly Dividend stocks for deep in-the-money (ITM)
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

    # Track which row's risk button was clicked (single observer pattern)
    # Use a counter to ensure re-triggers work even for the same row
    trigger_counter <- reactiveVal(0)
    clicked_row <- reactiveVal(NULL)

    # Create a SINGLE set of button observers at module init (not inside observe)
    # These watch buttons 1-50 (reasonable max) - only active ones will ever fire
    lapply(1:50, function(i) {
      local({
        idx <- i
        btn_id <- paste0("analyze_risk_btn_", idx)
        observeEvent(input[[btn_id]], {
          clicked_row(idx)
          trigger_counter(isolate(trigger_counter()) + 1)
        }, ignoreInit = TRUE)
      })
    })

    # Skew signal observers — one per possible card slot (1-50)
    lapply(1:50, function(i) {
      local({
        idx <- i
        skew_btn_id <- paste0("skew_btn_", idx)
        observeEvent(input[[skew_btn_id]], {
          req(results_data())
          if (idx > nrow(results_data())) return()
          ticker <- results_data()$ticker[idx]
          result <- compute_skew_signal(ticker)
          showModal(build_skew_modal(ticker, result))
        }, ignoreInit = TRUE)
      })
    })

    # Single risk analysis module instance
    mod_position_risk_server(
      id = "risk_analysis",
      trigger = reactive({ trigger_counter() }),
      ticker = reactive({
        req(clicked_row(), results_data())
        idx <- clicked_row()
        if (!is.null(idx) && idx <= nrow(results_data())) {
          results_data()$ticker[idx]
        } else NULL
      }),
      strike = reactive({
        req(clicked_row(), results_data())
        idx <- clicked_row()
        if (!is.null(idx) && idx <= nrow(results_data())) {
          results_data()$strike[idx]
        } else NULL
      }),
      expiration = reactive({
        req(clicked_row(), results_data())
        idx <- clicked_row()
        if (!is.null(idx) && idx <= nrow(results_data())) {
          results_data()$expiration[idx]
        } else NULL
      }),
      premium_received = reactive({
        req(clicked_row(), results_data())
        idx <- clicked_row()
        if (!is.null(idx) && idx <= nrow(results_data())) {
          results_data()$premium_received[idx]
        } else NULL
      }),
      current_price = reactive({
        req(clicked_row(), results_data())
        idx <- clicked_row()
        if (!is.null(idx) && idx <= nrow(results_data())) {
          results_data()$current_price[idx]
        } else NULL
      }),
      is_aristocrat = reactive(FALSE),
      simulation_paths = reactive(10000)
    )
  })
}

## To be copied in the UI
# mod_monthly_dividend_results_table_ui("monthly_dividend_results_table_1")

## To be copied in the server
# mod_monthly_dividend_results_table_server("monthly_dividend_results_table_1", results_data)
