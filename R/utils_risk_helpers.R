#' Risk Analysis UI Helpers
#'
#' Shared functions for risk analysis UI components across strategies.
#' Eliminates code duplication in aristocrats, zero_dividend, and collar modules.
#'
#' @name risk-ui-helpers
NULL

#' Create Risk Analysis Button
#'
#' Creates a standardized "Analyze Risk" button for strategy result cards.
#'
#' @param ns Namespace function from the calling module
#' @param idx Row index for the card (used in button ID)
#' @return tags$div with action button
#' @export
create_risk_analysis_button <- function(ns, idx) {
  tags$div(
    style = "margin-bottom: 15px;",
    actionButton(
      inputId = ns(paste0("analyze_risk_btn_", idx)),
      label = "Analyze Risk",
      icon = icon("chart-line"),
      class = "btn btn-primary btn-sm",
      style = "width: 100%;"
    )
  )
}

#' Setup Risk Analysis Modules
#'
#' Creates the observe() block and mod_position_risk_server() calls for all cards.
#' This pattern is used identically in aristocrats, zero_dividend, and collar modules.
#'
#' @param results_data Reactive containing results data frame
#' @param input Shiny input object
#' @param ns Namespace function
#' @param is_aristocrat Logical indicating if this is aristocrats strategy (default FALSE)
#' @return NULL (called for side effects - creates observers)
#' @export
#' @importFrom shiny observe reactive req
setup_risk_analysis_modules <- function(results_data, input, ns, is_aristocrat = FALSE) {

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
        is_aristocrat = reactive(is_aristocrat),
        simulation_paths = reactive(10000)
      )
    })
  })
}
