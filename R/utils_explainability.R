#' Explainability UI and Logic
#'
#' Provides functions to generate UI components and server logic for
#' explaining complex risk metrics in a user-friendly way.
#'
#' @name explainability
#' @importFrom shiny tags icon
#' @importFrom shinyBS bsTooltip
NULL

#' Add an info icon with a tooltip
#'
#' A helper function to wrap a UI element with an info icon that displays
#' a tooltip on hover.
#'
#' @param ui_element The Shiny UI element to add the icon to.
#' @param tooltip_text The text to display in the tooltip.
#' @param placement The placement of the tooltip (e.g., "top", "right").
#' @return A Shiny UI element.
#' @export
with_info_icon <- function(ui_element, tooltip_text, placement = "top") {
  # Generate a unique ID for the tooltip
  id <- paste0("tooltip_", sample.int(1e9, 1))

  tagList(
    ui_element,
    tags$span(
      id = id,
      icon("info-circle"),
      style = "margin-left: 5px; color: #007bff; cursor: pointer;"
    ),
    bsTooltip(id = id, title = tooltip_text, placement = placement, trigger = "hover")
  )
}


#' Generate explanation for portfolio risk level
#'
#' @param risk_level The calculated risk level ("Low", "Moderate", "High").
#' @param var_pct The VaR as a percentage of portfolio value.
#' @param top_contributors A data frame of the top risk contributors.
#' @return A Shiny UI element.
#' @export
generate_risk_level_explanation <- function(risk_level, var_pct, top_contributors) {
  # Title
  title <- paste("Why is my portfolio risk '", risk_level, "'?")

  # Explanation
  explanation <- sprintf(
    "Your portfolio's risk is considered '%s' because our simulations show that you could lose approximately %.1f%% of your portfolio's value in a severe downturn (the worst 5%% of scenarios).",
    risk_level,
    var_pct * 100
  )

  # Top Risk Drivers
  drivers <- lapply(1:nrow(top_contributors), function(i) {
    row <- top_contributors[i, ]
    tags$li(
      tags$strong(paste0(i, ". Your position in ", row$ticker, ":")),
      sprintf(
        " is contributing %.1f%% of your portfolio's total risk.",
        row$pct_of_portfolio_risk * 100
      )
    )
  })

  # Actionable Advice
  advice <- tagList(
    tags$h5("What can I do about it?"),
    tags$ul(
      tags$li("Consider reducing your exposure to the top risk-contributing positions."),
      tags$li("Diversify your portfolio by adding positions in different sectors."),
      tags$li("Review the Stress Test results to see how your portfolio would perform in a real market crash.")
    )
  )

  # Combine into a single UI element
  tagList(
    h4(title),
    p(explanation),
    tags$h5("Top 3 Risk Drivers"),
    tags$ul(drivers),
    tags$hr(),
    advice
  )
}

#' Generate explanation for early assignment risk
#'
#' @param ticker The stock ticker.
#' @param prob The early assignment probability.
#' @param dividend_date The next dividend date.
#' @param dividend_amount The dividend amount.
#' @param time_value The option's remaining time value.
#' @return A Shiny UI element.
#' @export
generate_early_assignment_explanation <- function(ticker, prob, dividend_date, dividend_amount, time_value) {
  title <- paste("Why is my early assignment risk for", ticker, "so high?")

  explanation <- sprintf(
    "Your covered call on %s has a %.1f%% probability of being assigned early. This is because the dividend you would miss by having your shares called away is greater than the remaining time value of the option.",
    ticker,
    prob * 100
  )

  drivers <- tagList(
    tags$h5("Key Drivers"),
    tags$ul(
      tags$li(paste("Next Ex-Dividend Date:", dividend_date)),
      tags$li(paste("Dividend Amount:", format_currency(dividend_amount))),
      tags$li(paste("Option's Time Value:", format_currency(time_value)))
    )
  )

  advice <- tagList(
    tags$h5("What can I do about it?"),
    tags$ul(
      tags$li(
        tags$strong("If you want to keep the stock and the dividend:"),
        " you should consider 'rolling' the option: buy back the current call and sell a new one with a later expiration date."
      ),
      tags$li(
        tags$strong("If you are okay with selling the stock:"),
        " you don't need to do anything. You will be assigned and sell your shares at the strike price."
      )
    )
  )

  tagList(
    h4(title),
    p(explanation),
    drivers,
    tags$hr(),
    advice
  )
}

#' Generate explanation for stress test results
#'
#' @param ticker The stock ticker.
#' @param sector The stock's sector.
#' @param scenario_name The name of the stress test scenario.
#' @param stock_price_change_pct The percentage change in the stock price.
#' @param premium_received The premium received for the covered call.
#' @param net_loss The net loss in the scenario.
#' @return A Shiny UI element.
#' @export
generate_stress_test_explanation <- function(ticker, sector, scenario_name, stock_price_change_pct, premium_received, net_loss) {
  explanation <- sprintf(
    "In the '%s' scenario, your position in %s (a %s stock) is modeled to lose %.1f%% of its value. Your covered call premium of %s provides some cushion, but it's not enough to offset the large drop in the stock price, resulting in a net loss of %s.",
    scenario_name,
    ticker,
    sector,
    stock_price_change_pct * 100,
    format_currency(premium_received),
    format_currency(net_loss)
  )

  tags$div(
    class = "alert alert-secondary",
    p(explanation)
  )
}
