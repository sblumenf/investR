#' Home Page
#'
#' This function creates the home page for the investR application.
#' Each Brochure page has its own independent Shiny session.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_home <- function() {
  brochure::page(
    href = "/",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("investR - Investment Research"),

          # Main content
          fluidRow(
                column(6,
                  div(class = "well",
                    h4("Dividend Aristocrats"),
                    p("Deep ITM covered call strategy for Dividend Aristocrats with interactive filters."),
                    p("Analyze stocks that have consistently increased dividends for 25+ consecutive years."),
                    tags$a(
                      href = "/aristocrats",
                      class = "btn btn-primary btn-lg",
                      "Analyze Aristocrats"
                    )
                  )
                ),
                column(6,
                  div(class = "well",
                    h4("Weekly Dividend Capture"),
                    p("Analyze dividend capture opportunities for 32 weekly dividend ETFs."),
                    p("Buy before ex-dividend, sell on ex-dividend day. Backtest historical performance with success rates and risk metrics."),
                    tags$a(
                      href = "/dividend-capture-weekly",
                      class = "btn btn-primary btn-lg",
                      "Analyze Weekly ETFs"
                    )
                  )
                )
              ),
          fluidRow(
                column(6,
                  div(class = "well",
                    h4("Monthly Dividend Capture"),
                    p("Analyze dividend capture opportunities for 76+ monthly dividend ETFs."),
                    p("Includes covered calls, REITs, BDCs, and Canadian securities. Buy before ex-dividend, sell on ex-dividend day."),
                    tags$a(
                      href = "/dividend-capture-monthly",
                      class = "btn btn-primary btn-lg",
                      "Analyze Monthly ETFs"
                    )
                  )
                ),
                column(6,
                  div(class = "well",
                    h4("Zero-Dividend Stocks"),
                    p("Deep ITM covered call strategy for S&P 500 growth stocks that don't pay dividends."),
                    p("Target high-growth stocks like AMZN, GOOGL, META, TSLA. Adjustable strike (50-100%) and expiration (30-365+ days)."),
                    tags$a(
                      href = "/zero-dividend",
                      class = "btn btn-primary btn-lg",
                      "Analyze Zero-Dividend"
                    )
                  )
                )
              ),
          fluidRow(
                column(6,
                  div(class = "well",
                    h4("Russell 2000 High-Yield Dividend Capture"),
                    p("Screen Russell 2000 stocks for high-yield (>10%) dividend opportunities with immediate buying windows."),
                    p("Filters for stocks with ex-dividend dates allowing purchase today or tomorrow. Backtests historical performance with risk metrics."),
                    tags$a(
                      href = "/dividend-capture-russell-2000",
                      class = "btn btn-primary btn-lg",
                      "Analyze Russell 2000"
                    )
                  )
                ),
                column(6,
                  div(class = "well",
                    h4("Dynamic Covered Calls"),
                    p("Intelligent covered call strategy that calculates unique strike and expiration for each stock based on historical drawdown."),
                    p("Automatically adapts to each stock's volatility profile. High-volatility stocks get deeper strikes and longer expirations."),
                    tags$a(
                      href = "/dynamic-covered-calls",
                      class = "btn btn-primary btn-lg",
                      "Analyze Dynamic"
                    )
                  )
                )
              ),
          fluidRow(
                column(6,
                  div(class = "well",
                    h4("Collar Strategy (Synthetic Bonds)"),
                    p("Create risk-free 'synthetic bond' positions with locked returns. Long stock + Short ATM call + Long ATM put."),
                    p("Works on S&P 500 stocks or liquid ETFs. Receive exactly the strike price at expiration regardless of stock movement."),
                    tags$a(
                      href = "/collar",
                      class = "btn btn-primary btn-lg",
                      "Analyze Collars"
                    )
                  )
                )
              )
        )
      )
    }
  )
}