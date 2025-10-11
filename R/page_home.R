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
  # Define strategy metadata
  strategies_data <- list(
    covered_calls = list(
      list(
        title = "Dividend Aristocrats",
        description = c(
          "Deep ITM covered call strategy for Dividend Aristocrats with interactive filters.",
          "Analyze stocks that have consistently increased dividends for 25+ consecutive years."
        ),
        href = "/aristocrats",
        button_text = "Analyze Aristocrats"
      ),
      list(
        title = "Zero-Dividend Stocks",
        description = c(
          "Deep ITM covered call strategy for S&P 500 growth stocks that don't pay dividends.",
          "Target high-growth stocks like AMZN, GOOGL, META, TSLA. Adjustable strike (50-100%) and expiration (30-365+ days)."
        ),
        href = "/zero-dividend",
        button_text = "Analyze Zero-Dividend"
      ),
      list(
        title = "Dynamic Covered Calls",
        description = c(
          "Intelligent covered call strategy that calculates unique strike and expiration for each stock based on historical drawdown.",
          "Automatically adapts to each stock's volatility profile. High-volatility stocks get deeper strikes and longer expirations."
        ),
        href = "/dynamic-covered-calls",
        button_text = "Analyze Dynamic"
      )
    ),
    collar_strategies = list(
      list(
        title = "Collar Strategy (Synthetic Bonds)",
        description = c(
          "Create risk-free 'synthetic bond' positions with locked returns. Long stock + Short ATM call + Long ATM put.",
          "Works on S&P 500 stocks or liquid ETFs. Receive exactly the strike price at expiration regardless of stock movement."
        ),
        href = "/collar",
        button_text = "Analyze Collars"
      )
    ),
    dividend_capture = list(
      list(
        title = "Weekly Dividend Capture",
        description = c(
          "Analyze dividend capture opportunities for 32 weekly dividend ETFs.",
          "Buy before ex-dividend, sell on ex-dividend day. Backtest historical performance with success rates and risk metrics."
        ),
        href = "/dividend-capture-weekly",
        button_text = "Analyze Weekly ETFs"
      ),
      list(
        title = "Monthly Dividend Capture",
        description = c(
          "Analyze dividend capture opportunities for 76+ monthly dividend ETFs.",
          "Includes covered calls, REITs, BDCs, and Canadian securities. Buy before ex-dividend, sell on ex-dividend day."
        ),
        href = "/dividend-capture-monthly",
        button_text = "Analyze Monthly ETFs"
      ),
      list(
        title = "Russell 2000 High-Yield Dividend Capture",
        description = c(
          "Screen Russell 2000 stocks for high-yield (>10%) dividend opportunities with immediate buying windows.",
          "Filters for stocks with ex-dividend dates allowing purchase today or tomorrow. Backtests historical performance with risk metrics."
        ),
        href = "/dividend-capture-russell-2000",
        button_text = "Analyze Russell 2000"
      )
    ),
    portfolio_management = list(
      list(
        title = "Position Groups",
        description = c(
          "View and manage your position groups with comprehensive tracking and reporting.",
          "Track open positions with projected returns and cash flows. Review closed positions with realized P&L and performance metrics."
        ),
        href = "/portfolio/groups",
        button_text = "View Groups"
      )
    )
  )

  brochure::page(
    href = "/",
    ui = function(request) {
      tagList(
        fluidPage(
          # Page header
          titlePanel("investR - Investment Research"),

          # Main content with accordion organization
          create_home_accordion_section(
            title = "Covered Call Strategies",
            strategies = strategies_data$covered_calls,
            is_open = FALSE
          ),
          create_home_accordion_section(
            title = "Collar Strategies",
            strategies = strategies_data$collar_strategies,
            is_open = FALSE
          ),
          create_home_accordion_section(
            title = "Dividend Capture Strategies",
            strategies = strategies_data$dividend_capture,
            is_open = FALSE
          ),
          create_home_accordion_section(
            title = "Portfolio Management",
            strategies = strategies_data$portfolio_management,
            is_open = FALSE
          )
        )
      )
    }
  )
}