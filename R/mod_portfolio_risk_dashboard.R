#' portfolio_risk_dashboard UI Function
#'
#' @description A shiny Module for portfolio-level risk analysis dashboard.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_portfolio_risk_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      # Page header
      titlePanel("Portfolio Risk Analysis"),

      # Description
      wellPanel(
        style = "background-color: #f8f9fa; border-left: 4px solid #007bff;",
        h4(icon("shield-alt"), " Portfolio Risk Management"),
        p("Analyze your entire options portfolio using academically rigorous correlated Monte Carlo simulation.
          Runs 10,000 simulations accounting for correlations between positions."),
        tags$ul(
          tags$li(strong("Expected Contribution:"), " Shows how much each position contributes to expected portfolio return (return attribution)"),
          tags$li(strong("Risk Contribution (Component VaR):"), " Shows how much each position contributes to portfolio downside risk using Component VaR methodology"),
          tags$li(strong("VaR/CVaR:"), " Portfolio value at risk at 95% and 99% confidence levels"),
          tags$li(strong("Stress Testing:"), " Portfolio response to 5 historical crisis scenarios (2008, COVID, Rising Rates, Stagflation, Vol Spike)"),
          tags$li(strong("Concentration:"), " Exposure by ticker (>25% flagged) and sector (>40% flagged)"),
          tags$li(strong("Correlation Matrix:"), " Uses Cholesky decomposition of historical return correlations (1-year lookback)")
        ),
        tags$p(
          style = "margin-top: 10px; color: #666; font-size: 0.9em;",
          tags$em("Note: Analysis typically takes 20-30 seconds. Correlations are estimated from historical price data;
                  positions with insufficient data use equity market average (0.3).")
        )
      ),

      # Analyze button
      actionButton(
        ns("analyze_portfolio"),
        "Analyze Portfolio Risk",
        class = "btn btn-primary btn-lg",
        icon = icon("chart-line"),
        style = "margin-bottom: 20px;"
      ),

      # Loading indicator
      uiOutput(ns("loading_indicator")),

      # Results
      uiOutput(ns("dashboard_content"))
    )
  )
}

#' portfolio_risk_dashboard Server Functions
#'
#' @noRd
#'
#' @importFrom shiny moduleServer observeEvent renderUI req showNotification
#' @importFrom promises future_promise %...>% %...!%
#' @importFrom future plan multisession future
#' @importFrom dplyr %>% arrange desc
#' @importFrom bslib value_box card card_header card_body
mod_portfolio_risk_dashboard_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Reactive to store analysis results
    analysis_results <- reactiveVal(NULL)

    # Loading state
    is_loading <- reactiveVal(FALSE)

    # Observe analyze button
    observeEvent(input$analyze_portfolio, {
      log_info("Portfolio Risk Dashboard: Analyze button clicked")

      is_loading(TRUE)

      # Run analysis asynchronously with 90-second timeout
      future_promise <- future({
        if (!"investR" %in% loadedNamespaces()) {
          suppressPackageStartupMessages(loadNamespace("investR"))
        }

        investR::analyze_portfolio_risk(
          simulation_paths = 10000,
          lookback_days = 252
        )
      }, seed = TRUE, timeout = 90)

      # Handle results
      future_promise %...>% (function(results) {
        is_loading(FALSE)

        if (!is.null(results$error)) {
          showNotification(
            paste("Error:", results$error),
            type = "error",
            duration = 10
          )
          analysis_results(NULL)
        } else {
          analysis_results(results)
          showNotification(
            "Portfolio analysis complete!",
            type = "message",
            duration = 3
          )
        }

      }) %...!% (function(error) {
        is_loading(FALSE)

        # Check if this was a timeout error
        if (grepl("timeout|timed out", error$message, ignore.case = TRUE)) {
          showNotification(
            paste(
              "Analysis timed out after 90 seconds.",
              "This may indicate API issues or an unusually large portfolio.",
              "Please try again or contact support if the problem persists."
            ),
            type = "warning",
            duration = 15
          )
          log_warn("Portfolio Risk Dashboard: Analysis timed out after 90 seconds")
        } else {
          showNotification(
            paste("Analysis failed:", error$message),
            type = "error",
            duration = 10
          )
          log_error("Portfolio Risk Dashboard: Analysis failed - {error$message}")
        }
      })
    })

    # Render loading indicator
    output$loading_indicator <- renderUI({
      if (is_loading()) {
        wellPanel(
          style = "text-align: center; padding: 40px;",
          tags$div(
            class = "spinner-border text-primary",
            role = "status",
            style = "width: 3rem; height: 3rem;",
            tags$span(class = "sr-only", "Loading...")
          ),
          tags$p(
            style = "margin-top: 20px; font-size: 16px;",
            "Running correlated Monte Carlo simulation..."
          ),
          tags$p(
            style = "color: #666;",
            "This may take 20-30 seconds depending on portfolio size."
          )
        )
      }
    })

    # Render dashboard content
    output$dashboard_content <- renderUI({
      results <- analysis_results()

      if (is.null(results)) {
        return(wellPanel(
          h4("No analysis results yet"),
          p("Click 'Analyze Portfolio Risk' to run the analysis.")
        ))
      }

      tagList(
        # Summary cards
        fluidRow(
          column(3,
            value_box(
              title = "Total Positions",
              value = results$total_positions,
              showcase = icon("briefcase"),
              theme = "primary"
            )
          ),
          column(3,
            value_box(
              title = "Portfolio Value",
              value = format_currency(results$total_value),
              showcase = icon("dollar-sign"),
              theme = "success"
            )
          ),
          column(3,
            value_box(
              title = "VaR 95%",
              value = format_currency(results$var_95),
              showcase = icon("exclamation-triangle"),
              theme = if (results$risk_level == "High") "danger" else if (results$risk_level == "Moderate") "warning" else "info"
            )
          ),
          column(3,
            value_box(
              title = "Risk Level",
              value = results$risk_level,
              showcase = icon("shield-alt"),
              theme = if (results$risk_level == "High") "danger" else if (results$risk_level == "Moderate") "warning" else "success"
            )
          )
        ),

        # Additional metrics
        fluidRow(
          column(12,
            card(
              card_header("Portfolio Risk Metrics"),
              card_body(
                fluidRow(
                  column(4,
                    create_metric_row("Expected Return", format_currency(results$expected_return)),
                    create_metric_row("Median Return", format_currency(results$median_return)),
                    create_metric_row("Probability of Loss", format_percentage(results$prob_loss))
                  ),
                  column(4,
                    create_metric_row("VaR 99%", format_currency(results$var_99)),
                    create_metric_row("CVaR 95%", format_currency(results$cvar_95)),
                    create_metric_row("CVaR 99%", format_currency(results$cvar_99))
                  ),
                  column(4,
                    create_metric_row("5th Percentile", format_currency(results$percentile_5)),
                    create_metric_row("95th Percentile", format_currency(results$percentile_95)),
                    create_metric_row("Std Deviation", format_currency(results$sd_return))
                  )
                )
              )
            )
          )
        ),

        # Portfolio P&L Distribution
        fluidRow(
          column(12,
            card(
              card_header("Portfolio P&L Distribution"),
              card_body(
                tags$p("Distribution of portfolio profit/loss across ", format(results$simulation_paths, big.mark = ","), " Monte Carlo simulations:"),
                create_portfolio_distribution_plot(results$portfolio_pnl, results$var_95, results$median_return, results$percentile_95)
              )
            )
          )
        ),

        # Positions table
        fluidRow(
          column(12,
            card(
              card_header("Position Risk Contributions"),
              card_body(
                create_positions_risk_table(
                  results$positions,
                  results$position_contributions,
                  ns
                )
              )
            )
          )
        ),

        # Stress tests
        fluidRow(
          column(12,
            card(
              card_header("Stress Test Results"),
              card_body(
                create_stress_test_table(results$stress_results)
              )
            )
          )
        ),

        # Concentration
        fluidRow(
          column(6,
            card(
              card_header("Concentration by Ticker"),
              card_body(
                create_concentration_table(results$concentration$by_ticker, "Ticker")
              )
            )
          ),
          column(6,
            card(
              card_header("Concentration by Sector"),
              card_body(
                create_concentration_table(results$concentration$by_sector, "Sector")
              )
            )
          )
        ),

        # Concentration alerts
        if (length(results$concentration$alerts) > 0) {
          fluidRow(
            column(12,
              tags$div(
                class = "alert alert-warning",
                style = "margin-top: 20px;",
                icon("exclamation-triangle"),
                strong(" Concentration Alerts:"),
                tags$ul(
                  purrr::map(results$concentration$alerts, ~tags$li(.x))
                )
              )
            )
          )
        }
      )
    })
  })
}

################################################################################
# HELPER FUNCTIONS FOR UI COMPONENTS
################################################################################

#' Create positions risk table
#'
#' Shows both Expected Contribution (return attribution) and Risk Contribution (Component VaR).
#'
#' @param positions Tibble of position details
#' @param contributions Tibble of risk contributions (with expected_contribution and risk_contribution)
#' @param ns Namespace function
#' @return HTML table
#' @noRd
create_positions_risk_table <- function(positions, contributions, ns) {

  # Merge positions with contributions
  data <- positions %>%
    left_join(contributions, by = c("group_id", "ticker"))

  # Create sortable table
  tags$table(
    class = "table table-striped table-hover",
    tags$thead(
      tags$tr(
        tags$th("Ticker"),
        tags$th("Strike"),
        tags$th("Expiration"),
        tags$th("Current Value"),
        tags$th(
          tags$span(
            "Expected Contribution",
            tags$br(),
            tags$small(style = "font-weight: normal; color: #666;", "(Return Attribution)")
          )
        ),
        tags$th(
          tags$span(
            "Risk Contribution",
            tags$br(),
            tags$small(style = "font-weight: normal; color: #666;", "(Component VaR)")
          )
        ),
        tags$th("% of Portfolio Risk"),
        tags$th(
          tags$span(
            "Risk/Return Ratio",
            tags$br(),
            tags$small(style = "font-weight: normal; color: #666;", "(Lower is Better)")
          )
        )
      )
    ),
    tags$tbody(
      purrr::map(seq_len(nrow(data)), function(i) {
        row <- data[i, ]

        # Color-code risk contribution if >10% of portfolio risk
        is_high_risk <- !is.na(row$pct_of_portfolio_risk) && row$pct_of_portfolio_risk > 0.10

        # Color-code based on expected contribution (green for positive, red for negative)
        exp_contrib_color <- if (!is.na(row$expected_contribution)) {
          if (row$expected_contribution > 0) "#28a745" else "#dc3545"
        } else {
          "#000"
        }

        tags$tr(
          style = if (is_high_risk) "background-color: #fff3cd;" else "",
          tags$td(
            if (is_high_risk) {
              tags$span(icon("exclamation-triangle"), " ", row$ticker)
            } else {
              row$ticker
            }
          ),
          tags$td(if (!is.na(row$strike)) format_currency(row$strike) else "—"),
          tags$td(if (!is.na(row$expiration)) as.character(row$expiration) else "—"),
          tags$td(format_currency(row$current_value)),
          tags$td(
            style = paste0("color: ", exp_contrib_color, ";"),
            format_currency(row$expected_contribution)
          ),
          tags$td(format_currency(row$risk_contribution)),
          tags$td(format_percentage(row$pct_of_portfolio_risk)),
          tags$td(
            if (!is.na(row$risk_return_ratio)) sprintf("%.2f", row$risk_return_ratio) else "—"
          )
        )
      })
    )
  )
}

#' Create stress test results table
#'
#' @param stress_results Tibble of stress test results
#' @return HTML table
#' @noRd
create_stress_test_table <- function(stress_results) {

  tags$table(
    class = "table table-striped",
    tags$thead(
      tags$tr(
        tags$th("Scenario"),
        tags$th("Portfolio P&L"),
        tags$th("Portfolio Return %")
      )
    ),
    tags$tbody(
      purrr::map(seq_len(nrow(stress_results)), function(i) {
        row <- stress_results[i, ]
        is_negative <- row$portfolio_pnl < 0

        tags$tr(
          tags$td(row$scenario),
          tags$td(
            style = if (is_negative) "color: #dc3545;" else "color: #28a745;",
            format_currency(row$portfolio_pnl)
          ),
          tags$td(
            style = if (is_negative) "color: #dc3545;" else "color: #28a745;",
            format_percentage(row$portfolio_return_pct)
          )
        )
      })
    )
  )
}

#' Create concentration table
#'
#' @param concentration_data Tibble with concentration data
#' @param label_col Name of the grouping column
#' @return HTML table
#' @noRd
create_concentration_table <- function(concentration_data, label_col) {

  if (nrow(concentration_data) == 0) {
    return(tags$p(class = "text-muted", "No data available"))
  }

  # Take top 10
  top_data <- concentration_data %>% head(10)

  tags$table(
    class = "table table-sm",
    tags$thead(
      tags$tr(
        tags$th(label_col),
        tags$th("Value"),
        tags$th("% of Portfolio"),
        tags$th("Positions")
      )
    ),
    tags$tbody(
      purrr::map(seq_len(nrow(top_data)), function(i) {
        row <- top_data[i, ]
        pct <- row$pct_of_portfolio

        # Alert if concentration > 25% (ticker) or 40% (sector)
        alert_threshold <- if (label_col == "Ticker") 0.25 else 0.40
        is_high <- pct > alert_threshold

        tags$tr(
          style = if (is_high) "background-color: #fff3cd;" else "",
          tags$td(
            if (is_high) tags$span(icon("exclamation-triangle"), " ", row[[tolower(label_col)]]) else row[[tolower(label_col)]]
          ),
          tags$td(format_currency(row$total_value)),
          tags$td(format_percentage(pct)),
          tags$td(row$n_positions)
        )
      })
    )
  )
}

#' Create portfolio P&L distribution plot
#'
#' @param portfolio_pnl Vector of portfolio P&L from Monte Carlo
#' @param var_95 Portfolio VaR (5th percentile)
#' @param median_pnl Median P&L
#' @param percentile_95 95th percentile P&L
#' @return Plotly widget
#' @noRd
#' @importFrom plotly plot_ly layout add_trace
create_portfolio_distribution_plot <- function(portfolio_pnl, var_95, median_pnl, percentile_95) {

  # Create plotly histogram
  p <- plotly::plot_ly() %>%
    plotly::add_histogram(
      x = portfolio_pnl,
      nbinsx = 50,
      name = "Portfolio P&L Distribution",
      marker = list(color = "#007bff", line = list(color = "white", width = 1))
    ) %>%
    plotly::add_trace(
      x = rep(var_95, 2),
      y = c(0, 1),
      type = "scatter",
      mode = "lines",
      name = "VaR 95% (5th %ile)",
      line = list(color = "red", width = 3, dash = "dash"),
      yaxis = "y2",
      showlegend = TRUE
    ) %>%
    plotly::add_trace(
      x = rep(median_pnl, 2),
      y = c(0, 1),
      type = "scatter",
      mode = "lines",
      name = "Median",
      line = list(color = "green", width = 3),
      yaxis = "y2",
      showlegend = TRUE
    ) %>%
    plotly::add_trace(
      x = rep(percentile_95, 2),
      y = c(0, 1),
      type = "scatter",
      mode = "lines",
      name = "95th %ile",
      line = list(color = "red", width = 3, dash = "dash"),
      yaxis = "y2",
      showlegend = TRUE
    ) %>%
    plotly::layout(
      xaxis = list(title = "Portfolio P&L ($)"),
      yaxis = list(title = "Frequency"),
      yaxis2 = list(overlaying = "y", side = "right", showticklabels = FALSE, showgrid = FALSE),
      showlegend = TRUE,
      hovermode = "x",
      height = 400
    )

  p
}

## To be copied in the UI
# mod_portfolio_risk_dashboard_ui("portfolio_risk_dashboard_1")

## To be copied in the server
# mod_portfolio_risk_dashboard_server("portfolio_risk_dashboard_1")
