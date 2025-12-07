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

      # Portfolio Return Summary
      mod_portfolio_return_summary_ui(ns("return_summary")),

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

      # Analysis settings
      wellPanel(
        style = "background-color: #f8f9fa;",
        h5(icon("cog"), " Analysis Settings"),
        checkboxInput(
          ns("enable_regime_detection"),
          "Enable Regime Detection (requires market data access)",
          value = TRUE
        ),
        tags$p(
          style = "color: #666; font-size: 0.85em; margin-top: -10px;",
          "Regime detection adjusts risk parameters based on current market conditions (VIX, correlations).
           Disable if market data is unavailable."
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

    # Initialize Portfolio Return Summary sub-module (no refresh trigger needed - calculates on load)
    mod_portfolio_return_summary_server("return_summary")

    # Reactive to store analysis results
    analysis_results <- reactiveVal(NULL)

    # Loading state
    is_loading <- reactiveVal(FALSE)

    # Observe analyze button
    observeEvent(input$analyze_portfolio, {
      log_info("Portfolio Risk Dashboard: Analyze button clicked")

      # Capture the regime detection setting
      enable_regime <- input$enable_regime_detection
      log_info("Portfolio Risk Dashboard: Regime detection = {enable_regime}")

      is_loading(TRUE)

      # Run analysis asynchronously with 90-second timeout
      future_promise <- future({
        if (!"investR" %in% loadedNamespaces()) {
          suppressPackageStartupMessages(loadNamespace("investR"))
        }

        investR::analyze_portfolio_risk(
          simulation_paths = 10000,
          lookback_days = 252,
          use_regime_detection = enable_regime
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

        # Market Regime (if enabled)
        if (!is.null(results$regime)) {
          fluidRow(
            column(12,
              card(
                card_header(
                  tags$div(
                    icon("cloud"), " Current Market Regime"
                  )
                ),
                card_body(
                  fluidRow(
                    column(3,
                      create_metric_row("Regime", results$regime$name)
                    ),
                    column(5,
                      create_metric_row("Description", results$regime$description)
                    ),
                    column(2,
                      create_metric_row("Risk Multiplier", sprintf("%.1fx", results$regime$risk_multiplier))
                    ),
                    column(2,
                      if (!is.null(results$regime$vix_current) && !is.na(results$regime$vix_current)) {
                        create_metric_row("VIX Level", sprintf("%.1f", results$regime$vix_current))
                      } else {
                        NULL
                      }
                    )
                  )
                )
              )
            )
          )
        } else {
          NULL
        },

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
#' @return DT datatable widget
#' @noRd
#' @importFrom DT datatable formatCurrency formatPercentage formatStyle styleInterval
create_positions_risk_table <- function(positions, contributions, ns) {

  # Merge positions with contributions
  data <- positions %>%
    left_join(contributions, by = c("group_id", "ticker")) %>%
    mutate(
      # Add warning icon for high risk positions (>10% of portfolio risk)
      ticker_display = ifelse(
        !is.na(pct_of_portfolio_risk) & pct_of_portfolio_risk > 0.10,
        paste0("⚠️ ", ticker),
        ticker
      ),
      strike_display = ifelse(!is.na(strike), strike, NA_real_),
      expiration_display = ifelse(!is.na(expiration), as.character(expiration), "—"),
      prob_assignment_display = ifelse(!is.na(prob_assignment), prob_assignment, NA_real_),
      risk_return_display = ifelse(!is.na(risk_return_ratio), risk_return_ratio, NA_real_)
    ) %>%
    select(
      Ticker = ticker_display,
      Strike = strike_display,
      Expiration = expiration_display,
      `Current Value` = current_value,
      `Expected Contribution` = expected_contribution,
      `Prob Assignment` = prob_assignment_display,
      `Risk Contribution` = risk_contribution,
      `% of Portfolio Risk` = pct_of_portfolio_risk,
      `Risk/Return Ratio` = risk_return_display
    )

  # Create DT table with formatting
  DT::datatable(
    data,
    options = list(
      pageLength = -1,  # Show all rows (was 25, hiding positions beyond row 25)
      order = list(list(7, 'desc')),  # Sort by Risk Contribution descending by default
      dom = 't',  # Only show table (no search box or pagination controls for cleaner look)
      scrollX = TRUE,
      columnDefs = list(
        list(targets = 1, visible = TRUE, className = 'dt-right'),  # Strike
        list(targets = 3, className = 'dt-right'),  # Current Value
        list(targets = 4, className = 'dt-right'),  # Expected Contribution
        list(targets = 5, className = 'dt-right'),  # Prob Assignment
        list(targets = 6, className = 'dt-right'),  # Risk Contribution
        list(targets = 7, className = 'dt-right'),  # % of Portfolio Risk
        list(targets = 8, className = 'dt-right')   # Risk/Return Ratio
      )
    ),
    rownames = FALSE,
    class = 'table table-striped table-hover compact',
    escape = FALSE  # Allow HTML in Ticker column for warning icon
  ) %>%
    # Format currency columns
    DT::formatCurrency(c('Strike', 'Current Value', 'Expected Contribution', 'Risk Contribution'), '$') %>%
    # Format percentage columns
    DT::formatPercentage(c('Prob Assignment', '% of Portfolio Risk'), digits = 1) %>%
    # Format Risk/Return Ratio
    DT::formatRound('Risk/Return Ratio', digits = 2) %>%
    # Color code Expected Contribution (green for positive, red for negative)
    DT::formatStyle(
      'Expected Contribution',
      color = DT::styleInterval(c(0), c('#dc3545', '#28a745'))
    ) %>%
    # Highlight high risk positions (>10% of portfolio risk)
    DT::formatStyle(
      '% of Portfolio Risk',
      backgroundColor = DT::styleInterval(c(0.10), c('white', '#fff3cd'))
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
