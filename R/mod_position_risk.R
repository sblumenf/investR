#' Position Risk Analysis Module
#'
#' Shiny module for displaying position risk analysis in a modal dialog.
#' Triggered by "Analyze Risk" buttons in strategy results.
#'
#' @name mod_position_risk
#' @importFrom shiny NS moduleServer observeEvent showModal modalDialog removeModal
#' @importFrom shiny renderUI renderPlot req tags h4 p strong tabsetPanel tabPanel
#' @importFrom promises promise %...>% %...!%
#' @importFrom future future
NULL

#' Position Risk Analysis UI
#'
#' Creates UI elements for risk analysis (minimal, as it's triggered by button)
#'
#' @param id Module namespace ID
#' @noRd
mod_position_risk_ui <- function(id) {
  ns <- NS(id)
  # No static UI - everything is in the modal
  tagList()
}

#' Position Risk Analysis Server
#'
#' Server logic for risk analysis modal. Runs async analysis and displays results.
#'
#' @param id Module namespace ID
#' @param trigger Reactive trigger (button click)
#' @param ticker Reactive ticker symbol
#' @param strike Reactive strike price
#' @param expiration Reactive expiration date
#' @param premium_received Reactive premium received
#' @param current_price Reactive current price (optional)
#' @param cost_basis Reactive cost basis (optional, for actual positions)
#' @param is_aristocrat Reactive logical for aristocrat status
#' @param simulation_paths Reactive number of paths (default 10000)
#' @noRd
mod_position_risk_server <- function(id,
                                    trigger,
                                    ticker,
                                    strike,
                                    expiration,
                                    premium_received,
                                    current_price = reactive(NULL),
                                    cost_basis = reactive(NULL),
                                    is_aristocrat = reactive(FALSE),
                                    simulation_paths = reactive(10000)) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to store analysis results
    analysis_results <- reactiveVal(NULL)

    # Observe trigger (button click)
    observeEvent(trigger(), {
      req(ticker(), strike(), expiration(), premium_received())

      log_info("Risk analysis triggered for {ticker()}")

      # Show loading modal immediately
      showModal(modalDialog(
        title = paste("Analyzing Risk:", ticker()),
        tags$div(
          style = "text-align: center; padding: 40px;",
          tags$div(
            class = "spinner-border text-primary",
            role = "status",
            tags$span(class = "sr-only", "Loading...")
          ),
          tags$p(
            style = "margin-top: 20px;",
            paste0("Running Monte Carlo simulation with ", simulation_paths(), " paths...")
          ),
          tags$p("This may take 5-10 seconds...")
        ),
        footer = NULL,
        size = "m",
        easyClose = FALSE
      ))

      # Run analysis asynchronously
      future_promise <- future({
        if (!"investR" %in% loadedNamespaces()) {
          suppressPackageStartupMessages(loadNamespace("investR"))
        }

        investR::analyze_position_risk(
          ticker = ticker(),
          strike = strike(),
          expiration = expiration(),
          premium_received = premium_received(),
          current_price = current_price(),
          cost_basis = cost_basis(),
          simulation_paths = simulation_paths(),
          is_aristocrat = is_aristocrat()
        )
      }, seed = TRUE)

      # Handle results
      future_promise %...>% (function(results) {
        analysis_results(results)

        # Show results modal
        showModal(create_risk_results_modal(results, ns))

      }) %...!% (function(error) {
        # Handle error
        showModal(modalDialog(
          title = paste("Error Analyzing", ticker()),
          tags$div(
            class = "alert alert-danger",
            tags$strong("Analysis Failed"),
            tags$p(error$message)
          ),
          footer = modalButton("Close"),
          easyClose = TRUE
        ))
      })

      NULL  # Return NULL from observeEvent
    })

    # Return analysis results for potential external use
    return(reactive({ analysis_results() }))
  })
}

################################################################################
# MODAL BUILDERS
################################################################################

#' Create modal dialog with risk analysis results
#'
#' @param results Results from analyze_position_risk()
#' @param ns Namespace function
#' @return Modal dialog
#' @noRd
create_risk_results_modal <- function(results, ns) {

  modalDialog(
    title = paste("Risk Analysis:", results$ticker),
    size = "xl",
    easyClose = TRUE,

    tabsetPanel(
      id = ns("risk_tabs"),

      # Tab 1: Summary
      tabPanel(
        "Summary",
        value = "summary",
        create_summary_tab(results)
      ),

      # Tab 2: Monte Carlo Results
      tabPanel(
        "Distribution",
        value = "distribution",
        create_distribution_tab(results)
      ),

      # Tab 3: Dividend Timeline
      tabPanel(
        "Dividend Timeline",
        value = "timeline",
        create_timeline_tab(results)
      ),

      # Tab 4: Stress Tests
      tabPanel(
        "Stress Tests",
        value = "stress",
        create_stress_tab(results)
      ),

      # Tab 5: Greeks & Details
      tabPanel(
        "Greeks & Details",
        value = "details",
        create_details_tab(results)
      )
    ),

    footer = modalButton("Close")
  )
}

#' Create summary tab content
#'
#' @param results Analysis results
#' @return HTML tags
#' @noRd
create_summary_tab <- function(results) {

  # Extract key metrics
  mc <- results$monte_carlo
  rql <- results$rquantlib

  tags$div(
    style = "padding: 20px;",

    # Return Metrics (Most Important - Top)
    tags$h5("Return Metrics"),
    if (!is.null(mc) && !is.null(results$risk_adjusted_return_annualized)) {
      tags$div(
        create_metric_row("Expected Return (annualized)",
                        sprintf("%.2f%%", results$risk_adjusted_return_annualized * 100)),
        create_metric_row("Median Return",
                        sprintf("%.2f%%", mc$median_return * 100)),
        create_metric_row("Probability of Profit",
                        sprintf("%.1f%%", mc$prob_profit * 100))
      )
    } else {
      tags$p(class = "text-muted", "Return metrics not available")
    },

    tags$hr(),

    # Return Distribution
    tags$h5("Return Distribution"),
    if (!is.null(mc)) {
      tags$div(
        create_metric_row("Best Case (95th percentile)",
                        sprintf("%.2f%%", mc$percentile_95 * 100)),
        create_metric_row("Worst Case (5th percentile)",
                        sprintf("%.2f%%", mc$percentile_5 * 100))
      )
    } else {
      tags$p(class = "text-muted", "Distribution not available")
    },

    tags$hr(),

    # Position Details
    tags$h5("Position Details"),
    tags$div(
      class = "row",
      tags$div(
        class = "col-md-6",
        create_metric_row("Current Stock Price", sprintf("$%.2f", results$current_price)),
        create_metric_row("Purchase Price (Cost Basis)", sprintf("$%.2f", results$purchase_price)),
        create_metric_row("Strike Price", sprintf("$%.2f", results$strike)),
        create_metric_row("Premium Received", sprintf("$%.2f", results$premium_received))
      ),
      tags$div(
        class = "col-md-6",
        create_metric_row("Days to Expiration", as.character(results$days_to_expiry)),
        create_metric_row("Expiration Date", results$expiration),
        create_metric_row("Dividend Status", if (results$is_aristocrat) "Dividend Aristocrat" else "Regular Dividend Payer")
      )
    ),

    tags$hr(),

    # Assignment Risk
    tags$h5("Assignment Risk"),
    if (!is.null(mc) && !is.null(mc$early_exercise_prob)) {
      tags$div(
        create_metric_row("Probability of Early Assignment",
                        sprintf("%.1f%%", mc$early_exercise_prob * 100)),
        create_metric_row("Most Likely Outcome",
                        if (mc$early_exercise_prob > 0.5) "Assigned before expiration" else "Held to expiration")
      )
    } else {
      tags$p(class = "text-muted", "Assignment probability not available")
    },

    tags$hr(),

    # Volatility & Simulation Details
    tags$h5("Volatility & Simulation Details"),
    if (!is.null(mc)) {
      tags$div(
        create_metric_row("Implied Volatility", sprintf("%.1f%%", mc$implied_volatility * 100)),
        create_metric_row("Simulation Model", mc$model),
        create_metric_row("Number of Paths", format(results$simulation_paths, big.mark = ","))
      )
    } else {
      tags$p(class = "text-muted", "Simulation details not available")
    }
  )
}

#' Create distribution tab content
#'
#' @param results Analysis results
#' @return HTML tags
#' @noRd
create_distribution_tab <- function(results) {

  if (is.null(results$monte_carlo)) {
    return(tags$div(
      class = "alert alert-info",
      "Monte Carlo simulation data not available"
    ))
  }

  mc <- results$monte_carlo

  tags$div(
    style = "padding: 20px;",

    tags$h5("Return Distribution from Monte Carlo Simulation"),
    tags$p("Distribution of possible returns across ", format(mc$n_paths, big.mark = ","), " simulated scenarios:"),

    # Simple histogram using plotly
    tags$div(
      create_return_distribution_plot(mc)
    ),

    tags$hr(),

    tags$div(
      class = "row",
      tags$div(
        class = "col-md-6",
        tags$h6("Scenarios Breakdown"),
        create_metric_row("Held to Expiration", sprintf("%.1f%% of paths", (1 - mc$early_exercise_prob) * 100)),
        create_metric_row("Assigned Early", sprintf("%.1f%% of paths", mc$early_exercise_prob * 100)),
        create_metric_row("Profitable Scenarios", sprintf("%.1f%% of paths", mc$prob_profit * 100))
      ),
      tags$div(
        class = "col-md-6",
        tags$h6("Expected Payoffs"),
        if (!is.na(mc$avg_payoff_if_held)) {
          create_metric_row("Average if Held", sprintf("$%.2f", mc$avg_payoff_if_held))
        },
        if (!is.na(mc$avg_payoff_if_exercised)) {
          create_metric_row("Average if Assigned Early", sprintf("$%.2f", mc$avg_payoff_if_exercised))
        }
      )
    )
  )
}

#' Create timeline tab content
#'
#' @param results Analysis results
#' @return HTML tags
#' @noRd
create_timeline_tab <- function(results) {

  div_schedule <- results$dividend_schedule

  if (nrow(div_schedule) == 0) {
    return(tags$div(
      class = "alert alert-info",
      "No dividends projected during option life (zero-dividend stock)"
    ))
  }

  tags$div(
    style = "padding: 20px;",

    tags$h5("Projected Dividend Schedule"),
    tags$p(sprintf("Based on historical dividend pattern for %s", results$ticker)),

    # Simple table for now (could enhance with timeline visualization)
    tags$table(
      class = "table table-striped",
      tags$thead(
        tags$tr(
          tags$th("Dividend Date"),
          tags$th("Amount"),
          tags$th("Days Until"),
          tags$th("Confidence")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(div_schedule)), function(i) {
          row <- div_schedule[i, ]
          tags$tr(
            tags$td(as.character(row$dividend_date)),
            tags$td(sprintf("$%.2f", row$dividend_amount)),
            tags$td(as.character(row$days_until)),
            tags$td(
              tags$span(
                class = paste("badge",
                            if (row$confidence == "high") "badge-success"
                            else if (row$confidence == "medium") "badge-warning"
                            else "badge-secondary"),
                row$confidence
              )
            )
          )
        })
      )
    ),

    tags$p(
      class = "text-muted small",
      "Confidence levels: ",
      tags$strong("High"), " = Dividend aristocrat with consistent growth, ",
      tags$strong("Medium"), " = Regular payer with 4+ quarters history, ",
      tags$strong("Low"), " = Limited history"
    )
  )
}

#' Create stress test tab content
#'
#' @param results Analysis results
#' @return HTML tags
#' @noRd
create_stress_tab <- function(results) {

  stress <- results$stress_tests

  if (is.null(stress) || nrow(stress) == 0) {
    return(tags$div(
      class = "alert alert-info",
      "Stress test results not available"
    ))
  }

  tags$div(
    style = "padding: 20px;",

    tags$h5("Stress Test Results"),
    tags$p("How this position would perform under historical crisis scenarios:"),

    tags$table(
      class = "table table-hover",
      tags$thead(
        tags$tr(
          tags$th("Scenario"),
          tags$th("Stock Price Change"),
          tags$th("Stressed Price"),
          tags$th("Position P&L"),
          tags$th("Return"),
          tags$th("Early Exercise Impact")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(stress)), function(i) {
          row <- stress[i, ]
          pnl_class <- if (row$position_pnl > 0) "text-success" else "text-danger"

          tags$tr(
            tags$td(tags$strong(row$scenario)),
            tags$td(
              tags$span(
                class = if (row$stock_price_change_pct < 0) "text-danger" else "text-success",
                sprintf("%+.1f%%", row$stock_price_change_pct * 100)
              )
            ),
            tags$td(sprintf("$%.2f", row$stressed_stock_price)),
            tags$td(
              tags$span(
                class = pnl_class,
                sprintf("$%.2f", row$position_pnl)
              )
            ),
            tags$td(
              tags$span(
                class = pnl_class,
                sprintf("%+.1f%%", row$position_return_pct * 100)
              )
            ),
            tags$td(tags$small(row$early_exercise_impact))
          )
        })
      )
    ),

    tags$div(
      class = "alert alert-secondary",
      tags$strong("Key Insight: "),
      "Covered calls provide downside cushion from premium received, ",
      "but gains are capped at strike price. In severe downturns, premium may not ",
      "fully offset stock losses."
    )
  )
}

#' Create details tab content
#'
#' @param results Analysis results
#' @return HTML tags
#' @noRd
create_details_tab <- function(results) {

  rql <- results$rquantlib

  tags$div(
    style = "padding: 20px;",

    tags$h5("Option Greeks"),
    if (!is.null(rql) && rql$success) {
      tags$div(
        tags$p("Sensitivity metrics from RQuantLib (using discrete dividends):"),
        tags$div(
          class = "row",
          tags$div(
            class = "col-md-6",
            create_metric_row("Delta", sprintf("%.4f", rql$delta),
                            is_primary = TRUE),
            tags$p(class = "text-muted small", "Change in option value per $1 stock move"),
            create_metric_row("Gamma", sprintf("%.4f", rql$gamma)),
            tags$p(class = "text-muted small", "Rate of change of delta"),
            create_metric_row("Vega", sprintf("%.4f", rql$vega)),
            tags$p(class = "text-muted small", "Change per 1% volatility increase")
          ),
          tags$div(
            class = "col-md-6",
            create_metric_row("Theta", sprintf("%.4f", rql$theta),
                            is_primary = TRUE),
            tags$p(class = "text-muted small", "Time decay per day (this is your friend!)"),
            create_metric_row("Rho", sprintf("%.4f", rql$rho)),
            tags$p(class = "text-muted small", "Change per 1% interest rate increase"),
            create_metric_row("Option Value", sprintf("$%.2f", rql$value)),
            tags$p(class = "text-muted small", "Theoretical fair value")
          )
        )
      )
    } else {
      tags$div(
        class = "alert alert-warning",
        "Greeks calculation failed: ", if (!is.null(rql)) rql$error else "Unknown error"
      )
    },

    tags$hr(),

    tags$h5("Technical Details"),
    tags$div(
      create_metric_row("Simulation Model", results$monte_carlo$model %||% "N/A"),
      create_metric_row("Number of Paths", format(results$simulation_paths, big.mark = ",")),
      create_metric_row("Implied Volatility", sprintf("%.1f%%", (results$monte_carlo$implied_volatility %||% 0) * 100)),
      create_metric_row("Risk-Free Rate", sprintf("%.2f%%", RISK_CONFIG$risk_free_rate * 100))
    )
  )
}

#' Create return distribution plot
#'
#' @param mc Monte Carlo results
#' @return Plotly widget
#' @noRd
#' @importFrom plotly plot_ly layout add_trace
create_return_distribution_plot <- function(mc) {

  # Create histogram data
  returns_pct <- mc$returns * 100

  # Create plotly histogram
  p <- plotly::plot_ly() %>%
    plotly::add_histogram(
      x = returns_pct,
      nbinsx = 50,
      name = "Return Distribution",
      marker = list(color = "#007bff", line = list(color = "white", width = 1))
    ) %>%
    plotly::add_trace(
      x = rep(mc$percentile_5 * 100, 2),
      y = c(0, 1),
      type = "scatter",
      mode = "lines",
      name = "5th %ile",
      line = list(color = "red", width = 2, dash = "dash"),
      yaxis = "y2",
      showlegend = TRUE
    ) %>%
    plotly::add_trace(
      x = rep(mc$median_return * 100, 2),
      y = c(0, 1),
      type = "scatter",
      mode = "lines",
      name = "Median",
      line = list(color = "green", width = 2),
      yaxis = "y2",
      showlegend = TRUE
    ) %>%
    plotly::add_trace(
      x = rep(mc$percentile_95 * 100, 2),
      y = c(0, 1),
      type = "scatter",
      mode = "lines",
      name = "95th %ile",
      line = list(color = "red", width = 2, dash = "dash"),
      yaxis = "y2",
      showlegend = TRUE
    ) %>%
    plotly::layout(
      xaxis = list(title = "Return (%)"),
      yaxis = list(title = "Frequency"),
      yaxis2 = list(overlaying = "y", side = "right", showticklabels = FALSE, showgrid = FALSE),
      showlegend = TRUE,
      hovermode = "x",
      height = 400
    )

  p
}

## To be copied in the UI
# mod_position_risk_ui("position_risk_1")

## To be copied in the server
# mod_position_risk_server("position_risk_1", trigger, ticker, strike, expiration, premium_received)
