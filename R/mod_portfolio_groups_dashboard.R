#' portfolio_groups_dashboard UI Function
#'
#' @description Sub-module for displaying portfolio groups summary dashboard.
#' Shows aggregate metrics for open and closed groups.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dashboard_section"))
  )
}

#' portfolio_groups_dashboard Server Functions
#'
#' @description Server logic for rendering dashboard metrics.
#'
#' @param id Module ID
#' @param status_filter Reactive containing status filter ("open", "closed", or NULL for all)
#' @param metrics Reactive containing pre-calculated metrics (from main orchestrator)
#'
#' @noRd
#'
#' @importFrom shiny moduleServer renderUI req tags
#' @importFrom dplyr %>%
mod_dashboard_server <- function(id, status_filter, metrics = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Render dashboard section
    output$dashboard_section <- renderUI({
      # Use pre-calculated metrics if provided (DRY), otherwise calculate
      if (!is.null(metrics)) {
        metrics_data <- metrics()
      } else {
        metrics_data <- calculate_dashboard_metrics(status_filter = NULL)
      }

      metrics <- metrics_data

      open_metrics <- metrics$open_metrics
      closed_metrics <- metrics$closed_metrics
      open_groups_detail <- metrics$open_groups_detail
      closed_groups_detail <- metrics$closed_groups_detail

      # Build dashboard content
      create_accordion_section(
        title = "Portfolio Summary",
        is_open = FALSE,  # Collapsed by default
        tags$div(
          class = "dashboard-metrics",
          # Open Groups Section
          if (nrow(open_metrics) > 0 && open_metrics$count > 0) {
            tags$div(
              tags$h4("Open Groups"),
              tags$div(
                class = "row",
                style = "margin-bottom: 20px;",
                # Metric cards
                tags$div(
                  class = "col-md-2",
                  tags$div(
                    class = "well text-center",
                    style = "background: #f8f9fa; border: 1px solid #dee2e6;",
                    tags$h3(
                      style = "margin: 0; color: #28a745;",
                      open_metrics$count
                    ),
                    tags$p(style = "margin: 5px 0 0 0; color: #6c757d;", "Active Groups")
                  )
                ),
                tags$div(
                  class = "col-md-2",
                  tags$div(
                    class = "well text-center",
                    style = "background: #f8f9fa; border: 1px solid #dee2e6;",
                    tags$h3(
                      style = "margin: 0; color: #007bff;",
                      format_currency(open_metrics$total_cost_basis)
                    ),
                    tags$p(style = "margin: 5px 0 0 0; color: #6c757d;", "Total Invested")
                  )
                ),
                tags$div(
                  class = "col-md-2",
                  tags$div(
                    class = "well text-center",
                    style = "background: #f8f9fa; border: 1px solid #dee2e6;",
                    tags$h3(
                      style = "margin: 0; color: #17a2b8;",
                      format_currency(open_metrics$total_cash_collected)
                    ),
                    tags$p(style = "margin: 5px 0 0 0; color: #6c757d;", "Cash Collected")
                  )
                ),
                tags$div(
                  class = "col-md-2",
                  tags$div(
                    class = "well text-center",
                    style = "background: #f8f9fa; border: 1px solid #dee2e6;",
                    tags$h3(
                      style = "margin: 0; color: #ffc107;",
                      format_currency(open_metrics$total_projected_income)
                    ),
                    tags$p(style = "margin: 5px 0 0 0; color: #6c757d;", "Projected Income")
                  )
                ),
                tags$div(
                  class = "col-md-2",
                  tags$div(
                    class = "well text-center",
                    style = "background: #f8f9fa; border: 1px solid #dee2e6;",
                    tags$h3(
                      style = "margin: 0; color: #28a745;",
                      format_percentage(open_metrics$avg_projected_return_pct)
                    ),
                    tags$p(style = "margin: 5px 0 0 0; color: #6c757d;", "Avg Proj. Return")
                  )
                ),
                tags$div(
                  class = "col-md-2",
                  tags$div(
                    class = "well text-center",
                    style = "background: #f8f9fa; border: 1px solid #dee2e6;",
                    tags$h3(
                      style = "margin: 0; color: #6c757d;",
                      format_percentage(open_metrics$avg_pct_recovered)
                    ),
                    tags$p(style = "margin: 5px 0 0 0; color: #6c757d;", "Avg % Recovered")
                  )
                )
              )
            )
          } else {
            NULL
          },

          # Closed Groups Section
          if (nrow(closed_metrics) > 0 && closed_metrics$count > 0) {
            tags$div(
              tags$h4("Closed Groups"),
              tags$div(
                class = "row",
                style = "margin-bottom: 20px;",
                # Metric cards
                tags$div(
                  class = "col-md-3",
                  tags$div(
                    class = "well text-center",
                    style = "background: #f8f9fa; border: 1px solid #dee2e6;",
                    tags$h3(
                      style = "margin: 0; color: #6c757d;",
                      closed_metrics$count
                    ),
                    tags$p(style = "margin: 5px 0 0 0; color: #6c757d;", "Closed Groups")
                  )
                ),
                tags$div(
                  class = "col-md-3",
                  tags$div(
                    class = "well text-center",
                    style = "background: #f8f9fa; border: 1px solid #dee2e6;",
                    tags$h3(
                      style = "margin: 0; color: #28a745;",
                      format_currency(closed_metrics$total_realized_pnl)
                    ),
                    tags$p(style = "margin: 5px 0 0 0; color: #6c757d;", "Total Realized P&L")
                  )
                ),
                tags$div(
                  class = "col-md-3",
                  tags$div(
                    class = "well text-center",
                    style = "background: #f8f9fa; border: 1px solid #dee2e6;",
                    tags$h3(
                      style = "margin: 0; color: #17a2b8;",
                      format_percentage(closed_metrics$avg_total_return_pct)
                    ),
                    tags$p(style = "margin: 5px 0 0 0; color: #6c757d;", "Avg Total Return")
                  )
                ),
                tags$div(
                  class = "col-md-3",
                  tags$div(
                    class = "well text-center",
                    style = "background: #f8f9fa; border: 1px solid #dee2e6;",
                    tags$h3(
                      style = "margin: 0; color: #007bff;",
                      format_percentage(closed_metrics$avg_annualized_return_pct)
                    ),
                    tags$p(style = "margin: 5px 0 0 0; color: #6c757d;", "Avg Ann. Return")
                  )
                )
              )
            )
          } else {
            NULL
          },

          # Strategy Breakdown
          tags$div(
            tags$h4("Strategy Breakdown"),
            tags$div(
              style = "margin-top: 10px;",
              create_strategy_breakdown_ui(open_groups_detail, closed_groups_detail)
            )
          )
        )
      )
    })
  })
}

#' Create strategy breakdown UI
#'
#' @param open_groups_detail Pre-calculated open group metrics
#' @param closed_groups_detail Pre-calculated closed group data
#' @return HTML div with strategy list
#' @noRd
create_strategy_breakdown_ui <- function(open_groups_detail, closed_groups_detail) {
  breakdown <- get_strategy_breakdown(
    open_groups_detail = open_groups_detail,
    closed_groups_detail = closed_groups_detail
  )

  if (nrow(breakdown) == 0) {
    return(tags$p(class = "text-muted", "No groups found"))
  }

  # Build list
  strategy_items <- purrr::map(seq_len(nrow(breakdown)), function(i) {
    strategy <- breakdown[i, ]

    tags$li(
      style = "margin-bottom: 5px;",
      sprintf("%s: %d group%s | Avg Return: %s",
              strategy$strategy_type,
              strategy$count,
              if (strategy$count > 1) "s" else "",
              format_percentage(strategy$avg_return_pct))
    )
  })

  tags$ul(
    style = "list-style-type: none; padding-left: 0;",
    strategy_items
  )
}
