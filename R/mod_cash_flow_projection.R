#' cash_flow_projection UI Function
#'
#' @description Module for displaying cash flow projections with visualization
#' and detailed transaction table.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
mod_cash_flow_projection_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Page title
    tags$h2("Cash Flow Projection"),
    tags$p("View projected and actual cash flows across all position groups. Dividends and option premiums aggregated by month."),
    tags$hr(),

    # Period filter
    tags$div(
      class = "well",
      style = "background: #f8f9fa; padding: 15px; margin-bottom: 20px;",
      tags$div(
        class = "row",
        tags$div(
          class = "col-md-6",
          tags$label("Time Period:"),
          shiny::radioButtons(
            ns("period_filter"),
            label = NULL,
            choices = c(
              "Past Periods (Historical)" = "past",
              "Current and Future Periods" = "current_future"
            ),
            selected = "current_future",
            inline = FALSE
          )
        ),
        tags$div(
          class = "col-md-6",
          tags$div(
            style = "margin-top: 25px; padding: 10px; background: #e7f3ff; border-left: 3px solid #0066cc;",
            tags$small(
              tags$strong("Note: "),
              "For the current month, the 'Actual/Projected' column shows whether each transaction has been received or is still expected."
            )
          )
        )
      )
    ),

    # Visualization section
    tags$h4("Monthly Cash Flow Summary"),
    plotly::plotlyOutput(ns("cash_flow_chart"), height = "400px"),
    tags$hr(),

    # Transaction detail table
    tags$h4("Transaction Details"),
    DT::DTOutput(ns("cash_flow_table"))
  )
}

#' cash_flow_projection Server Functions
#'
#' @description Server logic for cash flow projection module.
#' Handles data fetching, filtering, and rendering of chart and table.
#'
#' @param id Module ID
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive renderUI req observeEvent reactiveVal observe
#' @importFrom dplyr %>% filter mutate arrange case_when
#' @importFrom lubridate floor_date
#' @importFrom DBI dbDisconnect
#' @importFrom logger log_info log_debug
#' @importFrom plotly renderPlotly plot_ly layout config
#' @importFrom DT renderDT datatable formatCurrency
mod_cash_flow_projection_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Cache schema initialization (once per session)
    schema_initialized <- reactiveVal(FALSE)

    # Initialize database schemas once
    observe({
      if (!schema_initialized()) {
        conn <- get_portfolio_db_connection()
        initialize_groups_schema(conn)
        initialize_income_projection_schema(conn)
        initialize_activities_schema(conn)
        dbDisconnect(conn, shutdown = TRUE)
        schema_initialized(TRUE)
        log_info("Cash Flow Projection: Database schemas initialized")
      }
    })

    # Reactive: Get date range
    date_range <- reactive({
      req(schema_initialized())
      get_cash_flow_date_range()
    })

    # Reactive: Get combined cash flows
    combined_data <- reactive({
      req(schema_initialized())
      get_combined_cash_flows()
    })

    # Reactive: Filter data based on period selection
    filtered_transactions <- reactive({
      req(combined_data())
      req(input$period_filter)

      transactions <- combined_data()$transactions

      if (nrow(transactions) == 0) {
        return(transactions)
      }

      current_month_start <- floor_date(Sys.Date(), "month")

      if (input$period_filter == "past") {
        # Past: Before current month
        transactions %>%
          filter(floor_date(event_date, "month") < current_month_start)
      } else {
        # Current/Future: Current month onwards
        transactions %>%
          filter(floor_date(event_date, "month") >= current_month_start)
      }
    })

    # Reactive: Filter monthly aggregates for chart
    filtered_monthly <- reactive({
      req(filtered_transactions())

      transactions <- filtered_transactions()

      if (nrow(transactions) == 0) {
        return(tibble(
          month_label = character(),
          type = character(),
          total_amount = numeric()
        ))
      }

      # Recalculate aggregates from filtered transactions
      transactions %>%
        group_by(month_label, type) %>%
        summarise(
          total_amount = sum(amount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(month_label, type)
    })

    # Render plotly stacked bar chart
    output$cash_flow_chart <- renderPlotly({
      monthly_data <- filtered_monthly()

      if (nrow(monthly_data) == 0) {
        # Empty chart with message
        plot_ly() %>%
          layout(
            title = "No data available for selected period",
            xaxis = list(title = "Month"),
            yaxis = list(title = "Amount ($)")
          )
      } else {
        # Define colors for each transaction type
        color_map <- c(
          "dividend" = "#28a745",        # Green
          "option_premium" = "#007bff",  # Blue
          "option_gain" = "#ffc107"      # Yellow/Gold
        )

        # Create stacked bar chart
        plot_ly(
          data = monthly_data,
          x = ~month_label,
          y = ~total_amount,
          color = ~type,
          colors = color_map,
          type = "bar",
          hoverinfo = "text",
          text = ~paste0(
            "<b>", month_label, "</b><br>",
            tools::toTitleCase(gsub("_", " ", type)), ": $",
            format(round(total_amount, 2), big.mark = ",", nsmall = 2)
          )
        ) %>%
          layout(
            title = list(
              text = "Monthly Cash Flow by Type",
              font = list(size = 16)
            ),
            xaxis = list(
              title = "Month",
              tickangle = -45
            ),
            yaxis = list(
              title = "Total Amount ($)",
              tickformat = "$,.0f"
            ),
            barmode = "stack",
            hovermode = "x unified",
            legend = list(
              title = list(text = "Transaction Type"),
              orientation = "v",
              x = 1.02,
              y = 1
            ),
            margin = list(b = 100)  # Space for rotated labels
          ) %>%
          config(displayModeBar = TRUE, displaylogo = FALSE)
      }
    })

    # Render DT table
    output$cash_flow_table <- renderDT({
      transactions <- filtered_transactions()

      if (nrow(transactions) == 0) {
        # Return empty table with schema
        return(datatable(
          tibble(
            Month = character(),
            Ticker = character(),
            Amount = numeric(),
            Type = character(),
            Group = character(),
            `Actual/Projected` = character()
          ),
          options = list(
            pageLength = 25,
            order = list(list(0, "desc"))
          ),
          rownames = FALSE
        ))
      }

      # Format table data
      table_data <- transactions %>%
        transmute(
          Month = month_label,
          Ticker = ticker,
          Amount = amount,
          Type = tools::toTitleCase(gsub("_", " ", type)),
          Group = group_name,
          `Actual/Projected` = if_else(
            is_current_month,
            tools::toTitleCase(source),
            ""  # Empty for non-current months
          )
        ) %>%
        arrange(desc(Month), Ticker)

      # Create datatable with formatting
      datatable(
        table_data,
        options = list(
          pageLength = 25,
          order = list(list(0, "desc")),  # Sort by Month descending
          searching = TRUE,
          dom = "ftip"  # Filter, table, info, pagination
        ),
        rownames = FALSE,
        filter = "top"
      ) %>%
        formatCurrency("Amount", "$", digits = 2)
    })
  })
}

## To be copied in the UI
# mod_cash_flow_projection_ui("cash_flow_projection_1")

## To be copied in the server
# mod_cash_flow_projection_server("cash_flow_projection_1")
