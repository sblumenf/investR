#' Money Market Rotation UI Function
#'
#' @description Module for tracking money market rotation strategy performance.
#' Displays current position, performance metrics, rotation history, and dividends.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib value_box card card_header card_body
#' @importFrom DT DTOutput
mod_money_market_rotation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Page title
    tags$h2("Money Market Rotation Tracker"),
    tags$p("Track the dividend capture rotation strategy across money market ETFs."),
    tags$hr(),

    # Controls row: Refresh button and Period filter
    fluidRow(
      column(
        3,
        actionButton(
          ns("refresh_data"),
          "Refresh Data",
          icon = icon("sync"),
          class = "btn-primary"
        )
      ),
      column(
        4,
        selectInput(
          ns("period_filter"),
          "Performance Period:",
          choices = c("All Time" = "all"),
          selected = "all"
        )
      ),
      column(
        4,
        selectInput(
          ns("table_granularity"),
          "Table View:",
          choices = c("Yearly" = "yearly", "Quarterly" = "quarterly", "Monthly" = "monthly"),
          selected = "yearly"
        )
      )
    ),
    tags$br(),

    # Row 1: Current Position Card
    fluidRow(
      column(
        12,
        card(
          card_header(
            class = "bg-primary text-white",
            tags$h4(icon("chart-line"), " Current Position", class = "m-0")
          ),
          card_body(
            uiOutput(ns("current_position_ui"))
          )
        )
      )
    ),

    tags$br(),

    # Row 2: Performance Summary Cards
    fluidRow(
      column(
        12,
        card(
          card_header(
            class = "bg-success text-white",
            tags$h4(icon("chart-pie"), " Performance Summary", class = "m-0")
          ),
          card_body(
            fluidRow(
              column(3, uiOutput(ns("twr_box"))),
              column(3, uiOutput(ns("total_return_box"))),
              column(3, uiOutput(ns("dividends_box"))),
              column(3, uiOutput(ns("gains_box")))
            ),
            fluidRow(
              column(4, uiOutput(ns("capital_added_box"))),
              column(4, uiOutput(ns("capital_withdrawn_box"))),
              column(4, uiOutput(ns("annualized_box")))
            )
          )
        )
      )
    ),

    tags$br(),

    # Row 3: Returns by Period Table
    fluidRow(
      column(
        12,
        card(
          card_header(
            class = "bg-warning",
            tags$h4(icon("calendar"), " Returns by Period", class = "m-0")
          ),
          card_body(
            DT::DTOutput(ns("period_returns_table"))
          )
        )
      )
    ),

    tags$br(),

    # Row 4: Position P&L Table
    fluidRow(
      column(
        12,
        card(
          card_header(
            class = "bg-info text-white",
            tags$h4(icon("dollar-sign"), " Position P&L", class = "m-0")
          ),
          card_body(
            fluidRow(
              column(
                4,
                radioButtons(
                  ns("return_type"),
                  "Return Type:",
                  choices = c("Simple %" = "simple", "Annualized %" = "annualized"),
                  selected = "simple",
                  inline = TRUE
                )
              )
            ),
            tags$p(
              class = "text-muted",
              "Note: Dividends may post after position is closed. They are attributed to the position that held the ticker."
            ),
            DT::DTOutput(ns("position_pl_table"))
          )
        )
      )
    )
  )
}

#' Money Market Rotation Server Functions
#'
#' @description Server logic for money market rotation tracking module.
#'
#' @param id Module ID
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive renderUI req observeEvent reactiveVal observe updateSelectInput
#' @importFrom dplyr %>% filter mutate arrange select group_by summarise left_join
#' @importFrom DBI dbDisconnect
#' @importFrom logger log_info log_debug log_error
#' @importFrom DT renderDT datatable formatCurrency formatStyle
#' @importFrom bslib value_box
mod_money_market_rotation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive trigger for data refresh
    refresh_trigger <- reactiveVal(0)

    # Initial load and manual refresh
    observe({
      refresh_trigger()
      log_info("MM Rotation Module: Data refresh triggered")
    })

    observeEvent(input$refresh_data, {
      refresh_trigger(refresh_trigger() + 1)
    })

    # Reactive: Fetch all rotation data
    rotation_data <- reactive({
      refresh_trigger()
      log_debug("MM Rotation Module: Fetching rotation data")

      conn <- tryCatch({
        get_portfolio_db_connection(read_only = TRUE)
      }, error = function(e) {
        log_error("MM Rotation Module: Failed to connect - {e$message}")
        return(NULL)
      })

      if (is.null(conn)) {
        return(list(
          transactions = tibble(),
          dividends = tibble(),
          rotations = tibble(),
          dividends_attributed = tibble(),
          capital_flows = tibble(),
          performance = list(),
          current_position = list(),
          summary = list()
        ))
      }

      on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

      # Fetch data
      transactions <- fetch_mm_rotation_transactions(conn)
      dividends <- fetch_mm_rotation_dividends(conn)

      # Build analysis
      rotations <- build_rotation_history(transactions)
      dividends_attributed <- attribute_dividends_to_rotations(dividends, rotations)
      capital_flows <- infer_capital_flows(rotations, dividends_attributed)
      performance <- calculate_twr(rotations, dividends_attributed, capital_flows)
      current_position <- get_current_mm_position(rotations)
      summary <- get_mm_rotation_summary(rotations, dividends)

      yearly_returns <- calculate_returns_by_year(rotations, dividends_attributed, capital_flows)

      list(
        transactions = transactions,
        dividends = dividends,
        rotations = rotations,
        dividends_attributed = dividends_attributed,
        capital_flows = capital_flows,
        performance = performance,
        current_position = current_position,
        summary = summary,
        yearly_returns = yearly_returns
      )
    })

    # Update period filter choices when data changes
    observe({
      data <- rotation_data()
      if (nrow(data$rotations) > 0) {
        periods <- get_available_periods(data$rotations)
        updateSelectInput(
          session,
          "period_filter",
          choices = periods$choices,
          selected = periods$default
        )
      }
    })

    # Reactive: Calculate performance for selected period
    filtered_performance <- reactive({
      data <- rotation_data()
      period_value <- input$period_filter

      if (is.null(period_value) || period_value == "all") {
        return(data$performance)
      }

      date_range <- parse_period_selection(period_value, data$rotations)
      calculate_period_returns(
        data$rotations,
        data$dividends_attributed,
        data$capital_flows,
        date_range$start_date,
        date_range$end_date
      )
    })

    # Reactive: Calculate returns by selected granularity for table
    period_returns_table <- reactive({
      data <- rotation_data()
      granularity <- input$table_granularity

      if (is.null(granularity)) granularity <- "yearly"

      calculate_returns_by_period_type(
        data$rotations,
        data$dividends_attributed,
        data$capital_flows,
        granularity
      )
    })

    # Current Position UI
    output$current_position_ui <- renderUI({
      data <- rotation_data()
      pos <- data$current_position

      if (!pos$has_position) {
        return(tags$div(
          class = "alert alert-info",
          icon("info-circle"),
          " No open position. Waiting for first rotation."
        ))
      }

      fluidRow(
        column(2, tags$div(
          class = "text-center",
          tags$h5("Ticker"),
          tags$h3(class = "text-primary", pos$symbol)
        )),
        column(2, tags$div(
          class = "text-center",
          tags$h5("Shares"),
          tags$h3(format(pos$shares, big.mark = ","))
        )),
        column(2, tags$div(
          class = "text-center",
          tags$h5("Cost Basis"),
          tags$h3(paste0("$", format(round(pos$cost_basis, 2), big.mark = ",", nsmall = 2)))
        )),
        column(3, tags$div(
          class = "text-center",
          tags$h5("Buy Date"),
          tags$h3(format(pos$buy_date, "%Y-%m-%d"))
        )),
        column(3, tags$div(
          class = "text-center",
          tags$h5("Holding Days"),
          tags$h3(pos$holding_days)
        ))
      )
    })

    # Performance Value Boxes (use filtered_performance for period-specific data)
    output$twr_box <- renderUI({
      perf <- filtered_performance()
      value_box(
        title = "Time-Weighted Return",
        value = paste0(perf$twr_pct, "%"),
        showcase = icon("percentage"),
        theme = if (perf$twr_pct >= 0) "success" else "danger"
      )
    })

    output$total_return_box <- renderUI({
      perf <- filtered_performance()
      value_box(
        title = "Total Return",
        value = paste0("$", format(perf$total_return_dollars, big.mark = ",", nsmall = 2)),
        showcase = icon("coins"),
        theme = if (perf$total_return_dollars >= 0) "success" else "danger"
      )
    })

    output$dividends_box <- renderUI({
      perf <- filtered_performance()
      value_box(
        title = "Total Dividends",
        value = paste0("$", format(perf$total_dividends, big.mark = ",", nsmall = 2)),
        showcase = icon("hand-holding-dollar"),
        theme = "info"
      )
    })

    output$gains_box <- renderUI({
      perf <- filtered_performance()
      value_box(
        title = "Realized Gains",
        value = paste0("$", format(perf$total_realized_gains, big.mark = ",", nsmall = 2)),
        showcase = icon("chart-line"),
        theme = if (perf$total_realized_gains >= 0) "success" else "danger"
      )
    })

    output$capital_added_box <- renderUI({
      perf <- filtered_performance()
      value_box(
        title = "Capital Added",
        value = paste0("$", format(perf$total_capital_added, big.mark = ",", nsmall = 2)),
        showcase = icon("plus-circle"),
        theme = "secondary"
      )
    })

    output$capital_withdrawn_box <- renderUI({
      perf <- filtered_performance()
      value_box(
        title = "Capital Withdrawn",
        value = paste0("$", format(perf$total_capital_withdrawn, big.mark = ",", nsmall = 2)),
        showcase = icon("minus-circle"),
        theme = "secondary"
      )
    })

    output$annualized_box <- renderUI({
      perf <- filtered_performance()
      value_box(
        title = "Annualized Return",
        value = paste0(perf$annualized_return, "%"),
        showcase = icon("chart-line"),
        theme = if (perf$annualized_return >= 0) "success" else "danger"
      )
    })

    # Period Returns Table (uses granularity selector)
    output$period_returns_table <- DT::renderDT({
      data <- period_returns_table()

      if (nrow(data) == 0) {
        return(DT::datatable(
          tibble(Message = "No data available yet."),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }

      display_data <- data %>%
        select(
          Period = period,
          Dividends = dividends,
          `Cap Gains` = cap_gains,
          `Total Income` = total_income,
          `Return %` = return_pct
        )

      DT::datatable(
        display_data,
        options = list(
          pageLength = 12,
          order = list(list(0, "desc")),
          dom = "tip"
        ),
        rownames = FALSE
      ) %>%
        DT::formatCurrency(c("Dividends", "Cap Gains", "Total Income"), currency = "$") %>%
        DT::formatStyle(
          "Return %",
          color = DT::styleInterval(0, c("red", "green"))
        )
    })

    # Position P&L Table (combines rotation data with dividends)
    output$position_pl_table <- DT::renderDT({
      data <- rotation_data()
      rotations <- data$rotations
      dividends <- data$dividends_attributed
      return_type <- input$return_type

      if (is.null(return_type)) return_type <- "simple"

      if (nrow(rotations) == 0) {
        return(DT::datatable(
          tibble(Message = "No positions recorded yet."),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }

      # Aggregate dividends by rotation
      div_by_rotation <- dividends %>%
        group_by(rotation_id) %>%
        summarise(total_dividends = sum(amount, na.rm = TRUE), .groups = "drop")

      # Join and calculate P&L
      pl_data <- rotations %>%
        left_join(div_by_rotation, by = "rotation_id") %>%
        mutate(
          total_dividends = ifelse(is.na(total_dividends), 0, total_dividends),
          cap_gain = ifelse(status == "Closed", realized_gain_loss, NA_real_),
          total_pl = ifelse(status == "Closed", cap_gain + total_dividends, NA_real_),
          # Calculate returns
          cap_gain_pct_simple = ifelse(status == "Closed" & buy_amount > 0,
                                       (cap_gain / buy_amount) * 100, NA_real_),
          div_pct_simple = ifelse(buy_amount > 0,
                                  (total_dividends / buy_amount) * 100, NA_real_),
          total_pct_simple = ifelse(status == "Closed" & buy_amount > 0,
                                    (total_pl / buy_amount) * 100, NA_real_),
          # Annualized returns
          cap_gain_pct_ann = ifelse(status == "Closed" & holding_days > 0 & !is.na(cap_gain_pct_simple),
                                    ((1 + cap_gain_pct_simple / 100) ^ (365 / holding_days) - 1) * 100, NA_real_),
          div_pct_ann = ifelse(holding_days > 0 & !is.na(div_pct_simple),
                               ((1 + div_pct_simple / 100) ^ (365 / holding_days) - 1) * 100, NA_real_),
          total_pct_ann = ifelse(status == "Closed" & holding_days > 0 & !is.na(total_pct_simple),
                                 ((1 + total_pct_simple / 100) ^ (365 / holding_days) - 1) * 100, NA_real_)
        )

      # Select appropriate % columns based on toggle
      if (return_type == "annualized") {
        display_data <- pl_data %>%
          mutate(
            buy_date_fmt = format(buy_date, "%Y-%m-%d"),
            sell_date_fmt = ifelse(is.na(sell_date), "Open", format(sell_date, "%Y-%m-%d")),
            cap_gain_fmt = ifelse(is.na(cap_gain), "-", round(cap_gain, 2)),
            cap_gain_pct_fmt = ifelse(is.na(cap_gain_pct_ann), "-", paste0(round(cap_gain_pct_ann, 1), "%")),
            div_fmt = round(total_dividends, 2),
            div_pct_fmt = ifelse(is.na(div_pct_ann), "-", paste0(round(div_pct_ann, 1), "%")),
            total_pl_fmt = ifelse(is.na(total_pl), "-", round(total_pl, 2)),
            total_pct_fmt = ifelse(is.na(total_pct_ann), "-", paste0(round(total_pct_ann, 1), "%"))
          )
      } else {
        display_data <- pl_data %>%
          mutate(
            buy_date_fmt = format(buy_date, "%Y-%m-%d"),
            sell_date_fmt = ifelse(is.na(sell_date), "Open", format(sell_date, "%Y-%m-%d")),
            cap_gain_fmt = ifelse(is.na(cap_gain), "-", round(cap_gain, 2)),
            cap_gain_pct_fmt = ifelse(is.na(cap_gain_pct_simple), "-", paste0(round(cap_gain_pct_simple, 2), "%")),
            div_fmt = round(total_dividends, 2),
            div_pct_fmt = ifelse(is.na(div_pct_simple), "-", paste0(round(div_pct_simple, 2), "%")),
            total_pl_fmt = ifelse(is.na(total_pl), "-", round(total_pl, 2)),
            total_pct_fmt = ifelse(is.na(total_pct_simple), "-", paste0(round(total_pct_simple, 2), "%"))
          )
      }

      display_data <- display_data %>%
        select(
          Ticker = symbol,
          `Buy Date` = buy_date_fmt,
          `Sell Date` = sell_date_fmt,
          `Cap Gain` = cap_gain_fmt,
          `CG %` = cap_gain_pct_fmt,
          Dividends = div_fmt,
          `Div %` = div_pct_fmt,
          `Total PL` = total_pl_fmt,
          `Total %` = total_pct_fmt
        )

      DT::datatable(
        display_data,
        options = list(
          pageLength = 20,
          order = list(list(1, "desc")),
          dom = "tip"
        ),
        rownames = FALSE
      ) %>%
        DT::formatCurrency(c("Dividends"), currency = "$")
    })
  })
}
