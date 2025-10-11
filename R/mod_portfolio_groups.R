#' portfolio_groups UI Function
#'
#' @description Main module for Position Groups page. Orchestrates dashboard
#' and cards sub-modules, handles filtering and sorting.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_portfolio_groups_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Page title
    tags$h2("Position Groups"),
    tags$hr(),

    # Alert section (for broken/incomplete groups)
    uiOutput(ns("integrity_alerts")),

    # Filter and sort controls
    tags$div(
      class = "well",
      style = "background: #f8f9fa; padding: 15px; margin-bottom: 20px;",
      tags$div(
        class = "row",
        # Status filter
        tags$div(
          class = "col-md-4",
          tags$label("Status:"),
          shiny::radioButtons(
            ns("status_filter"),
            label = NULL,
            choices = c("Open" = "open", "Closed" = "closed", "All" = "all"),
            selected = "open",
            inline = TRUE
          )
        ),
        # Strategy filter
        tags$div(
          class = "col-md-4",
          tags$label("Strategy:"),
          uiOutput(ns("strategy_filter_ui"))
        ),
        # Sort dropdown
        tags$div(
          class = "col-md-4",
          tags$label("Sort by:"),
          shiny::selectInput(
            ns("sort_by"),
            label = NULL,
            choices = c(
              "Annualized Return (desc)" = "annualized_desc",
              "Created Date (desc)" = "created_desc",
              "Group Name" = "name_asc"
            ),
            selected = "annualized_desc"
          )
        )
      )
    ),

    # Dashboard section (sub-module)
    mod_dashboard_ui(ns("dashboard")),

    # Cards section (sub-module)
    mod_group_cards_ui(ns("cards"))
  )
}

#' portfolio_groups Server Functions
#'
#' @description Main orchestrator server logic. Coordinates filtering, sorting,
#' and passes data to sub-modules.
#'
#' @param id Module ID
#'
#' @noRd
#'
#' @importFrom shiny moduleServer renderUI reactive req observeEvent reactiveVal observe
#' @importFrom dplyr %>% filter arrange desc
#' @importFrom DBI dbDisconnect
mod_portfolio_groups_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Cache schema initialization (once per session)
    schema_initialized <- reactiveVal(FALSE)

    # Initialize database schema once (in an observe block)
    observe({
      if (!schema_initialized()) {
        conn <- get_portfolio_db_connection()
        initialize_groups_schema(conn)
        initialize_income_projection_schema(conn)
        initialize_activities_schema(conn)
        dbDisconnect(conn, shutdown = TRUE)
        schema_initialized(TRUE)
      }
    })

    # Reactive: Calculate metrics for ALL groups once (DRY principle)
    # This is the single source of truth for all metrics
    dashboard_metrics <- reactive({
      calculate_dashboard_metrics(status_filter = NULL)
    })

    # Reactive: Get all groups based on status filter
    all_groups <- reactive({
      req(input$status_filter)

      if (input$status_filter == "open") {
        get_all_groups(include_closed = FALSE)
      } else if (input$status_filter == "closed") {
        get_all_groups(include_closed = TRUE) %>%
          filter(status == "closed")
      } else {
        # All
        get_all_groups(include_closed = TRUE)
      }
    })

    # Reactive: Get unique strategies for filter dropdown
    available_strategies <- reactive({
      groups <- all_groups()

      if (nrow(groups) == 0) {
        return(c("All"))
      }

      strategies <- unique(groups$strategy_type)
      c("All", strategies)
    })

    # Render strategy filter dropdown
    output$strategy_filter_ui <- renderUI({
      shiny::selectInput(
        ns("strategy_filter"),
        label = NULL,
        choices = available_strategies(),
        selected = "All"
      )
    })

    # Reactive: Get status filter value for sub-modules
    status_filter_value <- reactive({
      req(input$status_filter)

      if (input$status_filter == "all") {
        NULL
      } else {
        input$status_filter
      }
    })

    # Reactive: Filter and sort groups
    filtered_sorted_groups <- reactive({
      req(all_groups())
      req(dashboard_metrics())

      groups <- all_groups()
      metrics <- dashboard_metrics()

      # Apply strategy filter
      if (!is.null(input$strategy_filter) && input$strategy_filter != "All") {
        groups <- groups %>%
          filter(strategy_type == input$strategy_filter)
      }

      # Apply sorting
      if (!is.null(input$sort_by)) {
        if (input$sort_by == "annualized_desc") {
          # Join with pre-calculated metrics (DRY - no recalculation!)
          groups <- groups %>%
            left_join(
              bind_rows(
                metrics$open_groups_detail %>% select(group_id, projected_annualized_return_pct),
                metrics$closed_groups_detail %>% select(group_id, annualized_return_pct) %>% rename(projected_annualized_return_pct = annualized_return_pct)
              ),
              by = "group_id"
            ) %>%
            arrange(desc(projected_annualized_return_pct)) %>%
            select(-projected_annualized_return_pct)

        } else if (input$sort_by == "created_desc") {
          groups <- groups %>%
            arrange(desc(created_at))

        } else if (input$sort_by == "name_asc") {
          groups <- groups %>%
            arrange(group_name)
        }
      }

      groups
    })

    # Render integrity alerts
    output$integrity_alerts <- renderUI({
      # This would check for broken/incomplete groups
      # For now, return NULL (can be implemented later)
      NULL
    })

    # Call dashboard sub-module (pass pre-calculated metrics)
    mod_dashboard_server("dashboard",
                        status_filter = status_filter_value,
                        metrics = dashboard_metrics)

    # Call cards sub-module (pass pre-calculated metrics)
    mod_group_cards_server("cards",
                          filtered_groups = filtered_sorted_groups,
                          metrics = dashboard_metrics)
  })
}
