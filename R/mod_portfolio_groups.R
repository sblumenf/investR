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
    uiOutput(ns("refresh_status_line")),
    tags$hr(),

    # Alert section (for broken/incomplete groups)
    uiOutput(ns("integrity_alerts")),

    # Unlinked activities banner
    uiOutput(ns("unlinked_activities_banner")),

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
#' @importFrom shiny moduleServer renderUI reactive req observeEvent reactiveVal observe reactiveTimer isolate eventReactive actionButton icon
#' @importFrom dplyr %>% filter arrange desc mutate
#' @importFrom purrr map_chr
#' @importFrom DBI dbDisconnect
#' @importFrom logger log_info
mod_portfolio_groups_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Normalize ticker by stripping exchange suffixes for grouping purposes
    # Examples: LB.TO -> LB, SRU.UN.TO -> SRU, AAPL.MX -> AAPL, BRK.B -> BRK.B
    normalize_ticker_for_grouping <- function(ticker) {
      if (is.na(ticker) || ticker == "") return(ticker)

      # Strip ALL exchange suffixes (handles multiple like .UN.TO)
      # Keeps stripping dot + 1-3 uppercase letters from end until no more matches
      # Won't match: BRK.B (has lowercase after), ticker internal dots
      while (grepl("\\.[A-Z]{1,3}$", ticker)) {
        ticker <- gsub("\\.[A-Z]{1,3}$", "", ticker)
      }
      return(ticker)
    }

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

    # Reactive value to track position refresh completion
    # This will trigger card re-rendering after refresh completes
    position_refresh_version <- reactiveVal(0)

    # Trigger position refresh on page load (runs once when module initializes)
    observe({
      log_info("Position Groups: Triggering position refresh on page load")
      refresh_questrade_positions()

      # Increment version after refresh is kicked off
      # Note: refresh runs async, but we increment immediately to trigger re-render
      isolate(position_refresh_version(position_refresh_version() + 1))
    }, priority = 10)  # High priority to run early

    # Set up hourly timer for position refresh (3600000 ms = 1 hour)
    hourly_timer <- reactiveTimer(3600000)

    # Trigger position refresh every hour
    observeEvent(hourly_timer(), {
      log_info("Position Groups: Hourly position refresh triggered")
      refresh_questrade_positions()

      # Increment version to trigger card re-render
      isolate(position_refresh_version(position_refresh_version() + 1))
    }, ignoreInit = TRUE)  # Don't run on initialization (already handled by page load observer)

    # ============================================================================
    # UNLINKED ACTIVITIES REVIEW WORKFLOW
    # ============================================================================

    # Capture unlinked activities data when review button is clicked
    virgin_by_ticker <- eventReactive(input$review_unlinked_btn, {
      # Auto-link cash equivalents (ZMMK.to and SGOV) before reviewing
      auto_link_cash_equivalents()

      # Get remaining unlinked activities
      unlinked <- get_unlinked_activities()

      # Add ticker column (extract underlying ticker from option symbols)
      unlinked <- unlinked %>%
        mutate(ticker = purrr::map2_chr(symbol, description, function(sym, desc) {
          # FALLBACK: If symbol is empty/NULL, try parsing description
          if (is.null(sym) || is.na(sym) || sym == "") {
            ticker <- extract_ticker_from_description(desc)
            if (!is.na(ticker)) return(normalize_ticker_for_grouping(ticker))
            return("UNKNOWN")  # Last resort if description parsing fails
          }

          # Normal logic: symbol exists
          if (grepl("\\d{1,2}[A-Z][a-z]{2}\\d{2}[CP]", sym)) {
            result <- parse_option_symbol(sym)
            if (is.na(result)) {
              normalize_ticker_for_grouping(sym)
            } else {
              normalize_ticker_for_grouping(result)
            }
          } else {
            normalize_ticker_for_grouping(sym)
          }
        })) %>%
        # Create composite key: ticker + account
        mutate(ticker_account_key = paste(ticker, account_number, sep = "_"))

      # Group by ticker AND account (not just ticker)
      split(unlinked, unlinked$ticker_account_key)
    })

    # Initialize review transactions module (returns reactive that tracks actions)
    review_actions <- mod_review_transactions_server(
      "review_transactions",
      trigger = reactive(input$review_unlinked_btn),
      virgin_by_ticker = virgin_by_ticker
    )

    # Render unlinked activities banner
    output$unlinked_activities_banner <- renderUI({
      # Create dependency on review actions to force refresh after modal actions
      review_actions()

      # Get count of unlinked activities
      unlinked <- get_unlinked_activities()

      if (nrow(unlinked) == 0) {
        return(NULL)
      }

      # Show banner with count and action button
      create_status_alert(
        type = "info",
        message = sprintf("%d unlinked activities need review", nrow(unlinked)),
        action_button = actionButton(
          ns("review_unlinked_btn"),
          "Review Activities",
          icon = icon("tasks")
        )
      )
    })

    # ============================================================================

    # Reactive: Calculate metrics for ALL groups once (DRY principle)
    # This is the single source of truth for all metrics
    # Depends on position_refresh_version to recalculate after position refresh
    dashboard_metrics <- reactive({
      # Create dependency on position refresh version
      position_refresh_version()

      calculate_dashboard_metrics(status_filter = NULL)
    })

    # Reactive: Get all groups based on status filter
    all_groups <- reactive({
      req(input$status_filter)

      # Create dependency on review actions to refresh when groups are created/modified via modal
      review_actions()

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
          # Only sort by annualized return if we have groups with metrics
          if (nrow(groups) > 0 && !is.null(metrics$open_groups_detail) && !is.null(metrics$closed_groups_detail)) {
            groups <- groups %>%
              left_join(
                bind_rows(
                  if (nrow(metrics$open_groups_detail) > 0 && "group_id" %in% names(metrics$open_groups_detail)) {
                    metrics$open_groups_detail %>% select(group_id, projected_annualized_return_pct)
                  } else {
                    tibble::tibble(group_id = character(), projected_annualized_return_pct = numeric())
                  },
                  if (nrow(metrics$closed_groups_detail) > 0 && "group_id" %in% names(metrics$closed_groups_detail)) {
                    metrics$closed_groups_detail %>% select(group_id, annualized_return_pct) %>% rename(projected_annualized_return_pct = annualized_return_pct)
                  } else {
                    tibble::tibble(group_id = character(), projected_annualized_return_pct = numeric())
                  }
                ),
                by = "group_id"
              ) %>%
              arrange(desc(projected_annualized_return_pct)) %>%
              select(-projected_annualized_return_pct)
          }

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

    # Render integrity alerts (including refresh status)
    output$integrity_alerts <- renderUI({
      # Create dependency on position refresh version (triggers re-render after each refresh)
      position_refresh_version()

      # Get current refresh status
      status <- get_refresh_status()

      # Check if there are any errors
      has_activities_error <- !is.null(status$activities$last_error)
      has_positions_error <- !is.null(status$positions$last_error)

      if (!has_activities_error && !has_positions_error) {
        return(NULL)
      }

      # Build error message
      error_parts <- c()

      if (has_activities_error && has_positions_error) {
        # Both failed
        activities_time <- format_time_ago(status$activities$last_error$timestamp)
        positions_time <- format_time_ago(status$positions$last_error$timestamp)

        last_success_time <- if (!is.null(status$positions$last_success)) {
          format_time_ago(status$positions$last_success)
        } else if (!is.null(status$activities$last_success)) {
          format_time_ago(status$activities$last_success)
        } else {
          "unknown"
        }

        message <- paste0(
          "Data refresh failed. ",
          "Activities: Error ", activities_time, ". ",
          "Positions: Error ", positions_time, ". ",
          "Last successful refresh: ", last_success_time, "."
        )

      } else if (has_activities_error) {
        # Only activities failed, positions succeeded
        error_time <- format_time_ago(status$activities$last_error$timestamp)
        success_time <- if (!is.null(status$positions$last_success)) {
          format_time_ago(status$positions$last_success)
        } else {
          "unknown"
        }

        message <- paste0(
          "Data refresh partially failed. ",
          "Activities: Error ", error_time, ". ",
          "Positions: OK (refreshed ", success_time, ")."
        )

      } else {
        # Only positions failed, activities succeeded
        error_time <- format_time_ago(status$positions$last_error$timestamp)
        success_time <- if (!is.null(status$activities$last_success)) {
          format_time_ago(status$activities$last_success)
        } else {
          "unknown"
        }

        message <- paste0(
          "Data refresh partially failed. ",
          "Activities: OK (refreshed ", success_time, "). ",
          "Positions: Error ", error_time, "."
        )
      }

      # Return warning alert using existing pattern
      create_status_alert(
        type = "warning",
        message = message
      )
    })

    # Render refresh status line (shows last successful refresh time)
    output$refresh_status_line <- renderUI({
      # Create dependency on position refresh version (triggers re-render after each refresh)
      position_refresh_version()

      # Get current refresh status
      status <- get_refresh_status()

      # Show last successful refresh time only if it's more recent than any errors
      if (!is.null(status$positions$last_success)) {
        show_success <- TRUE

        # Hide success indicator if there's a more recent error
        if (!is.null(status$positions$last_error)) {
          if (status$positions$last_error$timestamp > status$positions$last_success) {
            show_success <- FALSE
          }
        }
        # Also check activities errors (they affect overall data freshness)
        if (!is.null(status$activities$last_error)) {
          if (status$activities$last_error$timestamp > status$positions$last_success) {
            show_success <- FALSE
          }
        }

        if (show_success) {
          time_ago <- format_time_ago(status$positions$last_success)

          tags$div(
            style = "text-align: left; color: #6c757d; font-size: 12px; margin-top: -10px; margin-bottom: 5px;",
            icon("check-circle", style = "color: #28a745;"),
            " Last refreshed: ",
            time_ago
          )
        } else {
          NULL
        }
      } else {
        NULL
      }
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
