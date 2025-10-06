#' portfolio_positions_table UI Function
#'
#' @description A shiny Module for displaying portfolio positions in tabbed tables
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tabsetPanel tabPanel
#' @importFrom DT DTOutput
mod_portfolio_positions_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Empty state message (shown when no data)
    uiOutput(ns("empty_state")),

    # Tabbed tables (shown when data exists)
    uiOutput(ns("tabbed_tables"))
  )
}

#' portfolio_positions_table Server Functions
#'
#' @description Server logic for displaying positions in tabbed DT tables
#'
#' @param id Module ID
#' @param results_data Reactive containing positions data frame
#'
#' @noRd
#'
#' @importFrom shiny moduleServer renderUI req tags
#' @importFrom DT renderDT datatable formatCurrency formatRound formatStyle styleEqual dataTableProxy selectRows
#' @import dplyr
mod_portfolio_positions_table_server <- function(id, results_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Get configured visible columns
    visible_columns <- get_default_visible_columns()

    # Reactive trigger for forcing table refresh (e.g., after creating a group)
    refresh_trigger <- reactiveVal(0)

    # Render empty state when no data
    output$empty_state <- renderUI({
      if (is.null(results_data()) || nrow(results_data()) == 0) {
        tags$div(
          class = "alert alert-info",
          style = "margin-top: 20px;",
          tags$h4("No Position Data"),
          tags$p("Click 'Refresh Positions' to fetch current positions from your Questrade accounts.")
        )
      } else {
        NULL
      }
    })

    # Render tabbed tables when data exists
    output$tabbed_tables <- renderUI({
      req(results_data())
      data <- results_data()

      if (nrow(data) == 0) {
        return(NULL)
      }

      # Get unique account types for tabs
      account_types <- unique(data$account_type)

      # Create tab panels
      tab_panels <- list(
        # All Accounts tab
        tabPanel(
          "All Accounts",
          tags$br(),
          DT::DTOutput(ns("table_all"))
        )
      )

      # Add individual account tabs
      for (account_type in account_types) {
        tab_panels[[length(tab_panels) + 1]] <- tabPanel(
          account_type,
          tags$br(),
          DT::DTOutput(ns(paste0("table_", account_type)))
        )
      }

      do.call(tabsetPanel, tab_panels)
    })

    # Render table for All Accounts
    output$table_all <- DT::renderDT({
      refresh_trigger()  # Create dependency on refresh trigger
      req(results_data())
      data <- results_data()

      if (nrow(data) == 0) {
        return(NULL)
      }

      create_positions_table(data, visible_columns, ns)
    })

    # Dynamically render tables for each account type
    observe({
      refresh_trigger()  # Create dependency on refresh trigger
      req(results_data())
      data <- results_data()

      if (nrow(data) == 0) {
        return(NULL)
      }

      account_types <- unique(data$account_type)

      for (account_type in account_types) {
        local({
          at <- account_type
          output_name <- paste0("table_", at)

          output[[output_name]] <- DT::renderDT({
            filtered_data <- data %>%
              filter(account_type == at)

            create_positions_table(filtered_data, visible_columns, ns)
          })
        })
      }
    })

    # Create proxy for programmatic table manipulation
    proxy <- dataTableProxy("table_all", session = session)

    # Return interface
    list(
      selected_rows = reactive({
        req(input$table_all_rows_selected)
        results_data()[input$table_all_rows_selected, ]
      }),

      clear_selection = function() {
        selectRows(proxy, NULL)  # NULL = deselect all rows
      },

      refresh = function() {
        refresh_trigger(refresh_trigger() + 1)  # Increment to invalidate and force re-render
      }
    )
  })
}

################################################################################
# TABLE CREATION HELPER
################################################################################

#' Create a formatted DT table for positions
#'
#' @param data Tibble with position data
#' @param visible_columns Character vector of column names to display
#' @param ns Namespace function (optional)
#' @return DT datatable object
#' @noRd
create_positions_table <- function(data, visible_columns, ns = NULL) {
  # Add group names if group_name is in visible_columns
  if ("group_name" %in% visible_columns) {
    # Fetch all group mappings once (single batch operation)
    group_lookup <- build_group_name_lookup()

    # Left join to add group names (vectorized operation)
    data <- data %>%
      left_join(
        group_lookup %>% select(account_number, symbol, group_name),
        by = c("account_number", "symbol")
      ) %>%
      mutate(
        group_name = if_else(is.na(group_name), "", group_name),
        is_grouped = if_else(group_name != "", TRUE, FALSE)
      )
  }

  # Select and reorder columns
  # Include is_grouped for conditional styling (will be hidden in table)
  if ("group_name" %in% visible_columns) {
    display_data <- data %>%
      select(all_of(visible_columns), is_grouped)
  } else {
    display_data <- data %>%
      select(all_of(visible_columns))
  }

  # Create column name mapping (snake_case to Title Case)
  colnames_map <- c(
    symbol = "Symbol",
    open_quantity = "Quantity",
    current_price = "Current Price",
    average_entry_price = "Avg Entry Price",
    current_market_value = "Current Value",
    total_cost = "Total Cost",
    open_pnl = "Unrealized P/L",
    day_pnl = "Day P/L",
    account_type = "Account",
    group_name = "Group"
  )

  # Rename columns in the data directly
  for (i in seq_along(visible_columns)) {
    old_name <- visible_columns[i]
    new_name <- colnames_map[[old_name]] %||% old_name
    names(display_data)[i] <- new_name
  }

  # Identify which renamed columns need currency formatting
  currency_col_names <- c("Current Price", "Avg Entry Price",
                          "Current Value", "Total Cost",
                          "Unrealized P/L", "Day P/L")

  # Get indices of currency columns that actually exist in display_data
  currency_col_indices <- which(names(display_data) %in% currency_col_names)

  # Determine if we need to hide the is_grouped column
  has_grouped_col <- "is_grouped" %in% names(display_data)
  grouped_col_idx <- if (has_grouped_col) which(names(display_data) == "is_grouped") - 1 else NULL  # DT uses 0-based indexing

  # Create base datatable
  dt_options <- list(
    pageLength = 25,
    lengthMenu = c(10, 25, 50, 100),
    order = list(list(0, 'asc')),  # Sort by first column
    dom = 'tip',  # Just table, info, and pagination
    searching = FALSE  # Disable search
  )

  # Hide is_grouped column if present
  if (has_grouped_col) {
    dt_options$columnDefs <- list(
      list(targets = grouped_col_idx, visible = FALSE)
    )
  }

  dt <- datatable(
    display_data,
    selection = list(mode = 'multiple', target = 'row'),  # Enable multi-row selection
    options = dt_options,
    rownames = FALSE,
    filter = 'none'  # Disable column filters
  )

  # Apply conditional row styling for grouped positions
  if (has_grouped_col) {
    dt <- dt %>%
      formatStyle(
        columns = 0,  # 0 = apply to entire row
        target = 'row',
        backgroundColor = styleEqual(
          c(FALSE, TRUE),
          c('transparent', 'rgba(16, 185, 129, 0.08)')  # Light success-green for grouped rows
        ),
        valueColumns = 'is_grouped'
      )
  }

  # Format currency columns
  if (length(currency_col_indices) > 0) {
    dt <- dt %>%
      formatCurrency(currency_col_indices, currency = "$", digits = 2)
  }

  # Format quantity column if present
  qty_col_index <- which(names(display_data) == "Quantity")
  if (length(qty_col_index) > 0) {
    dt <- dt %>%
      formatRound(qty_col_index, digits = 0, mark = ",")
  }

  dt
}

#' Null coalescing operator for table helper
#'
#' @param x Value to check
#' @param y Default value
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

## To be copied in the UI
# mod_portfolio_positions_table_ui("portfolio_positions_table_1")

## To be copied in the server
# mod_portfolio_positions_table_server("portfolio_positions_table_1", results_data)
