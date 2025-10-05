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
#' @importFrom DT renderDT datatable formatCurrency formatRound
#' @import dplyr
mod_portfolio_positions_table_server <- function(id, results_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Get configured visible columns
    visible_columns <- get_default_visible_columns()

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
      req(results_data())
      data <- results_data()

      if (nrow(data) == 0) {
        return(NULL)
      }

      create_positions_table(data, visible_columns)
    })

    # Dynamically render tables for each account type
    observe({
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

            create_positions_table(filtered_data, visible_columns)
          })
        })
      }
    })
  })
}

################################################################################
# TABLE CREATION HELPER
################################################################################

#' Create a formatted DT table for positions
#'
#' @param data Tibble with position data
#' @param visible_columns Character vector of column names to display
#' @return DT datatable object
#' @noRd
create_positions_table <- function(data, visible_columns) {
  # Select and reorder columns
  display_data <- data %>%
    select(all_of(visible_columns))

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
    account_type = "Account"
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

  # Create base datatable
  dt <- datatable(
    display_data,
    options = list(
      pageLength = 25,
      lengthMenu = c(10, 25, 50, 100),
      order = list(list(0, 'asc')),  # Sort by first column
      dom = 'tip',  # Just table, info, and pagination
      searching = FALSE  # Disable search
    ),
    rownames = FALSE,
    filter = 'none'  # Disable column filters
  )

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
