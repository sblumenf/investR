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
    # JavaScript for handling Close Group button clicks
    tags$script(HTML(sprintf("
      $(document).on('click', '.close-group-btn', function() {
        var groupId = $(this).data('group-id');
        var groupName = $(this).data('group-name');
        Shiny.setInputValue('%s', {group_id: groupId, group_name: groupName}, {priority: 'event'});
      });
    ", ns("close_group_clicked")))),

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

    # Store the data with row IDs for selection matching
    table_data_with_ids <- reactiveVal(NULL)

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

      # Trim symbols and sort
      sorted_data <- data %>%
        mutate(symbol = stringr::str_trim(symbol)) %>%
        arrange(symbol)

      # Create table and get the final display data
      result <- create_positions_table(sorted_data, visible_columns, ns)

      # Store the exact data that will be displayed (for selection lookup)
      table_data_with_ids(result$display_data)

      # Return the DT table
      result$dt
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
              filter(account_type == at) %>%
              mutate(symbol = stringr::str_trim(symbol)) %>%
              arrange(symbol)

            result <- create_positions_table(filtered_data, visible_columns, ns)
            result$dt
          })
        })
      }
    })

    # Create proxy for programmatic table manipulation
    proxy <- dataTableProxy("table_all", session = session)

    # Handle Close Group button click
    observeEvent(input$close_group_clicked, {
      group_info <- input$close_group_clicked

      # Show confirmation modal
      showModal(modalDialog(
        title = "Close Position Group",
        size = "m",

        p(sprintf("Are you sure you want to close the group '%s'?", group_info$group_name)),
        p("This will:"),
        tags$ul(
          tags$li("Calculate final P&L for the group"),
          tags$li("Mark the group as closed"),
          tags$li("Remove all projected future cash flows")
        ),

        footer = tagList(
          actionButton(ns("cancel_close"), "Cancel"),
          actionButton(ns("confirm_close"), "Close Group", class = "btn-danger")
        )
      ))
    })

    # Handle confirmation of group closure
    observeEvent(input$confirm_close, {
      group_info <- input$close_group_clicked

      # Close the group and get P&L
      pnl <- close_position_group(group_info$group_id)

      if (nrow(pnl) > 0) {
        # Show P&L results
        showNotification(
          sprintf(
            "Group '%s' closed. Net P&L: $%.2f (%.2f%% total, %.2f%% annualized)",
            group_info$group_name,
            pnl$net_pnl,
            pnl$total_return_pct,
            pnl$annualized_return_pct
          ),
          type = "message",
          duration = 10
        )

        # Refresh table to show updated status
        refresh_trigger(refresh_trigger() + 1)
      } else {
        showNotification("Failed to close group", type = "error", duration = 5)
      }

      removeModal()
    })

    # Handle cancel
    observeEvent(input$cancel_close, {
      removeModal()
    })

    # Return interface
    list(
      selected_rows = reactive({
        req(input$table_all_rows_selected)
        req(table_data_with_ids())

        # Get selected indices
        selected_indices <- input$table_all_rows_selected
        sorted_data <- table_data_with_ids()

        # Simply extract the rows at those indices from the sorted data
        selected_data <- sorted_data[selected_indices, ]

        selected_data
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
#' @return List with display_data (raw data as displayed) and dt (DT object)
#' @noRd
create_positions_table <- function(data, visible_columns, ns = NULL) {
  # Assume data is already trimmed and sorted by caller

  # Add group names and status if group_name is in visible_columns
  if ("group_name" %in% visible_columns) {
    # Fetch all group mappings once (single batch operation)
    group_lookup <- build_group_name_lookup()

    # Get all groups to determine status
    all_groups <- get_all_groups()

    # If no groups exist, add empty columns directly
    if (nrow(group_lookup) == 0) {
      data <- data %>%
        mutate(
          group_name = "",
          is_grouped = FALSE,
          group_id = "",
          status = ""
        )
    } else {
      # Left join to add group names (vectorized operation)
      data <- data %>%
        left_join(
          group_lookup %>% select(account_number, symbol, group_name, group_id),
          by = c("account_number", "symbol")
        ) %>%
        left_join(
          all_groups %>% select(group_id, status),
          by = "group_id"
        ) %>%
        mutate(
          group_name = if_else(is.na(group_name), "", group_name),
          is_grouped = if_else(group_name != "", TRUE, FALSE),
          group_id = if_else(is.na(group_id), "", group_id),
          status = if_else(is.na(status), "", status)
        )
    }
  }

  # Add Actions column for groups if applicable
  if ("group_name" %in% visible_columns) {
    # Create actions column: show Close button only for first row of each open group
    data <- data %>%
      group_by(group_id) %>%
      mutate(
        is_first_in_group = row_number() == 1,
        show_close_btn = is_first_in_group & status == "open" & group_id != ""
      ) %>%
      ungroup() %>%
      mutate(
        actions = if_else(
          show_close_btn,
          sprintf(
            '<button class="btn btn-danger btn-xs close-group-btn" data-group-id="%s" data-group-name="%s">Close</button>',
            group_id,
            group_name
          ),
          ""
        )
      )
  }

  # Select and reorder columns
  # Always include account_number and account_type for selection logic
  # Include is_grouped, group_id, status for tracking (will be hidden in table)
  essential_cols <- c("account_number", "account_type")

  if ("group_name" %in% visible_columns) {
    display_data <- data %>%
      select(all_of(c(essential_cols, visible_columns)), actions, is_grouped, group_id, status)
  } else {
    display_data <- data %>%
      select(all_of(c(essential_cols, visible_columns)))
  }

  # Save the raw data with original column names for selection tracking
  raw_display_data <- display_data

  # Create column name mapping (snake_case to Title Case)
  colnames_map <- c(
    account_number = "account_number",  # Keep original name (will be hidden)
    account_type = "account_type",      # Keep original name (will be hidden)
    symbol = "Symbol",
    open_quantity = "Quantity",
    current_price = "Current Price",
    average_entry_price = "Avg Entry Price",
    current_market_value = "Current Value",
    total_cost = "Total Cost",
    open_pnl = "Unrealized P/L",
    day_pnl = "Day P/L",
    group_name = "Group",
    actions = "Actions",
    is_grouped = "is_grouped",  # Keep original name (will be hidden)
    group_id = "group_id",      # Keep original name (will be hidden)
    status = "status"            # Keep original name (will be hidden)
  )

  # Rename columns in the data directly
  # Handle both visible_columns and any added columns (actions, is_grouped, etc.)
  all_col_names <- names(display_data)
  for (i in seq_along(all_col_names)) {
    old_name <- all_col_names[i]
    new_name <- colnames_map[[old_name]] %||% old_name
    names(display_data)[i] <- new_name
  }

  # Identify which renamed columns need currency formatting
  currency_col_names <- c("Current Price", "Avg Entry Price",
                          "Current Value", "Total Cost",
                          "Unrealized P/L", "Day P/L")

  # Get indices of currency columns that actually exist in display_data
  currency_col_indices <- which(names(display_data) %in% currency_col_names)

  # Determine which columns to hide (account_number, account_type, is_grouped, group_id, status)
  cols_to_hide <- c("account_number", "account_type", "is_grouped", "group_id", "status")
  hidden_col_indices <- which(names(display_data) %in% cols_to_hide) - 1  # DT uses 0-based indexing
  has_hidden_cols <- length(hidden_col_indices) > 0

  # Create base datatable
  dt_options <- list(
    pageLength = 100,
    lengthMenu = c(25, 50, 100, 150),
    ordering = FALSE,  # Disable client-side sorting (data pre-sorted server-side)
    dom = 'ltip',  # Length menu, table, info, and pagination
    searching = FALSE  # Disable search
  )

  # Hide metadata columns if present
  if (has_hidden_cols) {
    dt_options$columnDefs <- list(
      list(targets = hidden_col_indices, visible = FALSE)
    )
  }

  dt <- datatable(
    display_data,
    selection = list(mode = 'multiple', target = 'row'),  # Enable multi-row selection
    options = dt_options,
    rownames = FALSE,
    filter = 'none',  # Disable column filters
    escape = FALSE  # Enable HTML rendering for Actions column
  )

  # Apply conditional row styling for grouped positions
  if ("is_grouped" %in% names(display_data)) {
    dt <- dt %>%
      formatStyle(
        columns = 0,  # 0 = apply to entire row
        target = 'row',
        backgroundColor = styleEqual(
          c(FALSE, TRUE),
          c('transparent', 'rgba(168, 85, 247, 0.6)')  # Purple for grouped rows
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

  # Return both the raw data and the formatted DT
  list(
    display_data = raw_display_data,
    dt = dt
  )
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
