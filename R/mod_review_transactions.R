#' Review Transactions Modal UI Function
#'
#' @description A shiny Module for the transaction review workflow.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList modalDialog fluidRow column actionButton uiOutput showModal removeModal showNotification reactiveVal reactive req moduleServer selectInput icon modalButton tags textInput
#' @importFrom dplyr select mutate %>%
#' @importFrom purrr map_chr
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom logger log_info
#' @importFrom tibble tibble
mod_review_transactions_ui <- function(id){
  ns <- NS(id)
  # The UI is rendered entirely on the server side within a modalDialog
  tagList()
}

#' Review Transactions Modal Server Functions
#'
#' @param id Module ID
#' @param trigger A reactive trigger to launch the modal.
#' @param virgin_by_ticker A reactive list of virgin transactions, grouped by ticker.
#'
#' @noRd
mod_review_transactions_server <- function(id, trigger, virgin_by_ticker){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # --- State Management ---
    # Index of the current ticker being reviewed
    current_index <- reactiveVal(1)
    # Mutable copy of virgin_by_ticker for refreshing after actions
    ticker_data <- reactiveVal(NULL)
    # Total number of tickers to review
    total_tickers <- reactive(length(ticker_data()))
    # Track actions to trigger banner refresh in parent module
    action_count <- reactiveVal(0)
    # Track which transaction rows are selected via checkboxes
    selected_rows <- reactiveVal(integer())

    # Helper function to fetch and group unlinked activities
    refresh_data <- function() {
      # Auto-link cash equivalents before fetching
      auto_link_cash_equivalents()

      unlinked <- get_unlinked_activities()

      if (nrow(unlinked) == 0) {
        return(list())
      }

      # Add ticker column (extract underlying ticker from option symbols)
      unlinked <- unlinked %>%
        mutate(ticker = purrr::map_chr(symbol, function(sym) {
          if (grepl("\\d{2}[A-Z][a-z]{2}\\d{2}[CP]", sym)) {
            result <- parse_option_symbol(sym)
            if (is.na(result)) sym else result
          } else {
            sym
          }
        }))

      # Group by ticker (combines all symbols for same underlying)
      split(unlinked, unlinked$ticker)
    }

    # --- Modal Launch ---
    observeEvent(trigger(), {
      req(trigger() > 0)

      # Initialize ticker_data with data from parent
      ticker_data(virgin_by_ticker())

      req(total_tickers() > 0)

      # Reset index to 1 every time modal is launched
      current_index(1)

      showModal(review_modal())
    })

    # Re-render modal when index changes (enables Next/Prev navigation)
    observeEvent(current_index(), {
      req(ticker_data())
      req(current_index() > 0)
      req(current_index() <= total_tickers())
      showModal(review_modal())
    }, ignoreInit = TRUE)

    # --- Dynamic Modal UI ---
    review_modal <- reactive({
      req(ticker_data())
      req(current_index() <= total_tickers())

      current_ticker_symbol <- names(ticker_data())[current_index()]
      current_activities <- ticker_data()[[current_index()]]
      
      # Get all existing open groups for the dropdown
      open_groups <- get_all_groups(include_closed = FALSE)
      group_choices <- setNames(open_groups$group_id, open_groups$group_name)

      modalDialog(
        title = tagList(
          tags$h3(sprintf("Reviewing %d transactions for: %s", nrow(current_activities), current_ticker_symbol)),
          tags$p(sprintf("Item %d of %d", current_index(), total_tickers()))
        ),
        size = "l",
        easyClose = TRUE,

        # Selection helper buttons
        fluidRow(
          column(12,
            actionButton(ns("select_all"), "Select All", class = "btn-sm"),
            actionButton(ns("deselect_all"), "Deselect All", class = "btn-sm"),
            tags$span(id = ns("selection_info"), style = "margin-left: 15px;")
          )
        ),
        tags$br(),

        # Transaction table
        DT::dataTableOutput(ns("transaction_table")),

        tags$hr(),

        # Actions
        fluidRow(
          column(6,
            tags$h4("Link to Existing Group"),
            selectInput(ns("existing_group_id"), label = "Select Group", choices = group_choices),
            actionButton(ns("link_to_group"), "Link to Group", icon = icon("link"))
          ),
          column(6,
            tags$h4("Other Actions"),
            actionButton(ns("create_new_group"), "Create New Group", icon = icon("plus"), class = "btn-success"),
            tags$br(), tags$br(),
            actionButton(ns("ignore_transactions"), "Ignore These Transactions", icon = icon("ban"), class = "btn-warning")
          )
        ),

        footer = tagList(
          modalButton("Close"),
          actionButton(ns("prev_ticker"), "Previous"),
          actionButton(ns("next_ticker"), "Next")
        )
      )
    })
    
    # Render the transaction table for the current ticker
    output$transaction_table <- DT::renderDataTable({
      req(ticker_data())
      req(current_index() <= total_tickers())
      ticker_data()[[current_index()]] %>%
        select(trade_date, action, description, quantity, net_amount)
    }, options = list(dom = 't', paging = FALSE), selection = list(mode = 'multiple', target = 'row'))

    # Update selected_rows when user clicks checkboxes
    observeEvent(input$transaction_table_rows_selected, {
      selected_rows(input$transaction_table_rows_selected)
    })

    # --- Server-Side Action Logic ---

    # Helper function to advance to the next ticker or close modal
    advance_or_close <- function() {
      # Refresh data from database to get current unlinked activities
      fresh_data <- refresh_data()
      ticker_data(fresh_data)

      # If no more unlinked activities, close modal
      if (length(fresh_data) == 0) {
        removeModal()
        showNotification("All transactions have been processed!", type = "message")
        return()
      }

      # If current index is now beyond the end, reset to last ticker
      if (current_index() > length(fresh_data)) {
        current_index(length(fresh_data))
      }

      # Current index is still valid, modal will auto-refresh via observer
    }
    
    # --- Button Handlers ---
    observeEvent(input$next_ticker, {
      req(current_index() < total_tickers())
      selected_rows(integer())  # Clear selection when changing tickers
      current_index(current_index() + 1)
    })

    observeEvent(input$prev_ticker, {
      req(current_index() > 1)
      selected_rows(integer())  # Clear selection when changing tickers
      current_index(current_index() - 1)
    })

    observeEvent(input$select_all, {
      req(ticker_data())
      req(current_index() <= total_tickers())
      num_rows <- nrow(ticker_data()[[current_index()]])
      DT::selectRows(DT::dataTableProxy("transaction_table"), 1:num_rows)
    })

    observeEvent(input$deselect_all, {
      DT::selectRows(DT::dataTableProxy("transaction_table"), NULL)
    })

    observeEvent(input$ignore_transactions, {
      req(length(selected_rows()) > 0)
      current_activities <- ticker_data()[[current_index()]]
      activity_ids <- current_activities$activity_id[selected_rows()]
      log_info(sprintf("Ignoring %d activities for ticker %s", length(activity_ids), names(ticker_data())[current_index()]))
      batch_ignore_activities(activity_ids)
      action_count(action_count() + 1)
      advance_or_close()
    })

    observeEvent(input$link_to_group, {
      req(input$existing_group_id)
      req(length(selected_rows()) > 0)
      current_activities <- ticker_data()[[current_index()]]
      activity_ids <- current_activities$activity_id[selected_rows()]
      group_id <- input$existing_group_id
      log_info(sprintf("Linking %d activities to group %s", length(activity_ids), group_id))
      link_activities_to_group(activity_ids, group_id)
      action_count(action_count() + 1)
      advance_or_close()
    })

    observeEvent(input$create_new_group, {
      # Get current ticker and activities
      current_ticker <- names(ticker_data())[current_index()]
      current_activities <- ticker_data()[[current_index()]]

      # Parse ticker from option symbols if needed
      # Option symbols have format: TICKER##Mon##CP### (e.g., HYG15Jan27P60.00)
      parsed_ticker <- if (grepl("\\d{2}[A-Z][a-z]{2}\\d{2}[CP]", current_ticker)) {
        parse_option_symbol(current_ticker)
      } else {
        current_ticker
      }

      # Generate default group name
      default_name <- sprintf("%s Position - %s",
                             parsed_ticker,
                             format(Sys.Date(), "%b %Y"))

      # Get strategy types
      strategy_types <- get_strategy_types()

      # Show confirmation modal
      showModal(modalDialog(
        title = "Create New Position Group",
        size = "m",

        textInput(
          ns("new_group_name"),
          "Group Name:",
          value = default_name
        ),

        selectInput(
          ns("new_group_strategy"),
          "Strategy Type:",
          choices = strategy_types,
          selected = "Other"
        ),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_create_group"), "Create", class = "btn-primary")
        )
      ))
    })

    # Handle confirmation of group creation
    observeEvent(input$confirm_create_group, {
      req(length(selected_rows()) > 0)

      # Get values from form
      group_name <- input$new_group_name
      strategy_type <- input$new_group_strategy

      # Get current ticker and SELECTED activities only
      current_ticker <- names(ticker_data())[current_index()]
      current_activities <- ticker_data()[[current_index()]]
      selected_activities <- current_activities[selected_rows(), ]

      log_info(sprintf("Creating new group '%s' for ticker: %s with %d selected activities",
                       group_name, current_ticker, nrow(selected_activities)))

      # Get account number from first selected activity
      account_number <- selected_activities$account_number[1]

      # Generate group ID
      group_id <- generate_group_id(strategy_type, account_number)

      # Build complete members list from selected activities
      unique_symbols <- unique(selected_activities$symbol)

      members <- tibble::tibble(
        symbol = unique_symbols,
        role = purrr::map_chr(unique_symbols, function(sym) {
          if (is_option_symbol(sym)) {
            "short_call"
          } else {
            "underlying_stock"
          }
        })
      )

      # Create the group
      success <- create_position_group(
        group_id = group_id,
        group_name = group_name,
        strategy_type = strategy_type,
        account_number = account_number,
        members = members,
        auto_name = FALSE
      )

      if (!success) {
        showNotification(
          "Failed to create group. Please try again.",
          type = "error",
          duration = 5
        )
        return()
      }

      # Link only selected activities to the group
      activity_ids <- selected_activities$activity_id
      link_success <- link_activities_to_group(activity_ids, group_id)

      if (!link_success) {
        showNotification(
          "Group created but failed to link activities.",
          type = "warning",
          duration = 5
        )
        return()
      }

      # Generate initial income projections for the new group
      projection_success <- generate_initial_projections(
        group_id = group_id,
        members = members,
        account_number = account_number
      )

      if (projection_success) {
        log_info(sprintf("Generated initial projections for group %s", group_id))
      } else {
        log_warn(sprintf("Failed to generate projections for group %s", group_id))
      }

      # Success
      showNotification(
        sprintf("Created group: %s", group_name),
        type = "message",
        duration = 3
      )

      log_info(sprintf("Successfully created group %s and linked %d activities",
                      group_id, length(activity_ids)))

      # Increment action count to trigger banner refresh
      action_count(action_count() + 1)

      # Remove the confirmation modal
      removeModal()

      # Move to next ticker or close review modal
      advance_or_close()
    })

    # Return reactive to allow parent module to track actions
    return(reactive(action_count()))
  })
}
