#' Review Transactions Modal UI Function
#'
#' @description A shiny Module for the transaction review workflow.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList modalDialog fluidRow column actionButton uiOutput
#' @importFrom DT dataTableOutput renderDataTable
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
    # Total number of tickers to review
    total_tickers <- reactive(length(virgin_by_ticker()))

    # --- Modal Launch ---
    observeEvent(trigger(), {
      req(trigger() > 0)
      req(total_tickers() > 0)

      # Reset index to 1 every time modal is launched
      current_index(1)

      showModal(review_modal())
    })

    # --- Dynamic Modal UI ---
    review_modal <- reactive({
      req(virgin_by_ticker())
      req(current_index() <= total_tickers())

      current_ticker_symbol <- names(virgin_by_ticker())[current_index()]
      current_activities <- virgin_by_ticker()[[current_index()]]
      
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
      virgin_by_ticker()[[current_index()]] %>% 
        select(trade_date, action, description, quantity, net_amount)
    }, options = list(dom = 't', pageLength = 10))

    # --- Server-Side Action Logic ---

    # Helper function to advance to the next ticker or close modal
    advance_or_close <- function() {
      if (current_index() < total_tickers()) {
        current_index(current_index() + 1)
      } else {
        removeModal()
        # Optionally, show a completion notification
        showNotification("All transactions have been processed!", type = "message")
      }
    }
    
    # --- Button Handlers ---
    observeEvent(input$next_ticker, {
      req(current_index() < total_tickers())
      current_index(current_index() + 1)
    })
    
    observeEvent(input$prev_ticker, {
      req(current_index() > 1)
      current_index(current_index() - 1)
    })
    
    observeEvent(input$ignore_transactions, {
      activity_ids <- virgin_by_ticker()[[current_index()]]$activity_id
      log_info(sprintf("Ignoring %d activities for ticker %s", length(activity_ids), names(virgin_by_ticker())[current_index()]))
      batch_ignore_activities(activity_ids)
      advance_or_close()
    })
    
    observeEvent(input$link_to_group, {
      req(input$existing_group_id)
      activity_ids <- virgin_by_ticker()[[current_index()]]$activity_id
      group_id <- input$existing_group_id
      log_info(sprintf("Linking %d activities to group %s", length(activity_ids), group_id))
      link_activities_to_group(activity_ids, group_id)
      advance_or_close()
    })
    
    observeEvent(input$create_new_group, {
      # For now, just log and advance. A sub-modal would be needed here.
      log_info("Triggered 'Create New Group'. This would launch another modal.")
      showNotification("'Create New Group' is not yet implemented.", type = "warning")
      # advance_or_close()
    })

  })
}
