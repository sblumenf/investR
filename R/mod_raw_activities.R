#' Raw Activities UI Function
#'
#' @description A shiny Module for displaying all transactions from the database.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT dataTableOutput
mod_raw_activities_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h2("Raw Account Activities Log"),
    tags$p("This table displays all records from the `account_activities` table for debugging and verification."),
    tags$hr(),
    DT::dataTableOutput(ns("raw_table"))
  )
}

#' Raw Activities Server Functions
#'
#' @noRd
mod_raw_activities_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Fetch all activities from the database
    all_activities <- reactive({
      conn <- get_portfolio_db_connection()
      on.exit(dbDisconnect(conn))
      dbGetQuery(conn, "SELECT * FROM account_activities ORDER BY trade_date DESC")
    })

    # Render the datatable
    output$raw_table <- DT::renderDataTable({
      all_activities()
    }, options = list(
      pageLength = 50,
      scrollX = TRUE
    ), filter = "top")

  })
}
