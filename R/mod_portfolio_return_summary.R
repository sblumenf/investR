#' Portfolio Return Summary UI Module
#'
#' @description Reusable Shiny module for displaying portfolio-wide expected
#' and realized returns with expandable strategy breakdown.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_portfolio_return_summary_ui <- function(id) {
 ns <- NS(id)
 tagList(
   uiOutput(ns("return_summary_container"))
 )
}

#' Portfolio Return Summary Server Functions
#'
#' @description Server logic for calculating and rendering portfolio return metrics.
#'
#' @param id Module ID
#' @param refresh_trigger Optional reactive to trigger recalculation
#'
#' @return Reactive with calculated return data (for downstream use)
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive renderUI req observeEvent reactiveVal
#' @importFrom dplyr %>% filter arrange desc mutate
#' @importFrom logger log_info log_debug log_error
mod_portfolio_return_summary_server <- function(id, refresh_trigger = NULL) {
 moduleServer(id, function(input, output, session) {
   ns <- session$ns

   # Reactive to store calculated returns
   return_data <- reactiveVal(NULL)

   # Calculate returns on load and when triggered
   observe({
     # React to refresh trigger if provided
     if (!is.null(refresh_trigger)) {
       refresh_trigger()
     }

     log_debug("Portfolio Return Summary: Calculating returns")

     tryCatch({
       data <- calculate_portfolio_expected_returns(include_breakdown = TRUE)
       return_data(data)
     }, error = function(e) {
       log_error("Portfolio Return Summary: Calculation failed - {e$message}")
       return_data(NULL)
     })
   })

   # Render the summary container
   output$return_summary_container <- renderUI({
     data <- return_data()

     if (is.null(data)) {
       return(create_status_alert(
         type = "info",
         message = "Loading portfolio returns..."
       ))
     }

     if (!is.null(data$error)) {
       return(create_status_alert(
         type = "warning",
         message = paste("Unable to calculate returns:", data$error)
       ))
     }

     # Build the compact summary display
     build_return_summary_ui(data, ns)
   })

   # Return the reactive data for downstream use
   return(return_data)
 })
}

#' Build the return summary UI
#'
#' @param data Return data from calculate_portfolio_expected_returns
#' @param ns Namespace function
#' @return Shiny tagList with UI elements
#' @noRd
build_return_summary_ui <- function(data, ns) {
 # Format return values
 expected_return_text <- if (is.na(data$expected_return_pct)) {
   "N/A"
 } else {
   sprintf("%+.1f%%", data$expected_return_pct)
 }

 realized_return_text <- if (is.na(data$realized_return_pct)) {
   "N/A"
 } else {
   sprintf("%+.1f%%", data$realized_return_pct)
 }

 # Determine color classes
 expected_class <- if (!is.na(data$expected_return_pct) && data$expected_return_pct < 0) {
   "text-danger"
 } else if (!is.na(data$expected_return_pct) && data$expected_return_pct > 0) {
   "text-success"
 } else {
   ""
 }

 realized_class <- if (!is.na(data$realized_return_pct) && data$realized_return_pct < 0) {
   "text-danger"
 } else if (!is.na(data$realized_return_pct) && data$realized_return_pct > 0) {
   "text-success"
 } else {
   ""
 }

 # Build exclusion note
 exclusion_note <- NULL
 if (data$exclusions$count > 0) {
   exclusion_note <- tags$small(
     class = "text-muted",
     style = "display: block; margin-top: 5px;",
     sprintf(
       "* Excludes %d position%s (%.1f%% of capital)",
       data$exclusions$count,
       if (data$exclusions$count > 1) "s" else "",
       data$exclusions$pct_of_portfolio * 100
     )
   )
 }

 # Build the main summary section
 summary_section <- tags$div(
   class = "well",
   style = "background-color: #f8f9fa; padding: 15px; margin-bottom: 15px;",

   # Header row with two metrics side by side
   tags$div(
     class = "row",
     style = "margin-bottom: 10px;",

     # Expected Return (Open Positions)
     tags$div(
       class = "col-sm-6",
       style = "text-align: center; border-right: 1px solid #dee2e6;",
       tags$div(
         style = "font-size: 0.85em; color: #6c757d; margin-bottom: 5px;",
         "Expected Return (Open)"
       ),
       tags$div(
         class = paste("h3", expected_class),
         style = "margin: 0; font-weight: bold;",
         expected_return_text
       ),
       tags$small(
         class = "text-muted",
         sprintf("$%s capital", format_currency_compact(data$total_open_capital))
       )
     ),

     # Realized Return (Closed Positions)
     tags$div(
       class = "col-sm-6",
       style = "text-align: center;",
       tags$div(
         style = "font-size: 0.85em; color: #6c757d; margin-bottom: 5px;",
         "Realized Return (Closed)"
       ),
       tags$div(
         class = paste("h3", realized_class),
         style = "margin: 0; font-weight: bold;",
         realized_return_text
       ),
       tags$small(
         class = "text-muted",
         sprintf("$%s capital", format_currency_compact(data$total_closed_capital))
       )
     )
   ),

   # Exclusion note
   exclusion_note,

   # Expandable breakdown section
   build_strategy_breakdown_accordion(data, ns)
 )

 return(summary_section)
}

#' Build the strategy breakdown accordion
#'
#' @param data Return data from calculate_portfolio_expected_returns
#' @param ns Namespace function
#' @return Shiny tags element
#' @noRd
build_strategy_breakdown_accordion <- function(data, ns) {
 if (nrow(data$strategy_breakdown) == 0) {
   return(NULL)
 }

 # Split breakdown by status
 open_breakdown <- data$strategy_breakdown %>%
   filter(status == "open") %>%
   arrange(desc(total_capital))

 closed_breakdown <- data$strategy_breakdown %>%
   filter(status == "closed") %>%
   arrange(desc(total_capital))

 # Build table rows for open positions
 open_rows <- if (nrow(open_breakdown) > 0) {
   lapply(seq_len(nrow(open_breakdown)), function(i) {
     row <- open_breakdown[i, ]
     return_class <- if (row$weighted_return_pct < 0) "text-danger" else "text-success"

     tags$tr(
       tags$td(row$strategy_type),
       tags$td(class = "text-center", row$position_count),
       tags$td(class = "text-right", format_currency_compact(row$total_capital)),
       tags$td(class = paste("text-right", return_class), sprintf("%+.1f%%", row$weighted_return_pct)),
       tags$td(class = "text-right", sprintf("%.0f%%", row$pct_of_portfolio * 100))
     )
   })
 } else {
   list(tags$tr(tags$td(colspan = "5", class = "text-center text-muted", "No open positions")))
 }

 # Build table rows for closed positions
 closed_rows <- if (nrow(closed_breakdown) > 0) {
   lapply(seq_len(nrow(closed_breakdown)), function(i) {
     row <- closed_breakdown[i, ]
     return_class <- if (row$weighted_return_pct < 0) "text-danger" else "text-success"

     tags$tr(
       tags$td(row$strategy_type),
       tags$td(class = "text-center", row$position_count),
       tags$td(class = "text-right", format_currency_compact(row$total_capital)),
       tags$td(class = paste("text-right", return_class), sprintf("%+.1f%%", row$weighted_return_pct)),
       tags$td(class = "text-right", sprintf("%.0f%%", row$pct_of_portfolio * 100))
     )
   })
 } else {
   list(tags$tr(tags$td(colspan = "5", class = "text-center text-muted", "No closed positions")))
 }

 # Create the accordion
 tags$details(
   class = "accordion-section",
   style = "margin-top: 15px;",

   tags$summary(
     class = "accordion-header",
     style = "cursor: pointer; font-weight: 500; color: #495057;",
     icon("chart-pie"),
     " Strategy Breakdown"
   ),

   tags$div(
     class = "accordion-body",
     style = "padding-top: 10px;",

     # Open positions section
     tags$h6(
       style = "margin-top: 10px; color: #28a745;",
       icon("folder-open"),
       " Open Positions"
     ),
     tags$table(
       class = "table table-sm table-hover",
       style = "font-size: 0.9em;",
       tags$thead(
         tags$tr(
           tags$th("Strategy"),
           tags$th(class = "text-center", "Positions"),
           tags$th(class = "text-right", "Capital"),
           tags$th(class = "text-right", "Wtd Return"),
           tags$th(class = "text-right", "% of Open")
         )
       ),
       tags$tbody(
         open_rows
       )
     ),

     # Closed positions section
     tags$h6(
       style = "margin-top: 20px; color: #6c757d;",
       icon("folder"),
       " Closed Positions"
     ),
     tags$table(
       class = "table table-sm table-hover",
       style = "font-size: 0.9em;",
       tags$thead(
         tags$tr(
           tags$th("Strategy"),
           tags$th(class = "text-center", "Positions"),
           tags$th(class = "text-right", "Capital"),
           tags$th(class = "text-right", "Wtd Return"),
           tags$th(class = "text-right", "% of Closed")
         )
       ),
       tags$tbody(
         closed_rows
       )
     )
   )
 )
}

#' Format currency in compact form
#'
#' @param value Numeric value
#' @return Formatted string (e.g., "45.2K", "1.2M")
#' @noRd
format_currency_compact <- function(value) {
 if (is.na(value) || value == 0) {
   return("0")
 }

 abs_value <- abs(value)
 sign <- if (value < 0) "-" else ""

 if (abs_value >= 1000000) {
   sprintf("%s%.1fM", sign, abs_value / 1000000)
 } else if (abs_value >= 1000) {
   sprintf("%s%.1fK", sign, abs_value / 1000)
 } else {
   sprintf("%s%.0f", sign, abs_value)
 }
}
