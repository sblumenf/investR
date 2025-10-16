#' Setup Analysis Controls Helper
#'
#' @description A reusable helper function for setting up analysis controls within a Shiny module.
#'   This function must be called from within a moduleServer context and receives the
#'   input, output, and session objects from the parent module.
#'
#'   This helper abstracts the common pattern of: run button -> progress -> analysis ->
#'   status message -> download handler. Used by all strategy modules to eliminate duplication.
#'
#' @param input Shiny input object from parent moduleServer
#' @param output Shiny output object from parent moduleServer
#' @param session Shiny session object from parent moduleServer
#' @param analysis_func Function to call when run button is clicked. Should return a tibble or NULL on error.
#' @param progress_message Character string to display during analysis
#' @param success_message_template Character string template with %d placeholder for number of results.
#'   Example: "Analysis complete! Found %d opportunities."
#' @param no_results_message Character string to display when analysis returns 0 rows
#' @param download_filename_prefix Character string prefix for downloaded CSV filename
#' @param additional_return_values Named list of additional reactive values to return (optional)
#'
#' @return A list with reactive values:
#'   - results_data: reactiveVal containing analysis results
#'   - status_ui: reactive returning status message UI
#'   - Any additional values passed via additional_return_values parameter
#'
#' @details
#' This is a helper function, not a standalone Shiny module. It must be called from
#' within a moduleServer context. The function sets up observers and outputs within
#' the parent module's namespace.
#'
#' @noRd
#'
#' @importFrom shiny reactiveVal observeEvent downloadHandler req reactive
#' @importFrom readr write_csv
setup_analysis_controls <- function(input,
                                   output,
                                   session,
                                   analysis_func,
                                   progress_message = "Analysis in progress...",
                                   success_message_template = "Analysis complete! Found %d results.",
                                   no_results_message = "No results found with current parameters.",
                                   download_filename_prefix = "analysis",
                                   additional_return_values = NULL) {

  # Reactive values to store results and status
  results_data <- reactiveVal(NULL)
  status_message <- reactiveVal(NULL)

  # Run analysis when button is clicked
  observeEvent(input$run_analysis, {
    # Reset fallback tracker before starting analysis
    reset_fallback_tracker()

    # Show progress message
    status_message(create_progress_alert(progress_message))

    # Run analysis function
    results <- tryCatch({
      analysis_func()
    }, error = function(e) {
      # Show error message
      status_message(
        create_status_alert(
          type = "danger",
          message = paste("Error:", e$message)
        )
      )
      return(NULL)
    })

    # Store results
    results_data(results)

    # Update status message (only if no error occurred)
    if (!is.null(results)) {
      if (nrow(results) > 0) {
        # Success - found results
        status_message(
          create_status_alert(
            type = "success",
            message = sprintf(success_message_template, nrow(results))
          )
        )
      } else {
        # Warning - no results
        status_message(
          create_status_alert(
            type = "warning",
            message = no_results_message
          )
        )
      }
    }

    # Check for Questrade fallbacks and notify user
    check_and_notify_fallbacks()
  })

  # Download handler - works with both download_results and download_csv button IDs
  download_handler_func <- downloadHandler(
    filename = function() {
      paste0(
        download_filename_prefix,
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv"
      )
    },
    content = function(file) {
      req(results_data())
      write_csv(results_data(), file)
    }
  )

  # Assign to both possible download button IDs for backward compatibility
  output$download_results <- download_handler_func
  output$download_csv <- download_handler_func

  # Build return list
  return_list <- list(
    results_data = results_data,
    status_ui = reactive({ status_message() })
  )

  # Add any additional reactive values
  if (!is.null(additional_return_values)) {
    return_list <- c(return_list, additional_return_values)
  }

  return(return_list)
}
