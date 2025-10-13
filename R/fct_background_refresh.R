#' Background Questrade Data Refresh
#'
#' Functions for automatically fetching data from Questrade in the background.
#'
#' @name background-refresh
#' @importFrom logger log_info log_error
#' @importFrom promises future_promise
NULL

#' Refresh Questrade Activities
#'
#' Fetches the latest account activities from Questrade API and saves them
#' to the database. This function is designed to be called from a background
#' process or a timer.
#'
#' @return A promise that resolves with the result of fetch_all_activities(),
#'   which is a tibble of fetched activities.
#' @export
refresh_questrade_activities <- function() {
  log_info("Background Refresh: Starting Questrade activities refresh...")

  # Use a future_promise to ensure this runs in the background and does not
  # block the main Shiny thread.
  future_promise({
    result <- tryCatch({
      fetch_all_activities()
    }, error = function(e) {
      log_error("Background Refresh: Failed to fetch activities - {e$message}")
      return(NULL)
    })
    log_info("Background Refresh: Questrade activities refresh finished.")
    result
  })
}
