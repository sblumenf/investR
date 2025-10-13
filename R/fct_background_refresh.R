#' Background Questrade Data Refresh
#'
#' Functions for automatically fetching data from Questrade in the background.
#'
#' @name background-refresh
#' @importFrom logger log_info log_error log_warn
#' @importFrom promises future_promise
NULL

################################################################################
# REFRESH STATUS TRACKING
################################################################################

# Module-level state for tracking refresh status
# Used by UI to display alerts about refresh failures
refresh_status <- new.env(parent = emptyenv())
refresh_status$activities_last_success <- NULL
refresh_status$positions_last_success <- NULL
refresh_status$activities_last_error <- NULL
refresh_status$positions_last_error <- NULL

#' Get the current refresh status
#'
#' Returns information about the last successful and failed refreshes
#' for both activities and positions data.
#'
#' @return List with status information
#' @export
get_refresh_status <- function() {
  list(
    activities = list(
      last_success = refresh_status$activities_last_success,
      last_error = refresh_status$activities_last_error
    ),
    positions = list(
      last_success = refresh_status$positions_last_success,
      last_error = refresh_status$positions_last_error
    )
  )
}

#' Refresh Questrade Activities
#'
#' Fetches the latest account activities from Questrade API and saves them
#' to the database. This function is designed to be called from a background
#' process or a timer. Updates refresh status for UI display.
#'
#' @return A promise that resolves with the result of fetch_all_activities(),
#'   which is a tibble of fetched activities.
#' @export
refresh_questrade_activities <- function() {
  log_info("Background Refresh: Starting Questrade activities refresh...")

  # Use a future_promise to ensure this runs in the background and does not
  # block the main Shiny thread.
  # seed = TRUE ensures parallel-safe RNG for statistical correctness
  future_promise(seed = TRUE, {
    result <- tryCatch({
      data <- fetch_all_activities()

      # Update success timestamp
      refresh_status$activities_last_success <- Sys.time()
      refresh_status$activities_last_error <- NULL

      log_info("Background Refresh: Questrade activities refresh finished.")
      data
    }, error = function(e) {
      # Update error timestamp and message
      refresh_status$activities_last_error <- list(
        timestamp = Sys.time(),
        message = e$message
      )

      log_error("Background Refresh: Failed to fetch activities - {e$message}")
      return(NULL)
    })

    result
  })
}

#' Refresh Questrade Positions
#'
#' Fetches the latest position data from Questrade API and saves a snapshot
#' to the positions_history table. This function is designed to be called from
#' a background process or a timer to keep stock prices up-to-date.
#' Updates refresh status for UI display.
#'
#' @return A promise that resolves with the save result ("saved", "unchanged", or "error")
#' @export
refresh_questrade_positions <- function() {
  log_info("Background Refresh: Starting Questrade positions refresh...")

  # Use a future_promise to ensure this runs in the background and does not
  # block the main Shiny thread.
  # seed = TRUE ensures parallel-safe RNG for statistical correctness
  future_promise(seed = TRUE, {
    result <- tryCatch({
      # Fetch current positions from Questrade
      positions <- fetch_all_positions_sequential()

      if (is.null(positions) || nrow(positions) == 0) {
        # Update error state
        refresh_status$positions_last_error <- list(
          timestamp = Sys.time(),
          message = "No positions fetched from Questrade"
        )

        log_warn("Background Refresh: No positions fetched from Questrade")
        return("error")
      }

      # Save snapshot to database
      save_result <- save_positions_snapshot(positions, Sys.time())
      log_info("Background Refresh: Positions snapshot save result: {save_result}")

      # Update success timestamp
      refresh_status$positions_last_success <- Sys.time()
      refresh_status$positions_last_error <- NULL

      log_info("Background Refresh: Questrade positions refresh finished.")
      return(save_result)
    }, error = function(e) {
      # Update error timestamp and message
      refresh_status$positions_last_error <- list(
        timestamp = Sys.time(),
        message = e$message
      )

      log_error("Background Refresh: Failed to fetch/save positions - {e$message}")
      return("error")
    })

    result
  })
}
