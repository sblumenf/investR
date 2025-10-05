#' Questrade API Health Check
#'
#' Minimal health check to keep Questrade refresh token alive.
#' Automatically rotates tokens on every ping.
#'
#' @name questrade-healthcheck
#' @importFrom httr POST content status_code
#' @importFrom logger log_debug log_warn
#' @importFrom later later
NULL

################################################################################
# TOKEN MANAGEMENT
################################################################################

#' Update refresh token in .Renviron file
#'
#' @param new_token Character new refresh token
#' @return Logical TRUE if successful, FALSE otherwise
#' @noRd
update_refresh_token <- function(new_token) {
  renviron_path <- file.path(getwd(), ".Renviron")

  if (!file.exists(renviron_path)) {
    log_warn("Questrade: .Renviron file not found, cannot update refresh token")
    return(FALSE)
  }

  tryCatch({
    # Read current .Renviron
    lines <- readLines(renviron_path)

    # Find and replace the token line
    token_pattern <- "^QUESTRADE_REFRESH_TOKEN="
    token_line_idx <- grep(token_pattern, lines)

    if (length(token_line_idx) > 0) {
      lines[token_line_idx[1]] <- paste0("QUESTRADE_REFRESH_TOKEN=", new_token)
    } else {
      # Add token if not present
      lines <- c(lines, paste0("QUESTRADE_REFRESH_TOKEN=", new_token))
    }

    # Write back to .Renviron
    writeLines(lines, renviron_path)

    # Update current session environment
    Sys.setenv(QUESTRADE_REFRESH_TOKEN = new_token)

    log_debug("Questrade: Refresh token updated successfully")
    return(TRUE)
  }, error = function(e) {
    log_warn("Questrade: Failed to update refresh token - {e$message}")
    return(FALSE)
  })
}

################################################################################
# HEALTH CHECK
################################################################################

#' Ping Questrade API to rotate refresh token
#'
#' Silent health check that authenticates with Questrade and automatically
#' saves the new refresh token. Keeps token alive as long as app runs at
#' least once every 7 days.
#'
#' @return Logical TRUE if successful, FALSE otherwise
#' @export
ping_questrade <- function() {
  refresh_token <- Sys.getenv("QUESTRADE_REFRESH_TOKEN")

  # Silently exit if no token configured
  if (refresh_token == "") {
    return(FALSE)
  }

  # Attempt authentication
  response <- tryCatch({
    POST(
      "https://login.questrade.com/oauth2/token",
      query = list(
        grant_type = "refresh_token",
        refresh_token = refresh_token
      ),
      encode = "form"
    )
  }, error = function(e) {
    log_warn("Questrade: Failed to connect to API - {e$message}")
    return(NULL)
  })

  # Check response
  if (is.null(response)) {
    return(FALSE)
  }

  if (status_code(response) != 200) {
    log_warn("Questrade: Authentication failed with status {status_code(response)}")
    return(FALSE)
  }

  # Extract and save new refresh token
  token_data <- content(response)

  if (!is.null(token_data$refresh_token)) {
    update_refresh_token(token_data$refresh_token)
  }

  return(TRUE)
}

#' Start background Questrade health check
#'
#' Initiates a recurring background task that pings Questrade every 24 hours.
#' Uses recursion with later::later() to keep token alive indefinitely while
#' app is running. Safe to call multiple times (won't create duplicate tasks).
#'
#' @return NULL (invisible)
#' @export
start_questrade_background_ping <- function() {
  # Check if token is configured
  if (Sys.getenv("QUESTRADE_REFRESH_TOKEN") == "") {
    return(invisible(NULL))
  }

  # Define the recurring task
  questrade_daily_ping <- function() {
    # Ping Questrade
    ping_questrade()

    # Schedule next ping in 24 hours (86400 seconds)
    later::later(questrade_daily_ping, delay = 86400)
  }

  # Start the recurring task
  questrade_daily_ping()

  log_debug("Questrade: Background health check started (24-hour cycle)")

  invisible(NULL)
}
