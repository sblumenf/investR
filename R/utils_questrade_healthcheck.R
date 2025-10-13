#' Questrade API Health Check
#'
#' Token management with persistent file-based storage and caching.
#' Automatically rotates tokens when access tokens expire.
#'
#' @name questrade-healthcheck
#' @importFrom httr POST content status_code
#' @importFrom logger log_debug log_warn log_info
#' @importFrom jsonlite fromJSON write_json
#' @importFrom later later
NULL

################################################################################
# TOKEN MANAGEMENT
################################################################################

#' Get path to token storage file
#'
#' @return Character path to token JSON file
#' @noRd
get_token_file_path <- function() {
  file.path(path.expand("~"), ".investr_questrade_tokens.json")
}

#' Get initial refresh token from .Renviron
#'
#' Reads the manually-provided refresh token from .Renviron file.
#' This is used for bootstrapping when no token file exists.
#'
#' @return Character refresh token or NULL if not found
#' @noRd
get_initial_refresh_token <- function() {
  # Try multiple possible locations for .Renviron
  possible_paths <- c(
    file.path(getwd(), ".Renviron"),                     # Current directory
    file.path(Sys.getenv("R_USER", "~"), ".Renviron"),  # User home
    ".Renviron"                                           # Relative path
  )

  for (path in possible_paths) {
    if (file.exists(path)) {
      lines <- readLines(path, warn = FALSE)
      token_line <- grep("^QUESTRADE_REFRESH_TOKEN=", lines, value = TRUE)

      if (length(token_line) > 0) {
        token <- sub("^QUESTRADE_REFRESH_TOKEN=", "", token_line[1])
        if (nzchar(token)) {
          log_debug("Questrade: Found initial refresh token in {path}")
          return(token)
        }
      }
    }
  }

  log_debug("Questrade: No initial refresh token found in .Renviron")
  return(NULL)
}

#' Read token data from persistent storage file
#'
#' Reads cached access token, refresh token, and metadata from JSON file.
#' Returns NULL if file doesn't exist or is invalid.
#'
#' @return List with access_token, refresh_token, api_server, expires_at, or NULL
#' @noRd
read_token_file <- function() {
  token_file <- get_token_file_path()

  if (!file.exists(token_file)) {
    log_debug("Questrade: Token file does not exist: {token_file}")
    return(NULL)
  }

  tryCatch({
    token_data <- jsonlite::fromJSON(token_file)

    # Validate required fields
    required_fields <- c("access_token", "refresh_token", "api_server", "expires_at")
    if (!all(required_fields %in% names(token_data))) {
      log_warn("Questrade: Token file missing required fields")
      return(NULL)
    }

    # Convert expires_at to POSIXct if it's numeric
    if (is.numeric(token_data$expires_at)) {
      token_data$expires_at <- as.POSIXct(token_data$expires_at, origin = "1970-01-01", tz = "UTC")
    }

    log_debug("Questrade: Successfully read token file")
    return(token_data)
  }, error = function(e) {
    log_warn("Questrade: Failed to read token file - {e$message}")
    return(NULL)
  })
}

#' Save token data to persistent storage file
#'
#' Saves access token, refresh token, API server, and expiration timestamp
#' to JSON file for reuse across R sessions.
#'
#' @param access_token Character access token
#' @param refresh_token Character refresh token
#' @param api_server Character API server URL
#' @param expires_in Integer seconds until access token expires (default 1800)
#' @return Logical TRUE if successful, FALSE otherwise
#' @noRd
save_token_file <- function(access_token, refresh_token, api_server, expires_in = 1800) {
  token_file <- get_token_file_path()

  tryCatch({
    # Calculate expiration timestamp
    expires_at <- Sys.time() + expires_in

    token_data <- list(
      access_token = access_token,
      refresh_token = refresh_token,
      api_server = api_server,
      expires_at = as.numeric(expires_at),  # Store as Unix timestamp
      saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )

    # Write to file with pretty formatting
    jsonlite::write_json(token_data, token_file, pretty = TRUE, auto_unbox = TRUE)

    # Verify write succeeded
    verify_data <- jsonlite::fromJSON(token_file)
    if (is.null(verify_data$access_token) || verify_data$access_token != access_token) {
      log_warn("Questrade: Token file write verification failed")
      return(FALSE)
    }

    log_info("Questrade: Token saved to {token_file}")
    log_info("Questrade: Access token expires at {format(expires_at, '%Y-%m-%d %H:%M:%S %Z')}")
    log_debug("Questrade: Refresh token: {substring(refresh_token, 1, 20)}...")

    return(TRUE)
  }, error = function(e) {
    log_warn("Questrade: Failed to save token file - {e$message}")
    return(FALSE)
  })
}

#' Delete cached token file
#'
#' Removes the persistent token file to force a fresh token exchange.
#' Used when a 401 error indicates the cached token is no longer valid.
#'
#' @return Logical TRUE if successful, FALSE otherwise
#' @noRd
delete_token_file <- function() {
  token_file <- get_token_file_path()

  if (file.exists(token_file)) {
    tryCatch({
      file.remove(token_file)
      log_info("Questrade: Deleted stale token file")
      return(TRUE)
    }, error = function(e) {
      log_warn("Questrade: Failed to delete token file - {e$message}")
      return(FALSE)
    })
  } else {
    log_debug("Questrade: Token file does not exist")
    return(TRUE)
  }
}

#' Update refresh token in .Renviron file (DEPRECATED)
#'
#' This function is kept for backward compatibility but is no longer used
#' in the main token management flow. Tokens are now stored in JSON file.
#'
#' @param new_token Character new refresh token
#' @return Logical TRUE if successful, FALSE otherwise
#' @noRd
update_refresh_token <- function(new_token) {
  # Try multiple possible locations for .Renviron
  possible_paths <- c(
    file.path(Sys.getenv("R_USER", "~"), ".Renviron"),  # User home
    file.path(getwd(), ".Renviron"),                     # Current directory
    ".Renviron"                                           # Relative path
  )

  renviron_path <- NULL
  for (path in possible_paths) {
    if (file.exists(path)) {
      renviron_path <- path
      break
    }
  }

  if (is.null(renviron_path)) {
    log_warn("Questrade: .Renviron file not found in any expected location")
    log_warn("Questrade: Searched: {paste(possible_paths, collapse=', ')}")
    return(FALSE)
  }

  tryCatch({
    # Read current .Renviron
    lines <- readLines(renviron_path, warn = FALSE)

    # Find and replace the token line
    token_pattern <- "^QUESTRADE_REFRESH_TOKEN="
    token_line_idx <- grep(token_pattern, lines)

    old_token_preview <- if (length(token_line_idx) > 0) {
      substring(lines[token_line_idx[1]], 1, 40)
    } else {
      "none"
    }

    if (length(token_line_idx) > 0) {
      lines[token_line_idx[1]] <- paste0("QUESTRADE_REFRESH_TOKEN=", new_token)
    } else {
      # Add token if not present
      lines <- c(lines, paste0("QUESTRADE_REFRESH_TOKEN=", new_token))
    }

    # Write back to .Renviron
    writeLines(lines, renviron_path)

    # Verify the write succeeded by reading back
    verify_lines <- readLines(renviron_path, warn = FALSE)
    verify_idx <- grep(token_pattern, verify_lines)

    if (length(verify_idx) == 0 || !grepl(new_token, verify_lines[verify_idx[1]], fixed = TRUE)) {
      log_warn("Questrade: Token write verification failed!")
      return(FALSE)
    }

    # Update current session environment
    Sys.setenv(QUESTRADE_REFRESH_TOKEN = new_token)

    log_info("Questrade: Refresh token updated successfully in {renviron_path}")
    log_info("Questrade: Old token: {old_token_preview}... -> New token: {substring(new_token, 1, 20)}...")
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
