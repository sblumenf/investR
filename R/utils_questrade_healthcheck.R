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
#' @param reason Character description of why the file is being deleted (for logging)
#' @return Logical TRUE if successful, FALSE otherwise
#' @noRd
delete_token_file <- function(reason = "unknown reason") {
  token_file <- get_token_file_path()

  if (file.exists(token_file)) {
    tryCatch({
      # Read token info before deletion for debugging
      cached <- read_token_file()
      if (!is.null(cached)) {
        log_info("Questrade: Deleting token cache - Reason: {reason}")
        log_debug("Questrade: Deleted token had refresh_token starting: {substring(cached$refresh_token, 1, 10)}...")
        log_debug("Questrade: Token was set to expire at: {format(cached$expires_at, '%Y-%m-%d %H:%M:%S %Z')}")
      }

      file.remove(token_file)
      log_info("Questrade: Token file deleted successfully from {token_file}")
      return(TRUE)
    }, error = function(e) {
      log_warn("Questrade: Failed to delete token file - {e$message}")
      return(FALSE)
    })
  } else {
    log_debug("Questrade: Token file does not exist at {token_file}")
    return(TRUE)
  }
}


