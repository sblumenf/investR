#' Questrade API Functions
#'
#' Functions for fetching portfolio position data from Questrade API
#'
#' @name questrade-api
#' @import dplyr
#' @importFrom httr POST GET add_headers content status_code
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_dfr
#' @importFrom logger log_info log_warn log_error log_debug
NULL

################################################################################
# AUTHENTICATION
################################################################################

#' Get Questrade authentication credentials
#'
#' Smart token management with caching and automatic refresh.
#' - Checks cached access token from file storage
#' - Reuses valid access tokens (< 30 min old, with 60s safety buffer)
#' - Only exchanges refresh token when access token is expired
#' - Persists tokens across R sessions in JSON file
#'
#' @param override_refresh_token Optional refresh token to use instead of cached/file token
#' @return List with access_token and api_server, or NULL on failure
#' @noRd
get_questrade_auth <- function(override_refresh_token = NULL) {
  # Step 1: Try to read cached token from file
  token_file_path <- get_token_file_path()
  log_debug("Questrade API: Token cache check - file: {token_file_path}, exists: {file.exists(token_file_path)}")

  cached_token <- read_token_file()

  # Step 2: Check if cached access token is still valid (with 60-second safety buffer)
  if (!is.null(cached_token)) {
    time_until_expiry <- as.numeric(difftime(cached_token$expires_at, Sys.time(), units = "secs"))

    if (time_until_expiry > 60) {
      log_info("Questrade API: Using cached access token (expires in {round(time_until_expiry)} seconds)")
      log_debug("Questrade API: Access token starts with: {substring(cached_token$access_token, 1, 10)}...")
      log_debug("Questrade API: Refresh token starts with: {substring(cached_token$refresh_token, 1, 10)}...")
      return(list(
        access_token = cached_token$access_token,
        api_server = cached_token$api_server
      ))
    } else {
      log_info("Questrade API: Cached access token expired or expiring soon, refreshing...")
    }
  }

  # Step 3: Need to refresh - get refresh token from override, cache, or .Renviron
  refresh_token <- if (!is.null(override_refresh_token)) {
    # Use the override token if provided (e.g., from 401 recovery)
    log_info("Questrade API: Using override refresh token for authentication")
    override_refresh_token
  } else if (!is.null(cached_token)) {
    cached_token$refresh_token
  } else {
    get_initial_refresh_token()
  }

  if (is.null(refresh_token) || refresh_token == "") {
    log_error("Questrade API: No refresh token available - cannot authenticate")
    log_error("Questrade API: Token cache file: {get_token_file_path()} - exists: {file.exists(get_token_file_path())}")
    log_error("Questrade API: .Renviron file: /home/sergeblumenfeld/investR/.Renviron")
    log_error("Questrade API: SOLUTION: Get fresh token from Questrade and add QUESTRADE_REFRESH_TOKEN to .Renviron")
    return(NULL)
  }

  # Step 4: Exchange refresh token for new access token
  log_info("Questrade API: Exchanging refresh token (starts with {substring(refresh_token, 1, 10)}...)")

  tryCatch({
    response <- POST(
      "https://login.questrade.com/oauth2/token",
      query = list(
        grant_type = "refresh_token",
        refresh_token = refresh_token
      ),
      encode = "form"
    )

    if (status_code(response) != 200) {
      log_error("Questrade API: Token exchange failed with status {status_code(response)}")

      # Provide helpful error messages
      if (status_code(response) == 400) {
        log_error("Questrade API: Status 400 - Refresh token is invalid or already used")
        log_error("Questrade API: Generate a new token from Questrade and update .Renviron")
      } else if (status_code(response) == 500) {
        log_error("Questrade API: Status 500 - Server error or token already consumed")
        log_error("Questrade API: Generate a new token from Questrade and update .Renviron")
      }

      return(NULL)
    }

    auth_data <- content(response)

    # Step 5: Save new tokens to persistent file
    save_success <- save_token_file(
      access_token = auth_data$access_token,
      refresh_token = auth_data$refresh_token,
      api_server = auth_data$api_server,
      expires_in = auth_data$expires_in %||% 1800  # Default 30 minutes
    )

    if (!save_success) {
      log_warn("Questrade API: Token exchange succeeded but failed to save tokens!")
      log_warn("Questrade API: Tokens will work for this session but won't persist")
    } else {
      log_info("Questrade API: Token exchange successful, new tokens saved")
    }

    list(
      access_token = auth_data$access_token,
      api_server = auth_data$api_server
    )
  }, error = function(e) {
    log_error("Questrade API: Authentication error - {e$message}")
    return(NULL)
  })
}

################################################################################
# ACCOUNT OPERATIONS
################################################################################

#' Fetch Questrade accounts
#'
#' Retrieves list of all accounts accessible via API
#'
#' @param auth List with access_token and api_server from get_questrade_auth()
#' @param retry_on_401 Logical, whether to retry once on 401 error (default TRUE)
#' @return Tibble with account information or empty tibble on failure
#' @noRd
fetch_questrade_accounts <- function(auth, retry_on_401 = TRUE) {
  if (is.null(auth)) {
    log_error("Questrade API: No authentication provided")
    return(tibble())
  }

  tryCatch({
    url <- paste0(auth$api_server, "v1/accounts")
    log_debug("Questrade API: Sending Authorization header with token starting: {substring(auth$access_token, 1, 10)}...")
    response <- GET(
      url,
      add_headers(Authorization = paste("Bearer", auth$access_token))
    )

    # Handle 401 Unauthorized - token is invalid despite expiry check
    if (status_code(response) == 401 && retry_on_401) {
      log_warn("Questrade API: Received 401 Unauthorized, cached token is invalid")
      log_info("Questrade API: Preserving refresh token and forcing re-authentication...")

      # CRITICAL FIX: Read and preserve refresh_token BEFORE deleting cache
      cached_token <- read_token_file()
      preserved_refresh_token <- if (!is.null(cached_token)) {
        cached_token$refresh_token
      } else {
        NULL
      }

      # Delete the stale cached token
      delete_token_file(reason = "401 error on accounts fetch - access token invalid")

      # Get fresh authentication using preserved refresh token
      # Pass the preserved token directly to get_questrade_auth()
      if (!is.null(preserved_refresh_token)) {
        log_info("Questrade API: Using preserved refresh token (starts with {substring(preserved_refresh_token, 1, 10)}...)")
      }

      fresh_auth <- get_questrade_auth(override_refresh_token = preserved_refresh_token)

      if (is.null(fresh_auth)) {
        log_error("Questrade API: Failed to refresh authentication after 401")
        return(tibble())
      }

      log_info("Questrade API: Retrying accounts fetch with fresh token")

      # Retry once with fresh token (retry_on_401 = FALSE prevents infinite loop)
      return(fetch_questrade_accounts(fresh_auth, retry_on_401 = FALSE))
    }

    if (status_code(response) != 200) {
      log_error("Questrade API: Accounts fetch failed with status {status_code(response)}")
      return(tibble())
    }

    accounts_data <- content(response)$accounts

    if (is.null(accounts_data) || length(accounts_data) == 0) {
      log_warn("Questrade API: No accounts found")
      return(tibble())
    }

    # Convert to tibble
    accounts_df <- map_dfr(accounts_data, function(acc) {
      tibble(
        account_number = as.character(acc$number),
        account_type = acc$type,
        status = acc$status,
        is_primary = acc$isPrimary %||% FALSE,
        is_billing = acc$isBilling %||% FALSE,
        client_account_type = acc$clientAccountType %||% NA_character_
      )
    })

    log_info("Questrade API: Retrieved {nrow(accounts_df)} accounts")
    return(accounts_df)
  }, error = function(e) {
    log_error("Questrade API: Accounts fetch error - {e$message}")
    return(tibble())
  })
}

################################################################################
# POSITION OPERATIONS
################################################################################

#' Fetch positions for a single account
#'
#' Retrieves current positions for a specific account
#'
#' @param account_id Character account number
#' @param auth List with access_token and api_server
#' @param retry_on_401 Logical, whether to retry once on 401 error (default TRUE)
#' @return Tibble with position data or empty tibble on failure
#' @noRd
fetch_questrade_positions <- function(account_id, auth, retry_on_401 = TRUE) {
  if (is.null(auth)) {
    log_error("Questrade API: No authentication provided")
    return(tibble())
  }

  tryCatch({
    url <- paste0(auth$api_server, "v1/accounts/", account_id, "/positions")
    response <- GET(
      url,
      add_headers(Authorization = paste("Bearer", auth$access_token))
    )

    # Handle 401 Unauthorized - token is invalid despite expiry check
    if (status_code(response) == 401 && retry_on_401) {
      log_warn("Questrade API: Received 401 Unauthorized for positions, cached token is invalid")
      log_info("Questrade API: Preserving refresh token and forcing re-authentication...")

      # CRITICAL FIX: Read and preserve refresh_token BEFORE deleting cache
      cached_token <- read_token_file()
      preserved_refresh_token <- if (!is.null(cached_token)) {
        cached_token$refresh_token
      } else {
        NULL
      }

      # Delete the stale cached token
      delete_token_file(reason = "401 error on positions fetch - access token invalid")

      # Get fresh authentication using preserved refresh token
      # Pass the preserved token directly to get_questrade_auth()
      if (!is.null(preserved_refresh_token)) {
        log_info("Questrade API: Using preserved refresh token (starts with {substring(preserved_refresh_token, 1, 10)}...)")
      }

      fresh_auth <- get_questrade_auth(override_refresh_token = preserved_refresh_token)

      if (is.null(fresh_auth)) {
        log_error("Questrade API: Failed to refresh authentication after 401")
        return(tibble())
      }

      log_info("Questrade API: Retrying positions fetch with fresh token for account {account_id}")

      # Retry once with fresh token (retry_on_401 = FALSE prevents infinite loop)
      return(fetch_questrade_positions(account_id, fresh_auth, retry_on_401 = FALSE))
    }

    if (status_code(response) != 200) {
      log_error("Questrade API: Positions fetch failed for account {account_id} with status {status_code(response)}")
      return(tibble())
    }

    positions_data <- content(response)$positions

    if (is.null(positions_data) || length(positions_data) == 0) {
      log_debug("Questrade API: No positions in account {account_id}")
      return(tibble())
    }

    # Convert to tibble with snake_case column names
    positions_df <- map_dfr(positions_data, function(pos) {
      tibble(
        symbol = pos$symbol,
        symbol_id = pos$symbolId,
        open_quantity = pos$openQuantity,
        closed_quantity = pos$closedQuantity,
        current_market_value = pos$currentMarketValue,
        current_price = pos$currentPrice,
        average_entry_price = pos$averageEntryPrice,
        day_pnl = pos$dayPnl %||% 0,
        closed_pnl = pos$closedPnl %||% 0,
        open_pnl = pos$openPnl %||% 0,
        total_cost = pos$totalCost,
        is_real_time = pos$isRealTime %||% FALSE,
        is_under_reorg = pos$isUnderReorg %||% FALSE
      )
    })

    log_debug("Questrade API: Retrieved {nrow(positions_df)} positions from account {account_id}")
    return(positions_df)
  }, error = function(e) {
    log_error("Questrade API: Positions fetch error for account {account_id} - {e$message}")
    return(tibble())
  })
}

#' Fetch positions from all accounts sequentially
#'
#' Retrieves positions from all accounts and combines them with account metadata
#'
#' @return Tibble with all positions from all accounts, or empty tibble on failure
#' @export
fetch_all_positions_sequential <- function() {
  log_info("Questrade API: Starting fetch of all positions")

  # Authenticate
  auth <- get_questrade_auth()
  if (is.null(auth)) {
    log_error("Questrade API: Authentication failed, cannot fetch positions")
    return(tibble())
  }

  # Get accounts
  accounts <- fetch_questrade_accounts(auth)
  if (nrow(accounts) == 0) {
    log_error("Questrade API: No accounts found, cannot fetch positions")
    return(tibble())
  }

  # Fetch positions from each account sequentially
  all_positions <- map_dfr(seq_len(nrow(accounts)), function(i) {
    account <- accounts[i, ]
    log_info("Questrade API: Fetching positions for account {account$account_number} ({account$account_type})")

    positions <- fetch_questrade_positions(account$account_number, auth)

    if (nrow(positions) > 0) {
      # Add account metadata
      positions %>%
        mutate(
          account_number = account$account_number,
          account_type = account$account_type
        )
    } else {
      tibble()
    }
  })

  if (nrow(all_positions) == 0) {
    log_warn("Questrade API: No positions found across any accounts")
    return(tibble())
  }

  log_info("Questrade API: Successfully fetched {nrow(all_positions)} total positions from {nrow(accounts)} accounts")
  return(all_positions)
}

################################################################################
# ACTIVITY OPERATIONS
################################################################################

#' Fetch account activities for a single account
#'
#' Retrieves account activities (trades, dividends, deposits, etc.) for a specific time period
#'
#' @param account_id Character account number
#' @param auth List with access_token and api_server
#' @param start_time Start date in ISO 8601 format (e.g., "2024-01-01T00:00:00-05:00")
#' @param end_time End date in ISO 8601 format (e.g., "2024-01-31T23:59:59-05:00")
#' @param retry_on_401 Logical, whether to retry once on 401 error (default TRUE)
#' @return Tibble with activity data or empty tibble on failure
#' @noRd
fetch_questrade_activities <- function(account_id, auth, start_time, end_time, retry_on_401 = TRUE) {
  if (is.null(auth)) {
    log_error("Questrade API: No authentication provided")
    return(tibble())
  }

  tryCatch({
    url <- paste0(auth$api_server, "v1/accounts/", account_id, "/activities")
    response <- GET(
      url,
      query = list(startTime = start_time, endTime = end_time),
      add_headers(Authorization = paste("Bearer", auth$access_token))
    )

    # Handle 401 Unauthorized - token is invalid despite expiry check
    if (status_code(response) == 401 && retry_on_401) {
      log_warn("Questrade API: Received 401 Unauthorized for activities, cached token is invalid")
      log_info("Questrade API: Preserving refresh token and forcing re-authentication...")

      # CRITICAL FIX: Read and preserve refresh_token BEFORE deleting cache
      cached_token <- read_token_file()
      preserved_refresh_token <- if (!is.null(cached_token)) {
        cached_token$refresh_token
      } else {
        NULL
      }

      # Delete the stale cached token
      delete_token_file(reason = "401 error on activities fetch - access token invalid")

      # Get fresh authentication using preserved refresh token
      # Pass the preserved token directly to get_questrade_auth()
      if (!is.null(preserved_refresh_token)) {
        log_info("Questrade API: Using preserved refresh token (starts with {substring(preserved_refresh_token, 1, 10)}...)")
      }

      fresh_auth <- get_questrade_auth(override_refresh_token = preserved_refresh_token)

      if (is.null(fresh_auth)) {
        log_error("Questrade API: Failed to refresh authentication after 401")
        return(tibble())
      }

      log_info("Questrade API: Retrying activities fetch with fresh token for account {account_id}")

      # Retry once with fresh token (retry_on_401 = FALSE prevents infinite loop)
      return(fetch_questrade_activities(account_id, fresh_auth, start_time, end_time, retry_on_401 = FALSE))
    }

    if (status_code(response) != 200) {
      log_error("Questrade API: Activities fetch failed for account {account_id} with status {status_code(response)}")
      return(tibble())
    }

    activities_data <- content(response)$activities

    if (is.null(activities_data) || length(activities_data) == 0) {
      log_debug("Questrade API: No activities in account {account_id} for specified period")
      return(tibble())
    }

    # Convert to tibble
    activities_df <- map_dfr(activities_data, function(act) {
      tibble(
        trade_date = act$tradeDate %||% NA_character_,
        transaction_date = act$transactionDate %||% NA_character_,
        settlement_date = act$settlementDate %||% NA_character_,
        action = act$action %||% NA_character_,
        symbol = act$symbol %||% NA_character_,
        symbol_id = act$symbolId %||% NA_integer_,
        description = act$description %||% NA_character_,
        currency = act$currency %||% NA_character_,
        quantity = act$quantity %||% 0,
        price = act$price %||% 0,
        gross_amount = act$grossAmount %||% 0,
        commission = act$commission %||% 0,
        net_amount = act$netAmount %||% 0,
        type = act$type %||% NA_character_
      )
    })

    log_debug("Questrade API: Retrieved {nrow(activities_df)} activities from account {account_id}")
    return(activities_df)
  }, error = function(e) {
    log_error("Questrade API: Activities fetch error for account {account_id} - {e$message}")
    return(tibble())
  })
}

#' Fetch activities from all accounts for a time period
#'
#' Retrieves activities from all accounts and combines them with account metadata
#'
#' @param days_back Number of days to look back (default 30, max 31)
#' @return Tibble with all activities from all accounts, or empty tibble on failure
#' @export
fetch_all_activities <- function(days_back = 30) {
  if (days_back > 31) {
    log_warn("Questrade API: Maximum 31 days allowed, adjusting to 31")
    days_back <- 31
  }

  log_info("Questrade API: Starting fetch of all activities for last {days_back} days")

  # Calculate time range with proper timezone format for Questrade API
  format_questrade_time <- function(time) {
    tz_str <- format(time, "%Y-%m-%dT%H:%M:%S%z")
    # Add colon in timezone (e.g., -0400 -> -04:00)
    gsub("([+-]\\d{2})(\\d{2})$", "\\1:\\2", tz_str)
  }

  end_time <- format_questrade_time(Sys.time())
  start_time <- format_questrade_time(Sys.time() - (days_back * 24 * 60 * 60))

  # Authenticate
  auth <- get_questrade_auth()
  if (is.null(auth)) {
    log_error("Questrade API: Authentication failed, cannot fetch activities")
    return(tibble())
  }

  # Get accounts
  accounts <- fetch_questrade_accounts(auth)
  if (nrow(accounts) == 0) {
    log_error("Questrade API: No accounts found, cannot fetch activities")
    return(tibble())
  }

  # Fetch activities from each account sequentially
  all_activities <- map_dfr(seq_len(nrow(accounts)), function(i) {
    account <- accounts[i, ]
    log_info("Questrade API: Fetching activities for account {account$account_number} ({account$account_type})")

    activities <- fetch_questrade_activities(account$account_number, auth, start_time, end_time)

    if (nrow(activities) > 0) {
      # Add account metadata
      activities %>%
        mutate(
          account_number = account$account_number,
          account_type = account$account_type
        )
    } else {
      tibble()
    }
  })

  if (nrow(all_activities) == 0) {
    log_warn("Questrade API: No activities found across any accounts")
    return(tibble())
  }

  # Filter to only include Trades, Dividends, and Other
  filtered_activities <- all_activities %>%
    filter(type %in% c("Trades", "Dividends", "Other"))

  log_info("Questrade API: Successfully fetched {nrow(all_activities)} total activities from {nrow(accounts)} accounts")
  log_info("Questrade API: Filtered to {nrow(filtered_activities)} activities (Trades, Dividends, Other)")

  # Save activities to database and run pattern matching
  if (nrow(filtered_activities) > 0) {
    # Save to database (skips duplicates automatically)
    save_result <- save_activities_batch(filtered_activities)
    log_info("Questrade API: Saved {save_result$inserted_count} new activities to database")

    # Get unprocessed activities for pattern matching
    unprocessed <- get_unprocessed_activities()

    if (nrow(unprocessed) > 0) {
      # Get current positions for delayed covered call detection
      current_positions <- get_latest_positions()

      # Generate suggestions from patterns
      suggestion_count <- generate_suggestions_from_patterns(unprocessed, current_positions)

      # Mark activities as processed
      activity_ids <- unprocessed$activity_id
      mark_activities_processed(activity_ids)

      log_info("Questrade API: Generated {suggestion_count} grouping suggestions")
    }
  }

  return(filtered_activities)
}

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Null coalescing operator
#'
#' Returns default value if x is NULL
#'
#' @param x Value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
