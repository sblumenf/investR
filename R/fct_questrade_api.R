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
#' Performs OAuth token exchange to get API access credentials
#' Integrates with existing token rotation logic
#'
#' @return List with access_token and api_server, or NULL on failure
#' @noRd
get_questrade_auth <- function() {
  refresh_token <- Sys.getenv("QUESTRADE_REFRESH_TOKEN")

  if (refresh_token == "") {
    log_error("Questrade API: QUESTRADE_REFRESH_TOKEN not found in environment")
    return(NULL)
  }

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
      log_error("Questrade API: Authentication failed with status {status_code(response)}")
      return(NULL)
    }

    auth_data <- content(response)

    # Update refresh token if provided (existing pattern from utils_questrade_healthcheck.R)
    if (!is.null(auth_data$refresh_token)) {
      update_refresh_token(auth_data$refresh_token)
    }

    log_debug("Questrade API: Authentication successful")

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
#' @return Tibble with account information or empty tibble on failure
#' @noRd
fetch_questrade_accounts <- function(auth) {
  if (is.null(auth)) {
    log_error("Questrade API: No authentication provided")
    return(tibble())
  }

  tryCatch({
    url <- paste0(auth$api_server, "v1/accounts")
    response <- GET(
      url,
      add_headers(Authorization = paste("Bearer", auth$access_token))
    )

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
#' @return Tibble with position data or empty tibble on failure
#' @noRd
fetch_questrade_positions <- function(account_id, auth) {
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
