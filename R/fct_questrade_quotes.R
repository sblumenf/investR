#' Questrade Quote Fetching Functions
#'
#' Functions for fetching stock quotes from Questrade API with Yahoo Finance
#' compatible output format.
#'
#' @name questrade-quotes
#' @importFrom httr GET add_headers content status_code
#' @importFrom logger log_debug log_warn log_error
#' @importFrom tibble tibble
NULL

################################################################################
# FALLBACK TRACKING
################################################################################

# Environment to track fallbacks during analysis runs
.fallback_tracker <- new.env(parent = emptyenv())
.fallback_tracker$tickers <- character(0)
.fallback_tracker$reasons <- character(0)

#' Reset fallback tracker
#'
#' Clears the list of tickers that fell back to Yahoo Finance.
#' Call this before starting an analysis run.
#'
#' @return NULL (invisibly)
#' @export
reset_fallback_tracker <- function() {
  .fallback_tracker$tickers <- character(0)
  .fallback_tracker$reasons <- character(0)
  invisible(NULL)
}

#' Record a fallback to Yahoo Finance
#'
#' Internal function to track when Questrade quote fetch fails
#' and Yahoo is used instead.
#'
#' @param ticker Character ticker symbol
#' @param reason Character reason for fallback
#' @noRd
record_fallback <- function(ticker, reason) {
  .fallback_tracker$tickers <- c(.fallback_tracker$tickers, ticker)
  .fallback_tracker$reasons <- c(.fallback_tracker$reasons, reason)
}

#' Get fallback summary
#'
#' Returns information about Questrade fallbacks that occurred
#' during the current analysis run.
#'
#' @return List with count, tickers, and summary message
#' @export
get_fallback_summary <- function() {
  count <- length(.fallback_tracker$tickers)

  if (count == 0) {
    return(list(
      count = 0,
      tickers = character(0),
      message = NULL
    ))
  }

  # Get unique tickers and count
  unique_tickers <- unique(.fallback_tracker$tickers)
  unique_count <- length(unique_tickers)

  # Create summary message
  if (unique_count <= 5) {
    ticker_list <- paste(unique_tickers, collapse = ", ")
    message <- sprintf(
      "Questrade API unavailable for %d ticker%s (%s). Using Yahoo Finance instead.",
      unique_count,
      if (unique_count > 1) "s" else "",
      ticker_list
    )
  } else {
    ticker_list <- paste(head(unique_tickers, 5), collapse = ", ")
    message <- sprintf(
      "Questrade API unavailable for %d tickers (%s, and %d more). Using Yahoo Finance instead.",
      unique_count,
      ticker_list,
      unique_count - 5
    )
  }

  list(
    count = unique_count,
    tickers = unique_tickers,
    message = message
  )
}

#' Search for Questrade symbol ID
#'
#' Converts a ticker symbol (e.g., "AAPL") to Questrade's internal symbol ID
#' required for quote requests.
#'
#' @param ticker Character ticker symbol
#' @param auth List with access_token and api_server from get_questrade_auth()
#' @param retry_on_401 Logical, whether to retry once on 401 error (default TRUE)
#' @return Integer symbol ID, or NULL on failure
#' @noRd
search_questrade_symbol <- function(ticker, auth, retry_on_401 = TRUE) {
  if (is.null(auth)) {
    log_error("Questrade Quotes: No authentication provided for symbol search")
    return(NULL)
  }

  tryCatch({
    url <- paste0(auth$api_server, "v1/symbols/search")
    log_debug("Questrade Quotes: Searching for symbol '{ticker}'")

    response <- GET(
      url,
      query = list(prefix = ticker),
      add_headers(Authorization = paste("Bearer", auth$access_token))
    )

    # Handle 401 Unauthorized - token is invalid despite expiry check
    if (status_code(response) == 401 && retry_on_401) {
      log_warn("Questrade Quotes: Received 401 Unauthorized for symbol search, cached token is invalid")
      log_info("Questrade Quotes: Preserving refresh token and forcing re-authentication...")

      # CRITICAL: Read and preserve refresh_token BEFORE deleting cache
      cached_token <- read_token_file()
      preserved_refresh_token <- if (!is.null(cached_token)) {
        cached_token$refresh_token
      } else {
        NULL
      }

      # Delete the stale cached token
      delete_token_file(reason = "401 error on symbol search - access token invalid")

      # Get fresh authentication using preserved refresh token
      if (!is.null(preserved_refresh_token)) {
        log_info("Questrade Quotes: Using preserved refresh token (starts with {substring(preserved_refresh_token, 1, 10)}...)")
      }

      fresh_auth <- get_questrade_auth(override_refresh_token = preserved_refresh_token)

      if (is.null(fresh_auth)) {
        log_error("Questrade Quotes: Failed to refresh authentication after 401")
        return(NULL)
      }

      log_info("Questrade Quotes: Retrying symbol search with fresh token for '{ticker}'")

      # Retry once with fresh token (retry_on_401 = FALSE prevents infinite loop)
      return(search_questrade_symbol(ticker, fresh_auth, retry_on_401 = FALSE))
    }

    if (status_code(response) != 200) {
      log_error("Questrade Quotes: Symbol search failed for '{ticker}' with status {status_code(response)}")
      return(NULL)
    }

    symbols_data <- content(response)$symbols

    if (is.null(symbols_data) || length(symbols_data) == 0) {
      log_warn("Questrade Quotes: No symbol found for ticker '{ticker}'")
      return(NULL)
    }

    # Find exact match (case-insensitive)
    exact_match <- NULL
    for (symbol in symbols_data) {
      if (toupper(symbol$symbol) == toupper(ticker)) {
        exact_match <- symbol
        break
      }
    }

    if (is.null(exact_match)) {
      log_warn("Questrade Quotes: No exact match found for ticker '{ticker}'")
      return(NULL)
    }

    symbol_id <- exact_match$symbolId
    log_debug("Questrade Quotes: Found symbolId {symbol_id} for '{ticker}'")

    return(symbol_id)

  }, error = function(e) {
    log_error("Questrade Quotes: Symbol search error for '{ticker}' - {e$message}")
    return(NULL)
  })
}

#' Fetch quote from Questrade API
#'
#' Retrieves current quote data for a given symbol ID.
#'
#' @param symbol_id Integer Questrade symbol ID
#' @param auth List with access_token and api_server from get_questrade_auth()
#' @param retry_on_401 Logical, whether to retry once on 401 error (default TRUE)
#' @return List with quote data, or NULL on failure
#' @noRd
fetch_questrade_quote_raw <- function(symbol_id, auth, retry_on_401 = TRUE) {
  if (is.null(auth)) {
    log_error("Questrade Quotes: No authentication provided for quote fetch")
    return(NULL)
  }

  tryCatch({
    url <- paste0(auth$api_server, "v1/markets/quotes/", symbol_id)
    log_debug("Questrade Quotes: Fetching quote for symbolId {symbol_id}")

    response <- GET(
      url,
      add_headers(Authorization = paste("Bearer", auth$access_token))
    )

    # Handle 401 Unauthorized - token is invalid despite expiry check
    if (status_code(response) == 401 && retry_on_401) {
      log_warn("Questrade Quotes: Received 401 Unauthorized for quote fetch, cached token is invalid")
      log_info("Questrade Quotes: Preserving refresh token and forcing re-authentication...")

      # CRITICAL: Read and preserve refresh_token BEFORE deleting cache
      cached_token <- read_token_file()
      preserved_refresh_token <- if (!is.null(cached_token)) {
        cached_token$refresh_token
      } else {
        NULL
      }

      # Delete the stale cached token
      delete_token_file(reason = "401 error on quote fetch - access token invalid")

      # Get fresh authentication using preserved refresh token
      if (!is.null(preserved_refresh_token)) {
        log_info("Questrade Quotes: Using preserved refresh token (starts with {substring(preserved_refresh_token, 1, 10)}...)")
      }

      fresh_auth <- get_questrade_auth(override_refresh_token = preserved_refresh_token)

      if (is.null(fresh_auth)) {
        log_error("Questrade Quotes: Failed to refresh authentication after 401")
        return(NULL)
      }

      log_info("Questrade Quotes: Retrying quote fetch with fresh token for symbolId {symbol_id}")

      # Retry once with fresh token (retry_on_401 = FALSE prevents infinite loop)
      return(fetch_questrade_quote_raw(symbol_id, fresh_auth, retry_on_401 = FALSE))
    }

    if (status_code(response) != 200) {
      log_error("Questrade Quotes: Quote fetch failed for symbolId {symbol_id} with status {status_code(response)}")
      return(NULL)
    }

    quotes_data <- content(response)$quotes

    if (is.null(quotes_data) || length(quotes_data) == 0) {
      log_warn("Questrade Quotes: No quote data returned for symbolId {symbol_id}")
      return(NULL)
    }

    # Return first quote (should only be one for single symbol ID)
    return(quotes_data[[1]])

  }, error = function(e) {
    log_error("Questrade Quotes: Quote fetch error for symbolId {symbol_id} - {e$message}")
    return(NULL)
  })
}

#' Convert Questrade quote to Yahoo Finance format
#'
#' Transforms Questrade quote data structure into Yahoo Finance compatible format
#' that existing strategies expect (data.frame with "Last" and "Name" columns).
#'
#' @param quote_data List containing Questrade quote data
#' @param ticker Character ticker symbol (for row name)
#' @return Data frame with Yahoo-compatible structure
#' @noRd
convert_quote_to_yahoo_format <- function(quote_data, ticker) {
  # Questrade quote fields:
  # - lastTradePrice: current price
  # - description: security name
  # - symbol: ticker symbol

  last_price <- quote_data$lastTradePrice %||% NA_real_
  name <- quote_data$description %||% ticker

  # Add Trade Time to match Yahoo structure (column 1)
  # Questrade doesn't provide trade time, use current time
  trade_time <- Sys.time()

  # Create data frame matching Yahoo Finance structure exactly
  # Yahoo always includes "Trade Time" as first column
  result <- data.frame(
    `Trade Time` = trade_time,
    Last = last_price,
    Name = name,
    row.names = ticker,
    stringsAsFactors = FALSE,
    check.names = FALSE  # Preserve "Trade Time" with space
  )

  return(result)
}

#' Fetch stock quote from Questrade with Yahoo-compatible output
#'
#' High-level function that orchestrates symbol lookup, quote fetching,
#' and format conversion. Returns data in same format as Yahoo Finance
#' for seamless integration with existing strategies.
#'
#' Automatically falls back to Yahoo Finance if Questrade fails.
#'
#' @param ticker Character ticker symbol
#' @param fields Character vector (ignored, for Yahoo compatibility)
#' @return Data frame with quote data (Yahoo format), or NULL on total failure
#' @export
fetch_questrade_quote <- function(ticker, fields = NULL) {
  log_debug("Questrade Quotes: Starting quote fetch for '{ticker}'")

  # Step 1: Authenticate
  auth <- get_questrade_auth()
  if (is.null(auth)) {
    log_warn("Questrade Quotes: Authentication failed for '{ticker}', falling back to Yahoo")
    record_fallback(ticker, "Authentication failed")
    return(fetch_current_quote_yahoo(ticker, fields))
  }

  # Step 2: Search for symbol ID
  symbol_id <- search_questrade_symbol(ticker, auth)
  if (is.null(symbol_id)) {
    log_warn("Questrade Quotes: Symbol lookup failed for '{ticker}', falling back to Yahoo")
    record_fallback(ticker, "Symbol not found")
    return(fetch_current_quote_yahoo(ticker, fields))
  }

  # Step 3: Fetch quote
  quote_data <- fetch_questrade_quote_raw(symbol_id, auth)
  if (is.null(quote_data)) {
    log_warn("Questrade Quotes: Quote fetch failed for '{ticker}', falling back to Yahoo")
    record_fallback(ticker, "Quote fetch failed")
    return(fetch_current_quote_yahoo(ticker, fields))
  }

  # Step 4: Convert to Yahoo format
  result <- convert_quote_to_yahoo_format(quote_data, ticker)

  log_debug("Questrade Quotes: Successfully fetched quote for '{ticker}': ${result$Last}")

  return(result)
}

#' Null coalescing operator
#'
#' @param x Value to check
