#' Questrade Options Chain Fetching Functions
#'
#' Functions for fetching options chains from Questrade API with Yahoo Finance
#' compatible output format. Implements full parity with Yahoo's getOptionChain()
#' by fetching complete options data and transforming to expected structure.
#'
#' @name questrade-options
#' @importFrom httr GET POST add_headers content status_code
#' @importFrom logger log_debug log_warn log_error log_info
#' @importFrom dplyr bind_rows arrange filter mutate
#' @importFrom tibble tibble
NULL

################################################################################
# MAIN ENTRY POINT
################################################################################

#' Fetch options chain from Questrade with Yahoo-compatible output
#'
#' High-level function that orchestrates the complete options chain fetch:
#' 1. Symbol lookup (reuses existing function)
#' 2. Fetch options structure (all expirations, strikes, option IDs)
#' 3. Fetch pricing data for all options (bid, ask, volume, OI)
#' 4. Convert to Yahoo Finance format
#'
#' Automatically falls back to Yahoo Finance if Questrade fails at any step.
#'
#' @param ticker Character ticker symbol
#' @param expiration Optional specific expiration date (NULL for all)
#' @return List with options chain data (Yahoo format), or NULL on total failure
#' @export
fetch_questrade_options_chain <- function(ticker, expiration = NULL) {
  log_debug("Questrade Options: Starting options chain fetch for '{ticker}'")

  # Step 1: Authenticate
  auth <- get_questrade_auth()
  if (is.null(auth)) {
    log_warn("Questrade Options: Authentication failed for '{ticker}', falling back to Yahoo")
    record_fallback(ticker, "Options: Authentication failed")
    return(fetch_options_chain_yahoo(ticker, expiration))
  }

  # Step 2: Search for symbol ID
  symbol_id <- search_questrade_symbol(ticker, auth)
  if (is.null(symbol_id)) {
    log_warn("Questrade Options: Symbol lookup failed for '{ticker}', falling back to Yahoo")
    record_fallback(ticker, "Options: Symbol not found")
    return(fetch_options_chain_yahoo(ticker, expiration))
  }
  log_info("Questrade Options: Step 2 complete - Found symbolId {symbol_id} for '{ticker}'")

  # Step 3: Fetch options structure (expirations, strikes, option IDs)
  log_info("Questrade Options: Step 3 starting - Fetching options structure for '{ticker}'")
  structure <- fetch_questrade_options_structure(symbol_id, auth)
  if (is.null(structure)) {
    log_warn("Questrade Options: Step 3 FAILED - Structure is NULL for '{ticker}', falling back to Yahoo")
    record_fallback(ticker, "Options: Structure fetch failed")
    return(fetch_options_chain_yahoo(ticker, expiration))
  }
  log_info("Questrade Options: Step 3 complete - Structure has {length(structure)} expiration dates for '{ticker}'")

  # Step 4: Extract all option IDs from structure
  log_info("Questrade Options: Step 4 starting - Extracting option IDs for '{ticker}'")
  option_ids <- extract_option_ids(structure)
  if (length(option_ids) == 0) {
    log_warn("Questrade Options: Step 4 FAILED - No option IDs extracted for '{ticker}', falling back to Yahoo")
    record_fallback(ticker, "Options: No options available")
    return(fetch_options_chain_yahoo(ticker, expiration))
  }
  log_info("Questrade Options: Step 4 complete - Extracted {length(option_ids)} option IDs for '{ticker}'")

  log_debug("Questrade Options: Found {length(option_ids)} option contracts for '{ticker}'")

  # Step 5: Fetch pricing data for all options
  quotes <- fetch_questrade_option_quotes(option_ids, auth)
  if (is.null(quotes)) {
    log_warn("Questrade Options: Pricing fetch failed for '{ticker}', falling back to Yahoo")
    record_fallback(ticker, "Options: Pricing fetch failed")
    return(fetch_options_chain_yahoo(ticker, expiration))
  }

  # Step 6: Convert to Yahoo format
  result <- tryCatch({
    convert_options_to_yahoo_format(structure, quotes, expiration)
  }, error = function(e) {
    log_error("Questrade Options: Format conversion failed for '{ticker}' - {e$message}")
    record_fallback(ticker, "Options: Format conversion failed")
    return(NULL)
  })

  if (is.null(result)) {
    return(fetch_options_chain_yahoo(ticker, expiration))
  }

  log_info("Questrade Options: Successfully fetched options chain for '{ticker}'")
  return(result)
}

################################################################################
# QUESTRADE API CALLS
################################################################################

#' Fetch options structure from Questrade API
#'
#' Calls GET symbols/{id}/options to retrieve the complete options chain
#' structure including all expirations, strikes, and option symbol IDs.
#'
#' @param symbol_id Integer Questrade symbol ID
#' @param auth List with access_token and api_server from get_questrade_auth()
#' @return List with options structure data, or NULL on failure
#' @noRd
fetch_questrade_options_structure <- function(symbol_id, auth) {
  if (is.null(auth)) {
    log_error("Questrade Options: No authentication provided for structure fetch")
    return(NULL)
  }

  tryCatch({
    url <- paste0(auth$api_server, "v1/symbols/", symbol_id, "/options")
    log_debug("Questrade Options: Fetching structure for symbolId {symbol_id}")

    response <- GET(
      url,
      add_headers(Authorization = paste("Bearer", auth$access_token))
    )

    if (status_code(response) != 200) {
      log_error("Questrade Options: Structure fetch failed for symbolId {symbol_id} with status {status_code(response)}")
      return(NULL)
    }

    # Log the raw response structure for debugging
    full_response <- content(response)
    response_fields <- names(full_response)
    log_info("Questrade API Response fields for symbolId {symbol_id}: {paste(response_fields, collapse=', ')}")

    # Check for error field
    if (!is.null(full_response$error)) {
      log_error("Questrade API returned error for symbolId {symbol_id}: {full_response$error}")
    }

    # Questrade returns 'optionChain' not 'options'
    options_data <- full_response$optionChain

    if (is.null(options_data)) {
      log_warn("Questrade Options: 'optionChain' field is NULL for symbolId {symbol_id}")
      log_info("Questrade API full response: {paste(capture.output(str(full_response)), collapse=' ')}")
      return(NULL)
    }

    if (length(options_data) == 0) {
      log_warn("Questrade Options: 'optionChain' array is EMPTY (length=0) for symbolId {symbol_id}")
      return(NULL)
    }

    log_debug("Questrade Options: Retrieved structure with {length(options_data)} expiration dates")
    return(options_data)

  }, error = function(e) {
    log_error("Questrade Options: Structure fetch error for symbolId {symbol_id} - {e$message}")
    return(NULL)
  })
}

#' Fetch option quotes from Questrade API
#'
#' Calls POST markets/quotes/options to retrieve pricing data (bid, ask, volume,
#' open interest) for a batch of option symbol IDs. Implements batching to handle
#' large option ID arrays that exceed API limits.
#'
#' @param option_ids Integer vector of Questrade option symbol IDs
#' @param auth List with access_token and api_server from get_questrade_auth()
#' @param batch_size Integer, number of option IDs to fetch per request (default: 100)
#' @return List of option quote data, or NULL on failure
#' @noRd
fetch_questrade_option_quotes <- function(option_ids, auth, batch_size = 100) {
  if (is.null(auth)) {
    log_error("Questrade Options: No authentication provided for quotes fetch")
    return(NULL)
  }

  if (length(option_ids) == 0) {
    log_warn("Questrade Options: No option IDs provided for quotes fetch")
    return(NULL)
  }

  tryCatch({
    url <- paste0(auth$api_server, "v1/markets/quotes/options")
    total_ids <- length(option_ids)
    log_info("Questrade Options: Fetching quotes for {total_ids} options in batches of {batch_size}")

    # Split option IDs into batches
    num_batches <- ceiling(total_ids / batch_size)
    all_quotes <- list()

    for (batch_num in seq_len(num_batches)) {
      start_idx <- (batch_num - 1) * batch_size + 1
      end_idx <- min(batch_num * batch_size, total_ids)
      batch_ids <- option_ids[start_idx:end_idx]

      log_debug("Questrade Options: Fetching batch {batch_num}/{num_batches} ({length(batch_ids)} IDs)")

      # Questrade expects optionIds array in POST body
      response <- POST(
        url,
        add_headers(Authorization = paste("Bearer", auth$access_token)),
        body = list(optionIds = batch_ids),
        encode = "json"
      )

      if (status_code(response) != 200) {
        # Log the actual error response for debugging
        error_content <- tryCatch({
          content(response, as = "text", encoding = "UTF-8")
        }, error = function(e) {
          "Could not parse error response"
        })
        log_error("Questrade Options: Batch {batch_num} fetch failed with status {status_code(response)}")
        log_error("Questrade Options: Error response: {error_content}")
        # Continue with other batches instead of failing completely
        next
      }

      batch_quotes <- content(response)$optionQuotes

      if (is.null(batch_quotes) || length(batch_quotes) == 0) {
        log_warn("Questrade Options: Batch {batch_num} returned no quote data")
        next
      }

      log_debug("Questrade Options: Batch {batch_num} retrieved {length(batch_quotes)} quotes")
      all_quotes <- c(all_quotes, batch_quotes)
    }

    if (length(all_quotes) == 0) {
      log_warn("Questrade Options: No quote data returned from any batch")
      return(NULL)
    }

    log_info("Questrade Options: Successfully retrieved {length(all_quotes)} total quotes across {num_batches} batches")
    return(all_quotes)

  }, error = function(e) {
    log_error("Questrade Options: Quotes fetch error - {e$message}")
    return(NULL)
  })
}

################################################################################
# DATA EXTRACTION AND TRANSFORMATION
################################################################################

#' Extract all option IDs from structure
#'
#' Walks through the nested options structure and extracts all call and put
#' symbol IDs for batch quote fetching.
#'
#' @param structure List from fetch_questrade_options_structure()
#' @return Integer vector of option symbol IDs
#' @noRd
extract_option_ids <- function(structure) {
  option_ids <- c()

  for (expiry in structure) {
    # Questrade API has chainPerRoot layer containing chainPerStrikePrice
    if (is.null(expiry$chainPerRoot)) next

    for (root in expiry$chainPerRoot) {
      root_symbol <- root$root %||% "unknown"
      log_debug("Questrade Options: Processing root '{root_symbol}' for expiry {expiry$expiryDate}")

      if (is.null(root$chainPerStrikePrice)) next

      strike_count <- length(root$chainPerStrikePrice)
      log_debug("Questrade Options: Found {strike_count} strikes for root '{root_symbol}'")

      for (strike_data in root$chainPerStrikePrice) {
        if (!is.null(strike_data$callSymbolId)) {
          option_ids <- c(option_ids, strike_data$callSymbolId)
        }
        if (!is.null(strike_data$putSymbolId)) {
          option_ids <- c(option_ids, strike_data$putSymbolId)
        }
      }
    }
  }

  return(unique(option_ids))
}

#' Convert Questrade options data to Yahoo Finance format
#'
#' Transforms Questrade's nested JSON structure into Yahoo's expected format:
#' a named list where each name is an expiration date string (e.g., "Mar.21.2025")
#' and each value is a list with $calls and $puts data frames.
#'
#' @param structure List from fetch_questrade_options_structure()
#' @param quotes List from fetch_questrade_option_quotes()
#' @param expiration Optional filter for specific expiration (NULL for all)
#' @return List in Yahoo Finance format
#' @noRd
convert_options_to_yahoo_format <- function(structure, quotes, expiration = NULL) {

  # Build lookup table: option_id -> quote data
  quotes_lookup <- list()
  for (quote in quotes) {
    if (!is.null(quote$symbolId)) {
      quotes_lookup[[as.character(quote$symbolId)]] <- quote
    }
  }

  # Result list: named by expiration date
  result <- list()

  for (expiry in structure) {
    # Parse expiration date
    expiry_date <- parse_questrade_date(expiry$expiryDate)
    if (is.null(expiry_date)) next

    # Format as Yahoo date string (e.g., "Mar.21.2025")
    yahoo_date_str <- format_yahoo_date(expiry_date)

    # Skip if filtering by specific expiration and this doesn't match
    if (!is.null(expiration) && yahoo_date_str != expiration) {
      next
    }

    # Flatten chainPerRoot structure - combine all strikes from all roots
    all_strikes <- list()
    if (!is.null(expiry$chainPerRoot)) {
      for (root in expiry$chainPerRoot) {
        root_symbol <- root$root %||% "unknown"
        if (!is.null(root$chainPerStrikePrice)) {
          strike_count <- length(root$chainPerStrikePrice)
          log_debug("Questrade Options: Flattening {strike_count} strikes from root '{root_symbol}' for expiry {yahoo_date_str}")
          all_strikes <- c(all_strikes, root$chainPerStrikePrice)
        }
      }
    }

    log_debug("Questrade Options: Total {length(all_strikes)} strikes for expiry {yahoo_date_str}")

    # Build calls and puts data frames for this expiration
    calls_df <- build_options_dataframe(
      all_strikes,
      quotes_lookup,
      type = "call"
    )

    puts_df <- build_options_dataframe(
      all_strikes,
      quotes_lookup,
      type = "put"
    )

    # Add to result if we have data
    if (nrow(calls_df) > 0 || nrow(puts_df) > 0) {
      result[[yahoo_date_str]] <- list(
        calls = calls_df,
        puts = puts_df
      )
    }
  }

  return(result)
}

#' Build options data frame for calls or puts
#'
#' Creates a data frame matching Yahoo's structure with columns:
#' Strike, Last, Chg, Bid, Ask, Vol, OI, IV (implied volatility)
#'
#' @param chain_per_strike List of strike price data from Questrade
#' @param quotes_lookup Named list of quote data by symbol ID
#' @param type Character: "call" or "put"
#' @return Data frame with Yahoo-compatible structure
#' @noRd
build_options_dataframe <- function(chain_per_strike, quotes_lookup, type = "call") {

  rows <- list()

  for (strike_data in chain_per_strike) {
    strike_price <- strike_data$strikePrice

    # Get the appropriate symbol ID
    symbol_id <- if (type == "call") {
      strike_data$callSymbolId
    } else {
      strike_data$putSymbolId
    }

    if (is.null(symbol_id)) next

    # Look up quote data
    quote <- quotes_lookup[[as.character(symbol_id)]]
    if (is.null(quote)) next

    # Extract fields (with defaults for missing data)
    last_price <- quote$lastTradePrice %||% NA_real_
    bid_price <- quote$bidPrice %||% NA_real_
    ask_price <- quote$askPrice %||% NA_real_
    volume <- quote$volume %||% 0
    open_interest <- quote$openInterest %||% 0
    implied_vol <- quote$volatility %||% NA_real_

    # Calculate change (if we have last trade and previous)
    change <- NA_real_  # Questrade doesn't provide previous close for options

    # Build row
    rows[[length(rows) + 1]] <- data.frame(
      Strike = strike_price,
      Last = last_price,
      Chg = change,
      Bid = bid_price,
      Ask = ask_price,
      Vol = volume,
      OI = open_interest,
      IV = implied_vol,
      stringsAsFactors = FALSE
    )
  }

  # Combine all rows
  if (length(rows) == 0) {
    # Return empty data frame with correct structure
    return(data.frame(
      Strike = numeric(0),
      Last = numeric(0),
      Chg = numeric(0),
      Bid = numeric(0),
      Ask = numeric(0),
      Vol = integer(0),
      OI = integer(0),
      IV = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  df <- bind_rows(rows)

  # Sort by strike price ascending
  df <- df %>% arrange(Strike)

  return(df)
}

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Parse Questrade date string to R Date
#'
#' Converts Questrade's ISO 8601 datetime (e.g., "2025-03-21T00:00:00.000000-05:00")
#' to R Date object.
#'
#' @param date_str Character Questrade date string
#' @return Date object, or NULL on parse failure
#' @noRd
parse_questrade_date <- function(date_str) {
  if (is.null(date_str)) return(NULL)

  tryCatch({
    # Extract just the date part (before 'T')
    date_part <- sub("T.*$", "", date_str)
    return(as.Date(date_part))
  }, error = function(e) {
    log_warn("Questrade Options: Failed to parse date '{date_str}'")
    return(NULL)
  })
}

#' Format Date as Yahoo expiration string
#'
#' Converts R Date to Yahoo's format: "Mar.21.2025"
#'
#' @param date Date object
#' @return Character string in Yahoo format
#' @noRd
format_yahoo_date <- function(date) {
  if (is.null(date) || is.na(date)) return(NULL)

  # Format: "Mar.21.2025"
  return(format(date, "%b.%d.%Y"))
}

#' Null coalescing operator
#'
#' @param x Value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
