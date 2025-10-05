#' Questrade API Utilities
#'
#' Functions for authenticating with Questrade API and fetching dividend data
#'
#' @name questrade-api
#' @importFrom httr GET POST content add_headers status_code
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom logger log_info log_warn log_error log_success log_debug
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows filter mutate
#' @importFrom purrr map_dfr
#' @importFrom lubridate parse_date_time as_date
NULL

################################################################################
# AUTHENTICATION
################################################################################

#' Get Questrade API access token from refresh token
#'
#' Exchanges refresh token for access token and API server URL.
#' IMPORTANT: Automatically updates .Renviron with new refresh token
#' (Questrade rotates tokens on every use)
#'
#' @return List with access_token, api_server, and expires_in
#' @noRd
get_questrade_token <- function() {
  refresh_token <- Sys.getenv("QUESTRADE_REFRESH_TOKEN")

  if (refresh_token == "") {
    log_error("QUESTRADE_REFRESH_TOKEN not found in environment variables")
    stop("Questrade API refresh token not configured. Check your .Renviron file.")
  }

  log_debug("Requesting Questrade access token...")

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
    log_error("Failed to connect to Questrade API: {e$message}")
    return(NULL)
  })

  if (is.null(response)) {
    return(NULL)
  }

  if (status_code(response) != 200) {
    log_error("Questrade authentication failed with status {status_code(response)}")
    return(NULL)
  }

  token_data <- content(response)

  # Save new refresh token to .Renviron (Questrade rotates on every use)
  if (!is.null(token_data$refresh_token)) {
    update_refresh_token(token_data$refresh_token)
  }

  log_success("Questrade access token obtained successfully")

  return(token_data)
}

#' Update refresh token in .Renviron file
#'
#' @param new_token Character new refresh token
#' @noRd
update_refresh_token <- function(new_token) {
  renviron_path <- file.path(getwd(), ".Renviron")

  if (!file.exists(renviron_path)) {
    log_warn(".Renviron file not found, cannot update refresh token")
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

    log_debug("Updated refresh token in .Renviron")
    return(TRUE)
  }, error = function(e) {
    log_warn("Failed to update refresh token: {e$message}")
    return(FALSE)
  })
}

################################################################################
# SYMBOL SEARCH AND LOOKUP
################################################################################

#' Search for Questrade symbol ID by ticker
#'
#' @param ticker Character ticker symbol (e.g., "MSTY", "QYLD")
#' @param token_data List with access_token and api_server from get_questrade_token()
#' @return Numeric symbol ID, or NULL if not found
#' @noRd
search_symbol_id <- function(ticker, token_data) {
  if (is.null(token_data)) {
    return(NULL)
  }

  log_debug("Searching for symbol ID: {ticker}")

  response <- tryCatch({
    GET(
      paste0(token_data$api_server, "v1/symbols/search"),
      query = list(prefix = ticker),
      add_headers(Authorization = paste("Bearer", token_data$access_token))
    )
  }, error = function(e) {
    log_warn("{ticker}: Failed to search symbol - {e$message}")
    return(NULL)
  })

  if (is.null(response) || status_code(response) != 200) {
    log_warn("{ticker}: Symbol search failed")
    return(NULL)
  }

  data <- content(response)

  if (length(data$symbols) == 0) {
    log_warn("{ticker}: Symbol not found")
    return(NULL)
  }

  # Find exact match (case-insensitive)
  for (symbol in data$symbols) {
    if (toupper(symbol$symbol) == toupper(ticker)) {
      log_debug("{ticker}: Found symbol ID {symbol$symbolId}")
      return(symbol$symbolId)
    }
  }

  log_warn("{ticker}: Exact match not found")
  return(NULL)
}

#' Get symbol details including dividend information
#'
#' @param symbol_ids Numeric vector of symbol IDs
#' @param token_data List with access_token and api_server
#' @return Tibble with symbol details including ex-dividend dates
#' @noRd
get_symbol_details <- function(symbol_ids, token_data) {
  if (is.null(token_data) || length(symbol_ids) == 0) {
    return(tibble())
  }

  # Batch request - max 100 symbols at a time
  ids_string <- paste(symbol_ids, collapse = ",")

  log_debug("Fetching details for {length(symbol_ids)} symbols...")

  response <- tryCatch({
    GET(
      paste0(token_data$api_server, "v1/symbols"),
      query = list(ids = ids_string),
      add_headers(Authorization = paste("Bearer", token_data$access_token))
    )
  }, error = function(e) {
    log_error("Failed to fetch symbol details: {e$message}")
    return(NULL)
  })

  if (is.null(response) || status_code(response) != 200) {
    log_error("Symbol details request failed with status {status_code(response)}")
    return(tibble())
  }

  data <- content(response)

  if (length(data$symbols) == 0) {
    return(tibble())
  }

  # Parse symbol data
  result <- map_dfr(data$symbols, function(symbol) {
    tibble(
      ticker = symbol$symbol,
      symbol_id = symbol$symbolId,
      dividend = ifelse(is.null(symbol$dividend), NA_real_, symbol$dividend),
      ex_date = ifelse(is.null(symbol$exDate), NA_character_, symbol$exDate),
      dividend_date = ifelse(is.null(symbol$dividendDate), NA_character_, symbol$dividendDate),
      yield = ifelse(is.null(symbol$yield), NA_real_, symbol$yield)
    )
  })

  # Parse dates
  result <- result %>%
    mutate(
      ex_date = ifelse(is.na(ex_date), NA, as.character(parse_date_time(ex_date, "ymd HMS"))),
      dividend_date = ifelse(is.na(dividend_date), NA, as.character(parse_date_time(dividend_date, "ymd HMS")))
    )

  log_success("Fetched details for {nrow(result)} symbols")

  return(result)
}

################################################################################
# CACHING
################################################################################

#' Get path to dividend cache file
#'
#' @return Character path to cache file
#' @noRd
get_dividend_cache_path <- function() {
  cache_dir <- system.file("cache", package = "investR")

  # If package not installed (development), use inst/cache
  if (cache_dir == "") {
    cache_dir <- file.path(getwd(), "inst", "cache")
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  file.path(cache_dir, "questrade_dividends.rds")
}

#' Check if dividend cache is stale (older than 7 days)
#'
#' @return Logical TRUE if cache should be refreshed
#' @noRd
is_cache_stale <- function() {
  cache_path <- get_dividend_cache_path()

  if (!file.exists(cache_path)) {
    log_info("Dividend cache does not exist")
    return(TRUE)
  }

  cache_age <- difftime(Sys.time(), file.info(cache_path)$mtime, units = "days")

  if (as.numeric(cache_age) > 7) {
    log_info("Dividend cache is {round(cache_age, 1)} days old (stale)")
    return(TRUE)
  }

  log_debug("Dividend cache is {round(cache_age, 1)} days old (fresh)")
  return(FALSE)
}

#' Load dividend data from cache
#'
#' @return Tibble with cached dividend data, or NULL if cache doesn't exist
#' @noRd
load_dividend_cache <- function() {
  cache_path <- get_dividend_cache_path()

  if (!file.exists(cache_path)) {
    return(NULL)
  }

  tryCatch({
    data <- readRDS(cache_path)
    log_debug("Loaded dividend cache with {nrow(data)} ETFs")
    return(data)
  }, error = function(e) {
    log_warn("Failed to load dividend cache: {e$message}")
    return(NULL)
  })
}

#' Save dividend data to cache
#'
#' @param data Tibble with dividend data
#' @noRd
save_dividend_cache <- function(data) {
  cache_path <- get_dividend_cache_path()

  tryCatch({
    saveRDS(data, cache_path)
    log_success("Saved dividend cache with {nrow(data)} ETFs")
  }, error = function(e) {
    log_error("Failed to save dividend cache: {e$message}")
  })
}

################################################################################
# MAIN FUNCTIONS
################################################################################

#' Fetch next ex-dividend dates for monthly ETFs from Questrade API
#'
#' Retrieves upcoming ex-dividend dates for all monthly dividend ETFs.
#' Uses caching with 7-day refresh cycle.
#'
#' @param force_refresh Logical, force refresh even if cache is fresh (default FALSE)
#' @return Tibble with ticker, ex_date, dividend, dividend_date, yield
#' @export
fetch_questrade_dividend_dates <- function(force_refresh = FALSE) {
  # Check cache first
  if (!force_refresh && !is_cache_stale()) {
    cached_data <- load_dividend_cache()
    if (!is.null(cached_data)) {
      return(cached_data)
    }
  }

  log_info("Fetching fresh dividend data from Questrade API...")

  # Get access token
  token_data <- get_questrade_token()
  if (is.null(token_data)) {
    log_error("Failed to authenticate with Questrade")
    # Try to return cached data even if stale
    cached_data <- load_dividend_cache()
    if (!is.null(cached_data)) {
      log_warn("Using stale cache due to authentication failure")
      return(cached_data)
    }
    return(tibble())
  }

  # Get all tickers
  tickers <- MONTHLY_ETFS$ticker

  log_info("Looking up symbol IDs for {length(tickers)} ETFs...")

  # Search for symbol IDs
  symbol_mapping <- map_dfr(tickers, function(ticker) {
    symbol_id <- search_symbol_id(ticker, token_data)

    if (!is.null(symbol_id)) {
      tibble(ticker = ticker, symbol_id = symbol_id)
    } else {
      tibble(ticker = ticker, symbol_id = NA_integer_)
    }
  })

  # Filter out tickers without symbol IDs
  valid_symbols <- symbol_mapping %>%
    filter(!is.na(symbol_id))

  log_info("Found {nrow(valid_symbols)}/{nrow(symbol_mapping)} symbol IDs")

  if (nrow(valid_symbols) == 0) {
    log_error("No valid symbol IDs found")
    return(tibble())
  }

  # Fetch dividend details in batches of 100
  all_details <- tibble()
  batch_size <- 100

  for (i in seq(1, nrow(valid_symbols), by = batch_size)) {
    end_idx <- min(i + batch_size - 1, nrow(valid_symbols))
    batch_ids <- valid_symbols$symbol_id[i:end_idx]

    log_debug("Fetching batch {ceiling(i/batch_size)}/{ceiling(nrow(valid_symbols)/batch_size)}...")

    batch_details <- get_symbol_details(batch_ids, token_data)
    all_details <- bind_rows(all_details, batch_details)

    # Rate limiting: wait 1 second between batches
    if (end_idx < nrow(valid_symbols)) {
      Sys.sleep(1)
    }
  }

  # Save to cache
  save_dividend_cache(all_details)

  log_success("Fetched dividend data for {nrow(all_details)} ETFs")

  return(all_details)
}
