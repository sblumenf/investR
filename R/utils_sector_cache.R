#' Sector Lookup with In-Memory Cache
#'
#' Fetches sector data from Questrade API and caches in memory for the R session.
#'
#' @name sector-cache
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows filter distinct slice
#' @importFrom tibble tibble
#' @importFrom logger log_debug log_info log_warn
NULL

# In-memory cache (environment is mutable, unlike package-level variables)
.sector_cache <- new.env(parent = emptyenv())

#' Fetch symbol details from Questrade API
#'
#' Calls GET /v1/symbols/:id to get full symbol information including sector/industry
#'
#' @param symbol_id Numeric symbol ID from Questrade
#' @param auth Authentication list from get_questrade_auth()
#' @return List with symbol details or NULL on error
#' @noRd
fetch_questrade_symbol_details <- function(symbol_id, auth) {
  if (is.null(auth)) return(NULL)

  tryCatch({
    url <- paste0(auth$api_server, "v1/symbols/", symbol_id)
    response <- GET(
      url,
      add_headers(Authorization = paste("Bearer", auth$access_token))
    )

    if (status_code(response) != 200) {
      log_warn("Sector Cache: Failed to fetch symbol {symbol_id} - status {status_code(response)}")
      return(NULL)
    }

    content(response)$symbols[[1]]  # API returns array, take first element

  }, error = function(e) {
    log_warn("Sector Cache: Error fetching symbol {symbol_id} - {e$message}")
    return(NULL)
  })
}

#' Lookup sector for ticker from Questrade positions
#'
#' Fetches sector from Questrade API using symbol_id from portfolio positions
#'
#' @param ticker Character ticker symbol
#' @param symbol_id Optional symbol ID (if known, faster)
#' @return Character sector name, or NULL if not found
#' @noRd
lookup_sector <- function(ticker, symbol_id = NULL) {
  # Check cache first
  if (!is.null(.sector_cache$data)) {
    cached <- .sector_cache$data %>%
      filter(symbol == !!ticker) %>%
      slice(1)

    if (nrow(cached) > 0 && !is.na(cached$sector)) {
      return(cached$sector)
    }
  }

  # Need to fetch from Questrade
  if (is.null(symbol_id) || is.na(symbol_id)) {
    # Don't have valid symbol_id, can't fetch
    # This will fall back to hardcoded map in get_ticker_sector()
    return(NULL)
  }

  # Fetch from Questrade API
  auth <- get_questrade_auth()
  if (is.null(auth)) return(NULL)

  symbol_details <- fetch_questrade_symbol_details(symbol_id, auth)
  if (is.null(symbol_details)) return(NULL)

  # Extract sector from Questrade's industrySector field
  sector <- symbol_details$industrySector

  # Validate the field exists and has a value
  if (is.null(sector) || is.na(sector) || sector == "") {
    log_warn("Sector Cache: Symbol {symbol_id} ({ticker}) has no industrySector field")
    return(NULL)
  }

  log_info("Sector Cache: Found sector '{sector}' for {ticker} (symbol_id: {symbol_id})")

  # Cache it
  if (is.null(.sector_cache$data)) {
    .sector_cache$data <- tibble(symbol = character(), sector = character())
  }

  .sector_cache$data <- .sector_cache$data %>%
    bind_rows(tibble(symbol = ticker, sector = sector)) %>%
    distinct(symbol, .keep_all = TRUE)

  return(sector)
}
