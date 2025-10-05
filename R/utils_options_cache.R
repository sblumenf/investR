#' Options Data Caching Utility
#'
#' Session-level caching for options chain data to reduce redundant API calls.
#' Cache is scoped to trading day (expires after configured hours).
#'
#' @name options-cache
#' @importFrom logger log_debug log_info
NULL

# Session-level cache environment (persists across function calls within session)
.options_cache <- new.env(parent = emptyenv())

#' Get cache key for ticker and date
#'
#' @param ticker Stock ticker symbol
#' @return Character cache key
#' @noRd
get_cache_key <- function(ticker) {
  # Cache key includes ticker and current date
  # This ensures cache expires daily
  cache_date <- format(Sys.Date(), "%Y%m%d")
  paste0(ticker, "_", cache_date)
}

#' Check if cached data is still valid
#'
#' @param cache_entry Cache entry with timestamp
#' @param ttl_hours Time-to-live in hours
#' @return Logical indicating if cache is valid
#' @noRd
is_cache_valid <- function(cache_entry, ttl_hours = 8) {
  if (is.null(cache_entry) || is.null(cache_entry$timestamp)) {
    return(FALSE)
  }

  elapsed_hours <- as.numeric(difftime(Sys.time(), cache_entry$timestamp, units = "hours"))
  elapsed_hours < ttl_hours
}

#' Get options chain from cache
#'
#' Retrieves cached options chain data if available and still valid.
#'
#' @param ticker Stock ticker symbol
#' @param ttl_hours Cache time-to-live in hours (default from config)
#' @return List with options data or NULL if not cached/expired
#' @export
get_cached_options <- function(ticker, ttl_hours = get_dynamic_config("cache_ttl_hours")) {
  # Check if caching is enabled
  if (!get_dynamic_config("cache_enabled")) {
    return(NULL)
  }

  cache_key <- get_cache_key(ticker)

  # Check if cache entry exists
  if (!exists(cache_key, envir = .options_cache, inherits = FALSE)) {
    log_debug("{ticker}: No cache entry found")
    return(NULL)
  }

  cache_entry <- get(cache_key, envir = .options_cache)

  # Validate cache freshness
  if (!is_cache_valid(cache_entry, ttl_hours)) {
    log_debug("{ticker}: Cache expired")
    rm(list = cache_key, envir = .options_cache)
    return(NULL)
  }

  log_debug("{ticker}: Using cached options data")
  cache_entry$data
}

#' Store options chain in cache
#'
#' Stores options chain data in session cache with timestamp.
#'
#' @param ticker Stock ticker symbol
#' @param options_data List containing options chain data
#' @export
set_cached_options <- function(ticker, options_data) {
  # Check if caching is enabled
  if (!get_dynamic_config("cache_enabled")) {
    return(invisible(NULL))
  }

  cache_key <- get_cache_key(ticker)

  cache_entry <- list(
    data = options_data,
    timestamp = Sys.time()
  )

  assign(cache_key, cache_entry, envir = .options_cache)
  log_debug("{ticker}: Cached options data")

  invisible(NULL)
}

#' Clear options cache
#'
#' Removes all cached options data. Useful for forcing fresh data fetch.
#'
#' @param ticker Optional ticker to clear. If NULL, clears entire cache.
#' @export
clear_options_cache <- function(ticker = NULL) {
  if (is.null(ticker)) {
    # Clear entire cache
    rm(list = ls(envir = .options_cache), envir = .options_cache)
    log_info("Cleared entire options cache")
  } else {
    # Clear specific ticker
    cache_key <- get_cache_key(ticker)
    if (exists(cache_key, envir = .options_cache, inherits = FALSE)) {
      rm(list = cache_key, envir = .options_cache)
      log_info("{ticker}: Cleared cache entry")
    }
  }

  invisible(NULL)
}

#' Get cache statistics
#'
#' Returns information about current cache state.
#'
#' @return List with cache statistics
#' @export
get_cache_stats <- function() {
  cache_keys <- ls(envir = .options_cache)

  if (length(cache_keys) == 0) {
    return(list(
      total_entries = 0,
      valid_entries = 0,
      expired_entries = 0
    ))
  }

  ttl_hours <- get_dynamic_config("cache_ttl_hours")

  valid_count <- 0
  expired_count <- 0

  for (key in cache_keys) {
    entry <- get(key, envir = .options_cache)
    if (is_cache_valid(entry, ttl_hours)) {
      valid_count <- valid_count + 1
    } else {
      expired_count <- expired_count + 1
    }
  }

  list(
    total_entries = length(cache_keys),
    valid_entries = valid_count,
    expired_entries = expired_count
  )
}
