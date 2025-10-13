#' Weekly Dividend Capture Strategy Configuration
#'
#' Configuration constants for the weekly dividend capture strategy.
#' ETF tickers are now fetched dynamically from stockanalysis.com.
#'
#' @name dividend-capture-weekly-config
#' @importFrom rvest read_html html_elements html_attr
#' @importFrom logger log_info log_warn log_error
NULL

################################################################################
# DYNAMIC TICKER FETCHING
################################################################################

# Session-level cache for ticker data
.weekly_etfs_cache <- new.env(parent = emptyenv())

#' Fetch weekly dividend ETF tickers dynamically
#'
#' Scrapes stockanalysis.com for current list of weekly dividend ETFs.
#' Results are cached for the duration specified in golem config (default: 24 hours).
#'
#' @param force_refresh Logical. If TRUE, ignores cache and fetches fresh data.
#'
#' @return Character vector of ticker symbols
#'
#' @details
#' The function implements a multi-tier fallback strategy:
#' 1. Check session cache (if not expired)
#' 2. Fetch from stockanalysis.com
#' 3. Fall back to config-specified fallback tickers
#'
#' Caching prevents excessive HTTP requests and improves performance.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get current list (uses cache if available)
#'   tickers <- fetch_weekly_dividend_tickers()
#'
#'   # Force refresh from web
#'   tickers <- fetch_weekly_dividend_tickers(force_refresh = TRUE)
#' }
fetch_weekly_dividend_tickers <- function(force_refresh = FALSE) {
  config <- get_dividend_capture_config()

  # Check if dynamic fetching is enabled
  use_dynamic <- get_golem_config_value("weekly_capture", "use_dynamic_tickers", TRUE)

  if (!use_dynamic) {
    log_info("Dynamic ticker fetching disabled, using fallback list")
    return(get_fallback_tickers())
  }

  # Check cache validity
  cache_hours <- get_golem_config_value("weekly_capture", "ticker_cache_hours", 24)

  if (!force_refresh && ticker_cache_is_valid(cache_hours)) {
    log_info("Using cached weekly ETF tickers ({length(.weekly_etfs_cache$tickers)} tickers)")
    return(.weekly_etfs_cache$tickers)
  }

  # Fetch fresh data
  log_info("Fetching weekly dividend ETF tickers from web...")

  tickers <- tryCatch(
    {
      scrape_weekly_etf_tickers()
    },
    error = function(e) {
      log_error("Failed to fetch tickers from web: {e$message}")
      NULL
    }
  )

  # Validate results
  if (is.null(tickers) || length(tickers) == 0) {
    log_warn("No tickers fetched, using fallback list")
    return(get_fallback_tickers())
  }

  # Update cache
  .weekly_etfs_cache$tickers <- tickers
  .weekly_etfs_cache$timestamp <- Sys.time()

  log_info("Successfully fetched {length(tickers)} weekly dividend ETF tickers")

  return(tickers)
}

#' Scrape weekly ETF tickers from stockanalysis.com
#'
#' @return Character vector of ticker symbols
#' @noRd
scrape_weekly_etf_tickers <- function() {
  url <- get_golem_config_value(
    "weekly_capture",
    "ticker_source_url",
    "https://stockanalysis.com/list/weekly-dividend-etfs/"
  )

  # Fetch and parse HTML
  page <- read_html(url)

  # Extract ticker links
  all_links <- page %>%
    html_elements('a[href*="/etf/"]') %>%
    html_attr('href')

  # Extract ticker symbols from URLs
  tickers <- gsub('.*/etf/([^/]+).*', '\\1', all_links)
  tickers <- unique(toupper(tickers))

  # Filter out navigation items
  nav_items <- c('/ETF/', 'SCREENER', 'COMPARE', 'LIST', 'PROVIDER', 'ETFS')
  tickers <- tickers[!tickers %in% nav_items]

  # Filter to reasonable ticker length (2-5 characters)
  tickers <- tickers[nchar(tickers) >= 2 & nchar(tickers) <= 5]

  return(tickers)
}

#' Check if ticker cache is still valid
#'
#' @param cache_hours Number of hours before cache expires
#' @return Logical
#' @noRd
ticker_cache_is_valid <- function(cache_hours) {
  if (!exists("tickers", envir = .weekly_etfs_cache)) {
    return(FALSE)
  }

  if (!exists("timestamp", envir = .weekly_etfs_cache)) {
    return(FALSE)
  }

  cache_age_hours <- as.numeric(
    difftime(Sys.time(), .weekly_etfs_cache$timestamp, units = "hours")
  )

  return(cache_age_hours < cache_hours)
}

#' Get fallback ticker list
#'
#' @return Character vector of fallback tickers
#' @noRd
get_fallback_tickers <- function() {
  fallback <- get_golem_config_value(
    "weekly_capture",
    "fallback_tickers",
    c("QQQY", "XDTE", "WDTE", "QDTE", "ULTY", "YMAX", "YMAG")
  )

  return(fallback)
}

#' Weekly Dividend Capture Strategy Configuration
#'
#' Internal configuration object for dividend capture strategy
#'
#' NOTE: Many constants are now in inst/golem-config.yml for centralized management.
#' This CONFIG object contains only weekly-specific parameters and provides
#' a convenient interface for accessing both local and golem config values.
#'
#' @format A list with strategy parameters
#' @noRd
NULL

DIVIDEND_CAPTURE_CONFIG <- list(
  # Time Constants (from golem-config.yml)
  trading_days_per_year = get_golem_config_value("shared", "trading_days_per_year", 252),
  weeks_per_year = get_golem_config_value("weekly_capture", "weeks_per_year", 52),
  days_per_year = get_golem_config_value("shared", "days_per_year", 365),

  # Investment Parameters (from golem-config.yml)
  investment_amount = get_golem_config_value("weekly_capture", "investment_amount", 10000),

  # Data Validation (from golem-config.yml)
  min_dividend_events = get_golem_config_value("weekly_capture", "min_dividend_events", 5),

  # Rate Limiting (from golem-config.yml)
  rate_limit_seconds = get_golem_config_value("weekly_capture", "rate_limit_seconds", 2),

  # Parallel Processing (from golem-config.yml)
  max_workers = get_golem_config_value("weekly_capture", "max_workers", 4),
  min_workers = get_golem_config_value("shared", "min_workers", 1),
  max_workers_limit = get_golem_config_value("shared", "max_workers_limit", 10),

  # Day of Week Mapping (strategy-specific, kept local)
  day_map = list(
    Monday = "Friday",
    Tuesday = "Monday",
    Wednesday = "Tuesday",
    Thursday = "Wednesday",
    Friday = "Thursday"
  )
)

#' Get dividend capture configuration value(s)
#'
#' Accessor function for strategy configuration.
#'
#' @param key Optional configuration key to retrieve. If NULL, returns entire config.
#' @return Configuration value(s)
#' @export
#' @examples
#' \dontrun{
#'   # Get entire configuration
#'   config <- get_dividend_capture_config()
#'
#'   # Get specific value
#'   weeks_per_year <- get_dividend_capture_config("weeks_per_year")
#' }
get_dividend_capture_config <- function(key = NULL) {
  if (is.null(key)) {
    return(DIVIDEND_CAPTURE_CONFIG)
  }

  if (!key %in% names(DIVIDEND_CAPTURE_CONFIG)) {
    stop(sprintf(
      "Configuration key '%s' not found. Available keys: %s",
      key,
      paste(names(DIVIDEND_CAPTURE_CONFIG), collapse = ", ")
    ))
  }

  DIVIDEND_CAPTURE_CONFIG[[key]]
}
