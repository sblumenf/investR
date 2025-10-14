#' Monthly High-Yield Dividend Capture Strategy Configuration
#'
#' Configuration constants for the monthly high-yield dividend capture strategy.
#' ETF tickers and yields are fetched dynamically from stockanalysis.com.
#'
#' @name dividend-capture-monthly-high-yield-config
#' @importFrom rvest read_html html_elements html_text
#' @importFrom logger log_info log_warn log_error
#' @importFrom tibble tibble
#' @importFrom dplyr %>% filter mutate
NULL

################################################################################
# DYNAMIC TICKER FETCHING WITH YIELD DATA
################################################################################

# Session-level cache for ticker data
.monthly_high_yield_etfs_cache <- new.env(parent = emptyenv())

#' Fetch monthly high-yield dividend ETF tickers dynamically
#'
#' Scrapes stockanalysis.com for current list of monthly dividend ETFs with yields.
#' Filters for ETFs with yield > 8% (configurable).
#' Results are cached for the duration specified in golem config (default: 24 hours).
#'
#' @param force_refresh Logical. If TRUE, ignores cache and fetches fresh data.
#' @param min_yield Numeric. Minimum yield percentage to include (default from config: 8.0)
#'
#' @return Tibble with columns: ticker, name, yield_pct, yield_numeric
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
#'   etfs <- fetch_monthly_high_yield_etfs()
#'
#'   # Force refresh from web
#'   etfs <- fetch_monthly_high_yield_etfs(force_refresh = TRUE)
#'
#'   # Use different yield threshold
#'   etfs <- fetch_monthly_high_yield_etfs(min_yield = 10.0)
#' }
fetch_monthly_high_yield_etfs <- function(force_refresh = FALSE, min_yield = NULL) {
  # Get min yield from config if not provided
  if (is.null(min_yield)) {
    min_yield <- get_golem_config_value("monthly_high_yield_capture", "min_yield_pct", 8.0)
  }

  # Check if dynamic fetching is enabled
  use_dynamic <- get_golem_config_value("monthly_high_yield_capture", "use_dynamic_tickers", TRUE)

  if (!use_dynamic) {
    log_info("Dynamic ticker fetching disabled, using fallback list")
    return(get_monthly_high_yield_fallback())
  }

  # Check cache validity
  cache_hours <- get_golem_config_value("monthly_high_yield_capture", "ticker_cache_hours", 24)

  if (!force_refresh && monthly_high_yield_cache_is_valid(cache_hours)) {
    log_info("Using cached monthly high-yield ETF data ({nrow(.monthly_high_yield_etfs_cache$etfs)} ETFs)")
    cached_etfs <- .monthly_high_yield_etfs_cache$etfs

    # Re-apply yield filter in case min_yield parameter changed
    filtered <- cached_etfs %>% filter(yield_numeric >= min_yield)
    log_info("Filtered to {nrow(filtered)} ETFs with yield >= {min_yield}%")

    return(filtered)
  }

  # Fetch fresh data
  log_info("Fetching monthly high-yield dividend ETF data from web...")

  etfs <- tryCatch(
    {
      scrape_monthly_high_yield_etfs()
    },
    error = function(e) {
      log_error("Failed to fetch ETFs from web: {e$message}")
      NULL
    }
  )

  # Validate results
  if (is.null(etfs) || nrow(etfs) == 0) {
    log_warn("No ETFs fetched, using fallback list")
    return(get_monthly_high_yield_fallback())
  }

  # Update cache (before filtering, so we cache all data)
  .monthly_high_yield_etfs_cache$etfs <- etfs
  .monthly_high_yield_etfs_cache$timestamp <- Sys.time()

  log_info("Successfully fetched {nrow(etfs)} monthly dividend ETF records")

  # Filter by yield
  filtered <- etfs %>% filter(yield_numeric >= min_yield)
  log_info("Filtered to {nrow(filtered)} ETFs with yield >= {min_yield}%")

  return(filtered)
}

#' Scrape monthly high-yield ETF data from stockanalysis.com
#'
#' Parses HTML table with pattern: ticker, name, price, return, yield (repeating every 5 cells)
#'
#' @return Tibble with ticker, name, yield_pct, yield_numeric
#' @noRd
scrape_monthly_high_yield_etfs <- function() {
  url <- get_golem_config_value(
    "monthly_high_yield_capture",
    "ticker_source_url",
    "https://stockanalysis.com/list/monthly-dividend-etfs/"
  )

  # Fetch and parse HTML
  page <- read_html(url)

  # Extract all table cells
  cells <- page %>% html_elements('td') %>% html_text()

  # Data pattern: ticker, name, price, return, yield (repeating every 5 cells)
  n_etfs <- length(cells) / 5

  if (n_etfs < 1) {
    log_error("No ETF data found in HTML table")
    return(tibble())
  }

  # Parse into structured data
  etfs <- tibble(
    ticker = cells[seq(1, length(cells), 5)],
    name = cells[seq(2, length(cells), 5)],
    price = cells[seq(3, length(cells), 5)],
    return_pct = cells[seq(4, length(cells), 5)],
    yield_pct = cells[seq(5, length(cells), 5)]
  )

  # Convert yield to numeric (remove % sign)
  etfs <- etfs %>%
    mutate(
      yield_numeric = as.numeric(gsub('%', '', yield_pct))
    )

  # Remove any rows with missing/invalid data
  etfs <- etfs %>%
    filter(!is.na(ticker), !is.na(yield_numeric), ticker != "")

  return(etfs %>% select(ticker, name, yield_pct, yield_numeric))
}

#' Check if monthly high-yield ETF cache is still valid
#'
#' @param cache_hours Number of hours before cache expires
#' @return Logical
#' @noRd
monthly_high_yield_cache_is_valid <- function(cache_hours) {
  if (!exists("etfs", envir = .monthly_high_yield_etfs_cache)) {
    return(FALSE)
  }

  if (!exists("timestamp", envir = .monthly_high_yield_etfs_cache)) {
    return(FALSE)
  }

  cache_age_hours <- as.numeric(
    difftime(Sys.time(), .monthly_high_yield_etfs_cache$timestamp, units = "hours")
  )

  return(cache_age_hours < cache_hours)
}

#' Get fallback monthly high-yield ticker list
#'
#' @return Tibble with ticker, name, yield_pct, yield_numeric
#' @noRd
get_monthly_high_yield_fallback <- function() {
  fallback_tickers <- get_golem_config_value(
    "monthly_high_yield_capture",
    "fallback_tickers",
    c("QYLD", "XYLD", "RYLD", "JEPI", "JEPQ", "FEPI", "SPYI")
  )

  # Return as tibble with placeholder yield data
  tibble(
    ticker = fallback_tickers,
    name = paste(fallback_tickers, "ETF"),
    yield_pct = "N/A",
    yield_numeric = 0.0
  )
}

################################################################################
# STRATEGY CONFIGURATION
################################################################################

#' Monthly High-Yield Dividend Capture Strategy Configuration
#'
#' Internal configuration object for monthly high-yield dividend capture strategy
#'
#' NOTE: Many constants are now in inst/golem-config.yml for centralized management.
#' This CONFIG object contains only strategy-specific parameters and provides
#' a convenient interface for accessing both local and golem config values.
#'
#' @format A list with strategy parameters
#' @noRd
NULL

DIVIDEND_CAPTURE_MONTHLY_HIGH_YIELD_CONFIG <- list(
  # Time Constants (from golem-config.yml)
  trading_days_per_year = get_golem_config_value("shared", "trading_days_per_year", 252),
  months_per_year = get_golem_config_value("monthly_high_yield_capture", "months_per_year", 12),
  days_per_year = get_golem_config_value("shared", "days_per_year", 365),

  # Investment Parameters (from golem-config.yml)
  investment_amount = get_golem_config_value("monthly_high_yield_capture", "investment_amount", 10000),

  # Data Validation (from golem-config.yml)
  min_dividend_events = get_golem_config_value("monthly_high_yield_capture", "min_dividend_events", 5),

  # Yield Filter (from golem-config.yml)
  min_yield_pct = get_golem_config_value("monthly_high_yield_capture", "min_yield_pct", 8.0),

  # Quality Filters (from golem-config.yml)
  min_success_rate = get_golem_config_value("monthly_high_yield_capture", "min_success_rate", 70.0),
  exclude_negative_returns = get_golem_config_value("monthly_high_yield_capture", "exclude_negative_returns", TRUE),

  # Rate Limiting (from golem-config.yml)
  rate_limit_seconds = get_golem_config_value("monthly_high_yield_capture", "rate_limit_seconds", 2),

  # Parallel Processing (from golem-config.yml)
  max_workers = get_golem_config_value("monthly_high_yield_capture", "max_workers", 4),
  min_workers = get_golem_config_value("shared", "min_workers", 1),
  max_workers_limit = get_golem_config_value("shared", "max_workers_limit", 10)
)

#' Get monthly high-yield dividend capture configuration value(s)
#'
#' Accessor function for strategy configuration.
#'
#' @param key Optional configuration key to retrieve. If NULL, returns entire config.
#' @return Configuration value(s)
#' @export
#' @examples
#' \dontrun{
#'   # Get entire configuration
#'   config <- get_monthly_high_yield_config()
#'
#'   # Get specific value
#'   months <- get_monthly_high_yield_config("months_per_year")
#' }
get_monthly_high_yield_config <- function(key = NULL) {
  if (is.null(key)) {
    return(DIVIDEND_CAPTURE_MONTHLY_HIGH_YIELD_CONFIG)
  }

  if (!key %in% names(DIVIDEND_CAPTURE_MONTHLY_HIGH_YIELD_CONFIG)) {
    stop(sprintf(
      "Configuration key '%s' not found. Available keys: %s",
      key,
      paste(names(DIVIDEND_CAPTURE_MONTHLY_HIGH_YIELD_CONFIG), collapse = ", ")
    ))
  }

  DIVIDEND_CAPTURE_MONTHLY_HIGH_YIELD_CONFIG[[key]]
}
