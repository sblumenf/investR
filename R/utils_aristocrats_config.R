#' Aristocrats Strategy Configuration
#'
#' Configuration constants for the dividend aristocrats covered call strategy
#'
#' @name aristocrats-config
NULL

# Strategy Parameters
# Internal configuration object - not exported
# Use get_config() to access configuration values
#
# NOTE: Many constants are now in inst/golem-config.yml for centralized management.
# This CONFIG object contains only aristocrats-specific parameters and provides
# a convenient interface for accessing both local and golem config values.

#' Get value from golem config with fallback (Aristocrats)
#'
#' @param section Section name (e.g., "shared", "aristocrats")
#' @param key Key name within section
#' @param fallback Fallback value if not found
#' @return Config value or fallback
#' @noRd
get_golem_config_value_aristocrats <- function(section, key, fallback = NULL) {
  tryCatch({
    golem_config <- golem::get_golem_config()
    value <- golem_config[[section]][[key]]
    if (is.null(value)) fallback else value
  }, error = function(e) {
    fallback
  })
}

ARISTOCRATS_CONFIG <- list(
  # Option Selection (from golem-config.yml)
  strike_threshold_pct = get_golem_config_value_aristocrats("aristocrats", "strike_threshold_pct", 0.8),
  target_days = get_golem_config_value_aristocrats("aristocrats", "target_days", NULL),

  # Parallel Processing (from golem-config.yml)
  max_workers = get_golem_config_value_aristocrats("aristocrats", "max_workers", 10),

  # Data Validation (from golem-config.yml)
  min_aristocrats = get_golem_config_value_aristocrats("aristocrats", "min_aristocrats", 50),
  min_dividend_quarters = get_golem_config_value_aristocrats("aristocrats", "min_dividend_quarters", 4),
  min_option_bid = get_golem_config_value_aristocrats("aristocrats", "min_option_bid", 0.01),
  min_open_interest = get_golem_config_value_aristocrats("aristocrats", "min_open_interest", 0),

  # Financial Constants (from golem-config.yml)
  sgov_yield_default = get_golem_config_value_aristocrats("aristocrats", "sgov_yield_default", 0.05),
  shares_per_contract = get_golem_config_value_aristocrats("aristocrats", "shares_per_contract", 100),
  days_per_year = get_golem_config_value_aristocrats("shared", "days_per_year", 365),

  # Date Ranges (from golem-config.yml)
  history_years = get_golem_config_value_aristocrats("aristocrats", "history_years", 5),

  # Thresholds and Limits (from golem-config.yml)
  short_expiry_warning_days = get_golem_config_value_aristocrats("aristocrats", "short_expiry_warning_days", 14),
  negative_return_threshold = get_golem_config_value_aristocrats("aristocrats", "negative_return_threshold", 0),
  low_dividend_threshold = get_golem_config_value_aristocrats("aristocrats", "low_dividend_threshold", 0.5),
  error_truncate_length = get_golem_config_value_aristocrats("aristocrats", "error_truncate_length", 50),
  max_sgov_yield_sanity = get_golem_config_value_aristocrats("aristocrats", "max_sgov_yield_sanity", 0.20),

  # Output (from golem-config.yml)
  default_top_n = get_golem_config_value_aristocrats("aristocrats", "default_top_n", 10),
  output_dir = get_golem_config_value_aristocrats("aristocrats", "output_dir", "strategies"),

  # Web Scraping (strategy-specific, kept local)
  urls = list(
    stockanalysis = "https://stockanalysis.com/list/dividend-aristocrats/",
    wikipedia = "https://en.wikipedia.org/wiki/S%26P_500_Dividend_Aristocrats"
  ),

  # Column Name Variations (strategy-specific, kept local)
  ticker_column_names = c("Ticker symbol", "Symbol", "Ticker")
)

#' Validate Strategy Configuration
#'
#' Ensures config values are valid
#'
#' @param config Configuration list
#' @return TRUE if valid, throws error otherwise
#' @noRd
validate_config <- function(config = ARISTOCRATS_CONFIG) {
  if (config$strike_threshold_pct <= 0 || config$strike_threshold_pct > 1) {
    stop("strike_threshold_pct must be between 0 and 1")
  }
  if (config$max_workers <= 0) {
    stop("max_workers must be positive")
  }
  if (config$min_aristocrats <= 0) {
    stop("min_aristocrats must be positive")
  }
  if (config$history_years <= 0) {
    stop("history_years must be positive")
  }
  invisible(TRUE)
}

#' Get configuration value(s)
#'
#' Accessor function for strategy configuration. Provides controlled access
#' to internal ARISTOCRATS_CONFIG object.
#'
#' @param key Optional configuration key to retrieve. If NULL, returns entire config.
#' @return Configuration value(s)
#' @export
#' @examples
#' \dontrun{
#'   # Get entire configuration
#'   config <- get_config()
#'
#'   # Get specific value
#'   strike_threshold <- get_config("strike_threshold_pct")
#' }
get_config <- function(key = NULL) {
  if (is.null(key)) {
    return(ARISTOCRATS_CONFIG)
  }

  if (!key %in% names(ARISTOCRATS_CONFIG)) {
    stop(sprintf("Configuration key '%s' not found. Available keys: %s",
                key, paste(names(ARISTOCRATS_CONFIG), collapse = ", ")))
  }

  ARISTOCRATS_CONFIG[[key]]
}