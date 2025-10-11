#' Collar Strategy Configuration
#'
#' Configuration constants for the collar (synthetic bond) strategy
#'
#' @name collar-config
NULL

# Strategy Parameters
# Internal configuration object - not exported
# Use get_collar_config() to access configuration values
#
# NOTE: Many constants are pulled from inst/golem-config.yml for centralized management.
# This CONFIG object contains only collar-specific parameters and provides
# a convenient interface for accessing both local and golem config values.

COLLAR_CONFIG <- list(
  # ETF Universe Filters (from golem-config.yml)
  min_market_cap = get_golem_config_value("collar", "min_market_cap", 1e9),  # $1 billion
  min_avg_volume = get_golem_config_value("collar", "min_avg_volume", 1e6),  # 1 million shares

  # Parallel Processing (from golem-config.yml)
  max_workers = get_golem_config_value("collar", "max_workers", 10),

  # Data Validation (from golem-config.yml)
  min_net_credit = get_golem_config_value("collar", "min_net_credit", 0.01),  # Must receive net credit
  min_open_interest = get_golem_config_value("collar", "min_open_interest", 10),
  max_stock_price = get_golem_config_value("collar", "max_stock_price", 1000),

  # Financial Constants (from golem-config.yml)
  shares_per_contract = get_golem_config_value("collar", "shares_per_contract", 100),
  days_per_year = get_golem_config_value("shared", "days_per_year", 365),

  # Date Ranges (from golem-config.yml)
  history_years = get_golem_config_value("collar", "history_years", 5),
  cache_ttl_days = get_golem_config_value("collar", "cache_ttl_days", 30),

  # Reinvestment (from golem-config.yml)
  sgov_yield_default = get_golem_config_value("collar", "sgov_yield_default", 0.0414),  # 4.14%
  max_sgov_yield_sanity = get_golem_config_value("collar", "max_sgov_yield_sanity", 0.15),  # 15% max

  # Thresholds and Limits (from golem-config.yml)
  negative_return_threshold = get_golem_config_value("collar", "negative_return_threshold", 0),
  error_truncate_length = get_golem_config_value("collar", "error_truncate_length", 50),

  # Output (from golem-config.yml)
  default_top_n = get_golem_config_value("collar", "default_top_n", 50),
  output_dir = get_golem_config_value("collar", "output_dir", "strategies")
)

#' Validate Collar Strategy Configuration
#'
#' Ensures config values are valid
#'
#' @param config Configuration list
#' @return TRUE if valid, throws error otherwise
#' @export
validate_collar_config <- function(config = COLLAR_CONFIG) {
  if (config$min_market_cap <= 0) {
    stop("min_market_cap must be positive")
  }
  if (config$min_avg_volume <= 0) {
    stop("min_avg_volume must be positive")
  }
  if (config$max_workers <= 0) {
    stop("max_workers must be positive")
  }
  if (config$history_years <= 0) {
    stop("history_years must be positive")
  }
  if (config$cache_ttl_days <= 0) {
    stop("cache_ttl_days must be positive")
  }
  if (config$sgov_yield_default <= 0 || config$sgov_yield_default >= 1) {
    stop("sgov_yield_default must be between 0 and 1")
  }
  invisible(TRUE)
}

#' Get collar configuration value(s)
#'
#' Accessor function for strategy configuration. Provides controlled access
#' to internal COLLAR_CONFIG object.
#'
#' @param key Optional configuration key to retrieve. If NULL, returns entire config.
#' @return Configuration value(s)
#' @export
#' @examples
#' \dontrun{
#'   # Get entire configuration
#'   config <- get_collar_config()
#'
#'   # Get specific value
#'   min_market_cap <- get_collar_config("min_market_cap")
#' }
get_collar_config <- function(key = NULL) {
  if (is.null(key)) {
    return(COLLAR_CONFIG)
  }

  if (!key %in% names(COLLAR_CONFIG)) {
    stop(sprintf("Configuration key '%s' not found. Available keys: %s",
                key, paste(names(COLLAR_CONFIG), collapse = ", ")))
  }

  COLLAR_CONFIG[[key]]
}
