#' Dynamic Covered Calls Strategy Configuration
#'
#' Configuration constants for the dynamic covered calls strategy.
#' Uses historical drawdown to dynamically set strike prices and expiration targets.
#'
#' @name dynamic-covered-calls-config
NULL

# Strategy Parameters
# Internal configuration object - not exported
# Use get_dynamic_config() to access configuration values
#
# NOTE: Configuration values are loaded from inst/golem-config.yml for centralized management.
# This CONFIG object provides a convenient interface for accessing golem config values.

DYNAMIC_CONFIG <- list(
  # Parallel Processing (from golem-config.yml)
  max_workers = get_golem_config_value("dynamic_covered_calls", "max_workers", 4),

  # Price Filtering (from golem-config.yml)
  default_max_price = get_golem_config_value("dynamic_covered_calls", "default_max_price", 250),

  # Lookback Period (from golem-config.yml)
  default_lookback_years = get_golem_config_value("dynamic_covered_calls", "default_lookback_years", 5),

  # Strike Bounds (from golem-config.yml)
  default_min_strike_pct = get_golem_config_value("dynamic_covered_calls", "default_min_strike_pct", 0.50),
  default_max_strike_pct = get_golem_config_value("dynamic_covered_calls", "default_max_strike_pct", 0.95),

  # Target Days Bounds (from golem-config.yml)
  min_target_days = get_golem_config_value("dynamic_covered_calls", "min_target_days", 30),
  max_target_days = get_golem_config_value("dynamic_covered_calls", "max_target_days", 730),

  # Expiration Filtering (from golem-config.yml)
  expiration_filter_tolerance = get_golem_config_value("dynamic_covered_calls", "expiration_filter_tolerance", 0.50),
  rate_limit_seconds = get_golem_config_value("dynamic_covered_calls", "rate_limit_seconds", 0.5),

  # Option Selection (from golem-config.yml)
  min_option_bid = get_golem_config_value("dynamic_covered_calls", "min_option_bid", 0.01),
  min_open_interest = get_golem_config_value("dynamic_covered_calls", "min_open_interest", 0),
  min_option_volume = get_golem_config_value("dynamic_covered_calls", "min_option_volume", 10),

  # Financial Constants (from golem-config.yml)
  shares_per_contract = get_golem_config_value("dynamic_covered_calls", "shares_per_contract", 100),
  days_per_year = get_golem_config_value("shared", "days_per_year", 365),

  # Thresholds and Limits (from golem-config.yml)
  negative_return_threshold = get_golem_config_value("dynamic_covered_calls", "negative_return_threshold", 0),
  error_truncate_length = get_golem_config_value("dynamic_covered_calls", "error_truncate_length", 50),

  # Caching (from golem-config.yml)
  cache_enabled = get_golem_config_value("dynamic_covered_calls", "cache_enabled", TRUE),
  cache_ttl_hours = get_golem_config_value("dynamic_covered_calls", "cache_ttl_hours", 8),

  # Async Processing (from golem-config.yml)
  enable_async = get_golem_config_value("dynamic_covered_calls", "enable_async", TRUE),

  # Output (from golem-config.yml)
  default_top_n = get_golem_config_value("dynamic_covered_calls", "default_top_n", 10),
  output_dir = get_golem_config_value("dynamic_covered_calls", "output_dir", "strategies")
)

#' Validate Dynamic Strategy Configuration
#'
#' Ensures config values are valid
#'
#' @param config Configuration list
#' @return TRUE if valid, throws error otherwise
#' @noRd
validate_dynamic_config <- function(config = DYNAMIC_CONFIG) {
  # Validate strike bounds
  if (config$default_min_strike_pct <= 0 || config$default_min_strike_pct > 1) {
    stop("default_min_strike_pct must be between 0 and 1")
  }
  if (config$default_max_strike_pct <= 0 || config$default_max_strike_pct > 1) {
    stop("default_max_strike_pct must be between 0 and 1")
  }
  if (config$default_min_strike_pct >= config$default_max_strike_pct) {
    stop("default_min_strike_pct must be less than default_max_strike_pct")
  }

  # Validate workers
  if (config$max_workers <= 0) {
    stop("max_workers must be positive")
  }

  # Validate lookback period
  if (config$default_lookback_years <= 0) {
    stop("default_lookback_years must be positive")
  }

  # Validate price threshold
  if (config$default_max_price <= 0) {
    stop("default_max_price must be positive")
  }

  # Validate target days bounds
  if (config$min_target_days <= 0) {
    stop("min_target_days must be positive")
  }
  if (config$max_target_days <= config$min_target_days) {
    stop("max_target_days must be greater than min_target_days")
  }

  # Validate expiration filter tolerance
  if (config$expiration_filter_tolerance <= 0) {
    stop("expiration_filter_tolerance must be positive")
  }

  invisible(TRUE)
}

#' Get dynamic configuration value(s)
#'
#' Accessor function for dynamic strategy configuration. Provides controlled access
#' to internal CONFIG object.
#'
#' @param key Optional configuration key to retrieve. If NULL, returns entire config.
#' @return Configuration value(s)
#' @export
#' @examples
#' \dontrun{
#'   # Get entire configuration
#'   config <- get_dynamic_config()
#'
#'   # Get specific value
#'   max_price <- get_dynamic_config("default_max_price")
#' }
get_dynamic_config <- function(key = NULL) {
  if (is.null(key)) {
    return(DYNAMIC_CONFIG)
  }

  if (!key %in% names(DYNAMIC_CONFIG)) {
    stop(sprintf("Configuration key '%s' not found. Available keys: %s",
                key, paste(names(DYNAMIC_CONFIG), collapse = ", ")))
  }

  DYNAMIC_CONFIG[[key]]
}
