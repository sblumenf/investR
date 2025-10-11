#' Zero-Dividend Strategy Configuration
#'
#' Configuration constants for the zero-dividend covered call strategy
#'
#' @name zero-dividend-config
NULL

# Strategy Parameters
# Internal configuration object - not exported
# Use get_config() to access configuration values
#
# NOTE: Many constants are pulled from inst/golem-config.yml for centralized management.
# This CONFIG object contains only zero-dividend-specific parameters and provides
# a convenient interface for accessing both local and golem config values.

ZERO_DIVIDEND_CONFIG <- list(
  # Option Selection (from golem-config.yml)
  strike_threshold_pct = get_golem_config_value("zero_dividend", "strike_threshold_pct", 0.85),
  min_days = get_golem_config_value("zero_dividend", "min_days", 45),
  max_days = get_golem_config_value("zero_dividend", "max_days", 120),

  # Parallel Processing (from golem-config.yml)
  max_workers = get_golem_config_value("zero_dividend", "max_workers", 10),

  # Data Validation (from golem-config.yml)
  min_option_bid = get_golem_config_value("zero_dividend", "min_option_bid", 0.01),
  min_open_interest = get_golem_config_value("zero_dividend", "min_open_interest", 10),
  max_stock_price = get_golem_config_value("zero_dividend", "max_stock_price", 250),

  # Financial Constants (from golem-config.yml)
  shares_per_contract = get_golem_config_value("zero_dividend", "shares_per_contract", 100),
  days_per_year = get_golem_config_value("shared", "days_per_year", 365),

  # Date Ranges (from golem-config.yml)
  history_years = get_golem_config_value("zero_dividend", "history_years", 5),
  cache_ttl_days = get_golem_config_value("zero_dividend", "cache_ttl_days", 30),

  # Thresholds and Limits (from golem-config.yml)
  short_expiry_warning_days = get_golem_config_value("zero_dividend", "short_expiry_warning_days", 14),
  negative_return_threshold = get_golem_config_value("zero_dividend", "negative_return_threshold", 0),
  error_truncate_length = get_golem_config_value("zero_dividend", "error_truncate_length", 50),

  # Output (from golem-config.yml)
  default_top_n = get_golem_config_value("zero_dividend", "default_top_n", 10),
  output_dir = get_golem_config_value("zero_dividend", "output_dir", "strategies")
)

#' Validate Zero Dividend Strategy Configuration
#'
#' Ensures config values are valid
#'
#' @param config Configuration list
#' @return TRUE if valid, throws error otherwise
#' @noRd
validate_zero_dividend_config <- function(config = ZERO_DIVIDEND_CONFIG) {
  if (config$strike_threshold_pct <= 0 || config$strike_threshold_pct > 1) {
    stop("strike_threshold_pct must be between 0 and 1")
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
  invisible(TRUE)
}

#' Get zero dividend configuration value(s)
#'
#' Accessor function for strategy configuration. Provides controlled access
#' to internal ZERO_DIVIDEND_CONFIG object.
#'
#' @param key Optional configuration key to retrieve. If NULL, returns entire config.
#' @return Configuration value(s)
#' @export
#' @examples
#' \dontrun{
#'   # Get entire configuration
#'   config <- get_zero_dividend_config()
#'
#'   # Get specific value
#'   strike_threshold <- get_zero_dividend_config("strike_threshold_pct")
#' }
get_zero_dividend_config <- function(key = NULL) {
  if (is.null(key)) {
    return(ZERO_DIVIDEND_CONFIG)
  }

  if (!key %in% names(ZERO_DIVIDEND_CONFIG)) {
    stop(sprintf("Configuration key '%s' not found. Available keys: %s",
                key, paste(names(ZERO_DIVIDEND_CONFIG), collapse = ", ")))
  }

  ZERO_DIVIDEND_CONFIG[[key]]
}
