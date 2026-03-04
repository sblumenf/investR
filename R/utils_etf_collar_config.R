#' ETF Collar Strategy Configuration
#'
#' Configuration constants for the ETF collar strategy (yfscreen-based)
#'
#' @name etf-collar-config
NULL

ETF_COLLAR_CONFIG <- list(
  # Parallel Processing (from golem-config.yml)
  max_workers = get_golem_config_value("etf_collar", "max_workers", 10),

  # Data Validation (from golem-config.yml)
  min_net_credit = get_golem_config_value("etf_collar", "min_net_credit", 0.01),
  min_open_interest = get_golem_config_value("etf_collar", "min_open_interest", 10),
  max_stock_price = get_golem_config_value("etf_collar", "max_stock_price", 1000),

  # Financial Constants (from golem-config.yml)
  shares_per_contract = get_golem_config_value("etf_collar", "shares_per_contract", 100),
  days_per_year = get_golem_config_value("shared", "days_per_year", 365),

  # Reinvestment (from golem-config.yml)
  sgov_yield_default = get_golem_config_value("etf_collar", "sgov_yield_default", 0.0414),
  max_sgov_yield_sanity = get_golem_config_value("etf_collar", "max_sgov_yield_sanity", 0.15),

  # Thresholds (from golem-config.yml)
  negative_return_threshold = get_golem_config_value("etf_collar", "negative_return_threshold", 0)
)

#' Validate ETF Collar Strategy Configuration
#'
#' Ensures config values are valid
#'
#' @param config Configuration list
#' @return TRUE if valid, throws error otherwise
#' @export
validate_etf_collar_config <- function(config = ETF_COLLAR_CONFIG) {
  if (config$max_workers <= 0) {
    stop("max_workers must be positive")
  }
  if (config$min_net_credit < 0) {
    stop("min_net_credit must be non-negative")
  }
  if (config$min_open_interest < 0) {
    stop("min_open_interest must be non-negative")
  }
  if (config$max_stock_price <= 0) {
    stop("max_stock_price must be positive")
  }
  if (config$shares_per_contract <= 0) {
    stop("shares_per_contract must be positive")
  }
  if (config$sgov_yield_default <= 0 || config$sgov_yield_default >= 1) {
    stop("sgov_yield_default must be between 0 and 1")
  }
  if (config$max_sgov_yield_sanity <= 0 || config$max_sgov_yield_sanity >= 1) {
    stop("max_sgov_yield_sanity must be between 0 and 1")
  }
  invisible(TRUE)
}

#' Get ETF collar configuration value(s)
#'
#' @param key Optional configuration key to retrieve. If NULL, returns entire config.
#' @return Configuration value(s)
#' @export
get_etf_collar_config <- function(key = NULL) {
  if (is.null(key)) {
    return(ETF_COLLAR_CONFIG)
  }

  if (!key %in% names(ETF_COLLAR_CONFIG)) {
    stop(sprintf("Configuration key '%s' not found. Available keys: %s",
                key, paste(names(ETF_COLLAR_CONFIG), collapse = ", ")))
  }

  ETF_COLLAR_CONFIG[[key]]
}
