#' ETF Screener Covered Calls Strategy Configuration
#'
#' Configuration constants for the ETF screener covered call strategy
#'
#' @name etf-screener-config
NULL

# Strategy Parameters
# Internal configuration object - not exported
# Use get_etf_screener_config() to access configuration values

ETF_SCREENER_CONFIG <- list(
  # Option Selection
  strike_threshold_pct = 0.85,  # Default 85% strike threshold
  min_days = 45,               # Default minimum 45 days to expiration
  max_days = 120,              # Default maximum 120 days to expiration

  # Parallel Processing
  max_workers = 4  # Default 4 workers for ETF screening
)

#' Validate ETF Screener Strategy Configuration
#'
#' Ensures config values are valid
#'
#' @param config Configuration list
#' @return TRUE if valid, throws error otherwise
#' @noRd
validate_etf_screener_config <- function(config = ETF_SCREENER_CONFIG) {
  if (config$strike_threshold_pct <= 0 || config$strike_threshold_pct > 1) {
    stop("strike_threshold_pct must be between 0 and 1")
  }
  if (config$max_workers <= 0) {
    stop("max_workers must be positive")
  }
  invisible(TRUE)
}

#' Get ETF screener configuration value(s)
#'
#' Accessor function for strategy configuration. Provides controlled access
#' to internal ETF_SCREENER_CONFIG object.
#'
#' @param key Optional configuration key to retrieve. If NULL, returns entire config.
#' @return Configuration value(s)
#' @export
#' @examples
#' \dontrun{
#'   # Get entire configuration
#'   config <- get_etf_screener_config()
#'
#'   # Get specific value
#'   strike_pct <- get_etf_screener_config("strike_threshold_pct")
#' }
get_etf_screener_config <- function(key = NULL) {
  if (is.null(key)) {
    return(ETF_SCREENER_CONFIG)
  }

  if (!key %in% names(ETF_SCREENER_CONFIG)) {
    stop(sprintf("Configuration key '%s' not found. Available keys: %s",
                key, paste(names(ETF_SCREENER_CONFIG), collapse = ", ")))
  }

  ETF_SCREENER_CONFIG[[key]]
}
