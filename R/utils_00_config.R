#' Configuration Utilities
#'
#' Generic utilities for accessing golem configuration values
#'
#' @name config-utils
NULL

#' Get Configuration Value from Golem Config
#'
#' Generic function to retrieve values from golem-config.yml with fallback support.
#' Replaces strategy-specific config getters to follow DRY principle.
#'
#' @param section Section name in config (e.g., "shared", "aristocrats", "zero_dividend")
#' @param key Key name within section
#' @param fallback Fallback value if not found or error occurs
#' @return Config value or fallback
#' @noRd
#'
#' @examples
#' \dontrun{
#'   # Get a value with fallback
#'   max_workers <- get_golem_config_value("aristocrats", "max_workers", 10)
#'
#'   # Get shared value
#'   days_per_year <- get_golem_config_value("shared", "days_per_year", 365)
#' }
get_golem_config_value <- function(section, key, fallback = NULL) {
  tryCatch({
    golem_config <- golem::get_golem_config()
    value <- golem_config[[section]][[key]]
    if (is.null(value)) fallback else value
  }, error = function(e) {
    fallback
  })
}
