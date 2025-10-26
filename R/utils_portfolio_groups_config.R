#' Portfolio Groups Configuration Utilities
#'
#' Helper functions for accessing position groups configuration
#' from golem-config.yml
#'
#' @name portfolio-groups-config
#' @importFrom config get
NULL

#' Get portfolio groups configuration value
#'
#' Retrieves a specific configuration value from the portfolio.groups section
#' of golem-config.yml
#'
#' @param key Configuration key to retrieve
#' @param default Default value if key not found
#' @return Configuration value
#' @noRd
get_groups_config <- function(key, default = NULL) {
  tryCatch({
    portfolio_config <- get_golem_config("portfolio")
    if ("groups" %in% names(portfolio_config)) {
      groups_config <- portfolio_config$groups
      if (key %in% names(groups_config)) {
        return(groups_config[[key]])
      }
    }
    return(default)
  }, error = function(e) {
    return(default)
  })
}

#' Get available strategy types
#'
#' Returns the list of defined strategy types for position grouping
#'
#' @return Character vector of strategy names
#' @noRd
get_strategy_types <- function() {
  get_groups_config(
    "strategy_types",
    c("Dividend Aristocrats", "Zero-Dividend Stocks", "Dynamic Covered Calls",
      "Legacy Covered Call", "Collar Strategy", "Weekly Dividend Capture",
      "Monthly Dividend Capture", "Russell 2000 High-Yield", "Other")
  )
}

#' Get available position roles
#'
#' Returns the list of valid position roles within a group
#'
#' @return Character vector of role names
#' @noRd
get_position_roles <- function() {
  get_groups_config(
    "position_roles",
    c("underlying_stock", "short_call")
  )
}

#' Get match confidence threshold
#'
#' Returns the minimum confidence score for suggesting position matches
#'
#' @return Numeric threshold (0-1)
#' @noRd
get_match_threshold <- function() {
  get_groups_config("match_confidence_threshold", 0.7)
}
