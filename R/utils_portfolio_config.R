#' Portfolio Configuration Utilities
#'
#' Helper functions for accessing portfolio management configuration
#' from golem-config.yml
#'
#' @name portfolio-config
#' @importFrom config get
NULL

#' Get portfolio configuration value
#'
#' Retrieves a specific configuration value from the portfolio section
#' of golem-config.yml
#'
#' @param key Configuration key to retrieve
#' @param default Default value if key not found
#' @return Configuration value
#' @noRd
get_portfolio_config <- function(key, default = NULL) {
  tryCatch({
    config <- get_golem_config("portfolio")
    if (key %in% names(config)) {
      return(config[[key]])
    } else {
      return(default)
    }
  }, error = function(e) {
    return(default)
  })
}

#' Get database file path
#'
#' Returns the full path to the portfolio DuckDB database file
#'
#' @return Character path to database file
#' @noRd
get_portfolio_db_path <- function() {
  db_filename <- get_portfolio_config("database_path", "portfolio.sqlite")
  db_dir <- app_sys("database")

  # Ensure directory exists
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  }

  file.path(db_dir, db_filename)
}

#' Get stale threshold in minutes
#'
#' Returns the number of minutes after which position data is considered stale
#'
#' @return Numeric minutes threshold
#' @noRd
get_stale_threshold_minutes <- function() {
  get_portfolio_config("stale_threshold_minutes", 15)
}

#' Get default visible columns
#'
#' Returns the list of column names to display by default in the positions table
#'
#' @return Character vector of column names
#' @noRd
get_default_visible_columns <- function() {
  get_portfolio_config(
    "default_visible_columns",
    c("symbol", "open_quantity", "current_price", "average_entry_price",
      "current_market_value", "total_cost")
  )
}

#' Check if position data is stale
#'
#' Compares a timestamp against the stale threshold
#'
#' @param timestamp POSIXct timestamp to check
#' @return Logical TRUE if data is stale, FALSE if fresh
#' @noRd
is_data_stale <- function(timestamp) {
  if (is.null(timestamp) || is.na(timestamp)) {
    return(TRUE)
  }

  threshold_minutes <- get_stale_threshold_minutes()
  age_minutes <- as.numeric(difftime(Sys.time(), timestamp, units = "mins"))

  age_minutes >= threshold_minutes
}
