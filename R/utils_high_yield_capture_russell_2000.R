#' Russell 2000 High-Yield Capture Configuration
#'
#' Universe-specific configuration for high-yield dividend capture strategy
#' applied to Russell 2000 small-cap stocks.
#'
#' @name high-yield-capture-russell-2000-config
NULL

#' Get value from golem config with fallback
#'
#' @param section Section name (e.g., "shared", "high_yield_capture")
#' @param key Key name within section
#' @param fallback Fallback value if not found
#' @return Config value or fallback
#' @noRd
get_golem_config_value_hy_r2000 <- function(section, key, fallback = NULL) {
  tryCatch({
    golem_config <- golem::get_golem_config()
    value <- golem_config[[section]][[key]]
    if (is.null(value)) fallback else value
  }, error = function(e) {
    fallback
  })
}

#' Get Russell 2000 high-yield capture configuration
#'
#' Returns configuration object for Russell 2000 universe including
#' universe metadata, ticker fetcher, and strategy defaults.
#'
#' @return List with configuration parameters
#' @export
#' @examples
#' \dontrun{
#'   config <- get_russell_2000_high_yield_config()
#'   tickers <- config$ticker_fetcher()
#' }
get_russell_2000_high_yield_config <- function() {
  list(
    # Universe Metadata
    universe_name = "Russell 2000",
    universe_key = "russell_2000",
    route = "dividend-capture-russell-2000",
    ticker_fetcher = get_russell_2000_stocks,

    # Time Constants
    trading_days_per_year = get_golem_config_value_hy_r2000("shared", "trading_days_per_year", 252),
    days_per_year = get_golem_config_value_hy_r2000("shared", "days_per_year", 365),

    # Investment Parameters
    investment_amount = get_golem_config_value_hy_r2000("high_yield_capture", "investment_amount", 10000),

    # Yield Filter
    default_yield_threshold = get_golem_config_value_hy_r2000("high_yield_capture", "default_yield_threshold", 10.0),
    min_yield_threshold = get_golem_config_value_hy_r2000("high_yield_capture", "min_yield_threshold", 5.0),
    max_yield_threshold = get_golem_config_value_hy_r2000("high_yield_capture", "max_yield_threshold", 20.0),

    # Data Validation
    min_dividend_events = get_golem_config_value_hy_r2000("high_yield_capture", "min_dividend_events", 5),

    # Quality Filters
    min_success_rate = get_golem_config_value_hy_r2000("high_yield_capture", "min_success_rate", 70.0),
    exclude_negative_returns = get_golem_config_value_hy_r2000("high_yield_capture", "exclude_negative_returns", TRUE),

    # Date Filtering (business days)
    max_days_until_ex_div = get_golem_config_value_hy_r2000("high_yield_capture", "max_days_until_ex_div", 2),

    # Parallel Processing
    max_workers = get_golem_config_value_hy_r2000("high_yield_capture", "max_workers", 4),
    min_workers = get_golem_config_value_hy_r2000("shared", "min_workers", 1),
    max_workers_limit = get_golem_config_value_hy_r2000("shared", "max_workers_limit", 10)
  )
}
