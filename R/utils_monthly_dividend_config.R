#' Monthly Dividend Stocks Strategy Configuration
#'
#' Configuration constants for the monthly dividend stocks covered call strategy
#'
#' @name monthly-dividend-config
#' @importFrom logger log_info log_warn
NULL

MONTHLY_DIVIDEND_CONFIG <- list(
  # Option Selection (from golem-config.yml)
  strike_threshold_pct = get_golem_config_value("monthly_dividend_stocks", "strike_threshold_pct", 0.8),
  target_days = get_golem_config_value("monthly_dividend_stocks", "target_days", NULL),

  # Parallel Processing (from golem-config.yml)
  max_workers = get_golem_config_value("monthly_dividend_stocks", "max_workers", 10),

  # Data Validation (from golem-config.yml)
  min_option_bid = get_golem_config_value("monthly_dividend_stocks", "min_option_bid", 0.01),
  min_open_interest = get_golem_config_value("monthly_dividend_stocks", "min_open_interest", 0),

  # Financial Constants (from golem-config.yml)
  sgov_yield_default = get_golem_config_value("monthly_dividend_stocks", "sgov_yield_default", 0.05),
  shares_per_contract = get_golem_config_value("monthly_dividend_stocks", "shares_per_contract", 100),
  days_per_year = get_golem_config_value("shared", "days_per_year", 365),

  # Date Ranges (from golem-config.yml)
  history_years = get_golem_config_value("monthly_dividend_stocks", "history_years", 5),

  # Thresholds and Limits (from golem-config.yml)
  short_expiry_warning_days = get_golem_config_value("monthly_dividend_stocks", "short_expiry_warning_days", 14),
  negative_return_threshold = get_golem_config_value("monthly_dividend_stocks", "negative_return_threshold", 0),
  low_dividend_threshold = get_golem_config_value("monthly_dividend_stocks", "low_dividend_threshold", 0.5),
  error_truncate_length = get_golem_config_value("monthly_dividend_stocks", "error_truncate_length", 50),
  max_sgov_yield_sanity = get_golem_config_value("monthly_dividend_stocks", "max_sgov_yield_sanity", 0.20),

  # Output (from golem-config.yml)
  default_top_n = get_golem_config_value("monthly_dividend_stocks", "default_top_n", 10),
  output_dir = get_golem_config_value("monthly_dividend_stocks", "output_dir", "strategies")
)

#' Validate Monthly Dividend Stocks Configuration
#'
#' Ensures config values are valid
#'
#' @return invisibly TRUE
#' @export
validate_monthly_dividend_config <- function() {
  if (MONTHLY_DIVIDEND_CONFIG$strike_threshold_pct <= 0 || MONTHLY_DIVIDEND_CONFIG$strike_threshold_pct >= 1) {
    log_warn("strike_threshold_pct must be between 0 and 1 (exclusive)")
  }
  if (MONTHLY_DIVIDEND_CONFIG$max_workers < 1) {
    log_warn("max_workers must be >= 1")
  }
  if (MONTHLY_DIVIDEND_CONFIG$history_years < 1) {
    log_warn("history_years must be >= 1")
  }
  log_info("Monthly dividend stocks config validated successfully")
  invisible(TRUE)
}
