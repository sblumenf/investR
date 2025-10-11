#' Weekly Dividend Capture Strategy Configuration
#'
#' Configuration constants and ETF list for the weekly dividend capture strategy
#'
#' @name dividend-capture-weekly-config
#' @importFrom tibble tribble
NULL

#' Weekly Dividend ETFs List
#'
#' List of 32 weekly dividend ETFs for analysis
#'
#' @format A tibble with 32 rows and 2 columns:
#' \describe{
#'   \item{ticker}{ETF ticker symbol}
#'   \item{description}{ETF description/name}
#' }
#' @export
WEEKLY_ETFS <- tribble(
  ~ticker, ~description,
  "AAPW", "Apple WeeklyPay",
  "AMDW", "AMD WeeklyPay",
  "AMZW", "Amazon WeeklyPay",
  "AVGW", "Broadcom WeeklyPay",
  "BRKW", "Berkshire WeeklyPay",
  "CHPY", "WeeklyPay ETF",
  "COIW", "Coinbase WeeklyPay",
  "GOOW", "Google WeeklyPay",
  "GPTY", "WeeklyPay ETF",
  "HOOW", "Robinhood WeeklyPay",
  "IWMY", "WeeklyPay ETF",
  "LFGY", "WeeklyPay ETF",
  "MAGY", "Magnificent 7 WeeklyPay",
  "METW", "Meta WeeklyPay",
  "MSFW", "Microsoft WeeklyPay",
  "MSTW", "MicroStrategy WeeklyPay",
  "NFLW", "Netflix WeeklyPay",
  "NVDW", "NVIDIA WeeklyPay",
  "PLTW", "Palantir WeeklyPay",
  "QDTE", "Nasdaq-100 Weekly",
  "QDTY", "Nasdaq-100 Weekly",
  "QQQY", "Nasdaq-100 Weekly",
  "RDTE", "Russell 2000 Weekly",
  "RDTY", "Russell 2000 Weekly",
  "SDTY", "S&P 500 Weekly",
  "TSLW", "Tesla WeeklyPay",
  "ULTY", "Ultra Treasury Weekly",
  "WDTE", "S&P 500 Weekly",
  "WEEK", "Weekly T-Bill",
  "XDTE", "S&P 500 Weekly",
  "YMAG", "YieldMax Magnificent 7",
  "YMAX", "YieldMax Universe"
)

#' Weekly Dividend Capture Strategy Configuration
#'
#' Internal configuration object for dividend capture strategy
#'
#' NOTE: Many constants are now in inst/golem-config.yml for centralized management.
#' This CONFIG object contains only weekly-specific parameters and provides
#' a convenient interface for accessing both local and golem config values.
#'
#' @format A list with strategy parameters
#' @noRd
NULL

DIVIDEND_CAPTURE_CONFIG <- list(
  # Time Constants (from golem-config.yml)
  trading_days_per_year = get_golem_config_value("shared", "trading_days_per_year", 252),
  weeks_per_year = get_golem_config_value("weekly_capture", "weeks_per_year", 52),
  days_per_year = get_golem_config_value("shared", "days_per_year", 365),

  # Investment Parameters (from golem-config.yml)
  investment_amount = get_golem_config_value("weekly_capture", "investment_amount", 10000),

  # Data Validation (from golem-config.yml)
  min_dividend_events = get_golem_config_value("weekly_capture", "min_dividend_events", 5),

  # Rate Limiting (from golem-config.yml)
  rate_limit_seconds = get_golem_config_value("weekly_capture", "rate_limit_seconds", 2),

  # Parallel Processing (from golem-config.yml)
  max_workers = get_golem_config_value("weekly_capture", "max_workers", 4),
  min_workers = get_golem_config_value("shared", "min_workers", 1),
  max_workers_limit = get_golem_config_value("shared", "max_workers_limit", 10),

  # Day of Week Mapping (strategy-specific, kept local)
  day_map = list(
    Monday = "Friday",
    Tuesday = "Monday",
    Wednesday = "Tuesday",
    Thursday = "Wednesday",
    Friday = "Thursday"
  )
)

#' Get dividend capture configuration value(s)
#'
#' Accessor function for strategy configuration.
#'
#' @param key Optional configuration key to retrieve. If NULL, returns entire config.
#' @return Configuration value(s)
#' @export
#' @examples
#' \dontrun{
#'   # Get entire configuration
#'   config <- get_dividend_capture_config()
#'
#'   # Get specific value
#'   weeks_per_year <- get_dividend_capture_config("weeks_per_year")
#' }
get_dividend_capture_config <- function(key = NULL) {
  if (is.null(key)) {
    return(DIVIDEND_CAPTURE_CONFIG)
  }

  if (!key %in% names(DIVIDEND_CAPTURE_CONFIG)) {
    stop(sprintf(
      "Configuration key '%s' not found. Available keys: %s",
      key,
      paste(names(DIVIDEND_CAPTURE_CONFIG), collapse = ", ")
    ))
  }

  DIVIDEND_CAPTURE_CONFIG[[key]]
}
