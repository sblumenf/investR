#' Monthly Dividend Capture Strategy Configuration
#'
#' Configuration constants and ETF list for the monthly dividend capture strategy
#'
#' @name dividend-capture-monthly-config
#' @importFrom tibble tribble
NULL

#' Monthly Dividend ETFs List
#'
#' List of 76 monthly dividend ETFs for analysis
#'
#' @format A tibble with 76 rows and 3 columns:
#' \describe{
#'   \item{ticker}{ETF ticker symbol}
#'   \item{description}{ETF description/name}
#'   \item{schedule_type}{Dividend schedule pattern}
#' }
#' @export
MONTHLY_ETFS <- tribble(
  ~ticker, ~description, ~schedule_type,
  # US High-Yield Monthly ETFs (Beginning of Month)
  "MSTY", "YieldMax MSTR Option Income", "Beginning of Month",
  "ULTY", "YieldMax Ultra Option Income", "Beginning of Month",
  "NVDY", "YieldMax NVDA Option Income", "Beginning of Month",
  "TSLY", "YieldMax TSLA Option Income", "Beginning of Month",

  # US High-Yield Monthly ETFs (End of Month)
  "QYLD", "Global X Nasdaq-100 Covered Call", "End of Month",
  "XYLD", "Global X S&P 500 Covered Call", "End of Month",
  "JEPQ", "JPMorgan Nasdaq Equity Premium", "End of Month",
  "RYLD", "Global X Russell 2000 Covered Call", "End of Month",

  # US High-Yield Monthly ETFs (Mid-Month)
  "KBWD", "Invesco KBW High Dividend Yield", "Mid-Month",
  "DIV", "Global X SuperDividend", "Mid-Month",
  "SPYI", "NEOS S&P 500 High Income", "Mid-Month",
  "QQQI", "NEOS Nasdaq-100 High Income", "Mid-Month",

  # Mortgage REITs
  "AGNC", "AGNC Investment Corp", "REIT",
  "ARR", "ARMOUR Residential REIT", "REIT",
  "EFC", "Ellington Financial", "REIT",
  "ORC", "Orchid Island Capital", "REIT",

  # Business Development Companies
  "PSEC", "Prospect Capital Corp", "BDC",

  # Canadian Energy & Royalties
  "WCP.TO", "Whitecap Resources", "Canadian Energy",
  "FRU.TO", "Freehold Royalties", "Canadian Royalties",

  # Canadian REITs
  "AP-UN.TO", "Allied Properties REIT", "Canadian REIT",
  "SRU-UN.TO", "SmartCentres REIT", "Canadian REIT",
  "NXR-UN.TO", "Nexus Industrial REIT", "Canadian REIT",
  "REI-UN.TO", "RioCan REIT", "Canadian REIT",

  # Hamilton ETFs
  "HMAX.TO", "Hamilton Canadian Financials Covered Call", "Canadian Covered Call",
  "HDIV.TO", "Hamilton Multi-Sector Covered Call", "Canadian Covered Call",
  "HYLD.TO", "Hamilton US Covered Call", "Canadian Covered Call",
  "HCAL.TO", "Hamilton Canadian Banks Enhanced", "Canadian Covered Call",

  # Evolve ETFs
  "BANK.TO", "Evolve Banks & Lifecos Enhanced Yield", "Canadian Covered Call",
  "CANY.TO", "Evolve Canadian Equity UltraYield", "Canadian Covered Call",
  "CALL.TO", "Evolve US Banks Enhanced Yield", "Canadian Covered Call",
  "QQQY.TO", "Evolve NASDAQ Tech Enhanced Yield", "Canadian Covered Call",

  # BMO ETFs
  "ZWC.TO", "BMO Canadian High Div Covered Call", "Canadian Covered Call",
  "ZWB.TO", "BMO Canadian Banks Covered Call", "Canadian Covered Call",
  "ZWU.TO", "BMO Utilities Covered Call", "Canadian Covered Call",

  # Harvest ETFs
  "HDIF.TO", "Harvest Diversified Income (Leveraged)", "Canadian Covered Call",
  "HRIF.TO", "Harvest Diversified Equity Income", "Canadian Covered Call",
  "HHIS.TO", "Harvest High Income Shares", "Canadian Covered Call",
  "HUBL.TO", "Harvest US Bank Leaders Income", "Canadian Covered Call",
  "HPYT.TO", "Harvest Premium Yield Treasury", "Canadian Covered Call",

  # Global X ETFs (Canada)
  "BKCL.TO", "Global X Enhanced Canadian Banks Covered Call", "Canadian Covered Call",
  "QQCL.TO", "Global X Enhanced NASDAQ-100 Covered Call", "Canadian Covered Call",
  "CNCL.TO", "Global X Enhanced S&P/TSX 60 Covered Call", "Canadian Covered Call",
  "EQCL.TO", "Global X Enhanced All-Equity Covered Call", "Canadian Covered Call",
  "USCC.TO", "Global X S&P 500 Covered Call CAD", "Canadian Covered Call",

  # CI Financial ETFs
  "TXF.TO", "CI Tech Giants Covered Call", "Canadian Covered Call",
  "FHI.TO", "CI Health Care Giants Covered Call", "Canadian Covered Call",
  "FLI.TO", "CI US & Canada Lifeco Covered Call", "Canadian Covered Call",

  # First Trust ETFs (Canada)
  "FST.TO", "First Trust Canadian Capital Strength", "Canadian Covered Call",
  "FUD.TO", "First Trust Value Line Dividend CAD-Hedged", "Canadian Covered Call",

  # RBC ETFs
  "RCDC.TO", "RBC Canadian Dividend Covered Call", "Canadian Covered Call",
  "RUDC.TO", "RBC US Dividend Covered Call", "Canadian Covered Call"
)

#' Monthly Dividend Capture Strategy Configuration
#'
#' Internal configuration object for monthly dividend capture strategy
#'
#' NOTE: Many constants are now in inst/golem-config.yml for centralized management.
#' This CONFIG object contains only monthly-specific parameters and provides
#' a convenient interface for accessing both local and golem config values.
#'
#' @format A list with strategy parameters
#' @noRd
NULL

DIVIDEND_CAPTURE_MONTHLY_CONFIG <- list(
  # Time Constants (from golem-config.yml)
  trading_days_per_year = get_golem_config_value("shared", "trading_days_per_year", 252),
  months_per_year = get_golem_config_value("monthly_capture", "months_per_year", 12),
  days_per_year = get_golem_config_value("shared", "days_per_year", 365),

  # Investment Parameters (from golem-config.yml)
  investment_amount = get_golem_config_value("monthly_capture", "investment_amount", 10000),

  # Data Validation (from golem-config.yml)
  min_dividend_events = get_golem_config_value("monthly_capture", "min_dividend_events", 5),

  # Quality Filters (from golem-config.yml)
  min_success_rate = get_golem_config_value("monthly_capture", "min_success_rate", 70.0),
  exclude_negative_returns = get_golem_config_value("monthly_capture", "exclude_negative_returns", TRUE),

  # Rate Limiting (from golem-config.yml)
  rate_limit_seconds = get_golem_config_value("monthly_capture", "rate_limit_seconds", 2),

  # Parallel Processing (from golem-config.yml)
  max_workers = get_golem_config_value("monthly_capture", "max_workers", 10),
  min_workers = get_golem_config_value("shared", "min_workers", 1),
  max_workers_limit = get_golem_config_value("shared", "max_workers_limit", 10),

  # Schedule Type Groupings (strategy-specific, kept local)
  schedule_types = c(
    "Beginning of Month",
    "End of Month",
    "Mid-Month",
    "REIT",
    "BDC",
    "Canadian Energy",
    "Canadian Royalties",
    "Canadian REIT",
    "Canadian Covered Call"
  ),

  # Days Since Last Dividend Filter Ranges (strategy-specific, kept local)
  days_since_dividend_ranges = c(
    "0-7 days ago",
    "8-14 days ago",
    "15-21 days ago",
    "22-30 days ago",
    "31+ days ago"
  )
)

#' Get monthly dividend capture configuration value(s)
#'
#' Accessor function for strategy configuration.
#'
#' @param key Optional configuration key to retrieve. If NULL, returns entire config.
#' @return Configuration value(s)
#' @export
#' @examples
#' \dontrun{
#'   # Get entire configuration
#'   config <- get_dividend_capture_monthly_config()
#'
#'   # Get specific value
#'   months_per_year <- get_dividend_capture_monthly_config("months_per_year")
#' }
get_dividend_capture_monthly_config <- function(key = NULL) {
  if (is.null(key)) {
    return(DIVIDEND_CAPTURE_MONTHLY_CONFIG)
  }

  if (!key %in% names(DIVIDEND_CAPTURE_MONTHLY_CONFIG)) {
    stop(sprintf(
      "Configuration key '%s' not found. Available keys: %s",
      key,
      paste(names(DIVIDEND_CAPTURE_MONTHLY_CONFIG), collapse = ", ")
    ))
  }

  DIVIDEND_CAPTURE_MONTHLY_CONFIG[[key]]
}
