#' Cash-Secured Puts Strategy Configuration
#'
#' Configuration constants and validation functions for cash-secured puts strategy
#'
#' @name cash-secured-puts-config
NULL

#' Cash-Secured Puts Configuration Constants
#'
#' Configuration object containing all strategy-specific parameters for
#' cash-secured put analysis.
#'
#' @format List with configuration parameters:
#' \describe{
#'   \item{strike_threshold_pct}{Minimum strike as % of current price (default 0.95)}
#'   \item{min_days}{Minimum days to expiration (default 45)}
#'   \item{max_days}{Maximum days to expiration (default 120)}
#'   \item{max_workers}{Number of parallel workers (default 10)}
#'   \item{min_option_bid}{Minimum option bid price filter (default 0.01)}
#'   \item{min_open_interest}{Minimum open interest for liquidity (default 10)}
#'   \item{max_stock_price}{Maximum stock price filter (default 250)}
#'   \item{shares_per_contract}{Shares per option contract (default 100)}
#'   \item{days_per_year}{Days per year for annualization (default 365)}
#'   \item{history_years}{Years of historical data (default 5)}
#'   \item{short_expiry_warning_days}{Threshold for short expiry warning (default 14)}
#'   \item{negative_return_threshold}{Filter threshold for negative returns (default 0)}
#'   \item{default_top_n}{Default number of results to display (default 10)}
#'   \item{output_dir}{Output directory for results (default "strategies")}
#' }
#' @export
CASH_SECURED_PUTS_CONFIG <- list(
  # Option Selection
  strike_threshold_pct = 0.85,  # 85% of current price (15% OTM for conservative puts)
  min_days = 45,                 # Minimum days to expiration
  max_days = 120,                # Maximum days to expiration

  # Parallel Processing
  max_workers = 10,              # Parallel processing workers

  # Data Validation
  min_option_bid = 0.01,         # Filter out low-premium options
  min_open_interest = 10,        # Liquidity filter
  max_stock_price = 300,         # Skip very expensive stocks

  # Financial Constants
  shares_per_contract = 100,     # Standard option contract size
  days_per_year = 365,           # Days per year for annualization

  # Date Ranges
  history_years = 5,             # Historical data lookback

  # Thresholds
  short_expiry_warning_days = 14, # Warn on short expirations
  negative_return_threshold = 0,  # Filter negative returns

  # Output
  default_top_n = 10,            # Default results display
  output_dir = "strategies"      # Output directory
)

#' Validate Cash-Secured Puts Configuration
#'
#' Validates that configuration parameters are within acceptable ranges
#'
#' @param config List of configuration parameters (defaults to CASH_SECURED_PUTS_CONFIG)
#' @return TRUE if valid, throws error otherwise
#' @export
#' @examples
#' \dontrun{
#'   validate_puts_config(CASH_SECURED_PUTS_CONFIG)
#' }
validate_puts_config <- function(config = CASH_SECURED_PUTS_CONFIG) {
  # Strike threshold validation
  if (!is.numeric(config$strike_threshold_pct) ||
      config$strike_threshold_pct < 0.5 ||
      config$strike_threshold_pct > 1.0) {
    stop("strike_threshold_pct must be between 0.5 and 1.0")
  }

  # Days validation
  if (!is.numeric(config$min_days) || config$min_days < 0) {
    stop("min_days must be a positive number")
  }

  if (!is.numeric(config$max_days) || config$max_days < config$min_days) {
    stop("max_days must be >= min_days")
  }

  # Worker validation
  if (!is.numeric(config$max_workers) ||
      config$max_workers < 1 ||
      config$max_workers > 50) {
    stop("max_workers must be between 1 and 50")
  }

  # Data validation thresholds
  if (!is.numeric(config$min_option_bid) || config$min_option_bid < 0) {
    stop("min_option_bid must be non-negative")
  }

  if (!is.numeric(config$min_open_interest) || config$min_open_interest < 0) {
    stop("min_open_interest must be non-negative")
  }

  if (!is.numeric(config$max_stock_price) || config$max_stock_price <= 0) {
    stop("max_stock_price must be positive")
  }

  # Financial constants validation
  if (!is.numeric(config$shares_per_contract) || config$shares_per_contract != 100) {
    stop("shares_per_contract must be 100")
  }

  if (!is.numeric(config$days_per_year) || config$days_per_year <= 0) {
    stop("days_per_year must be positive")
  }

  TRUE
}

#' Get Cash-Secured Puts Configuration
#'
#' Accessor function for configuration with optional overrides
#'
#' @param ... Named arguments to override default config values
#' @return List with configuration parameters
#' @export
#' @examples
#' \dontrun{
#'   # Get default config
#'   config <- get_puts_config()
#'
#'   # Get config with overrides
#'   config <- get_puts_config(strike_threshold_pct = 0.90, max_workers = 4)
#' }
get_puts_config <- function(...) {
  config <- CASH_SECURED_PUTS_CONFIG
  overrides <- list(...)

  if (length(overrides) > 0) {
    for (name in names(overrides)) {
      if (name %in% names(config)) {
        config[[name]] <- overrides[[name]]
      } else {
        warning(sprintf("Unknown config parameter: %s", name))
      }
    }

    # Validate modified config
    validate_puts_config(config)
  }

  config
}
