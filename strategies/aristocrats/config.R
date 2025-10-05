################################################################################
# Configuration Constants
#
# Central configuration file for the covered call aristocrat strategy
################################################################################

# Strategy Parameters
CONFIG <- list(
  # Option Selection
  strike_threshold_pct = 0.8,  # Maximum strike as % of current price (80%)
  target_days = NULL,           # Target days to expiry (NULL = longest available)

  # Parallel Processing
  max_workers = 10,             # Number of parallel workers

  # Data Validation
  min_aristocrats = 50,         # Minimum expected aristocrats count
  min_dividend_quarters = 4,    # Minimum quarters for yield calculation
  min_option_bid = 0.01,        # Minimum option bid price
  min_open_interest = 0,        # Minimum open interest filter

  # Financial Constants
  sgov_yield_default = 0.05,    # Default SGOV yield (5%) if fetch fails
  shares_per_contract = 100,    # Standard options contract size
  days_per_year = 365,          # Days for annualization

  # Date Ranges
  history_years = 5,            # Years of price history for max drawdown

  # Thresholds and Limits
  short_expiry_warning_days = 14,  # Warn if expiry < 14 days
  negative_return_threshold = 0,    # Filter out returns <= 0

  # Output
  default_top_n = 10,           # Number of top results to display
  output_dir = "strategies",    # Directory for CSV output

  # Web Scraping
  urls = list(
    stockanalysis = "https://stockanalysis.com/list/dividend-aristocrats/",
    wikipedia = "https://en.wikipedia.org/wiki/S%26P_500_Dividend_Aristocrats"
  ),

  # Column Name Variations (for parsing web data)
  ticker_column_names = c("Ticker symbol", "Symbol", "Ticker")
)

# Validation: Ensure config values are valid
validate_config <- function(config = CONFIG) {
  if (config$strike_threshold_pct <= 0 || config$strike_threshold_pct > 1) {
    stop("strike_threshold_pct must be between 0 and 1")
  }
  if (config$max_workers <= 0) {
    stop("max_workers must be positive")
  }
  if (config$min_aristocrats <= 0) {
    stop("min_aristocrats must be positive")
  }
  if (config$history_years <= 0) {
    stop("history_years must be positive")
  }
  invisible(TRUE)
}

# Run validation on load
validate_config()