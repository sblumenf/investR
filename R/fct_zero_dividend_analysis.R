#' Zero-Dividend Strategy Analysis Functions
#'
#' Core business logic for analyzing zero-dividend S&P 500 stocks for
#' covered call opportunities. Focuses on growth stocks that reinvest
#' profits rather than paying dividends.
#'
#' @name zero-dividend-analysis
#' @importFrom logger log_info log_warn
NULL

#' Analyze zero-dividend stocks for covered call opportunities
#'
#' Analyzes S&P 500 stocks that don't pay dividends for deep ITM covered call
#' opportunities. Targets growth stocks like AMZN, GOOGL, META, TSLA, etc.
#' Uses shared generic analysis functions with zero-dividend stock universe.
#'
#' Key differences from dividend aristocrats strategy:
#' - Target: Growth stocks (zero dividend)
#' - Default strike: 85% (deeper ITM)
#' - No dividend income component
#' - Higher option premiums expected
#'
#' @param limit Optional limit on number of stocks to analyze (for testing)
#' @param strike_threshold_pct Maximum strike as % of current price (default 0.85)
#' @param min_days Minimum days to expiry (default 45)
#' @param max_days Maximum days to expiry (default 120)
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers for processing
#' @return Tibble with all opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze all zero-dividend stocks with default parameters
#'   results <- analyze_zero_dividend()
#'
#'   # Analyze with custom strike threshold and expiration range
#'   results <- analyze_zero_dividend(
#'     strike_threshold_pct = 0.90,
#'     min_days = 60,
#'     max_days = 90,
#'     expiry_month = 6
#'   )
#'
#'   # Test with limited number of stocks
#'   results <- analyze_zero_dividend(limit = 10)
#' }
analyze_zero_dividend <- function(limit = NULL,
                                  strike_threshold_pct = ZERO_DIVIDEND_CONFIG$strike_threshold_pct,
                                  min_days = ZERO_DIVIDEND_CONFIG$min_days,
                                  max_days = ZERO_DIVIDEND_CONFIG$max_days,
                                  expiry_month = NULL,
                                  max_workers = ZERO_DIVIDEND_CONFIG$max_workers) {

  # Get zero-dividend stock universe (cached with 30-day TTL)
  log_info("Fetching zero-dividend stock universe...")
  zero_div_stocks <- get_zero_dividend_stocks(limit)

  # Validate we have stocks to analyze
  if (length(zero_div_stocks) == 0) {
    stop("No zero-dividend stocks found in S&P 500. This may indicate an issue with data fetching.")
  }

  log_info("Found {length(zero_div_stocks)} zero-dividend stocks to analyze")

  # Apply limit if specified (for testing)
  if (!is.null(limit)) {
    log_info("Limiting analysis to first {limit} stocks (testing mode)")
  }

  # Call generic covered call analyzer
  analyze_covered_calls_generic(
    stock_universe = zero_div_stocks,
    strategy_name = "Zero-Dividend Stocks",
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    max_workers = max_workers,
    result_flags = list(
      is_zero_dividend = TRUE,
      is_aristocrat = FALSE
    )
  )
}
