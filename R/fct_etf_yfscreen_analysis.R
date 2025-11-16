#' ETF yfscreen Covered Calls Analysis Functions
#'
#' Core business logic for screening ETFs using yfscreen and analyzing them
#' for covered call opportunities.
#'
#' @name etf-yfscreen-analysis
#' @importFrom logger log_info log_warn
NULL

#' Analyze ETFs from yfscreen for covered call opportunities
#'
#' Uses yfscreen package to filter ETFs by expense ratio, dividend yield, and
#' net assets, then analyzes the filtered ETFs for deep ITM covered call
#' opportunities. Provides dynamic, real-time ETF screening based on current
#' market data.
#'
#' Key differences from static ETF lists:
#' - Dynamic filtering based on real-time ETF characteristics
#' - Customizable screening criteria (expense ratio, yield, assets)
#' - Automatically includes new ETFs that meet criteria
#' - Excludes ETFs that no longer meet filtering thresholds
#'
#' @param dividend_yield_min Minimum dividend yield (default NULL)
#' @param dividend_yield_max Maximum dividend yield (default NULL)
#' @param dividend_filter Dividend filter type: "all", "dividend_paying", or "zero_dividend" (default "all")
#' @param market_cap_min Minimum market cap in dollars (default 1 billion)
#' @param market_cap_max Maximum market cap in dollars (default 100 billion)
#' @param top_n Maximum number of ETFs to analyze, sorted by volume (default 50)
#' @param strike_threshold_pct Maximum strike as % of current price (default 0.85)
#' @param min_days Minimum days to expiry (default 45)
#' @param max_days Maximum days to expiry (default 120)
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers for processing
#' @return Tibble with all opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze ETFs with default screening parameters
#'   results <- analyze_etf_covered_calls_yfscreen()
#'
#'   # Analyze with custom screening criteria
#'   results <- analyze_etf_covered_calls_yfscreen(
#'     dividend_yield_min = 3,
#'     dividend_yield_max = 8,
#'     dividend_filter = "dividend_paying",
#'     market_cap_min = 10e9,
#'     market_cap_max = 50e9,
#'     strike_threshold_pct = 0.90,
#'     min_days = 60,
#'     max_days = 90
#'   )
#'
#'   # Zero-dividend ETF screening
#'   results <- analyze_etf_covered_calls_yfscreen(
#'     dividend_filter = "zero_dividend",
#'     market_cap_min = 5e9,
#'     market_cap_max = 100e9,
#'     top_n = 30
#'   )
#' }
analyze_etf_covered_calls_yfscreen <- function(dividend_yield_min = NULL,
                                               dividend_yield_max = NULL,
                                               dividend_filter = "all",
                                               market_cap_min = 1e9,
                                               market_cap_max = 100e9,
                                               top_n = 50,
                                               strike_threshold_pct = ETF_SCREENER_CONFIG$strike_threshold_pct,
                                               min_days = ETF_SCREENER_CONFIG$min_days,
                                               max_days = ETF_SCREENER_CONFIG$max_days,
                                               expiry_month = NULL,
                                               max_workers = ETF_SCREENER_CONFIG$max_workers) {

  # Fetch ETF tickers using yfscreen (with error handling and logging)
  log_info("Fetching ETFs using yfscreen with specified criteria...")
  etf_tickers <- fetch_yfscreen_etfs(
    dividend_yield_min = dividend_yield_min,
    dividend_yield_max = dividend_yield_max,
    dividend_filter = dividend_filter,
    market_cap_min = market_cap_min,
    market_cap_max = market_cap_max,
    top_n = top_n
  )

  # Validate we have ETFs to analyze
  if (length(etf_tickers) == 0) {
    log_warn("No ETFs found matching yfscreen criteria")
    return(tibble())
  }

  log_info("Found {length(etf_tickers)} ETFs from yfscreen to analyze")

  # Call generic covered call analyzer
  analyze_covered_calls_generic(
    stock_universe = etf_tickers,
    strategy_name = "ETF Screener - yfscreen Filtered",
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    max_workers = max_workers,
    result_flags = list(
      is_zero_dividend = FALSE,
      is_aristocrat = FALSE,
      is_etf = TRUE,
      is_yfscreen = TRUE
    )
  )
}
