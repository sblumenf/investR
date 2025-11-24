#' ETF Cash-Secured Puts Analysis (yfscreen Filtered)
#'
#' Analyzes ETFs from Yahoo Finance screener for cash-secured put opportunities.
#' Uses yfscreen package to filter ETFs by market cap, dividend characteristics,
#' and trading volume before analyzing put options.
#'
#' Key differences from aristocrats variant:
#' - Dynamic ETF filtering based on real-time criteria
#' - Customizable dividend and market cap filters
#' - Volume-sorted selection for optimal liquidity
#' - Works with any ETF universe (dividend-paying, zero-dividend, or all)
#'
#' @param dividend_yield_min Minimum dividend yield (e.g., 2 for 2%)
#' @param dividend_yield_max Maximum dividend yield (e.g., 6 for 6%)
#' @param dividend_filter Dividend filter type: "all", "dividend_paying", or "zero_dividend"
#' @param market_cap_min Minimum fund net assets in dollars (default 1e9 = $1B)
#' @param market_cap_max Maximum fund net assets in dollars (default 100e9 = $100B)
#' @param top_n Maximum number of ETFs to analyze, sorted by volume (default 50)
#' @param strike_threshold_pct Minimum strike as % of current price (default from config)
#' @param min_days Minimum days to expiry (default from config)
#' @param max_days Maximum days to expiry (default from config)
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers (default from config)
#' @return Tibble with cash-secured put opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze default set of ETFs
#'   results <- analyze_etf_cash_secured_puts_yfscreen()
#'
#'   # Analyze large-cap dividend-paying ETFs
#'   results <- analyze_etf_cash_secured_puts_yfscreen(
#'     dividend_filter = "dividend_paying",
#'     dividend_yield_min = 2,
#'     dividend_yield_max = 6,
#'     market_cap_min = 10e9,
#'     top_n = 30
#'   )
#'
#'   # Analyze zero-dividend ETFs for maximum capital gains focus
#'   results <- analyze_etf_cash_secured_puts_yfscreen(
#'     dividend_filter = "zero_dividend",
#'     market_cap_min = 5e9,
#'     strike_threshold_pct = 0.90,
#'     top_n = 25
#'   )
#' }
analyze_etf_cash_secured_puts_yfscreen <- function(
  dividend_yield_min = NULL,
  dividend_yield_max = NULL,
  dividend_filter = "all",
  market_cap_min = 1e9,
  market_cap_max = 100e9,
  top_n = 50,
  strike_threshold_pct = CASH_SECURED_PUTS_CONFIG$strike_threshold_pct,
  min_days = CASH_SECURED_PUTS_CONFIG$min_days,
  max_days = CASH_SECURED_PUTS_CONFIG$max_days,
  expiry_month = NULL,
  max_workers = CASH_SECURED_PUTS_CONFIG$max_workers
) {
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

  # Call generic put analyzer
  analyze_puts_generic(
    stock_universe = etf_tickers,
    strategy_name = "ETF Screener - yfscreen Filtered Cash-Secured Puts",
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    max_workers = max_workers,
    result_flags = list(
      is_put = TRUE,
      is_aristocrat = FALSE,
      is_etf = TRUE,
      is_yfscreen = TRUE
    )
  )
}
