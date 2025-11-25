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
  zero_div_stocks <- get_zero_dividend_stocks(limit, max_workers)

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

#' Analyze custom ticker list for zero-dividend covered call opportunities
#'
#' Analyzes custom ticker lists (overbought, oversold, most shorted, leveraged ETFs)
#' for deep ITM covered call opportunities using zero-dividend strategy parameters.
#'
#' @param list_type Type of custom list: "overbought", "oversold", "most_shorted", "leveraged_2x", or "leveraged_3x"
#' @param strike_threshold_pct Maximum strike as % of current price (default 0.85)
#' @param min_days Minimum days to expiry (default 45)
#' @param max_days Maximum days to expiry (default 120)
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers for processing
#' @return Tibble with all opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze overbought stocks
#'   results <- analyze_zero_dividend_custom_list("overbought")
#'
#'   # Analyze 2x leveraged ETFs with custom parameters
#'   results <- analyze_zero_dividend_custom_list(
#'     "leveraged_2x",
#'     strike_threshold_pct = 0.90,
#'     min_days = 60,
#'     max_days = 90
#'   )
#' }
analyze_zero_dividend_custom_list <- function(list_type,
                                              strike_threshold_pct = ZERO_DIVIDEND_CONFIG$strike_threshold_pct,
                                              min_days = ZERO_DIVIDEND_CONFIG$min_days,
                                              max_days = ZERO_DIVIDEND_CONFIG$max_days,
                                              expiry_month = NULL,
                                              max_workers = ZERO_DIVIDEND_CONFIG$max_workers) {

  # Get display name for logging
  list_name <- switch(list_type,
    "overbought" = "Overbought Stocks",
    "oversold" = "Oversold Stocks",
    "most_shorted" = "Most Shorted Stocks",
    "leveraged_2x" = "2x Leveraged ETFs",
    "leveraged_3x" = "3x Leveraged ETFs",
    stop("Invalid list_type. Must be 'overbought', 'oversold', 'most_shorted', 'leveraged_2x', or 'leveraged_3x'")
  )

  # Fetch ticker list (web scraping with caching)
  log_info("Fetching {list_name} ticker list...")
  ticker_list <- switch(list_type,
    "overbought" = fetch_overbought_tickers(),
    "oversold" = fetch_oversold_tickers(),
    "most_shorted" = fetch_most_shorted_tickers(),
    "leveraged_2x" = fetch_2x_leveraged_etfs(),
    "leveraged_3x" = fetch_3x_leveraged_etfs()
  )

  # Validate we have tickers to analyze
  if (length(ticker_list) == 0) {
    log_warn("No tickers found for {list_name}")
    return(tibble())
  }

  log_info("Found {length(ticker_list)} tickers in {list_name}")

  # Call generic covered call analyzer
  analyze_covered_calls_generic(
    stock_universe = ticker_list,
    strategy_name = paste("Zero-Dividend -", list_name),
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    max_workers = max_workers,
    result_flags = list(
      is_zero_dividend = TRUE,
      is_aristocrat = FALSE,
      custom_list = list_type
    )
  )
}

#' Analyze liquid ETFs for zero-dividend covered call opportunities
#'
#' Analyzes highly liquid ETFs for deep ITM covered call opportunities using
#' zero-dividend strategy parameters. Uses the same curated ETF list as the
#' collar strategy (50+ liquid ETFs across index, sector, bond, commodity, etc.).
#'
#' Note: Many liquid ETFs pay dividends (e.g., SPY, VOO, VYM, SCHD). This is
#' acceptable as the strategy name refers to the deep ITM covered call approach,
#' not the dividend status of the underlying.
#'
#' @param min_market_cap Minimum market capitalization (default from COLLAR_CONFIG)
#' @param min_avg_volume Minimum average daily volume (default from COLLAR_CONFIG)
#' @param strike_threshold_pct Maximum strike as % of current price (default 0.85)
#' @param min_days Minimum days to expiry (default 45)
#' @param max_days Maximum days to expiry (default 120)
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers for processing
#' @return Tibble with all opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze liquid ETFs with default parameters
#'   results <- analyze_zero_dividend_etfs()
#'
#'   # Analyze with custom parameters
#'   results <- analyze_zero_dividend_etfs(
#'     strike_threshold_pct = 0.90,
#'     min_days = 60,
#'     max_days = 90
#'   )
#' }
analyze_zero_dividend_etfs <- function(min_market_cap = COLLAR_CONFIG$min_market_cap,
                                       min_avg_volume = COLLAR_CONFIG$min_avg_volume,
                                       strike_threshold_pct = ZERO_DIVIDEND_CONFIG$strike_threshold_pct,
                                       min_days = ZERO_DIVIDEND_CONFIG$min_days,
                                       max_days = ZERO_DIVIDEND_CONFIG$max_days,
                                       expiry_month = NULL,
                                       max_workers = ZERO_DIVIDEND_CONFIG$max_workers) {

  # Get liquid ETF universe (same list as collar strategy)
  log_info("Fetching liquid ETF universe...")
  etf_universe <- get_liquid_etfs(min_market_cap, min_avg_volume)

  # Validate we have ETFs to analyze
  if (length(etf_universe) == 0) {
    log_warn("No liquid ETFs found")
    return(tibble())
  }

  log_info("Found {length(etf_universe)} liquid ETFs to analyze")

  # Call generic covered call analyzer
  analyze_covered_calls_generic(
    stock_universe = etf_universe,
    strategy_name = "Zero-Dividend - Liquid ETFs",
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    max_workers = max_workers,
    result_flags = list(
      is_zero_dividend = TRUE,
      is_aristocrat = FALSE,
      is_etf = TRUE
    )
  )
}

#' Analyze Yahoo most active ETFs for zero-dividend covered call opportunities
#'
#' Analyzes the most actively traded ETFs from Yahoo Finance for deep ITM
#' covered call opportunities. The list updates dynamically based on real-time
#' market activity, typically including leveraged ETFs, crypto ETFs, and high-volume
#' trading vehicles.
#'
#' Unlike the static liquid ETF list, this variant:
#' - Updates daily based on actual trading volume
#' - Includes speculative/leveraged ETFs if they're actively traded
#' - May have tighter bid/ask spreads due to extreme liquidity
#' - Captures current market interest and momentum
#'
#' @param count Number of most active ETFs to analyze (default 25)
#' @param strike_threshold_pct Maximum strike as % of current price (default 0.85)
#' @param min_days Minimum days to expiry (default 45)
#' @param max_days Maximum days to expiry (default 120)
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers for processing
#' @return Tibble with all opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze top 25 most active ETFs
#'   results <- analyze_zero_dividend_yahoo_active_etfs()
#'
#'   # Analyze top 50 with custom parameters
#'   results <- analyze_zero_dividend_yahoo_active_etfs(
#'     count = 50,
#'     strike_threshold_pct = 0.90,
#'     min_days = 60,
#'     max_days = 90
#'   )
#' }
analyze_zero_dividend_yahoo_active_etfs <- function(count = 25,
                                                    strike_threshold_pct = ZERO_DIVIDEND_CONFIG$strike_threshold_pct,
                                                    min_days = ZERO_DIVIDEND_CONFIG$min_days,
                                                    max_days = ZERO_DIVIDEND_CONFIG$max_days,
                                                    expiry_month = NULL,
                                                    max_workers = ZERO_DIVIDEND_CONFIG$max_workers) {

  # Get most active ETFs from Yahoo Finance (real-time scrape)
  log_info("Fetching most active ETFs from Yahoo Finance...")
  etf_universe <- get_yahoo_most_active_etfs(count)

  # Validate we have ETFs to analyze
  if (length(etf_universe) == 0) {
    log_warn("No ETFs found from Yahoo Finance")
    return(tibble())
  }

  log_info("Found {length(etf_universe)} most active ETFs from Yahoo Finance")

  # Call generic covered call analyzer
  analyze_covered_calls_generic(
    stock_universe = etf_universe,
    strategy_name = "Zero-Dividend - Yahoo Most Active ETFs",
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    max_workers = max_workers,
    result_flags = list(
      is_zero_dividend = TRUE,
      is_aristocrat = FALSE,
      is_etf = TRUE,
      is_yahoo_active = TRUE
    )
  )
}
