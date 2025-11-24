#' S&P 500 Cash-Secured Puts Analysis
#'
#' Analyzes S&P 500 stocks for cash-secured put opportunities with flexible
#' dividend filtering. Can analyze all S&P 500 stocks, only dividend-paying
#' stocks, or only zero-dividend growth stocks.
#'
#' Key features:
#' - Flexible dividend filtering (all, dividend-paying, zero-dividend)
#' - Uses established S&P 500 stock universes
#' - Optional limit parameter for faster analysis
#' - Comprehensive result flags for strategy identification
#'
#' @param dividend_filter Dividend filter type: "all", "dividend_paying", or "zero_dividend"
#' @param limit Maximum number of stocks to analyze (NULL for no limit). Applied only when
#'   dividend_filter is "dividend_paying" or "zero_dividend". Default NULL.
#' @param strike_threshold_pct Minimum strike as % of current price (default from config)
#' @param min_days Minimum days to expiry (default from config)
#' @param max_days Maximum days to expiry (default from config)
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers (default from config)
#' @return Tibble with cash-secured put opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze all S&P 500 stocks
#'   results <- analyze_sp500_cash_secured_puts(
#'     dividend_filter = "all"
#'   )
#'
#'   # Analyze top 50 dividend-paying stocks only
#'   results <- analyze_sp500_cash_secured_puts(
#'     dividend_filter = "dividend_paying",
#'     limit = 50,
#'     strike_threshold_pct = 0.85,
#'     min_days = 30,
#'     max_days = 45
#'   )
#'
#'   # Analyze zero-dividend growth stocks
#'   results <- analyze_sp500_cash_secured_puts(
#'     dividend_filter = "zero_dividend",
#'     limit = 25,
#'     strike_threshold_pct = 0.90
#'   )
#' }
analyze_sp500_cash_secured_puts <- function(
  dividend_filter = "all",
  limit = NULL,
  strike_threshold_pct = CASH_SECURED_PUTS_CONFIG$strike_threshold_pct,
  min_days = CASH_SECURED_PUTS_CONFIG$min_days,
  max_days = CASH_SECURED_PUTS_CONFIG$max_days,
  expiry_month = NULL,
  max_workers = CASH_SECURED_PUTS_CONFIG$max_workers
) {
  # Validate dividend_filter parameter
  valid_filters <- c("all", "dividend_paying", "zero_dividend")
  if (!dividend_filter %in% valid_filters) {
    stop(
      "Invalid dividend_filter: must be 'all', 'dividend_paying', or 'zero_dividend'. ",
      "Got: ", dividend_filter
    )
  }

  # Select stock universe based on dividend filter
  log_info("Selecting S&P 500 stock universe with dividend filter: {dividend_filter}")

  stock_universe <- switch(dividend_filter,
    "all" = {
      log_info("Using all S&P 500 stocks (no dividend filter)")
      get_sp500_stocks()
    },
    "dividend_paying" = {
      log_info("Fetching dividend-paying S&P 500 stocks with limit: {limit %||% 'none'}")
      get_dividend_paying_sp500(limit = limit, max_workers = max_workers)
    },
    "zero_dividend" = {
      log_info("Fetching zero-dividend S&P 500 stocks with limit: {limit %||% 'none'}")
      get_zero_dividend_stocks(limit = limit, max_workers = max_workers)
    },
    stop("Invalid dividend_filter: must be 'all', 'dividend_paying', or 'zero_dividend'")
  )

  # Validate we have stocks to analyze
  if (length(stock_universe) == 0) {
    log_warn("No S&P 500 stocks found matching dividend filter: {dividend_filter}")
    return(tibble())
  }

  log_info("Found {length(stock_universe)} S&P 500 stocks to analyze")

  # Call generic put analyzer
  analyze_puts_generic(
    stock_universe = stock_universe,
    strategy_name = "S&P 500 Cash-Secured Puts",
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    max_workers = max_workers,
    result_flags = list(
      is_put = TRUE,
      is_aristocrat = FALSE,
      is_etf = FALSE,
      is_yfscreen = FALSE,
      is_sp500 = TRUE,
      dividend_filter = dividend_filter
    )
  )
}
