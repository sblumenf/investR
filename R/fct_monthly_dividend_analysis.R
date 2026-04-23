#' Monthly Dividend Stocks Strategy Analysis Functions
#'
#' Core business logic for analyzing monthly dividend stocks for
#' covered call opportunities.
#'
#' @name monthly-dividend-analysis
#' @import dplyr
#' @importFrom purrr map_dfr compact possibly keep
#' @importFrom tibble tibble
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multisession
#' @importFrom logger log_info log_warn log_success log_debug log_error
#' @importFrom utils head tail
#' @importFrom stats na.omit
NULL

#' Analyze monthly dividend stocks for covered call opportunities
#'
#' Fetches monthly dividend-paying stocks from stockanalysis.com and analyzes
#' them for deep ITM covered call opportunities. Uses shared generic analysis
#' functions with the monthly dividend stock universe.
#'
#' @param strike_threshold_pct Maximum strike as % of current price
#' @param target_days Target days to expiry (NULL = longest available)
#' @param max_workers Number of parallel workers
#' @param strategy_name Label used in logging and result output
#' @return Tibble with all opportunities sorted by annualized return
#' @export
analyze_monthly_dividend_stocks <- function(
  strike_threshold_pct = MONTHLY_DIVIDEND_CONFIG$strike_threshold_pct,
  target_days = MONTHLY_DIVIDEND_CONFIG$target_days,
  max_workers = MONTHLY_DIVIDEND_CONFIG$max_workers,
  strategy_name = "Monthly Dividend Stocks"
) {

  log_info("Fetching monthly dividend stocks from stockanalysis.com...")

  stocks <- fetch_monthly_dividend_stocks()

  if (nrow(stocks) == 0) {
    log_warn("No monthly dividend stocks found")
    return(tibble())
  }

  tickers <- stocks$ticker

  log_info("Analyzing {length(tickers)} monthly dividend stocks for covered call opportunities...")

  analyze_covered_calls_generic(
    stock_universe = tickers,
    strategy_name = strategy_name,
    strike_threshold_pct = strike_threshold_pct,
    target_days = target_days,
    max_workers = max_workers,
    result_flags = list(is_aristocrat = FALSE, is_monthly_dividend = TRUE)
  )
}
