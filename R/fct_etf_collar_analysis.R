#' ETF Collar Strategy Analysis Functions (yfscreen-based)
#'
#' Wrapper that fetches ETFs from Yahoo Finance screener and runs collar analysis.
#'
#' @name etf-collar-analysis
#' @importFrom logger log_info log_warn log_error
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan
#' @importFrom purrr compact
#' @importFrom dplyr bind_rows arrange desc %>%
NULL

#' Analyze ETF collar opportunities using yfscreen-sourced ETFs
#'
#' Fetches ETFs from Yahoo Finance screener based on the selected dividend filter,
#' then runs collar analysis on each ETF in parallel using \code{analyze_collar_single()}.
#'
#' @param dividend_filter Character. Either "dividend_paying" or "zero_dividend".
#' @param target_days Integer. Target days to expiry (default 300).
#' @param strike_adjustment_pct Numeric. Strike adjustment as decimal (0 = ATM, default 0).
#' @param max_workers Integer. Number of parallel workers (default from config).
#'
#' @return A tibble of collar opportunities sorted by annualized_return descending.
#'   Returns an empty tibble if no ETFs match or all lack options chains.
#'
#' @export
analyze_etf_collar_yfscreen <- function(dividend_filter = "dividend_paying",
                                         target_days = 300,
                                         strike_adjustment_pct = 0,
                                         max_workers = ETF_COLLAR_CONFIG$max_workers) {

  valid_filters <- c("dividend_paying", "zero_dividend")
  if (!dividend_filter %in% valid_filters) {
    stop(sprintf("dividend_filter must be one of: %s", paste(valid_filters, collapse = ", ")))
  }

  log_analysis_header_generic("ETF Collar Strategy (yfscreen)")
  log_info("Dividend filter: {dividend_filter}, target_days: {target_days}, strike_adj: {strike_adjustment_pct}")

  # Fetch ETF universe from yfscreen
  etf_universe <- tryCatch(
    fetch_yfscreen_etfs(dividend_filter = dividend_filter),
    error = function(e) {
      log_error("Failed to fetch ETFs from yfscreen: {conditionMessage(e)}")
      character(0)
    }
  )

  if (length(etf_universe) == 0) {
    log_warn("No ETFs returned from yfscreen for filter: {dividend_filter}")
    return(dplyr::tibble())
  }

  log_info("Analyzing {length(etf_universe)} ETFs from yfscreen...")

  # Get reinvestment rate
  reinvest_rate <- get_reinvestment_rate()
  log_info("SGOV yield for reinvestment: {sprintf('%.2f%%', reinvest_rate * 100)}")

  # Setup parallel processing
  oplan <- setup_parallel_processing(max_workers)
  on.exit(future::plan(oplan), add = TRUE)

  # Capture quote source to propagate to workers
  quote_source <- get_quote_source()
  log_info("Quote source: {toupper(quote_source)}")

  # Process ETFs in parallel — same pattern as analyze_collar_etfs()
  log_info("Processing ETFs in parallel (workers: {max_workers})...")

  results <- future_map(etf_universe, function(ticker) {
    if (!"investR" %in% loadedNamespaces()) {
      suppressPackageStartupMessages(loadNamespace("investR"))
    }

    options(investR.quote_source = quote_source)

    investR::analyze_collar_single(ticker, target_days, strike_adjustment_pct)
  }, .options = furrr_options(seed = TRUE, packages = "investR"))

  # Combine and sort
  results_df <- compact(results) %>% bind_rows()

  if (nrow(results_df) > 0) {
    results_df <- results_df %>% arrange(desc(annualized_return))
  }

  log_analysis_footer(nrow(results_df))

  return(results_df)
}
