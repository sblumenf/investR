#' Shared Covered Calls Analysis Functions
#'
#' Generic, reusable functions for covered call analysis across different
#' stock selection strategies. These functions are strategy-agnostic and
#' compose with strategy-specific stock universe functions.
#'
#' @name covered-calls-shared
#' @importFrom logger log_info log_warn log_success log_debug
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multisession
#' @importFrom purrr compact
#' @importFrom dplyr bind_rows arrange desc %>%
NULL

################################################################################
# GENERIC LOGGING FUNCTIONS
################################################################################

#' Log analysis header with strategy name
#'
#' @param strategy_name Name of the strategy (e.g., "Dividend Aristocrats")
#' @noRd
log_analysis_header_generic <- function(strategy_name) {
  log_info("")
  log_info(strrep("=", 60))
  log_info("Deep ITM Covered Calls Analysis - {strategy_name}")
  log_info("Timestamp: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")
  log_info(strrep("=", 60))
}

#' Log analysis parameters
#'
#' @param n_stocks Number of stocks to analyze
#' @param strategy_name Name of the strategy
#' @param strike_threshold_pct Strike threshold percentage
#' @param min_days Minimum days to expiry
#' @param max_days Maximum days to expiry
#' @param max_workers Number of parallel workers
#' @noRd
log_analysis_params_generic <- function(n_stocks, strategy_name,
                                       strike_threshold_pct, min_days,
                                       max_days, max_workers) {
  log_info("Analyzing {n_stocks} {strategy_name} stocks...")

  days_range <- if (is.null(min_days) && is.null(max_days)) {
    "any"
  } else if (is.null(min_days)) {
    sprintf("<= %d", max_days)
  } else if (is.null(max_days)) {
    sprintf(">= %d", min_days)
  } else {
    sprintf("%d-%d", min_days, max_days)
  }

  log_info("Strike threshold: {sprintf('%.0f%%', strike_threshold_pct * 100)}, Days range: {days_range}, Workers: {max_workers}")
}

################################################################################
# GENERIC PARALLEL PROCESSING
################################################################################

#' Process stocks in parallel with generic analyzer
#'
#' @param stock_universe Character vector of ticker symbols
#' @param strike_threshold_pct Strike threshold percentage
#' @param min_days Minimum days to expiry
#' @param max_days Maximum days to expiry
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param result_flags Named list of flags to add to results
#' @return List of analysis results
#' @noRd
process_stocks_parallel_generic <- function(stock_universe, strike_threshold_pct,
                                           min_days = NULL, max_days = NULL,
                                           expiry_month = NULL, target_days = NULL,
                                           result_flags) {
  log_info("Processing stocks in parallel...")

  # Capture quote source setting to pass to workers
  quote_source <- get_quote_source()
  log_info("Quote source for this analysis: {toupper(quote_source)}")

  # Export package to workers to ensure they have access to all functions
  results <- future_map(stock_universe, function(ticker) {
    # Ensure package is loaded in worker
    if (!"investR" %in% loadedNamespaces()) {
      suppressPackageStartupMessages(loadNamespace("investR"))
    }

    # Set quote source in this worker to match main process
    options(investR.quote_source = quote_source)

    log_info("Analyzing {ticker}...")
    analyze_single_stock_generic(ticker, strike_threshold_pct, min_days, max_days, expiry_month, target_days, result_flags)
  }, .options = furrr_options(
    seed = TRUE,
    packages = "investR"  # Load investR package in each worker
  ))

  return(results)
}

################################################################################
# GENERIC SINGLE STOCK ANALYZER
################################################################################

#' Analyze a single stock for covered call opportunity (generic version)
#'
#' Generic version of single stock analysis that works with any stock universe.
#' Delegates to existing specialized functions for data fetching and calculations.
#'
#' @param ticker Stock ticker symbol
#' @param strike_threshold_pct Maximum strike as % of current price
#' @param min_days Minimum days to expiry
#' @param max_days Maximum days to expiry
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param result_flags Named list of additional flags to add to results
#' @return Tibble row with analysis results or NULL
#' @noRd
analyze_single_stock_generic <- function(ticker,
                                        strike_threshold_pct,
                                        min_days = NULL,
                                        max_days = NULL,
                                        expiry_month = NULL,
                                        target_days = NULL,
                                        result_flags = list()) {

  validate_ticker(ticker)

  # Get stock data (already generic!)
  stock_data <- get_stock_data(ticker)
  if (is.null(stock_data)) {
    return(NULL)
  }

  # Filter by maximum stock price
  max_price <- tryCatch(CONFIG$max_stock_price, error = function(e) 250)
  if (stock_data$current_price > max_price) {
    log_debug("{ticker}: Price ${stock_data$current_price} exceeds max ${max_price}")
    return(NULL)
  }

  # Get options chain (already generic!)
  options_df <- get_options_chain(ticker, stock_data$current_price)
  if (nrow(options_df) == 0) {
    log_debug("{ticker}: No options data available")
    return(NULL)
  }

  # Select optimal option (already parameterized!)
  selection <- select_optimal_option(ticker, stock_data$current_price, options_df,
                                    strike_threshold_pct, min_days, max_days, expiry_month, target_days)
  if (is.null(selection)) {
    log_debug("{ticker}: No suitable options found")
    return(NULL)
  }

  # Calculate metrics (already generic!)
  metrics <- calculate_metrics(ticker, stock_data$current_price,
                              selection$option, stock_data, selection$warning_flag)

  # Add strategy-specific result flags
  for (flag_name in names(result_flags)) {
    metrics[[flag_name]] <- result_flags[[flag_name]]
  }

  # Get negative return threshold from CONFIG (should work for all strategies)
  negative_threshold <- tryCatch(
    CONFIG$negative_return_threshold,
    error = function(e) 0
  )

  # Filter negative returns
  if (metrics$annualized_return <= negative_threshold) {
    log_debug("{ticker}: Negative return: {sprintf('%.2f%%', metrics$annualized_return * 100)}")
    return(NULL)
  }

  log_success("{ticker}: Annualized return: {sprintf('%.2f%%', metrics$annualized_return * 100)}")

  metrics
}

################################################################################
# GENERIC ORCHESTRATOR
################################################################################

#' Generic covered calls analysis orchestrator
#'
#' Orchestrates the complete covered call analysis workflow for any stock universe.
#' This is the main entry point that coordinates parallel processing, logging,
#' and result aggregation.
#'
#' @param stock_universe Character vector of tickers to analyze
#' @param strategy_name Name for logging (e.g., "Dividend Aristocrats")
#' @param strike_threshold_pct Maximum strike as % of current price
#' @param min_days Minimum days to expiry (NULL = no minimum)
#' @param max_days Maximum days to expiry (NULL = longest available)
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers
#' @param result_flags Named list of additional flags to add to results
#' @return Tibble with all opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze custom stock universe
#'   my_stocks <- c("AAPL", "MSFT", "GOOGL")
#'   results <- analyze_covered_calls_generic(
#'     stock_universe = my_stocks,
#'     strategy_name = "Tech Giants",
#'     strike_threshold_pct = 0.80,
#'     min_days = 60,
#'     max_days = 90,
#'     expiry_month = 6,
#'     max_workers = 4,
#'     result_flags = list(is_tech = TRUE)
#'   )
#' }
analyze_covered_calls_generic <- function(stock_universe,
                                         strategy_name,
                                         strike_threshold_pct = 0.8,
                                         min_days = NULL,
                                         max_days = NULL,
                                         expiry_month = NULL,
                                         target_days = NULL,
                                         max_workers = 10,
                                         result_flags = list()) {

  # Validate inputs
  if (length(stock_universe) == 0) {
    stop("stock_universe cannot be empty")
  }
  if (!is.character(stock_universe)) {
    stop("stock_universe must be character vector")
  }

  # Log analysis start
  log_analysis_header_generic(strategy_name)

  # Log analysis parameters
  log_analysis_params_generic(
    n_stocks = length(stock_universe),
    strategy_name = strategy_name,
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    max_workers = max_workers
  )

  # Setup parallel processing with cleanup guarantee
  oplan <- setup_parallel_processing(max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Process stocks in parallel
  results <- process_stocks_parallel_generic(
    stock_universe = stock_universe,
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    target_days = target_days,
    result_flags = result_flags
  )

  # Finalize and sort results (reuse existing function)
  results_df <- finalize_results(results)

  # Log completion
  log_analysis_footer(nrow(results_df))

  results_df
}
