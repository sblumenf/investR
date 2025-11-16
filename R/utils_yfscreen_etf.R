#' yfscreen ETF Screening Utilities
#'
#' Helper functions for filtering and fetching ETFs using the yfscreen package
#'
#' @name yfscreen-etf-utils
#' @importFrom logger log_info log_warn log_error
#' @importFrom rlang %||%
#' @importFrom dplyr arrange desc slice_head %>%
NULL

#' Build yfscreen filter list from ETF screening parameters
#'
#' Converts user-friendly parameter values into the nested list format
#' required by yfscreen API.
#'
#' NOTE: Yahoo Finance ETF screener does NOT support dividend yield filtering via API.
#' Dividend filtering is handled post-fetch in fetch_yfscreen_etfs().
#'
#' @param dividend_yield_min Minimum dividend yield (e.g., 2 for 2%) - used post-fetch
#' @param dividend_yield_max Maximum dividend yield (e.g., 6 for 6%) - used post-fetch
#' @param dividend_filter Dividend filter type: "all", "dividend_paying", or "zero_dividend" - used post-fetch
#' @param market_cap_min Minimum fund net assets (ETF size/AUM) in dollars (e.g., 1000000000 for $1B)
#' @param market_cap_max Maximum fund net assets (ETF size/AUM) in dollars (e.g., 100000000000 for $100B)
#' @return List of yfscreen filter specifications
#' @noRd
#' @examples
#' \dontrun{
#'   filters <- build_yfscreen_etf_filters(
#'     dividend_yield_min = 2,
#'     dividend_yield_max = 6,
#'     dividend_filter = "dividend_paying",
#'     market_cap_min = 1e9,
#'     market_cap_max = 100e9
#'   )
#' }
build_yfscreen_etf_filters <- function(dividend_yield_min = NULL,
                                        dividend_yield_max = NULL,
                                        dividend_filter = "all",
                                        market_cap_min = NULL,
                                        market_cap_max = NULL) {
  filters <- list()

  # Region filter (always US for now)
  filters <- append(filters, list(list("eq", list("region", "us"))))

  # NOTE: Dividend yield filtering is NOT supported by Yahoo Finance ETF screener
  # We will filter by dividend yield after fetching the data instead
  # The dividend_filter parameter is still accepted for post-fetch filtering

  # Fund net assets filter (ETF size / AUM)
  if (!is.null(market_cap_min) && !is.null(market_cap_max)) {
    filters <- append(filters, list(list("btwn", list("fundnetassets", market_cap_min, market_cap_max))))
  } else if (!is.null(market_cap_min)) {
    filters <- append(filters, list(list("gt", list("fundnetassets", market_cap_min))))
  } else if (!is.null(market_cap_max)) {
    filters <- append(filters, list(list("lt", list("fundnetassets", market_cap_max))))
  }

  return(filters)
}

#' Fetch ETF tickers from yfscreen API
#'
#' Wrapper function for yfscreen API calls with proper error handling,
#' logging, and ticker extraction.
#'
#' @param dividend_yield_min Minimum dividend yield (e.g., 2 for 2%)
#' @param dividend_yield_max Maximum dividend yield (e.g., 6 for 6%)
#' @param dividend_filter Dividend filter type: "all", "dividend_paying", or "zero_dividend"
#' @param market_cap_min Minimum market cap in dollars (e.g., 1000000000 for $1B)
#' @param market_cap_max Maximum market cap in dollars (e.g., 100000000000 for $100B)
#' @param top_n Maximum number of ETFs to return, sorted by volume (default NULL for all)
#' @return Character vector of ticker symbols
#' @export
#' @examples
#' \dontrun{
#'   # Fetch ETFs with default parameters
#'   tickers <- fetch_yfscreen_etfs()
#'
#'   # Fetch ETFs with custom filters
#'   tickers <- fetch_yfscreen_etfs(
#'     dividend_yield_min = 3,
#'     dividend_yield_max = 8,
#'     dividend_filter = "dividend_paying",
#'     market_cap_min = 10e9,
#'     market_cap_max = 50e9
#'   )
#'
#'   # Fetch zero-dividend ETFs
#'   tickers <- fetch_yfscreen_etfs(
#'     dividend_filter = "zero_dividend"
#'   )
#'
#'   # Fetch top 50 most traded dividend-paying ETFs
#'   tickers <- fetch_yfscreen_etfs(
#'     dividend_filter = "dividend_paying",
#'     top_n = 50
#'   )
#'
#'   # Fetch large-cap zero-dividend ETFs
#'   tickers <- fetch_yfscreen_etfs(
#'     dividend_filter = "zero_dividend",
#'     market_cap_min = 10e9,
#'     market_cap_max = 100e9,
#'     top_n = 30
#'   )
#' }
fetch_yfscreen_etfs <- function(dividend_yield_min = NULL,
                                dividend_yield_max = NULL,
                                dividend_filter = "all",
                                market_cap_min = NULL,
                                market_cap_max = NULL,
                                top_n = NULL) {

  log_info("Building yfscreen filters for ETF screener...")
  log_info("Parameters: Dividend filter: {dividend_filter}, Dividend yield {dividend_yield_min %||% 'any'}-{dividend_yield_max %||% 'any'}%, Market cap ${market_cap_min %||% 'any'}-${market_cap_max %||% 'any'}, Top N: {top_n %||% 'all'}")

  # Build filter list
  filters <- build_yfscreen_etf_filters(
    dividend_yield_min = dividend_yield_min,
    dividend_yield_max = dividend_yield_max,
    dividend_filter = dividend_filter,
    market_cap_min = market_cap_min,
    market_cap_max = market_cap_max
  )

  # Log the actual filter structure for debugging
  log_info("Built filters structure: {jsonlite::toJSON(filters, auto_unbox = TRUE)}")

  # Execute yfscreen API call with error handling
  tryCatch({
    log_info("Executing yfscreen API call...")

    # Create query and payload
    query <- yfscreen::create_query(filters)
    log_info("Query structure: {jsonlite::toJSON(query, auto_unbox = TRUE)}")

    # Request more results than we need so we can filter and sort properly
    # If top_n is specified, request 10x that amount to ensure enough after filtering
    # Otherwise request a large batch (500 ETFs)
    request_size <- if (!is.null(top_n)) max(top_n * 10, 100) else 500
    log_info("Requesting {request_size} ETFs from API (to filter and sort to top {top_n %||% 'all'})")

    payload <- yfscreen::create_payload("etf", query, size = request_size)
    log_info("Payload structure: {jsonlite::toJSON(payload, auto_unbox = TRUE)}")

    # Fetch data
    data <- yfscreen::get_data(payload)

    # Validate response
    if (is.null(data) || nrow(data) == 0) {
      log_warn("No ETFs found matching filter criteria")
      return(character(0))
    }

    # Extract tickers (column name is 'symbol')
    if (!"symbol" %in% names(data)) {
      log_error("yfscreen response missing 'symbol' column. Available columns: {paste(names(data), collapse = ', ')}")
      stop("Invalid yfscreen API response format")
    }

    # Post-fetch dividend yield filtering (since API doesn't support it)
    if (dividend_filter == "dividend_paying") {
      log_info("Filtering for dividend-paying ETFs (post-fetch)")
      if ("dividendYield.raw" %in% names(data)) {
        original_count <- nrow(data)
        if (!is.null(dividend_yield_min) && !is.null(dividend_yield_max)) {
          data <- data %>%
            dplyr::filter(!is.na(dividendYield.raw) & dividendYield.raw >= dividend_yield_min & dividendYield.raw <= dividend_yield_max)
          log_info("Filtered to ETFs with {dividend_yield_min}-{dividend_yield_max}% yield: {nrow(data)} from {original_count}")
        } else {
          data <- data %>%
            dplyr::filter(!is.na(dividendYield.raw) & dividendYield.raw > 0)
          log_info("Filtered to dividend-paying ETFs: {nrow(data)} from {original_count}")
        }
      } else {
        log_warn("dividendYield.raw column not found, cannot filter by dividend yield")
      }
    } else if (dividend_filter == "zero_dividend") {
      log_info("Filtering for zero-dividend ETFs using dividend history (post-fetch)")
      original_count <- nrow(data)

      # Check actual dividend payment history for each ticker (parallel)
      log_info("Checking dividend history for {original_count} ETFs (this may take 1-2 minutes)...")
      tickers <- data$symbol

      # Use parallel processing for speed
      oplan <- future::plan(future::multisession, workers = 4)
      on.exit(future::plan(oplan), add = TRUE)

      dividend_status <- furrr::future_map_lgl(
        tickers,
        stock_pays_dividend,
        .options = furrr::furrr_options(seed = TRUE)
      )

      # Filter for zero-dividend (FALSE means no dividends)
      # Exclude NAs (errors) and TRUE (pays dividends)
      zero_div_mask <- !is.na(dividend_status) & !dividend_status
      data <- data[zero_div_mask, ]

      log_info("Filtered to zero-dividend ETFs: {nrow(data)} from {original_count}")
    }

    # Sort by average volume (descending) if volume column exists
    if ("averageDailyVolume3Month.raw" %in% names(data)) {
      log_info("Sorting ETFs by average daily volume (3-month)")
      data <- data %>%
        dplyr::arrange(dplyr::desc(averageDailyVolume3Month.raw))
    } else if ("regularMarketVolume.raw" %in% names(data)) {
      log_info("Sorting ETFs by regular market volume")
      data <- data %>%
        dplyr::arrange(dplyr::desc(regularMarketVolume.raw))
    } else {
      log_warn("No volume column found in yfscreen data. Available columns: {paste(names(data), collapse = ', ')}")
    }

    # Limit to top N if specified
    if (!is.null(top_n) && top_n > 0) {
      original_count <- nrow(data)
      data <- data %>%
        dplyr::slice_head(n = top_n)
      log_info("Limiting to top {top_n} ETFs (from {original_count} total)")
    }

    tickers <- data$symbol
    log_info("Returning {length(tickers)} ETF tickers")
    log_info("ETF tickers: {paste(tickers, collapse = ', ')}")

    return(tickers)

  }, error = function(e) {
    log_error("yfscreen API error: {e$message}")
    log_error("Error class: {class(e)}")
    log_error("Full error object: {jsonlite::toJSON(as.list(e), auto_unbox = TRUE)}")
    if (exists("filters")) {
      log_error("Filters that caused error: {jsonlite::toJSON(filters, auto_unbox = TRUE)}")
    }
    stop(sprintf("Failed to fetch ETF data from yfscreen: %s", e$message))
  })
}
