#' Stock Universe Fetching Functions
#'
#' Centralized functions for fetching and filtering stock universes.
#' Supports caching to minimize expensive API calls.
#'
#' @name stock-universe
#' @importFrom rvest read_html html_table
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr filter %>%
#' @importFrom purrr map_lgl compact
#' @importFrom logger log_info log_warn log_debug log_success
NULL

# S&P 500 fallback list (current as of 2025)
SP500_FALLBACK_TICKERS <- c(
  "AAPL", "MSFT", "AMZN", "NVDA", "GOOGL", "GOOG", "META", "TSLA", "BRK.B", "UNH",
  "XOM", "LLY", "JPM", "JNJ", "V", "PG", "MA", "AVGO", "HD", "CVX",
  "MRK", "ABBV", "COST", "PEP", "ADBE", "KO", "WMT", "CRM", "TMO", "MCD",
  "CSCO", "ACN", "ABT", "BAC", "NFLX", "LIN", "AMD", "DIS", "PM", "DHR",
  "TXN", "VZ", "CMCSA", "INTC", "NKE", "ORCL", "WFC", "INTU", "COP", "UNP",
  "NEE", "PFE", "T", "LOW", "UPS", "HON", "BMY", "QCOM", "RTX", "MS",
  "CAT", "SPGI", "AMGN", "BA", "NOW", "GE", "IBM", "AMAT", "DE", "BLK",
  "ELV", "PLD", "GS", "AXP", "SYK", "ISRG", "BKNG", "TJX", "LMT", "GILD",
  "MMC", "VRTX", "ADI", "REGN", "CVS", "ADP", "C", "MDLZ", "AMT", "SBUX",
  "TMUS", "ZTS", "MO", "CI", "CB", "PGR", "SO", "DUK", "ETN", "EOG",
  "SCHW", "MMM", "SLB", "BDX", "FI", "ITW", "MU", "CME", "LRCX", "BSX"
)

#' Cache directory path
#'
#' @return Path to cache directory
#' @noRd
get_cache_dir <- function() {
  cache_dir <- system.file("cache", package = "investR")

  # If package cache doesn't exist, use inst/cache for development
  if (cache_dir == "") {
    cache_dir <- here::here("inst", "cache")
  }

  # Create directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  cache_dir
}

#' Check if cached data is fresh
#'
#' @param cache_file Cache file name
#' @param max_age_days Maximum age in days (default 30)
#' @return TRUE if cache exists and is fresh, FALSE otherwise
#' @noRd
is_cache_fresh <- function(cache_file, max_age_days = 30) {
  cache_path <- file.path(get_cache_dir(), cache_file)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  tryCatch({
    cached_data <- readRDS(cache_path)

    if (is.null(cached_data$timestamp)) {
      return(FALSE)
    }

    days_old <- as.numeric(difftime(Sys.time(), cached_data$timestamp, units = "days"))

    if (days_old <= max_age_days) {
      log_debug("Cache is fresh ({round(days_old, 1)} days old)")
      return(TRUE)
    } else {
      log_debug("Cache is stale ({round(days_old, 1)} days old)")
      return(FALSE)
    }

  }, error = function(e) {
    log_warn("Error reading cache: {e$message}")
    return(FALSE)
  })
}

#' Save data to cache
#'
#' @param cache_file Cache file name
#' @param data Data to cache
#' @noRd
save_to_cache <- function(cache_file, data) {
  tryCatch({
    cache_path <- file.path(get_cache_dir(), cache_file)

    cached_data <- list(
      data = data,
      timestamp = Sys.time()
    )

    saveRDS(cached_data, cache_path)
    log_debug("Saved to cache: {cache_file}")

  }, error = function(e) {
    log_warn("Failed to save cache: {e$message}")
  })
}

#' Load data from cache
#'
#' @param cache_file Cache file name
#' @return Cached data or NULL
#' @noRd
load_from_cache <- function(cache_file) {
  tryCatch({
    cache_path <- file.path(get_cache_dir(), cache_file)
    cached_data <- readRDS(cache_path)
    return(cached_data$data)
  }, error = function(e) {
    log_warn("Failed to load cache: {e$message}")
    return(NULL)
  })
}

#' Fetch S&P 500 list from Wikipedia
#'
#' Primary source for S&P 500 constituents. Uses rvest to scrape the
#' Wikipedia page maintained by S&P Dow Jones Indices.
#'
#' @return Character vector of tickers, or NULL on failure
#' @noRd
fetch_sp500_from_wikipedia <- function() {
  safely_fetch(
    expr = {
      url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
      log_debug("Fetching S&P 500 list from Wikipedia...")

      tables <- read_html(url) %>%
        html_table(fill = TRUE)

      if (length(tables) == 0) {
        stop("No tables found on Wikipedia page")
      }

      # First table contains the constituents
      sp500_df <- tables[[1]]

      if (!"Symbol" %in% names(sp500_df)) {
        stop("Symbol column not found in Wikipedia table")
      }

      tickers <- sp500_df$Symbol %>%
        na.omit() %>%
        unique() %>%
        as.character()

      # Sanity check: S&P 500 should have ~500 stocks
      if (length(tickers) < 400) {
        stop("Too few tickers found ({length(tickers)}), expected ~500")
      }

      log_success("Found {length(tickers)} S&P 500 stocks from Wikipedia")
      return(tickers)
    },
    error_msg = "Wikipedia fetch failed",
    default = NULL,
    log_level = "warn"
  )
}

#' Fetch S&P 500 list from DataHub.io
#'
#' Secondary source for S&P 500 constituents. Uses direct CSV download.
#'
#' @return Character vector of tickers, or NULL on failure
#' @noRd
fetch_sp500_from_datahub <- function() {
  safely_fetch(
    expr = {
      url <- "https://datahub.io/core/s-and-p-500-companies/r/constituents.csv"
      log_debug("Fetching S&P 500 list from DataHub.io...")

      sp500_df <- read_csv(url, show_col_types = FALSE, col_types = cols())

      if (!"Symbol" %in% names(sp500_df)) {
        stop("Symbol column not found in DataHub CSV")
      }

      tickers <- sp500_df$Symbol %>%
        na.omit() %>%
        unique() %>%
        as.character()

      if (length(tickers) < 400) {
        stop("Too few tickers found ({length(tickers)}), expected ~500")
      }

      log_success("Found {length(tickers)} S&P 500 stocks from DataHub")
      return(tickers)
    },
    error_msg = "DataHub fetch failed",
    default = NULL,
    log_level = "warn"
  )
}

#' Get S&P 500 stock list with caching and fallback
#'
#' Fetches the current list of S&P 500 constituent stocks. Uses Wikipedia as
#' primary source, DataHub as secondary, and hardcoded fallback as last resort.
#' Results are cached for 30 days to minimize API calls.
#'
#' @return Character vector of S&P 500 ticker symbols
#' @export
#' @examples
#' \dontrun{
#'   sp500_stocks <- get_sp500_stocks()
#'   length(sp500_stocks)  # Should be ~500
#' }
get_sp500_stocks <- function() {
  cache_file <- "sp500_stocks.rds"

  # Check cache first
  if (is_cache_fresh(cache_file, max_age_days = 30)) {
    log_info("Using cached S&P 500 list")
    return(load_from_cache(cache_file))
  }

  # Try Wikipedia
  tickers <- fetch_sp500_from_wikipedia()
  if (!is.null(tickers)) {
    save_to_cache(cache_file, tickers)
    return(tickers)
  }

  # Try DataHub
  tickers <- fetch_sp500_from_datahub()
  if (!is.null(tickers)) {
    save_to_cache(cache_file, tickers)
    return(tickers)
  }

  # Use fallback
  log_warn("Using fallback S&P 500 list - may be outdated")
  log_info("Found {length(SP500_FALLBACK_TICKERS)} S&P 500 stocks from fallback")
  return(SP500_FALLBACK_TICKERS)
}

#' Check if a stock pays dividends
#'
#' Uses dividend history from Yahoo Finance to determine if stock pays dividends.
#'
#' @param ticker Stock ticker symbol
#' @param lookback_years Years of history to check (default 2)
#' @return TRUE if pays dividends, FALSE if zero dividend
#' @noRd
stock_pays_dividend <- function(ticker, lookback_years = 2) {
  safely_fetch(
    expr = {
      start_date <- Sys.Date() - lubridate::years(lookback_years)

      dividends <- fetch_dividend_history(ticker, from = start_date)

      # NULL or empty xts object means no dividends
      # Note: nrow() on empty xts returns NULL, so use length() instead
      if (is.null(dividends) || length(dividends) == 0) {
        return(FALSE)
      }

      # Has dividend history
      return(TRUE)
    },
    error_msg = "{ticker}: Error checking dividend status",
    default = NA,  # Return NA for errors
    log_level = "debug"
  )
}

#' Get zero-dividend S&P 500 stocks with caching
#'
#' Filters S&P 500 for stocks that don't pay dividends. Results are cached
#' for 30 days to avoid expensive repeated scans.
#'
#' @param limit Optional limit on number of stocks to check (for testing)
#' @param max_workers Number of parallel workers to use (default 4)
#' @return Character vector of zero-dividend ticker symbols
#' @export
#' @examples
#' \dontrun{
#'   zero_div_stocks <- get_zero_dividend_stocks()
#'   length(zero_div_stocks)  # Should be ~80-100
#'
#'   # Test with limit
#'   zero_div_stocks <- get_zero_dividend_stocks(limit = 50)
#' }
get_zero_dividend_stocks <- function(limit = NULL, max_workers = 4) {
  cache_file <- "zero_dividend_stocks.rds"

  # Check cache first (unless limit specified, which is for testing)
  if (is.null(limit) && is_cache_fresh(cache_file, max_age_days = 30)) {
    log_info("Using cached zero-dividend stocks list")
    return(load_from_cache(cache_file))
  }

  # Get S&P 500 universe
  sp500_stocks <- get_sp500_stocks()

  # Apply limit if specified (for testing)
  if (!is.null(limit)) {
    sp500_stocks <- head(sp500_stocks, limit)
    log_info("Limiting scan to first {limit} S&P 500 stocks (testing mode)")
  } else {
    log_info("Scanning {length(sp500_stocks)} S&P 500 stocks for dividend status...")
    log_info("This may take 5-10 minutes. Results will be cached for 30 days.")
  }

  # Use parallel processing for speed
  log_info("Setting up {max_workers} parallel workers for dividend status check")
  oplan <- future::plan(future::multisession, workers = max_workers)
  on.exit(future::plan(oplan), add = TRUE)

  # Check dividend status for each stock
  log_info("Checking dividend status in parallel...")
  dividend_status <- furrr::future_map_lgl(
    sp500_stocks,
    stock_pays_dividend,
    .options = furrr::furrr_options(seed = TRUE),
    .progress = TRUE
  )

  # Filter for zero-dividend stocks (FALSE means no dividends)
  # Exclude NAs (errors) and TRUE (pays dividends)
  zero_div_stocks <- sp500_stocks[!is.na(dividend_status) & !dividend_status]

  log_success("Found {length(zero_div_stocks)} zero-dividend stocks")

  # Cache results (unless limit was specified)
  if (is.null(limit)) {
    save_to_cache(cache_file, zero_div_stocks)
  }

  return(zero_div_stocks)
}

#' Get dividend-paying S&P 500 stocks with caching
#'
#' Filters S&P 500 for stocks that pay dividends. Results are cached
#' for 30 days to avoid expensive repeated scans.
#'
#' @param limit Optional limit on number of stocks to check (for testing)
#' @param max_workers Number of parallel workers to use (default 4)
#' @return Character vector of dividend-paying ticker symbols
#' @export
#' @examples
#' \dontrun{
#'   div_stocks <- get_dividend_paying_sp500()
#'   length(div_stocks)  # Should be ~400
#'
#'   # Test with limit
#'   div_stocks <- get_dividend_paying_sp500(limit = 50)
#' }
get_dividend_paying_sp500 <- function(limit = NULL, max_workers = 4) {
  cache_file <- "dividend_paying_stocks.rds"

  # Check cache first (unless limit specified, which is for testing)
  if (is.null(limit) && is_cache_fresh(cache_file, max_age_days = 30)) {
    log_info("Using cached dividend-paying stocks list")
    return(load_from_cache(cache_file))
  }

  # Get S&P 500 universe
  sp500_stocks <- get_sp500_stocks()

  # Apply limit if specified (for testing)
  if (!is.null(limit)) {
    sp500_stocks <- head(sp500_stocks, limit)
    log_info("Limiting scan to first {limit} S&P 500 stocks (testing mode)")
  } else {
    log_info("Scanning {length(sp500_stocks)} S&P 500 stocks for dividend status...")
    log_info("This may take 5-10 minutes. Results will be cached for 30 days.")
  }

  # Use parallel processing for speed
  log_info("Setting up {max_workers} parallel workers for dividend status check")
  oplan <- future::plan(future::multisession, workers = max_workers)
  on.exit(future::plan(oplan), add = TRUE)

  # Check dividend status for each stock
  log_info("Checking dividend status in parallel...")
  dividend_status <- furrr::future_map_lgl(
    sp500_stocks,
    stock_pays_dividend,
    .options = furrr::furrr_options(seed = TRUE),
    .progress = TRUE
  )

  # Filter for dividend-paying stocks (TRUE means pays dividends)
  # Exclude NAs (errors) and FALSE (zero dividends)
  dividend_paying_stocks <- sp500_stocks[!is.na(dividend_status) & dividend_status]

  log_success("Found {length(dividend_paying_stocks)} dividend-paying stocks")

  # Cache results (unless limit was specified)
  if (is.null(limit)) {
    save_to_cache(cache_file, dividend_paying_stocks)
  }

  return(dividend_paying_stocks)
}

#' Get Russell 2000 constituents
#'
#' Fetches Russell 2000 stock tickers from iShares IWM ETF holdings CSV.
#' Results are cached for 7 days since Russell 2000 constituents change regularly.
#'
#' @return Character vector of ticker symbols
#' @export
#' @examples
#' \dontrun{
#'   # Get current Russell 2000 constituents
#'   r2000_stocks <- get_russell_2000_stocks()
#' }
get_russell_2000_stocks <- function() {
  cache_file <- "russell_2000_stocks.rds"

  # Check cache first (7 day expiry for index constituents)
  if (is_cache_fresh(cache_file, max_age_days = 7)) {
    log_info("Using cached Russell 2000 constituents")
    return(load_from_cache(cache_file))
  }

  tryCatch({
    log_info("Fetching Russell 2000 constituents from iShares IWM ETF...")

    # iShares IWM (Russell 2000 ETF) holdings CSV
    csv_url <- "https://www.ishares.com/us/products/239710/ishares-russell-2000-etf/1467271812596.ajax?fileType=csv&fileName=IWM_holdings&dataType=fund"

    # Read CSV, skipping first 9 header rows (line 10 has column names)
    russell_data <- read_csv(
      csv_url,
      skip = 9,
      col_types = cols(
        Ticker = col_character(),
        .default = col_character()
      ),
      show_col_types = FALSE
    )

    # Extract tickers and clean
    tickers <- russell_data$Ticker %>%
      as.character() %>%
      trimws() %>%
      toupper() %>%
      unique() %>%
      .[. != "" & !is.na(.) & . != "-"]  # Remove blanks, NAs, and "-" placeholders

    log_success("Fetched {length(tickers)} Russell 2000 tickers from iShares IWM")

    # Cache results
    save_to_cache(cache_file, tickers)

    return(tickers)

  }, error = function(e) {
    log_warn("Failed to fetch Russell 2000 from iShares: {e$message}")
    log_warn("Returning empty vector. Check internet connection.")
    return(character(0))
  })
}

#' Clear stock universe caches
#'
#' Utility function to force refresh of cached data. Useful when S&P 500
#' constituents change or for troubleshooting.
#'
#' @param cache_type Type of cache to clear: "sp500", "zero_dividend", "dividend_paying", or "all"
#' @return TRUE if successful
#' @export
#' @examples
#' \dontrun{
#'   # Clear specific cache
#'   clear_stock_cache("zero_dividend")
#'
#'   # Clear all caches
#'   clear_stock_cache("all")
#' }
clear_stock_cache <- function(cache_type = "all") {
  cache_dir <- get_cache_dir()

  files_to_remove <- switch(cache_type,
    "sp500" = "sp500_stocks.rds",
    "zero_dividend" = "zero_dividend_stocks.rds",
    "dividend_paying" = "dividend_paying_stocks.rds",
    "all" = c("sp500_stocks.rds", "zero_dividend_stocks.rds", "dividend_paying_stocks.rds"),
    stop("Invalid cache_type. Use 'sp500', 'zero_dividend', 'dividend_paying', or 'all'")
  )

  for (file in files_to_remove) {
    cache_path <- file.path(cache_dir, file)
    if (file.exists(cache_path)) {
      file.remove(cache_path)
      log_info("Cleared cache: {file}")
    }
  }

  invisible(TRUE)
}
