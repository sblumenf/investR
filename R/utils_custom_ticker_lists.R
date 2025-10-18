#' Strategy-Agnostic Custom Ticker Lists
#'
#' Functions for fetching ticker lists from various sources for custom
#' strategy variants. Supports overbought/oversold/most shorted stocks
#' and leveraged ETFs. Used by both collar and zero-dividend strategies.
#'
#' @name custom-ticker-lists
#' @importFrom rvest read_html html_elements html_attr
#' @importFrom logger log_info log_warn log_error
NULL

################################################################################
# CACHING
################################################################################

# Session-level caches for each list
.overbought_cache <- new.env(parent = emptyenv())
.oversold_cache <- new.env(parent = emptyenv())
.most_shorted_cache <- new.env(parent = emptyenv())
.leveraged_2x_cache <- new.env(parent = emptyenv())
.leveraged_3x_cache <- new.env(parent = emptyenv())

#' Check if cache is still valid
#'
#' @param cache_env Environment containing cached data
#' @param cache_hours Number of hours before cache expires
#' @return Logical
#' @noRd
collar_cache_is_valid <- function(cache_env, cache_hours) {
  if (!exists("tickers", envir = cache_env)) {
    return(FALSE)
  }

  if (!exists("timestamp", envir = cache_env)) {
    return(FALSE)
  }

  cache_age_hours <- as.numeric(
    difftime(Sys.time(), cache_env$timestamp, units = "hours")
  )

  return(cache_age_hours < cache_hours)
}

################################################################################
# SCRAPING FUNCTIONS
################################################################################

#' Scrape stock tickers from stockanalysis.com
#'
#' Generic scraper for stockanalysis.com stock list pages.
#' Extracts ticker symbols from stock links.
#'
#' @param url URL to scrape
#' @return Character vector of ticker symbols
#' @noRd
scrape_stockanalysis_tickers <- function(url) {
  # Fetch and parse HTML
  page <- read_html(url)

  # Extract ticker links (stock links have /stocks/ in path)
  all_links <- page %>%
    html_elements('a[href*="/stocks/"]') %>%
    html_attr('href')

  # Extract ticker symbols from URLs
  tickers <- gsub('.*/stocks/([^/]+).*', '\\1', all_links)
  tickers <- unique(toupper(tickers))

  # Filter out navigation items and common page elements
  nav_items <- c('/STOCKS/', 'SCREENER', 'COMPARE', 'LIST', 'TRENDING',
                 'GAINERS', 'LOSERS', 'ACTIVE', 'STOCKS')
  tickers <- tickers[!tickers %in% nav_items]

  # Filter to reasonable ticker length (1-5 characters)
  tickers <- tickers[nchar(tickers) >= 1 & nchar(tickers) <= 5]

  return(tickers)
}

#' Scrape overbought stocks from stockanalysis.com
#'
#' @return Character vector of ticker symbols
#' @noRd
scrape_overbought_tickers <- function() {
  url <- get_golem_config_value(
    "custom_ticker_lists",
    "overbought_url",
    "https://stockanalysis.com/list/overbought-stocks/"
  )

  scrape_stockanalysis_tickers(url)
}

#' Scrape oversold stocks from stockanalysis.com
#'
#' @return Character vector of ticker symbols
#' @noRd
scrape_oversold_tickers <- function() {
  url <- get_golem_config_value(
    "custom_ticker_lists",
    "oversold_url",
    "https://stockanalysis.com/list/oversold-stocks/"
  )

  scrape_stockanalysis_tickers(url)
}

#' Scrape most shorted stocks from stockanalysis.com
#'
#' @return Character vector of ticker symbols
#' @noRd
scrape_most_shorted_tickers <- function() {
  url <- get_golem_config_value(
    "custom_ticker_lists",
    "most_shorted_url",
    "https://stockanalysis.com/list/most-shorted-stocks/"
  )

  scrape_stockanalysis_tickers(url)
}

#' Scrape 2x leveraged ETFs from 2xetfs.com
#'
#' @return Character vector of ticker symbols
#' @noRd
scrape_2x_leveraged_etfs <- function() {
  url <- get_golem_config_value(
    "custom_ticker_lists",
    "leveraged_2x_url",
    "https://www.2xetfs.com/"
  )

  # Fetch and parse HTML
  page <- read_html(url)

  # Extract all link text
  all_links <- page %>%
    html_elements('a') %>%
    html_text()

  # Filter for ticker pattern (2-5 uppercase letters)
  tickers <- all_links[grepl('^[A-Z]{2,5}$', all_links)]
  tickers <- unique(tickers)

  return(tickers)
}

#' Scrape 3x leveraged ETFs from 3xetf.com
#'
#' @return Character vector of ticker symbols
#' @noRd
scrape_3x_leveraged_etfs <- function() {
  url <- get_golem_config_value(
    "custom_ticker_lists",
    "leveraged_3x_url",
    "https://3xetf.com/"
  )

  # Fetch and parse HTML
  page <- read_html(url)

  # Extract all link text
  all_links <- page %>%
    html_elements('a') %>%
    html_text()

  # Filter for ticker pattern (2-5 uppercase letters)
  tickers <- all_links[grepl('^[A-Z]{2,5}$', all_links)]
  tickers <- unique(tickers)

  return(tickers)
}

################################################################################
# FETCH FUNCTIONS (with caching)
################################################################################

#' Fetch overbought stocks tickers
#'
#' Scrapes stockanalysis.com for current list of overbought stocks.
#' Results are cached for 24 hours (configurable).
#'
#' @param force_refresh Logical. If TRUE, ignores cache and fetches fresh data.
#' @return Character vector of ticker symbols (empty if scraping fails)
#' @export
#'
#' @examples
#' \dontrun{
#'   tickers <- fetch_overbought_tickers()
#'   tickers <- fetch_overbought_tickers(force_refresh = TRUE)
#' }
fetch_overbought_tickers <- function(force_refresh = FALSE) {
  cache_hours <- get_golem_config_value("custom_ticker_lists", "ticker_cache_hours", 24)

  # Check cache
  if (!force_refresh && collar_cache_is_valid(.overbought_cache, cache_hours)) {
    log_info("Using cached overbought stocks ({length(.overbought_cache$tickers)} tickers)")
    return(.overbought_cache$tickers)
  }

  # Fetch fresh data
  log_info("Fetching overbought stocks from web...")

  tickers <- tryCatch(
    {
      scrape_overbought_tickers()
    },
    error = function(e) {
      log_error("Failed to fetch overbought tickers: {e$message}")
      NULL
    }
  )

  # Return empty vector if scraping failed (no fallback)
  if (is.null(tickers) || length(tickers) == 0) {
    log_warn("No overbought stocks fetched from web")
    return(character(0))
  }

  # Update cache
  .overbought_cache$tickers <- tickers
  .overbought_cache$timestamp <- Sys.time()

  log_info("Successfully fetched {length(tickers)} overbought stocks")

  return(tickers)
}

#' Fetch oversold stocks tickers
#'
#' Scrapes stockanalysis.com for current list of oversold stocks.
#' Results are cached for 24 hours (configurable).
#'
#' @param force_refresh Logical. If TRUE, ignores cache and fetches fresh data.
#' @return Character vector of ticker symbols (empty if scraping fails)
#' @export
#'
#' @examples
#' \dontrun{
#'   tickers <- fetch_oversold_tickers()
#'   tickers <- fetch_oversold_tickers(force_refresh = TRUE)
#' }
fetch_oversold_tickers <- function(force_refresh = FALSE) {
  cache_hours <- get_golem_config_value("custom_ticker_lists", "ticker_cache_hours", 24)

  # Check cache
  if (!force_refresh && collar_cache_is_valid(.oversold_cache, cache_hours)) {
    log_info("Using cached oversold stocks ({length(.oversold_cache$tickers)} tickers)")
    return(.oversold_cache$tickers)
  }

  # Fetch fresh data
  log_info("Fetching oversold stocks from web...")

  tickers <- tryCatch(
    {
      scrape_oversold_tickers()
    },
    error = function(e) {
      log_error("Failed to fetch oversold tickers: {e$message}")
      NULL
    }
  )

  # Return empty vector if scraping failed (no fallback)
  if (is.null(tickers) || length(tickers) == 0) {
    log_warn("No oversold stocks fetched from web")
    return(character(0))
  }

  # Update cache
  .oversold_cache$tickers <- tickers
  .oversold_cache$timestamp <- Sys.time()

  log_info("Successfully fetched {length(tickers)} oversold stocks")

  return(tickers)
}

#' Fetch most shorted stocks tickers
#'
#' Scrapes stockanalysis.com for current list of most shorted stocks.
#' Results are cached for 24 hours (configurable).
#'
#' @param force_refresh Logical. If TRUE, ignores cache and fetches fresh data.
#' @return Character vector of ticker symbols (empty if scraping fails)
#' @export
#'
#' @examples
#' \dontrun{
#'   tickers <- fetch_most_shorted_tickers()
#'   tickers <- fetch_most_shorted_tickers(force_refresh = TRUE)
#' }
fetch_most_shorted_tickers <- function(force_refresh = FALSE) {
  cache_hours <- get_golem_config_value("custom_ticker_lists", "ticker_cache_hours", 24)

  # Check cache
  if (!force_refresh && collar_cache_is_valid(.most_shorted_cache, cache_hours)) {
    log_info("Using cached most shorted stocks ({length(.most_shorted_cache$tickers)} tickers)")
    return(.most_shorted_cache$tickers)
  }

  # Fetch fresh data
  log_info("Fetching most shorted stocks from web...")

  tickers <- tryCatch(
    {
      scrape_most_shorted_tickers()
    },
    error = function(e) {
      log_error("Failed to fetch most shorted tickers: {e$message}")
      NULL
    }
  )

  # Return empty vector if scraping failed (no fallback)
  if (is.null(tickers) || length(tickers) == 0) {
    log_warn("No most shorted stocks fetched from web")
    return(character(0))
  }

  # Update cache
  .most_shorted_cache$tickers <- tickers
  .most_shorted_cache$timestamp <- Sys.time()

  log_info("Successfully fetched {length(tickers)} most shorted stocks")

  return(tickers)
}

#' Fetch 2x leveraged ETF tickers
#'
#' Scrapes 2xetfs.com for current list of 2x leveraged ETFs.
#' Results are cached for 24 hours (configurable).
#'
#' @param force_refresh Logical. If TRUE, ignores cache and fetches fresh data.
#' @return Character vector of ticker symbols (empty if scraping fails)
#' @export
#'
#' @examples
#' \dontrun{
#'   tickers <- fetch_2x_leveraged_etfs()
#'   tickers <- fetch_2x_leveraged_etfs(force_refresh = TRUE)
#' }
fetch_2x_leveraged_etfs <- function(force_refresh = FALSE) {
  cache_hours <- get_golem_config_value("custom_ticker_lists", "ticker_cache_hours", 24)

  # Check cache
  if (!force_refresh && collar_cache_is_valid(.leveraged_2x_cache, cache_hours)) {
    log_info("Using cached 2x leveraged ETFs ({length(.leveraged_2x_cache$tickers)} tickers)")
    return(.leveraged_2x_cache$tickers)
  }

  # Fetch fresh data
  log_info("Fetching 2x leveraged ETFs from web...")

  tickers <- tryCatch(
    {
      scrape_2x_leveraged_etfs()
    },
    error = function(e) {
      log_error("Failed to fetch 2x leveraged ETF tickers: {e$message}")
      NULL
    }
  )

  # Return empty vector if scraping failed (no fallback)
  if (is.null(tickers) || length(tickers) == 0) {
    log_warn("No 2x leveraged ETFs fetched from web")
    return(character(0))
  }

  # Update cache
  .leveraged_2x_cache$tickers <- tickers
  .leveraged_2x_cache$timestamp <- Sys.time()

  log_info("Successfully fetched {length(tickers)} 2x leveraged ETFs")

  return(tickers)
}

#' Fetch 3x leveraged ETF tickers
#'
#' Scrapes 3xetf.com for current list of 3x leveraged ETFs.
#' Results are cached for 24 hours (configurable).
#'
#' @param force_refresh Logical. If TRUE, ignores cache and fetches fresh data.
#' @return Character vector of ticker symbols (empty if scraping fails)
#' @export
#'
#' @examples
#' \dontrun{
#'   tickers <- fetch_3x_leveraged_etfs()
#'   tickers <- fetch_3x_leveraged_etfs(force_refresh = TRUE)
#' }
fetch_3x_leveraged_etfs <- function(force_refresh = FALSE) {
  cache_hours <- get_golem_config_value("custom_ticker_lists", "ticker_cache_hours", 24)

  # Check cache
  if (!force_refresh && collar_cache_is_valid(.leveraged_3x_cache, cache_hours)) {
    log_info("Using cached 3x leveraged ETFs ({length(.leveraged_3x_cache$tickers)} tickers)")
    return(.leveraged_3x_cache$tickers)
  }

  # Fetch fresh data
  log_info("Fetching 3x leveraged ETFs from web...")

  tickers <- tryCatch(
    {
      scrape_3x_leveraged_etfs()
    },
    error = function(e) {
      log_error("Failed to fetch 3x leveraged ETF tickers: {e$message}")
      NULL
    }
  )

  # Return empty vector if scraping failed (no fallback)
  if (is.null(tickers) || length(tickers) == 0) {
    log_warn("No 3x leveraged ETFs fetched from web")
    return(character(0))
  }

  # Update cache
  .leveraged_3x_cache$tickers <- tickers
  .leveraged_3x_cache$timestamp <- Sys.time()

  log_info("Successfully fetched {length(tickers)} 3x leveraged ETFs")

  return(tickers)
}
