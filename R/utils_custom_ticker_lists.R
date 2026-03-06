#' Strategy-Agnostic Custom Ticker Lists
#'
#' Functions for fetching ticker lists from various sources for custom
#' strategy variants. Supports overbought/oversold/most shorted stocks
#' and leveraged ETFs. Used by both collar and zero-dividend strategies.
#'
#' @name custom-ticker-lists
#' @importFrom rvest read_html html_elements html_attr html_node html_table html_text
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

################################################################################
# FINVIZ SCREENED TICKERS
################################################################################

# Session-level cache for Finviz screened tickers
.finviz_screened_cache <- new.env(parent = emptyenv())

#' Scrape a single Finviz screener page (with pagination)
#'
#' Fetches all result pages for a Finviz screener URL and extracts ticker symbols.
#' Handles pagination automatically (20 results per page).
#'
#' @param url Finviz screener URL
#' @param delay Seconds between paginated requests (default 0.3)
#' @return Character vector of ticker symbols
#' @noRd
scrape_finviz_page <- function(url, delay = 0.3) {
  # Fetch first page
  first_page <- rvest::read_html(url)

  # Get total result count from #screener-total element
  total_text <- first_page %>%
    rvest::html_node("#screener-total") %>%
    rvest::html_text()

  if (is.na(total_text)) {
    log_warn("Could not find result count on Finviz page: {url}")
    return(character(0))
  }

  nums <- regmatches(total_text, gregexpr("[0-9]+", total_text))[[1]]
  total_results <- as.integer(nums[length(nums)])

  if (length(nums) == 0 || is.na(total_results) || total_results == 0) {
    return(character(0))
  }

  total_pages <- ceiling(total_results / 20)

  # Parse first page table
  all_tickers <- character(0)
  tbl <- tryCatch(
    first_page %>%
      rvest::html_node("table.screener_table") %>%
      rvest::html_table(),
    error = function(e) NULL
  )

  if (!is.null(tbl) && "Ticker" %in% names(tbl)) {
    all_tickers <- c(all_tickers, tbl$Ticker)
  }

  # Fetch remaining pages
  if (total_pages > 1) {
    for (p in 2:total_pages) {
      Sys.sleep(delay)
      r_param <- (p - 1) * 20 + 1
      page_url <- paste0(url, "&r=", r_param)

      page_tbl <- tryCatch({
        page_html <- rvest::read_html(page_url)
        page_html %>%
          rvest::html_node("table.screener_table") %>%
          rvest::html_table()
      }, error = function(e) {
        log_warn("Failed to fetch Finviz page {p}: {e$message}")
        NULL
      })

      if (is.null(page_tbl) || nrow(page_tbl) == 0) {
        log_warn("Empty table on Finviz page {p}, skipping to next")
        next
      }
      if ("Ticker" %in% names(page_tbl)) {
        all_tickers <- c(all_tickers, page_tbl$Ticker)
      }
    }
  }

  unique(toupper(all_tickers))
}

#' Fetch Finviz screened tickers
#'
#' Scrapes all configured Finviz screener URLs, extracts ticker symbols,
#' and returns a deduplicated list. Results are cached for 24 hours.
#'
#' @param force_refresh Logical. If TRUE, ignores cache and fetches fresh data.
#' @return Character vector of unique ticker symbols (empty if all scraping fails)
#' @export
#'
#' @examples
#' \dontrun{
#'   tickers <- fetch_finviz_screened_tickers()
#'   tickers <- fetch_finviz_screened_tickers(force_refresh = TRUE)
#' }
fetch_finviz_screened_tickers <- function(force_refresh = FALSE) {
  cache_hours <- get_golem_config_value("custom_ticker_lists", "ticker_cache_hours", 24)

  # Check cache
  if (!force_refresh && collar_cache_is_valid(.finviz_screened_cache, cache_hours)) {
    log_info("Using cached Finviz screened tickers ({length(.finviz_screened_cache$tickers)} tickers)")
    return(.finviz_screened_cache$tickers)
  }

  # Get URLs from config
  urls <- get_golem_config_value("custom_ticker_lists", "finviz_screener_urls", list())

  if (length(urls) == 0) {
    log_warn("No Finviz screener URLs configured in golem-config.yml")
    return(character(0))
  }

  log_info("Fetching tickers from {length(urls)} Finviz screener URLs...")

  # Scrape each URL and collect tickers
  all_tickers <- character(0)
  success_count <- 0

  for (i in seq_along(urls)) {
    url <- urls[[i]]
    log_info("Scraping Finviz URL {i}/{length(urls)}...")

    page_tickers <- tryCatch(
      scrape_finviz_page(url),
      error = function(e) {
        log_error("Failed to scrape Finviz URL {i}: {e$message}")
        character(0)
      }
    )

    if (length(page_tickers) > 0) {
      all_tickers <- c(all_tickers, page_tickers)
      success_count <- success_count + 1
    }

    # Brief delay between different screener URLs
    if (i < length(urls)) Sys.sleep(0.5)
  }

  # Deduplicate
  tickers <- unique(toupper(all_tickers))

  if (length(tickers) == 0) {
    log_warn("No tickers fetched from any Finviz screener URL")
    return(character(0))
  }

  # Update cache
  .finviz_screened_cache$tickers <- tickers
  .finviz_screened_cache$timestamp <- Sys.time()

  log_info("Successfully fetched {length(tickers)} unique tickers from {success_count}/{length(urls)} Finviz screens")

  return(tickers)
}

################################################################################
# FINVIZ CALL SKEW TICKERS
################################################################################

# Session-level caches for call skew variants
.finviz_call_skew_div_cache <- new.env(parent = emptyenv())
.finviz_call_skew_nodiv_cache <- new.env(parent = emptyenv())

#' Fetch Finviz call skew dividend tickers
#'
#' Scrapes all configured Finviz call skew screener URLs (dividend payers),
#' extracts ticker symbols, and returns a deduplicated list.
#' Results are cached for 24 hours.
#'
#' @param force_refresh Logical. If TRUE, ignores cache and fetches fresh data.
#' @return Character vector of unique ticker symbols (empty if all scraping fails)
#' @export
#'
#' @examples
#' \dontrun{
#'   tickers <- fetch_finviz_call_skew_div_tickers()
#'   tickers <- fetch_finviz_call_skew_div_tickers(force_refresh = TRUE)
#' }
fetch_finviz_call_skew_div_tickers <- function(force_refresh = FALSE) {
  cache_hours <- get_golem_config_value("custom_ticker_lists", "ticker_cache_hours", 24)

  if (!force_refresh && collar_cache_is_valid(.finviz_call_skew_div_cache, cache_hours)) {
    log_info("Using cached Finviz call skew dividend tickers ({length(.finviz_call_skew_div_cache$tickers)} tickers)")
    return(.finviz_call_skew_div_cache$tickers)
  }

  urls <- get_golem_config_value("custom_ticker_lists", "finviz_call_skew_div_urls", list())

  if (length(urls) == 0) {
    log_warn("No Finviz call skew dividend URLs configured in golem-config.yml")
    return(character(0))
  }

  log_info("Fetching tickers from {length(urls)} Finviz call skew dividend URLs...")

  all_tickers <- character(0)
  success_count <- 0

  for (i in seq_along(urls)) {
    url <- urls[[i]]
    log_info("Scraping Finviz call skew div URL {i}/{length(urls)}...")

    page_tickers <- tryCatch(
      scrape_finviz_page(url),
      error = function(e) {
        log_warn("Failed to scrape Finviz call skew div URL {i}: {e$message}")
        character(0)
      }
    )

    if (length(page_tickers) > 0) {
      all_tickers <- c(all_tickers, page_tickers)
      success_count <- success_count + 1
    }

    if (i < length(urls)) Sys.sleep(0.5)
  }

  tickers <- unique(toupper(all_tickers))

  if (length(tickers) == 0) {
    log_warn("No tickers fetched from any Finviz call skew dividend URL")
    return(character(0))
  }

  .finviz_call_skew_div_cache$tickers <- tickers
  .finviz_call_skew_div_cache$timestamp <- Sys.time()

  log_info("Successfully fetched {length(tickers)} unique tickers from {success_count}/{length(urls)} Finviz call skew dividend screens")

  return(tickers)
}

#' Fetch Finviz call skew non-dividend tickers
#'
#' Scrapes all configured Finviz call skew screener URLs (non-dividend payers),
#' extracts ticker symbols, and returns a deduplicated list.
#' Results are cached for 24 hours.
#'
#' @param force_refresh Logical. If TRUE, ignores cache and fetches fresh data.
#' @return Character vector of unique ticker symbols (empty if all scraping fails)
#' @export
#'
#' @examples
#' \dontrun{
#'   tickers <- fetch_finviz_call_skew_nodiv_tickers()
#'   tickers <- fetch_finviz_call_skew_nodiv_tickers(force_refresh = TRUE)
#' }
fetch_finviz_call_skew_nodiv_tickers <- function(force_refresh = FALSE) {
  cache_hours <- get_golem_config_value("custom_ticker_lists", "ticker_cache_hours", 24)

  if (!force_refresh && collar_cache_is_valid(.finviz_call_skew_nodiv_cache, cache_hours)) {
    log_info("Using cached Finviz call skew non-dividend tickers ({length(.finviz_call_skew_nodiv_cache$tickers)} tickers)")
    return(.finviz_call_skew_nodiv_cache$tickers)
  }

  urls <- get_golem_config_value("custom_ticker_lists", "finviz_call_skew_nodiv_urls", list())

  if (length(urls) == 0) {
    log_warn("No Finviz call skew non-dividend URLs configured in golem-config.yml")
    return(character(0))
  }

  log_info("Fetching tickers from {length(urls)} Finviz call skew non-dividend URLs...")

  all_tickers <- character(0)
  success_count <- 0

  for (i in seq_along(urls)) {
    url <- urls[[i]]
    log_info("Scraping Finviz call skew nodiv URL {i}/{length(urls)}...")

    page_tickers <- tryCatch(
      scrape_finviz_page(url),
      error = function(e) {
        log_warn("Failed to scrape Finviz call skew nodiv URL {i}: {e$message}")
        character(0)
      }
    )

    if (length(page_tickers) > 0) {
      all_tickers <- c(all_tickers, page_tickers)
      success_count <- success_count + 1
    }

    if (i < length(urls)) Sys.sleep(0.5)
  }

  tickers <- unique(toupper(all_tickers))

  if (length(tickers) == 0) {
    log_warn("No tickers fetched from any Finviz call skew non-dividend URL")
    return(character(0))
  }

  .finviz_call_skew_nodiv_cache$tickers <- tickers
  .finviz_call_skew_nodiv_cache$timestamp <- Sys.time()

  log_info("Successfully fetched {length(tickers)} unique tickers from {success_count}/{length(urls)} Finviz call skew non-dividend screens")

  return(tickers)
}

################################################################################
# DRIP INVESTING
################################################################################

#' Fetch DRIP Investing dividend stocks dataset
#'
#' Downloads the full dataset from DRIPInvesting.org JSON API.
#' Caches the result as an RDS file in inst/cache/ for 30 days.
#'
#' @return A data frame with all DRIP Investing stocks
#' @export
fetch_drip_investing_data <- function() {
  cache_path <- system.file("cache", "drip_investing_data.rds", package = "investR")
  if (cache_path == "") {
    cache_path <- file.path("inst", "cache", "drip_investing_data.rds")
  }

  cache_days <- get_golem_config_value("aristocrats", "drip_cache_days", 30)

  if (file.exists(cache_path)) {
    cache_age_days <- as.numeric(
      difftime(Sys.time(), file.info(cache_path)$mtime, units = "days")
    )
    if (cache_age_days < cache_days) {
      log_info("Using cached DRIP Investing data ({round(cache_age_days, 1)} days old)")
      return(readRDS(cache_path))
    }
  }

  api_url <- get_golem_config_value(
    "aristocrats",
    "drip_api_url",
    "https://www.dripinvesting.org/wp-json/drip-investing/v1/stocks-dataset"
  )

  result <- tryCatch({
    response <- httr::GET(api_url, httr::user_agent("investR/1.0"))
    if (httr::status_code(response) != 200) {
      stop("DRIP Investing API returned HTTP ", httr::status_code(response))
    }
    parsed <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    dataset <- parsed$dataset
    if (!dir.exists(dirname(cache_path))) {
      dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    }
    saveRDS(dataset, cache_path)
    log_info("Successfully fetched {nrow(dataset)} stocks from DRIP Investing API")
    dataset
  }, error = function(e) {
    if (file.exists(cache_path)) {
      log_warn("DRIP Investing API fetch failed, returning stale cache: {e$message}")
      return(readRDS(cache_path))
    }
    stop("Failed to fetch DRIP Investing data and no cache available: ", e$message)
  })

  result
}

#' Get ticker symbols from DRIP Investing by dividend category
#'
#' @param category One of "King", "Champion", "Contender", "Challenger"
#' @return Character vector of ticker symbols
#' @export
get_drip_tickers <- function(category) {
  valid_categories <- c("King", "Champion", "Contender", "Challenger")
  if (!category %in% valid_categories) {
    stop("category must be one of: ", paste(valid_categories, collapse = ", "))
  }

  data <- fetch_drip_investing_data()

  tickers <- data %>%
    dplyr::filter(stock_type == category) %>%
    dplyr::pull(ticker) %>%
    as.character()

  if (length(tickers) == 0) {
    stop("No tickers found for DRIP Investing category: ", category)
  }

  log_info("Retrieved {length(tickers)} {category} tickers from DRIP Investing")

  tickers
}
