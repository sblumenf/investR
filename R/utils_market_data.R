#' Market Data Fetching Functions
#'
#' Centralized wrappers around quantmod functions for fetching market data
#' from Yahoo Finance. Provides consistent error handling and logging across
#' all strategies.
#'
#' @name market-data
#' @importFrom quantmod getSymbols getDividends getQuote yahooQF getOptionChain
#' @importFrom logger log_debug log_warn
#' @importFrom readr read_csv cols col_date col_double
#' @importFrom dplyr filter slice_tail pull
NULL

#' Fetch price history from Yahoo Finance
#'
#' Wrapper around getSymbols() with consistent error handling
#'
#' @param ticker Character ticker symbol
#' @param from Start date (Date or character in "YYYY-MM-DD" format)
#' @param to End date (Date or character in "YYYY-MM-DD" format), defaults to today
#' @param auto_adjust Logical, adjust for splits/dividends (default FALSE for raw prices)
#' @return xts object with OHLCV data, or NULL on error
#' @export
fetch_price_history <- function(ticker,
                                from = "1900-01-01",
                                to = Sys.Date(),
                                auto_adjust = FALSE) {
  tryCatch({
    log_debug("Fetching price history for {ticker}")

    price_data <- getSymbols(
      ticker,
      src = "yahoo",
      from = from,
      to = to,
      auto.assign = FALSE,
      warnings = FALSE
    )

    # Adjust prices if requested
    if (auto_adjust && !is.null(price_data)) {
      # quantmod::adjustOHLC would be used here, but we default to FALSE
      # for dividend capture we need raw prices
    }

    return(price_data)

  }, error = function(e) {
    log_warn("{ticker}: Failed to fetch price history - {e$message}")
    return(NULL)
  })
}

#' Fetch dividend history from Yahoo Finance
#'
#' Wrapper around getDividends() with consistent error handling
#'
#' @param ticker Character ticker symbol
#' @param from Start date (Date or character in "YYYY-MM-DD" format)
#' @param to End date (Date or character in "YYYY-MM-DD" format), defaults to today
#' @return xts object with dividend amounts and dates, or NULL on error
#' @export
fetch_dividend_history <- function(ticker,
                                   from = "1900-01-01",
                                   to = Sys.Date()) {
  tryCatch({
    log_debug("Fetching dividend history for {ticker}")

    div_data <- getDividends(
      ticker,
      from = from,
      to = to,
      auto.assign = FALSE
    )

    return(div_data)

  }, error = function(e) {
    log_warn("{ticker}: Failed to fetch dividend history - {e$message}")
    return(NULL)
  })
}

#' Fetch current quote from Yahoo Finance
#'
#' Internal Yahoo-specific quote fetcher.
#' Wrapper around getQuote() with consistent error handling.
#'
#' @param ticker Character ticker symbol (or vector of tickers)
#' @param fields Character vector of fields to fetch. Common fields:
#'   - "Last Trade (Price Only)" - current price
#'   - "Name" - company name
#'   - "Dividend Yield" - dividend yield
#' @return Data frame with quote data, or NULL on error
#' @noRd
fetch_current_quote_yahoo <- function(ticker, fields = c("Last Trade (Price Only)", "Name")) {
  tryCatch({
    log_debug("Fetching current quote for {ticker} from Yahoo Finance")

    quote_data <- getQuote(ticker, what = yahooQF(fields))

    return(quote_data)

  }, error = function(e) {
    log_warn("{ticker}: Failed to fetch quote from Yahoo - {e$message}")
    return(NULL)
  })
}

#' Fetch current quote from configured source
#'
#' Source-aware quote fetcher that delegates to Yahoo Finance or Questrade API
#' based on the global option 'investR.quote_source'.
#'
#' The quote source is controlled by strategy modules via the quote source toggle.
#' Defaults to Yahoo Finance if no source is specified.
#'
#' @param ticker Character ticker symbol (or vector of tickers)
#' @param fields Character vector of fields to fetch (for Yahoo compatibility)
#' @return Data frame with quote data, or NULL on error
#' @export
fetch_current_quote <- function(ticker, fields = c("Last Trade (Price Only)", "Name")) {
  # Get configured quote source (defaults to "yahoo")
  source <- getOption("investR.quote_source", default = "yahoo")

  if (source == "questrade") {
    # Use Questrade API (with automatic Yahoo fallback on error)
    return(fetch_questrade_quote(ticker, fields))
  } else {
    # Use Yahoo Finance (default)
    return(fetch_current_quote_yahoo(ticker, fields))
  }
}

#' Fetch options chain from Yahoo Finance
#'
#' Wrapper around getOptionChain() with consistent error handling
#'
#' @param ticker Character ticker symbol
#' @param expiration Optional specific expiration date, NULL for all
#' @return List with calls and puts for each expiration, or NULL on error
#' @export
fetch_options_chain <- function(ticker, expiration = NULL) {
  tryCatch({
    log_debug("Fetching options chain for {ticker}")

    opt_chain <- getOptionChain(ticker, Exp = expiration)

    return(opt_chain)

  }, error = function(e) {
    log_warn("{ticker}: Failed to fetch options chain - {e$message}")
    return(NULL)
  })
}

#' Fetch current SOFR rate from FRED
#'
#' Retrieves Secured Overnight Financing Rate from Federal Reserve Economic Data.
#' Used as risk-free rate in Sharpe ratio calculations.
#'
#' @param fallback_rate Fallback rate if fetch fails (default 0.0414 = 4.14%)
#' @return Numeric annual SOFR rate as decimal (0.05 = 5%)
#' @export
fetch_sofr_rate <- function(fallback_rate = 0.0414) {
  tryCatch({
    log_debug("Fetching SOFR rate from FRED")

    csv_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=SOFR"

    sofr_data <- read_csv(
      csv_url,
      col_types = cols(
        DATE = col_date(),
        SOFR = col_double()
      ),
      show_col_types = FALSE
    )

    # Get most recent non-null rate
    latest_rate <- sofr_data %>%
      filter(!is.na(SOFR)) %>%
      slice_tail(n = 1) %>%
      pull(SOFR)

    if (length(latest_rate) == 0 || is.na(latest_rate)) {
      log_warn("SOFR data is empty or NA, using fallback rate: {fallback_rate * 100}%")
      return(fallback_rate)
    }

    # Convert from percentage to decimal
    rate <- latest_rate / 100

    log_debug("SOFR rate: {rate * 100}%")
    return(rate)

  }, error = function(e) {
    log_warn("Failed to fetch SOFR rate, using fallback: {fallback_rate * 100}% - {e$message}")
    return(fallback_rate)
  })
}
