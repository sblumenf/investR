#' Cash Equivalent (Money Market) Ticker Utilities
#'
#' Functions for working with cash equivalent/money market tickers.
#' These tickers receive special treatment in portfolio analysis:
#' - No expiration date required
#' - Expected returns based on SEC yield
#' - Minimal time horizon in Monte Carlo simulations
#'
#' @name cash-equivalent-utils
#' @importFrom logger log_debug log_warn
NULL

#' Get cash equivalent tickers from config
#'
#' Returns the list of money market/cash equivalent tickers from golem-config.yml.
#' Falls back to legacy defaults if config is unavailable.
#'
#' @return Character vector of cash equivalent ticker symbols (uppercase)
#' @export
get_cash_equivalent_tickers <- function() {
  tickers <- tryCatch({
    golem::get_golem_config("cash_equivalents")$tickers
  }, error = function(e) {
    log_warn("Cash Equivalents: Failed to read config, using defaults - {e$message}")
    NULL
  })

 if (is.null(tickers) || length(tickers) == 0) {
    # Fallback for backwards compatibility
    log_debug("Cash Equivalents: Using fallback tickers (config not found)")
    return(c("SGOV", "ZMMK.TO"))
  }

  # Normalize to uppercase for consistent comparison
 toupper(tickers)
}

#' Check if a ticker is a cash equivalent
#'
#' @param ticker Ticker symbol to check (case-insensitive)
#' @return Logical TRUE if ticker is a cash equivalent/money market fund
#' @export
is_cash_equivalent <- function(ticker) {
  if (is.null(ticker) || is.na(ticker) || ticker == "") {
    return(FALSE)
  }
  toupper(ticker) %in% get_cash_equivalent_tickers()
}

#' Get SQL-ready list of cash equivalent tickers
#'
#' Returns a string formatted for SQL IN clauses, e.g., ('SGOV', 'ZMMK.TO', 'BIL')
#'
#' @return String suitable for SQL IN clause
#' @noRd
get_cash_equivalent_sql_list <- function() {
  tickers <- get_cash_equivalent_tickers()
  paste0("('", paste(tickers, collapse = "', '"), "')")
}
