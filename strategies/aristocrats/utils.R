################################################################################
# Utility Functions
#
# Reusable helper functions for the covered call aristocrat strategy
################################################################################

#' Find ticker column in a data frame
#'
#' Searches for common ticker column name variations
#'
#' @param table_names Character vector of column names
#' @param possible_names Character vector of possible ticker column names
#' @return Column name if found, NULL otherwise
find_ticker_column <- function(table_names,
                               possible_names = CONFIG$ticker_column_names) {
  match <- intersect(possible_names, table_names)
  if (length(match) > 0) match[1] else NULL
}

#' Validate ticker symbol
#'
#' @param ticker Character vector of length 1
#' @return TRUE if valid, throws error otherwise
validate_ticker <- function(ticker) {
  if (!is.character(ticker)) stop("ticker must be character")
  if (length(ticker) != 1) stop("ticker must be length 1")
  if (nchar(ticker) == 0) stop("ticker must not be empty")
  if (!grepl("^[A-Z.]+$", ticker)) stop("ticker must be uppercase letters/dots")
  invisible(TRUE)
}

#' Validate price
#'
#' @param price Numeric value
#' @param name Name of the price field (for error messages)
#' @return TRUE if valid, throws error otherwise
validate_price <- function(price, name = "price") {
  if (is.na(price)) stop(sprintf("%s must not be NA", name))
  if (!is.numeric(price)) stop(sprintf("%s must be numeric", name))
  if (price <= 0) stop(sprintf("%s must be positive", name))
  invisible(TRUE)
}

#' Validate percentage
#'
#' @param pct Numeric value between 0 and 1
#' @param name Name of the percentage field
#' @return TRUE if valid, throws error otherwise
validate_percentage <- function(pct, name = "percentage") {
  if (is.na(pct)) stop(sprintf("%s must not be NA", name))
  if (!is.numeric(pct)) stop(sprintf("%s must be numeric", name))
  if (pct < 0 || pct > 1) stop(sprintf("%s must be between 0 and 1", name))
  invisible(TRUE)
}

#' Validate data frame has required columns
#'
#' @param df Data frame to validate
#' @param required_cols Character vector of required column names
#' @param df_name Name of data frame (for error messages)
#' @return TRUE if valid, throws error otherwise
validate_columns <- function(df, required_cols, df_name = "data frame") {
  if (!is.data.frame(df)) {
    stop(sprintf("%s must be a data frame or tibble", df_name))
  }

  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(sprintf("%s missing required columns: %s",
                 df_name, paste(missing, collapse = ", ")))
  }
  invisible(TRUE)
}

#' Calculate annualized return
#'
#' @param total_profit Total profit amount
#' @param capital_required Initial capital investment
#' @param days Number of days for the investment
#' @return Annualized return as decimal (0.10 = 10%)
calculate_annualized_return <- function(total_profit, capital_required, days) {
  if (capital_required <= 0 || days <= 0) {
    return(0)
  }

  years <- days / CONFIG$days_per_year
  total_return <- total_profit / capital_required

  # Annualized return = (1 + total_return) ^ (1/years) - 1
  annualized <- if (years > 0) (1 + total_return)^(1/years) - 1 else 0

  return(annualized)
}

#' Get SGOV reinvestment rate with fallback
#'
#' Fetches current SGOV yield for dividend reinvestment calculations
#'
#' @return Numeric yield as decimal (0.05 = 5%)
get_reinvestment_rate <- function() {
  rate <- tryCatch({
    sgov <- getQuote("SGOV", what = yahooQF("Dividend Yield"))
    yield <- as.numeric(sgov$`Dividend Yield`)

    if (!is.na(yield) && yield > 0 && yield < 0.20) {  # Sanity check < 20%
      return(yield)
    }

    CONFIG$sgov_yield_default
  }, error = function(e) {
    CONFIG$sgov_yield_default
  })

  return(rate)
}

#' Format timestamp for filenames
#'
#' @return Character timestamp in format YYYYMMDD_HHMMSS
format_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

#' Truncate error message
#'
#' @param msg Error message
#' @param max_chars Maximum characters (default 50)
#' @return Truncated message
truncate_error <- function(msg, max_chars = 50) {
  if (nchar(msg) > max_chars) {
    paste0(substr(msg, 1, max_chars), "...")
  } else {
    msg
  }
}