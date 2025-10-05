#' Aristocrats Strategy Helper Functions
#'
#' Reusable utility functions for validation, calculation, and formatting
#'
#' @name aristocrats-helpers
NULL

#' Find ticker column in a data frame
#'
#' Searches for common ticker column name variations
#'
#' @param table_names Character vector of column names
#' @param possible_names Character vector of possible ticker column names
#' @return Column name if found, NULL otherwise
#' @noRd
find_ticker_column <- function(table_names,
                               possible_names = ARISTOCRATS_CONFIG$ticker_column_names) {
  match <- intersect(possible_names, table_names)
  if (length(match) > 0) match[1] else NULL
}

#' Validate ticker symbol
#'
#' @param ticker Character vector of length 1
#' @return TRUE if valid, throws error otherwise
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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


#' Get SGOV reinvestment rate with fallback
#'
#' Fetches current SGOV yield for dividend reinvestment calculations
#'
#' @return Numeric yield as decimal (0.05 = 5%)
#' @noRd
get_reinvestment_rate <- function() {
  rate <- tryCatch({
    sgov <- fetch_current_quote("SGOV", fields = "Dividend Yield")
    yield <- as.numeric(sgov$`Dividend Yield`)

    if (!is.na(yield) && yield > 0 && yield < ARISTOCRATS_CONFIG$max_sgov_yield_sanity) {
      return(yield)
    }

    ARISTOCRATS_CONFIG$sgov_yield_default
  }, error = function(e) {
    ARISTOCRATS_CONFIG$sgov_yield_default
  })

  return(rate)
}

#' Truncate error message
#'
#' @param msg Error message
#' @param max_chars Maximum characters (defaults to ARISTOCRATS_CONFIG$error_truncate_length)
#' @return Truncated message
#' @noRd
truncate_error <- function(msg, max_chars = ARISTOCRATS_CONFIG$error_truncate_length) {
  if (nchar(msg) > max_chars) {
    paste0(substr(msg, 1, max_chars), "...")
  } else {
    msg
  }
}

#' Safely execute expression with standardized error handling
#'
#' Wrapper around tryCatch that provides consistent error handling pattern
#' for data fetching operations. Logs errors and returns default value on failure.
#'
#' @param expr Expression to evaluate
#' @param error_msg Error message prefix (will be combined with actual error)
#' @param default Default value to return on error (default: NULL)
#' @param log_level Logging level: "warn", "error", "debug" (default: "warn")
#' @param truncate_errors Whether to truncate error messages in logs (default: TRUE)
#' @return Result of expr on success, default on failure
#' @importFrom logger log_warn log_error log_debug
#' @noRd
safely_fetch <- function(expr,
                        error_msg,
                        default = NULL,
                        log_level = "warn",
                        truncate_errors = TRUE) {

  tryCatch({
    eval(substitute(expr), parent.frame())
  }, error = function(e) {
    # Format full error message
    full_error <- paste0(error_msg, ": ", e$message)

    # If truncating, log full error at debug level first to preserve it
    if (truncate_errors) {
      log_debug("Full error: {full_error}")
    }

    # Optionally truncate for main logging
    log_error_msg <- if (truncate_errors) {
      truncate_error(full_error, max_chars = ARISTOCRATS_CONFIG$error_truncate_length)
    } else {
      full_error
    }

    # Log based on level
    if (log_level == "error") {
      log_error(log_error_msg)
    } else if (log_level == "debug") {
      log_debug(log_error_msg)
    } else {
      log_warn(log_error_msg)
    }

    return(default)
  })
}