#' Formatting Functions
#'
#' Consistent formatting for display across all strategies.
#' Ensures uniform presentation of financial data.
#'
#' @name formatting
NULL

#' Format numeric value as currency with thousands separator
#'
#' @param value Numeric value to format
#' @param digits Number of decimal places (default 2)
#' @param prefix Currency symbol prefix (default "$")
#' @return Character string formatted as currency
#' @export
#' @examples
#' \dontrun{
#'   format_currency(1234.56)       # "$1,234.56"
#'   format_currency(1234.567, 3)   # "$1,234.567"
#' }
format_currency <- function(value, digits = 2, prefix = "$") {
  if (is.na(value) || !is.numeric(value)) {
    return("N/A")
  }

  formatted <- sprintf(
    "%s%s",
    prefix,
    format(round(value, digits), big.mark = ",", nsmall = digits, scientific = FALSE)
  )

  return(formatted)
}

#' Format numeric value as percentage
#'
#' @param value Numeric value as decimal (0.15 = 15%)
#' @param digits Number of decimal places (default 2)
#' @return Character string formatted as percentage
#' @export
#' @examples
#' \dontrun{
#'   format_percentage(0.1534)      # "15.34%"
#'   format_percentage(0.1534, 1)   # "15.3%"
#' }
format_percentage <- function(value, digits = 2) {
  if (is.na(value) || !is.numeric(value)) {
    return("N/A")
  }

  formatted <- sprintf(
    "%.*f%%",
    digits,
    value * 100
  )

  return(formatted)
}

#' Format date consistently
#'
#' @param date Date object or character date
#' @param format Date format string (default "%Y-%m-%d")
#' @return Character string formatted date
#' @export
#' @examples
#' \dontrun{
#'   format_date(Sys.Date())                      # "2025-09-30"
#'   format_date(Sys.Date(), "%b %d, %Y")         # "Sep 30, 2025"
#' }
format_date <- function(date, format = "%Y-%m-%d") {
  if (is.na(date)) {
    return("N/A")
  }

  tryCatch({
    formatted <- format(as.Date(date), format = format)
    return(formatted)
  }, error = function(e) {
    return("N/A")
  })
}

#' Format large number with abbreviations
#'
#' Converts large numbers to K (thousands), M (millions), B (billions)
#'
#' @param value Numeric value to format
#' @param digits Number of decimal places (default 1)
#' @return Character string with abbreviated number
#' @export
#' @examples
#' \dontrun{
#'   format_number(1234)           # "1.2K"
#'   format_number(1234567)        # "1.2M"
#'   format_number(1234567890)     # "1.2B"
#' }
format_number <- function(value, digits = 1) {
  if (is.na(value) || !is.numeric(value)) {
    return("N/A")
  }

  abs_value <- abs(value)
  sign <- if (value < 0) "-" else ""

  if (abs_value >= 1e9) {
    formatted <- sprintf("%s%.*fB", sign, digits, abs_value / 1e9)
  } else if (abs_value >= 1e6) {
    formatted <- sprintf("%s%.*fM", sign, digits, abs_value / 1e6)
  } else if (abs_value >= 1e3) {
    formatted <- sprintf("%s%.*fK", sign, digits, abs_value / 1e3)
  } else {
    formatted <- sprintf("%s%.*f", sign, digits, abs_value)
  }

  return(formatted)
}

#' Format number with thousands separator
#'
#' Simple formatting with commas for thousands
#'
#' @param value Numeric value to format
#' @param digits Number of decimal places (default 0)
#' @return Character string with comma separators
#' @export
#' @examples
#' \dontrun{
#'   format_with_commas(1234567)        # "1,234,567"
#'   format_with_commas(1234.56, 2)     # "1,234.56"
#' }
format_with_commas <- function(value, digits = 0) {
  if (is.na(value) || !is.numeric(value)) {
    return("N/A")
  }

  formatted <- format(
    round(value, digits),
    big.mark = ",",
    nsmall = digits,
    scientific = FALSE
  )

  return(formatted)
}
