#' Volatility Skew Signal
#'
#' Computes IV skew between calls and puts at five delta levels using the
#' nearest 30-day options chain. Used as a real-time indicator of post-entry
#' stock move risk for deep ITM covered call positions.
#'
#' @name skew-signal
#' @importFrom logger log_debug log_info log_warn log_error
#' @importFrom dplyr %>% filter arrange mutate
#' @importFrom stats pnorm median
NULL

TARGET_DELTAS <- c(0.20, 0.30, 0.40, 0.50, 0.60)

#' Compute IV skew signal for a ticker
#'
#' Fetches the nearest 30-day options chain and computes IV asymmetry between
#' calls and puts at delta levels 0.20, 0.30, 0.40, 0.50, 0.60.
#'
#' @param ticker Character ticker symbol
#' @return Named list with $table, $aggregate, $expiry_date, $days_to_expiry,
#'   $data_source, and $error (NULL on success, character on failure)
#' @export
compute_skew_signal <- function(ticker) {
  log_info("Skew signal: Starting fetch for '{ticker}'")

  # Fetch stock price (needed for delta computation via Black-Scholes)
  stock_price <- tryCatch({
    quote <- fetch_current_quote(ticker, fields = "Last Trade (Price Only)")
    price <- as.numeric(quote$Last)
    if (is.na(price) || price <= 0) stop("invalid price")
    price
  }, error = function(e) {
    log_warn("Skew signal: Could not fetch stock price for '{ticker}': {e$message}")
    NULL
  })

  if (is.null(stock_price)) {
    return(list(
      table = NULL, aggregate = NULL, expiry_date = NULL,
      days_to_expiry = NULL, data_source = NULL,
      error = paste0("Could not fetch stock price for ", ticker)
    ))
  }

  # Try Questrade first, then Yahoo fallback
  chain <- tryCatch({
    fetch_questrade_options_chain(ticker, expiration = NULL)
  }, error = function(e) {
    log_warn("Skew signal: Questrade chain failed for '{ticker}': {e$message}")
    NULL
  })

  data_source <- "Questrade"

  if (is.null(chain) || length(chain) == 0) {
    log_info("Skew signal: Falling back to Yahoo Finance for '{ticker}'")
    data_source <- "Yahoo Finance"
    chain <- tryCatch({
      quantmod::getOptionChain(ticker, NULL)
    }, error = function(e) {
      log_warn("Skew signal: Yahoo chain failed for '{ticker}': {e$message}")
      NULL
    })
  }

  if (is.null(chain) || length(chain) == 0) {
    return(list(
      table = NULL, aggregate = NULL, expiry_date = NULL,
      days_to_expiry = NULL, data_source = NULL,
      error = paste0("No options chain available for ", ticker, " from Questrade or Yahoo Finance")
    ))
  }

  # Parse expiry dates from chain names
  exp_names <- names(chain)
  exp_dates <- parse_chain_expiry_dates(exp_names, data_source)

  if (is.null(exp_dates) || all(is.na(exp_dates))) {
    return(list(
      table = NULL, aggregate = NULL, expiry_date = NULL,
      days_to_expiry = NULL, data_source = data_source,
      error = paste0("Could not parse expiry dates for ", ticker)
    ))
  }

  # Select expiry nearest to today + 30 days
  today <- Sys.Date()
  days_to_exps <- as.integer(exp_dates - today)
  valid <- !is.na(exp_dates) & days_to_exps > 0
  if (!any(valid)) {
    return(list(
      table = NULL, aggregate = NULL, expiry_date = NULL,
      days_to_expiry = NULL, data_source = data_source,
      error = paste0("No future expiry dates found for ", ticker)
    ))
  }

  distances <- abs(days_to_exps[valid] - 30L)
  chosen_name <- exp_names[valid][which.min(distances)]
  chosen_date <- exp_dates[valid][which.min(distances)]
  chosen_dte  <- days_to_exps[valid][which.min(distances)]

  log_info("Skew signal: Using expiry '{chosen_name}' ({chosen_dte} days) for '{ticker}'")

  calls_df <- chain[[chosen_name]]$calls
  puts_df  <- chain[[chosen_name]]$puts

  if (is.null(calls_df) || nrow(calls_df) == 0 ||
      is.null(puts_df)  || nrow(puts_df)  == 0) {
    return(list(
      table = NULL, aggregate = NULL, expiry_date = chosen_date,
      days_to_expiry = chosen_dte, data_source = data_source,
      error = paste0("Empty calls or puts data for ", ticker, " at expiry ", chosen_name)
    ))
  }

  # Normalize IV: Questrade returns percent (e.g., 23.74), Yahoo returns decimal (e.g., 0.2374)
  calls_df <- normalize_iv_column(calls_df)
  puts_df  <- normalize_iv_column(puts_df)

  # Compute Black-Scholes delta for each option row
  time_to_expiry <- chosen_dte / 365
  risk_free_rate <- 0.05  # approximate; not critical for delta ranking

  calls_df <- add_bs_delta(calls_df, stock_price, time_to_expiry, risk_free_rate, "call")
  puts_df  <- add_bs_delta(puts_df,  stock_price, time_to_expiry, risk_free_rate, "put")

  # Match each target delta to nearest call and put
  table_rows <- lapply(TARGET_DELTAS, function(target_delta) {
    call_row <- match_nearest_delta(calls_df, target_delta, "call")
    put_row  <- match_nearest_delta(puts_df,  target_delta, "put")

    if (is.null(call_row) || is.null(put_row)) return(NULL)

    call_iv <- call_row$IV
    put_iv  <- put_row$IV
    iv_diff <- if (!is.na(call_iv) && !is.na(put_iv)) call_iv - put_iv else NA_real_

    data.frame(
      delta        = target_delta,
      call_strike  = call_row$Strike,
      call_ask     = call_row$Ask,
      call_iv      = call_iv,
      put_strike   = put_row$Strike,
      put_ask      = put_row$Ask,
      put_iv       = put_iv,
      iv_diff      = iv_diff,
      stringsAsFactors = FALSE
    )
  })

  table_rows <- Filter(Negate(is.null), table_rows)

  if (length(table_rows) == 0) {
    return(list(
      table = NULL, aggregate = NULL, expiry_date = chosen_date,
      days_to_expiry = chosen_dte, data_source = data_source,
      error = paste0("Could not match any delta levels for ", ticker)
    ))
  }

  skew_table <- do.call(rbind, table_rows)

  # Weighted aggregate: sum(delta * iv_diff) / sum(delta)
  valid_rows <- !is.na(skew_table$iv_diff)
  aggregate_val <- if (any(valid_rows)) {
    sum(skew_table$delta[valid_rows] * skew_table$iv_diff[valid_rows]) /
      sum(skew_table$delta[valid_rows])
  } else {
    NA_real_
  }

  log_info("Skew signal: Aggregate IV diff = {round(aggregate_val * 100, 2)}% for '{ticker}'")

  list(
    table          = skew_table,
    aggregate      = aggregate_val,
    expiry_date    = chosen_date,
    days_to_expiry = chosen_dte,
    data_source    = data_source,
    error          = NULL
  )
}

################################################################################
# MODAL RENDERING HELPER
################################################################################

#' Build skew signal modal content
#'
#' Shared helper used by both aristocrats and zero-dividend modules to render
#' the Fetch Skew modal. Returns a modalDialog() that can be passed to
#' showModal().
#'
#' @param ticker Character ticker symbol
#' @param result List returned by compute_skew_signal()
#' @return A shiny modalDialog object
#' @export
build_skew_modal <- function(ticker, result) {
  if (!is.null(result$error)) {
    return(shiny::modalDialog(
      title = paste0("IV Skew Signal \u2014 ", ticker),
      shiny::tags$p(
        shiny::tags$strong("Error: "),
        result$error
      ),
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    ))
  }

  expiry_label <- format(result$expiry_date, "%b %d, %Y")
  subtitle <- paste0("Expiry: ", expiry_label, " (", result$days_to_expiry, " days)")

  table_html <- build_skew_table_html(result$table, result$aggregate)

  footnote <- if (!is.null(result$data_source) && result$data_source == "Yahoo Finance") {
    shiny::tags$p(
      shiny::tags$em("Source: Yahoo Finance (Questrade unavailable)"),
      style = "font-size: 0.85em; color: #666; margin-top: 8px;"
    )
  } else {
    NULL
  }

  shiny::modalDialog(
    title = paste0("IV Skew Signal \u2014 ", ticker),
    shiny::tags$p(subtitle, style = "color: #666; margin-bottom: 12px;"),
    table_html,
    footnote,
    easyClose = TRUE,
    footer = shiny::modalButton("Close"),
    size = "l"
  )
}

#' Render skew table as HTML
#'
#' @param skew_table Data frame from compute_skew_signal()$table
#' @param aggregate Numeric from compute_skew_signal()$aggregate
#' @return shiny HTML tag
#' @noRd
build_skew_table_html <- function(skew_table, aggregate) {
  header_row <- shiny::tags$tr(
    shiny::tags$th("Delta"),
    shiny::tags$th("Call Strike"),
    shiny::tags$th("Call Ask"),
    shiny::tags$th("Call IV"),
    shiny::tags$th("Put Strike"),
    shiny::tags$th("Put Ask"),
    shiny::tags$th("Put IV"),
    shiny::tags$th("IV Diff (C\u2013P)")
  )

  data_rows <- lapply(seq_len(nrow(skew_table)), function(i) {
    row <- skew_table[i, ]
    diff_cell <- format_iv_diff_cell(row$iv_diff)
    shiny::tags$tr(
      shiny::tags$td(sprintf("%.2f", row$delta)),
      shiny::tags$td(format_currency(row$call_strike)),
      shiny::tags$td(ifelse(is.na(row$call_ask), "\u2014", format_currency(row$call_ask))),
      shiny::tags$td(ifelse(is.na(row$call_iv),  "\u2014", sprintf("%.1f%%", row$call_iv * 100))),
      shiny::tags$td(format_currency(row$put_strike)),
      shiny::tags$td(ifelse(is.na(row$put_ask),  "\u2014", format_currency(row$put_ask))),
      shiny::tags$td(ifelse(is.na(row$put_iv),   "\u2014", sprintf("%.1f%%", row$put_iv * 100))),
      diff_cell
    )
  })

  agg_diff_cell <- format_iv_diff_cell(aggregate)
  agg_row <- shiny::tags$tr(
    style = "font-weight: bold; border-top: 2px solid #dee2e6;",
    shiny::tags$td(shiny::tags$strong("Weighted Avg")),
    shiny::tags$td(""),
    shiny::tags$td(""),
    shiny::tags$td(""),
    shiny::tags$td(""),
    shiny::tags$td(""),
    shiny::tags$td(""),
    agg_diff_cell
  )

  shiny::tags$table(
    class = "table table-sm table-bordered",
    shiny::tags$thead(header_row),
    shiny::tags$tbody(c(data_rows, list(agg_row)))
  )
}

#' Format IV diff value as a colored table cell
#'
#' @param iv_diff Numeric IV difference (decimal, e.g. 0.031 for 3.1%)
#' @return shiny tags$td element
#' @noRd
format_iv_diff_cell <- function(iv_diff) {
  if (is.na(iv_diff) || is.null(iv_diff)) {
    return(shiny::tags$td("\u2014"))
  }

  pct <- iv_diff * 100
  threshold <- 0.5

  if (pct > threshold) {
    label <- sprintf("\u2191 +%.1f%%", pct)
    style <- "color: #28a745; font-weight: 600;"
  } else if (pct < -threshold) {
    label <- sprintf("\u2193 %.1f%%", pct)
    style <- "color: #dc3545; font-weight: 600;"
  } else {
    label <- sprintf("\u2014 %+.1f%%", pct)
    style <- ""
  }

  shiny::tags$td(label, style = style)
}

################################################################################
# INTERNAL HELPERS
################################################################################

#' Parse expiry date strings from chain names
#'
#' Handles both Questrade format ("Mar.21.2025") and Yahoo format ("2025-03-21")
#'
#' @param exp_names Character vector of expiry name strings
#' @param data_source Character "Questrade" or "Yahoo Finance"
#' @return Date vector (NAs for unparseable entries)
#' @noRd
parse_chain_expiry_dates <- function(exp_names, data_source) {
  if (data_source == "Questrade") {
    as.Date(exp_names, format = "%b.%d.%Y")
  } else {
    # Yahoo Finance: try ISO first, then dotted format as fallback
    dates <- suppressWarnings(as.Date(exp_names))
    if (all(is.na(dates))) {
      dates <- suppressWarnings(as.Date(exp_names, format = "%b.%d.%Y"))
    }
    dates
  }
}

#' Normalize IV column to decimal form
#'
#' Questrade returns IV as percent (e.g., 23.74); Yahoo returns decimal (e.g., 0.2374).
#' Detection: if median of non-NA values > 1, assume percent scale.
#'
#' @param df Data frame with IV column
#' @return Data frame with IV in decimal form
#' @noRd
normalize_iv_column <- function(df) {
  if (!"IV" %in% names(df)) return(df)
  iv_vals <- df$IV[!is.na(df$IV)]
  if (length(iv_vals) > 0 && stats::median(iv_vals) > 1) {
    df$IV <- df$IV / 100
  }
  df
}

#' Compute Black-Scholes delta and attach to options data frame
#'
#' Delta is the derivative of option price with respect to stock price.
#' For calls: delta = N(d1); for puts: delta = N(d1) - 1 (negative).
#'
#' @param df Data frame with Strike and IV columns
#' @param stock_price Current stock price
#' @param time_to_expiry Years to expiry
#' @param risk_free_rate Risk-free rate (decimal)
#' @param option_type "call" or "put"
#' @return Data frame with added bs_delta column
#' @noRd
add_bs_delta <- function(df, stock_price, time_to_expiry, risk_free_rate, option_type) {
  if (nrow(df) == 0) return(df)
  df$bs_delta <- mapply(function(strike, iv) {
    if (is.na(strike) || is.na(iv) || iv <= 0 || time_to_expiry <= 0) return(NA_real_)
    d1 <- (log(stock_price / strike) + (risk_free_rate + 0.5 * iv^2) * time_to_expiry) /
            (iv * sqrt(time_to_expiry))
    if (option_type == "call") pnorm(d1) else pnorm(d1) - 1
  }, df$Strike, df$IV)
  df
}

#' Match option row with delta nearest to target
#'
#' For calls: uses bs_delta directly (positive, 0-1).
#' For puts: uses abs(bs_delta) (puts have negative delta).
#' On ties: lower absolute delta wins.
#'
#' @param df Data frame with bs_delta column
#' @param target_delta Target delta value (positive, e.g. 0.30)
#' @param option_type "call" or "put"
#' @return Single-row data frame or NULL
#' @noRd
match_nearest_delta <- function(df, target_delta, option_type) {
  if (nrow(df) == 0) return(NULL)

  abs_delta <- if (option_type == "call") df$bs_delta else abs(df$bs_delta)
  valid <- !is.na(abs_delta) & abs_delta > 0 & abs_delta < 1
  if (!any(valid)) return(NULL)

  valid_idx <- which(valid)
  distances <- abs(abs_delta[valid_idx] - target_delta)
  min_dist  <- min(distances)

  # On tie: use lower abs delta
  tied <- valid_idx[distances == min_dist]
  if (length(tied) > 1) {
    tied <- tied[which.min(abs_delta[tied])]
  } else {
    tied <- tied[1]
  }

  df[tied, , drop = FALSE]
}
