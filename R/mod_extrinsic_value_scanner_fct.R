
#' Helper functions for extrinsic_value_scanner module
#'
#' @description This file contains helper functions specific to the
#'   extrinsic_value_scanner module.
#'
#' @noRd
NULL

#' Convert Yahoo-format options chain to flat tibble
#'
#' Transforms the nested list structure returned by fetch_questrade_options_chain()
#' into a flat tibble with columns: expirationDate, strikePrice, optionType, lastPrice, etc.
#'
#' @param options_chain List in Yahoo format (nested by expiration date)
#' @return Tibble with flattened options data
#' @noRd
convert_options_chain_to_tibble <- function(options_chain) {
  if (is.null(options_chain) || length(options_chain) == 0) {
    return(tibble(
      expirationDate = as.Date(character(0)),
      strikePrice = numeric(0),
      optionType = character(0),
      lastPrice = numeric(0),
      bidPrice = numeric(0),
      askPrice = numeric(0),
      volume = integer(0),
      openInterest = integer(0),
      impliedVolatility = numeric(0)
    ))
  }

  all_options <- list()

  # Iterate through each expiration date
  for (expiry_str in names(options_chain)) {
    # Parse the expiration date (e.g., "Mar.21.2025" -> Date)
    expiry_date <- tryCatch({
      as.Date(expiry_str, format = "%b.%d.%Y")
    }, error = function(e) {
      log_warn("Failed to parse expiration date: {expiry_str}")
      return(NULL)
    })

    if (is.null(expiry_date) || is.na(expiry_date)) next

    expiry_data <- options_chain[[expiry_str]]

    # Process calls
    if (!is.null(expiry_data$calls) && nrow(expiry_data$calls) > 0) {
      calls_tibble <- expiry_data$calls %>%
        dplyr::mutate(
          expirationDate = expiry_date,
          optionType = "Call",
          strikePrice = Strike,
          lastPrice = Last,
          bidPrice = Bid,
          askPrice = Ask,
          volume = Vol,
          openInterest = OI,
          impliedVolatility = IV
        ) %>%
        dplyr::select(
          expirationDate,
          strikePrice,
          optionType,
          lastPrice,
          bidPrice,
          askPrice,
          volume,
          openInterest,
          impliedVolatility
        )
      all_options[[length(all_options) + 1]] <- calls_tibble
    }

    # Process puts
    if (!is.null(expiry_data$puts) && nrow(expiry_data$puts) > 0) {
      puts_tibble <- expiry_data$puts %>%
        dplyr::mutate(
          expirationDate = expiry_date,
          optionType = "Put",
          strikePrice = Strike,
          lastPrice = Last,
          bidPrice = Bid,
          askPrice = Ask,
          volume = Vol,
          openInterest = OI,
          impliedVolatility = IV
        ) %>%
        dplyr::select(
          expirationDate,
          strikePrice,
          optionType,
          lastPrice,
          bidPrice,
          askPrice,
          volume,
          openInterest,
          impliedVolatility
        )
      all_options[[length(all_options) + 1]] <- puts_tibble
    }
  }

  # Combine all options into one tibble
  if (length(all_options) == 0) {
    return(tibble(
      expirationDate = as.Date(character(0)),
      strikePrice = numeric(0),
      optionType = character(0),
      lastPrice = numeric(0),
      bidPrice = numeric(0),
      askPrice = numeric(0),
      volume = integer(0),
      openInterest = integer(0),
      impliedVolatility = numeric(0)
    ))
  }

  result <- dplyr::bind_rows(all_options)
  return(result)
}

#' Calculate Extrinsic Value of an Option
#'
#' @param option_premium Numeric, the premium of the option.
#' @param stock_price Numeric, the current price of the underlying stock.
#' @param strike_price Numeric, the strike price of the option.
#' @param option_type Character, 'call' or 'put'.
#' @return Numeric, the extrinsic value of the option.
#' @noRd
calculate_extrinsic_value <- function(option_premium, stock_price, strike_price, option_type) {
  intrinsic_value <- 0
  if (option_type == "call") {
    intrinsic_value <- max(0, stock_price - strike_price)
  } else if (option_type == "put") {
    intrinsic_value <- max(0, strike_price - stock_price)
  }
  extrinsic_value <- option_premium - intrinsic_value
  return(extrinsic_value)
}

#' Estimate Margin Required for a Reverse Collar Position
#'
#' This is a simplified estimation. Actual margin requirements are complex and
#' broker-specific.
#'
#' @param stock_price Numeric, the current price of the underlying stock.
#' @param strike_price Numeric, the strike price of the options (assuming ATM).
#' @param option_type Character, 'call' or 'put' (not directly used in this simplified margin,
#'   but kept for consistency if logic expands).
#' @return Numeric, estimated margin required for 100 shares.
#' @noRd
estimate_reverse_collar_margin <- function(stock_price, strike_price, option_type) {
  # Assuming 50% initial margin for short stock position for 100 shares
  # This is a placeholder and should be refined with actual broker rules.
  estimated_margin <- 0.5 * stock_price * 100
  return(estimated_margin)
}

#' Calculate Annualized Return for Extrinsic Value Scanner
#'
#' Calculates the annualized return based on extrinsic value harvested
#' relative to the estimated margin requirement for a reverse collar position.
#'
#' @param extrinsic_value Numeric, the extrinsic value of the option.
#' @param estimated_margin Numeric, the estimated margin required.
#' @param days_to_expiry Numeric, days remaining until option expiration.
#' @return Numeric, the annualized return as a decimal (not percentage).
#' @noRd
calculate_scanner_annualized_return <- function(extrinsic_value, estimated_margin, days_to_expiry) {
  if (estimated_margin <= 0 || days_to_expiry <= 0) {
    return(0) # Avoid division by zero or non-positive days
  }
  # Annualized Return = (Extrinsic Value / Estimated Margin) * (365 / Days to Expiry)
  annualized_return <- (extrinsic_value / estimated_margin) * (365 / days_to_expiry)
  return(annualized_return)
}

#' Create a card for a scanned opportunity
#'
#' @param opportunity_data A single-row tibble containing opportunity details.
#' @return A bslib card component.
#' @importFrom shiny tags div
#' @importFrom bslib card card_header card_body
#' @noRd
create_scanner_opportunity_card <- function(opportunity_data) {
  # Assuming format_currency, format_percentage are available globally or sourced
  # from utils_formatting.R

  # Card Header
  header_primary <- tags$div(
    opportunity_data$symbol,
    tags$span(
      class = "badge badge-primary pull-right",
      style = "font-size: 14px; margin-left: 10px;",
      format_percentage(opportunity_data$annualized_return_pct)
    )
  )

  header_secondary <- sprintf(
    "%s | Strike: %s | Expiry: %s (%d days)",
    opportunity_data$optionType,
    format_currency(opportunity_data$strikePrice),
    format(opportunity_data$expirationDate, "%b %d, %Y"),
    opportunity_data$days_to_expiry
  )

  header <- bslib::card_header(
    tags$h4(header_primary),
    tags$p(header_secondary, class = "text-muted")
  )

  # Card Body
  body <- bslib::card_body(
    create_metric_row("Current Stock Price", format_currency(opportunity_data$current_stock_price)),
    create_metric_row("Option Premium", format_currency(opportunity_data$lastPrice)),
    create_metric_row("Intrinsic Value", format_currency(opportunity_data$intrinsic_value)),
    create_metric_row("Extrinsic Value", format_currency(opportunity_data$extrinsic_value)),
    create_metric_row("Estimated Margin", format_currency(opportunity_data$estimated_margin)),
    create_metric_row("Annualized Return", format_percentage(opportunity_data$annualized_return_pct), is_primary = TRUE)
  )

  bslib::card(header, body)
}
