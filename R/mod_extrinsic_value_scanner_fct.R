
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

#' Build section configuration for extrinsic value scanner cards
#'
#' Defines the accordion section structure specific to the extrinsic value
#' scanner strategy following the standardized card pattern.
#'
#' @param row Single-row tibble with opportunity data
#' @return List of section configurations for create_strategy_opportunity_card()
#' @noRd
build_scanner_card_sections <- function(row) {
  # Calculate bid-ask spread
  bid_ask_spread <- if (!is.null(row$askPrice) && !is.null(row$bidPrice)) {
    row$askPrice - row$bidPrice
  } else {
    NA_real_
  }

  # Calculate time value percentage
  time_value_pct <- if (!is.null(row$extrinsic_value) && !is.null(row$lastPrice) && row$lastPrice > 0) {
    row$extrinsic_value / row$lastPrice
  } else {
    NA_real_
  }

  list(
    # Section 1: Quick Overview (open by default)
    list(
      title = "Quick Overview",
      is_open = TRUE,
      metrics = list(
        list(
          label = "Annualized Return",
          value = format_percentage(row$annualized_return_pct),
          is_primary = TRUE
        ),
        list(
          label = "Extrinsic Value",
          value = format_currency(row$extrinsic_value),
          is_primary = TRUE
        ),
        list(
          label = "Estimated Margin",
          value = format_currency(row$estimated_margin)
        ),
        list(
          label = "Days to Expiry",
          value = as.character(row$days_to_expiry)
        )
      )
    ),

    # Section 2: Position Details (open by default)
    list(
      title = "Position Details",
      is_open = TRUE,
      metrics = list(
        list(
          label = "Current Stock Price",
          value = format_currency(row$current_stock_price)
        ),
        list(
          label = "Strike Price",
          value = format_currency(row$strikePrice)
        ),
        list(
          label = "Option Premium",
          value = format_currency(row$lastPrice)
        ),
        list(
          label = "Expiration Date",
          value = as.character(format(row$expirationDate, "%b %d, %Y"))
        )
      )
    ),

    # Section 3: Value Breakdown (collapsed)
    list(
      title = "Value Breakdown",
      is_open = FALSE,
      metrics = list(
        list(
          label = "Intrinsic Value",
          value = format_currency(row$intrinsic_value)
        ),
        list(
          label = "Extrinsic Value",
          value = format_currency(row$extrinsic_value)
        ),
        list(
          label = "Time Value %",
          value = if (!is.na(time_value_pct)) format_percentage(time_value_pct) else "N/A"
        ),
        list(
          label = "Bid-Ask Spread",
          value = if (!is.na(bid_ask_spread)) format_currency(bid_ask_spread) else "N/A"
        )
      )
    ),

    # Section 4: Option Details (collapsed)
    list(
      title = "Option Details",
      is_open = FALSE,
      metrics = list(
        list(
          label = "Bid Price",
          value = if (!is.null(row$bidPrice)) format_currency(row$bidPrice) else "N/A"
        ),
        list(
          label = "Ask Price",
          value = if (!is.null(row$askPrice)) format_currency(row$askPrice) else "N/A"
        ),
        list(
          label = "Last Trade Price",
          value = format_currency(row$lastPrice)
        ),
        list(
          label = "Volume",
          value = if (!is.null(row$volume)) format(row$volume, big.mark = ",") else "N/A"
        ),
        list(
          label = "Open Interest",
          value = if (!is.null(row$openInterest)) format(row$openInterest, big.mark = ",") else "N/A"
        ),
        list(
          label = "Implied Volatility",
          value = if (!is.null(row$impliedVolatility) && !is.na(row$impliedVolatility)) {
            format_percentage(row$impliedVolatility)
          } else {
            "N/A"
          }
        )
      )
    )
  )
}

#' Create a card for a scanned opportunity
#'
#' Creates a standardized opportunity card for the extrinsic value scanner
#' using the generic card builder pattern.
#'
#' @param opportunity_data A single-row tibble containing opportunity details.
#' @return A bslib card component with accordion sections.
#' @noRd
create_scanner_opportunity_card <- function(opportunity_data) {
  # Build primary header text
  primary_text <- if (!is.null(opportunity_data$company_name)) {
    paste0(opportunity_data$company_name, " (", opportunity_data$symbol, ")")
  } else {
    opportunity_data$symbol
  }

  # Build secondary header text
  secondary_text <- format_currency(opportunity_data$current_stock_price)

  # Build sections using strategy-specific configuration
  sections <- build_scanner_card_sections(opportunity_data)

  # Use generic card builder
  create_strategy_opportunity_card(
    row = opportunity_data,
    primary_text = primary_text,
    secondary_text = secondary_text,
    sections = sections,
    include_risk_button = FALSE,  # No risk button for now
    card_class = "opportunity-card"
  )
}
