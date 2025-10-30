
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

#' Calculate Reverse Collar Position Metrics
#'
#' Computes complete cash flows and returns for a reverse collar position:
#' Short stock + Sell ATM put + Buy ATM call
#'
#' @param stock_price Numeric, current stock price
#' @param strike Numeric, ATM strike price (same for put and call)
#' @param put_option Single-row tibble with put option data
#' @param call_option Single-row tibble with call option data
#' @return Tibble with complete position metrics
#' @noRd
calculate_reverse_collar_metrics <- function(stock_price, strike, put_option, call_option) {
  # Validate inputs - ensure we have the required price data
  if (is.null(put_option$bidPrice) || is.na(put_option$bidPrice) ||
      is.null(call_option$askPrice) || is.na(call_option$askPrice)) {
    log_warn("Missing bid/ask prices for options - skipping")
    return(NULL)
  }

  # CASH FLOWS
  # Money IN:
  short_stock_proceeds <- stock_price * 100  # Selling 100 shares
  put_premium_received <- put_option$bidPrice * 100  # Selling put at bid

  # Money OUT:
  call_premium_paid <- call_option$askPrice * 100  # Buying call at ask

  # NET POSITION
  net_credit <- short_stock_proceeds + put_premium_received - call_premium_paid

  # MARGIN CALCULATION
  # For reverse collar (short stock + long call + short put):
  # - Short stock margin: typically 50% of stock value
  # - Long call: paid for, no margin (already deducted from net credit)
  # - Short put: covered by short stock position
  # Simplified: Use short stock margin requirement
  estimated_margin <- 0.5 * stock_price * 100

  # EXTRINSIC VALUE (from the put we're selling)
  put_intrinsic <- max(0, strike - stock_price)
  put_extrinsic <- max(0, put_premium_received / 100 - put_intrinsic)

  # RETURNS
  # Return on margin
  days_to_expiry <- as.numeric(difftime(put_option$expirationDate, Sys.Date(), units = "days"))

  # Handle edge cases for days_to_expiry
  if (is.na(days_to_expiry) || days_to_expiry <= 0) {
    log_debug("Invalid days_to_expiry: {days_to_expiry} - skipping")
    return(NULL)
  }

  return_on_margin <- if (estimated_margin > 0) net_credit / estimated_margin else 0
  annualized_return <- return_on_margin * (365 / days_to_expiry)

  # Return tibble with all fields needed for card display
  tibble::tibble(
    # Position structure
    strike = strike,
    expirationDate = put_option$expirationDate,
    days_to_expiry = days_to_expiry,

    # Put option details
    put_bid = put_option$bidPrice,
    put_ask = put_option$askPrice,
    put_last = put_option$lastPrice,
    put_volume = put_option$volume,
    put_oi = put_option$openInterest,
    put_iv = put_option$impliedVolatility,

    # Call option details
    call_bid = call_option$bidPrice,
    call_ask = call_option$askPrice,
    call_last = call_option$lastPrice,
    call_volume = call_option$volume,
    call_oi = call_option$openInterest,
    call_iv = call_option$impliedVolatility,

    # Cash flows
    short_stock_proceeds = short_stock_proceeds,
    put_premium_received = put_premium_received,
    call_premium_paid = call_premium_paid,
    net_credit = net_credit,

    # Metrics
    put_extrinsic_value = put_extrinsic,
    estimated_margin = estimated_margin,
    return_on_margin = return_on_margin,
    annualized_return = annualized_return
  )
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

#' Build section configuration for reverse collar scanner cards
#'
#' Defines clear, human-readable accordion sections showing complete
#' cash flows for reverse collar positions (short stock + short put + long call).
#'
#' @param row Single-row tibble with opportunity data
#' @return List of section configurations for create_strategy_opportunity_card()
#' @noRd
build_scanner_card_sections <- function(row) {
  list(
    # Section 1: Quick Overview (open by default)
    list(
      title = "Quick Overview",
      is_open = TRUE,
      metrics = list(
        list(
          label = "Net Credit Received",
          value = format_currency(row$net_credit / 100),
          is_primary = TRUE
        ),
        list(
          label = "Annualized Return",
          value = format_percentage(row$annualized_return),
          is_primary = TRUE
        ),
        list(
          label = "Days to Expiry",
          value = as.character(row$days_to_expiry)
        )
      )
    ),

    # Section 2: Cash Flow Breakdown (open by default) - THE KEY SECTION!
    list(
      title = "Cash Flow Breakdown",
      is_open = TRUE,
      metrics = list(
        list(
          label = "💰 Short 100 Shares → Receive",
          value = format_currency(row$short_stock_proceeds / 100)
        ),
        list(
          label = "💰 Sell ATM Put → Receive",
          value = format_currency(row$put_premium_received / 100)
        ),
        list(
          label = "💸 Buy ATM Call → Pay",
          value = paste0("-", format_currency(row$call_premium_paid / 100))
        ),
        list(
          label = "➡️ Net Cash In Hand",
          value = format_currency(row$net_credit / 100),
          is_primary = TRUE
        )
      )
    ),

    # Section 3: Position Structure (open by default)
    list(
      title = "Position Structure",
      is_open = TRUE,
      metrics = list(
        list(
          label = "Stock Price",
          value = format_currency(row$current_stock_price)
        ),
        list(
          label = "Strike Price (ATM)",
          value = format_currency(row$strike)
        ),
        list(
          label = "Expiration Date",
          value = as.character(format(row$expirationDate, "%b %d, %Y"))
        )
      )
    ),

    # Section 4: Put Option Details (collapsed)
    list(
      title = "Put Option (Sold for Income)",
      is_open = FALSE,
      metrics = list(
        list(
          label = "Bid (You Receive)",
          value = format_currency(row$put_bid),
          is_primary = TRUE
        ),
        list(
          label = "Ask",
          value = format_currency(row$put_ask)
        ),
        list(
          label = "Last Trade",
          value = format_currency(row$put_last)
        ),
        list(
          label = "Volume",
          value = format(row$put_volume, big.mark = ",")
        ),
        list(
          label = "Open Interest",
          value = format(row$put_oi, big.mark = ",")
        ),
        list(
          label = "Implied Volatility",
          value = if (!is.na(row$put_iv)) format_percentage(row$put_iv) else "N/A"
        )
      )
    ),

    # Section 5: Call Option Details (collapsed)
    list(
      title = "Call Option (Bought for Protection)",
      is_open = FALSE,
      metrics = list(
        list(
          label = "Ask (You Pay)",
          value = format_currency(row$call_ask),
          is_primary = TRUE
        ),
        list(
          label = "Bid",
          value = format_currency(row$call_bid)
        ),
        list(
          label = "Last Trade",
          value = format_currency(row$call_last)
        ),
        list(
          label = "Volume",
          value = format(row$call_volume, big.mark = ",")
        ),
        list(
          label = "Open Interest",
          value = format(row$call_oi, big.mark = ",")
        ),
        list(
          label = "Implied Volatility",
          value = if (!is.na(row$call_iv)) format_percentage(row$call_iv) else "N/A"
        )
      )
    ),

    # Section 6: Margin & Returns (collapsed)
    list(
      title = "Margin & Returns Analysis",
      is_open = FALSE,
      metrics = list(
        list(
          label = "Estimated Margin Required",
          value = format_currency(row$estimated_margin / 100)
        ),
        list(
          label = "Net Credit / Margin Ratio",
          value = format_percentage(row$return_on_margin)
        ),
        list(
          label = "Annualized Return on Margin",
          value = format_percentage(row$annualized_return),
          is_primary = TRUE
        ),
        list(
          label = "Put Extrinsic Value",
          value = format_currency(row$put_extrinsic_value)
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
