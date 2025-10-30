
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

  # CASH FLOWS AT ENTRY
  # Money IN:
  short_stock_proceeds <- stock_price * 100  # Selling 100 shares
  put_premium_received <- put_option$bidPrice * 100  # Selling put at bid

  # Money OUT:
  call_premium_paid <- call_option$askPrice * 100  # Buying call at ask

  # CASH FLOWS AT EXPIRY
  # Regardless of stock price, position closes at strike price
  # (Either call exercised or put assigned - one will be ITM)
  stock_buyback_cost <- strike * 100  # Must buy back stock at strike

  # TRUE PROFIT CALCULATION
  # Profit = Cash in - Cash out
  # Cash in: Short proceeds + Put premium
  # Cash out: Call premium + Stock buyback at strike
  profit <- short_stock_proceeds + put_premium_received - call_premium_paid - stock_buyback_cost

  # Alternative formula (equivalent):
  # profit <- (stock_price - strike) * 100 + put_premium_received - call_premium_paid

  # MARGIN CALCULATION
  # Broker holds: Short proceeds + Put premium as collateral
  # This is the capital tied up in the position
  margin_required <- short_stock_proceeds + put_premium_received

  # EXTRINSIC VALUE (from the put we're selling)
  put_intrinsic <- max(0, strike - stock_price)
  put_extrinsic <- max(0, put_premium_received / 100 - put_intrinsic)

  # RETURNS
  days_to_expiry <- as.numeric(difftime(put_option$expirationDate, Sys.Date(), units = "days"))

  # Handle edge cases for days_to_expiry
  if (is.na(days_to_expiry) || days_to_expiry <= 0) {
    log_debug("Invalid days_to_expiry: {days_to_expiry} - skipping")
    return(NULL)
  }

  # Return on margin (the capital tied up)
  return_on_margin <- if (margin_required > 0) profit / margin_required else 0
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

    # Cash flows at entry
    short_stock_proceeds = short_stock_proceeds,
    put_premium_received = put_premium_received,
    call_premium_paid = call_premium_paid,

    # Cash flows at expiry
    stock_buyback_cost = stock_buyback_cost,

    # Profit & Margin
    profit = profit,
    margin_required = margin_required,

    # Metrics
    put_extrinsic_value = put_extrinsic,
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
#' Redesigned for clarity - shows profit, margin, and returns without repetition.
#' Focused on decision-making, not teaching.
#'
#' @param row Single-row tibble with opportunity data
#' @return List of section configurations for create_strategy_opportunity_card()
#' @noRd
build_scanner_card_sections <- function(row) {
  list(
    # Section 1: Quick Overview (OPEN) - Everything needed for go/no-go decision
    list(
      title = "Quick Overview",
      is_open = TRUE,
      metrics = list(
        list(
          label = "Net Profit at Expiry",
          value = format_currency(row$profit / 100),
          is_primary = TRUE
        ),
        list(
          label = "Capital Tied Up (Margin)",
          value = format_currency(row$margin_required / 100)
        ),
        list(
          label = "Return on Margin",
          value = format_percentage(row$return_on_margin),
          is_primary = TRUE
        ),
        list(
          label = "Annualized Return",
          value = format_percentage(row$annualized_return)
        ),
        list(
          label = "Days to Expiry",
          value = as.character(row$days_to_expiry)
        )
      )
    ),

    # Section 2: Position Breakdown (OPEN) - Cash flow tables showing entry & exit
    list(
      title = "Position Breakdown",
      is_open = TRUE,
      metrics = list(
        list(
          label = "═══ At Entry ═══",
          value = ""
        ),
        list(
          label = "Short 100 shares @ current price",
          value = paste0("+", format_currency(row$short_stock_proceeds / 100))
        ),
        list(
          label = paste0("Sell Put @ $", format(row$strike, nsmall = 2)),
          value = paste0("+", format_currency(row$put_premium_received / 100))
        ),
        list(
          label = paste0("Buy Call @ $", format(row$strike, nsmall = 2)),
          value = paste0("-", format_currency(row$call_premium_paid / 100))
        ),
        list(
          label = "Total Capital Tied Up",
          value = format_currency(row$margin_required / 100),
          is_primary = TRUE
        ),
        list(
          label = "═══ At Expiry ═══",
          value = ""
        ),
        list(
          label = paste0("Buy back stock @ $", format(row$strike, nsmall = 2)),
          value = paste0("-", format_currency(row$stock_buyback_cost / 100))
        ),
        list(
          label = "Options settle (one ITM)",
          value = "(included above)"
        ),
        list(
          label = "Net Profit",
          value = format_currency(row$profit / 100),
          is_primary = TRUE
        )
      )
    ),

    # Section 3: Strategy Explanation (COLLAPSED) - Educational content
    list(
      title = "How This Position Works",
      is_open = FALSE,
      metrics = list(
        list(
          label = "Position closes at strike price",
          value = paste0("$", format(row$strike, nsmall = 2))
        ),
        list(
          label = "If stock rises",
          value = "Call exercised → buy @ strike"
        ),
        list(
          label = "If stock falls",
          value = "Put assigned → buy @ strike"
        ),
        list(
          label = "Profit is locked in at entry",
          value = format_currency(row$profit / 100)
        ),
        list(
          label = "⚠ Key Risk: Stock borrow cost",
          value = "Not calculated - verify with broker"
        ),
        list(
          label = "⚠ Margin is estimated",
          value = "Verify actual requirement with broker"
        )
      )
    ),

    # Section 4: Option Details (COLLAPSED) - Due diligence data
    list(
      title = "Option Details",
      is_open = FALSE,
      metrics = list(
        list(
          label = paste0("Put Sold ($", format(row$strike, nsmall = 2), " Strike)"),
          value = ""
        ),
        list(
          label = "Bid (You Receive)",
          value = format_currency(row$put_bid),
          is_primary = TRUE
        ),
        list(
          label = "Ask / Last",
          value = paste0(format_currency(row$put_ask), " / ", format_currency(row$put_last))
        ),
        list(
          label = "Volume / OI",
          value = paste0(format(row$put_volume, big.mark = ","), " / ", format(row$put_oi, big.mark = ","))
        ),
        list(
          label = "Extrinsic Value",
          value = format_currency(row$put_extrinsic_value)
        ),
        list(
          label = paste0("Call Bought ($", format(row$strike, nsmall = 2), " Strike)"),
          value = ""
        ),
        list(
          label = "Ask (You Pay)",
          value = format_currency(row$call_ask),
          is_primary = TRUE
        ),
        list(
          label = "Bid / Last",
          value = paste0(format_currency(row$call_bid), " / ", format_currency(row$call_last))
        ),
        list(
          label = "Volume / OI",
          value = paste0(format(row$call_volume, big.mark = ","), " / ", format(row$call_oi, big.mark = ","))
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
