#' Income Projection Engine
#'
#' Core logic for generating cash flow projections for position groups.
#' Handles dividend projections and option gain calculations.
#'
#' @name income-projection-engine
#' @import dplyr
#' @importFrom purrr map_dfr map_dbl
#' @importFrom tibble tibble
#' @importFrom lubridate days
#' @importFrom logger log_info log_warn log_debug
#' @importFrom zoo index
NULL

################################################################################
# MAIN PROJECTION GENERATION
################################################################################

#' Generate initial income projections for a position group
#'
#' Creates projected cash flow events (dividends + option gain) based on
#' historical data and current positions.
#'
#' @param group_id Group identifier
#' @param members Tibble with columns: symbol, role
#' @param account_number Account number
#' @return Logical TRUE if successful, FALSE otherwise
#' @noRd
generate_initial_projections <- function(group_id, members, account_number) {

  tryCatch({
    log_info("Income Projection: Generating initial projections for group {group_id}")

    # Check strategy type - skip projections for strategies that don't hold long-term
    group_info <- get_group_by_id(group_id)

    if (nrow(group_info) == 0) {
      log_warn("Income Projection: Group {group_id} not found")
      return(FALSE)
    }

    # Strategies that should NOT have projections (short-term or unstructured)
    no_projection_strategies <- c(
      "Other",
      "Weekly Dividend Capture",
      "Monthly Dividend Capture",
      "Dividend Capture"
    )

    if (group_info$strategy_type[1] %in% no_projection_strategies) {
      log_info("Income Projection: Skipping projections for '{group_info$strategy_type[1]}' strategy group {group_id}")
      log_projection_recalculation(group_id, "initial_creation", 0, 0)
      return(TRUE)
    }

    # Get activities for this group to calculate actual costs
    activities <- get_activities_by_group(group_id)

    if (nrow(activities) == 0) {
      log_warn("Income Projection: No activities found for group {group_id}")
      return(FALSE)
    }

    # Extract underlying stock and option symbols
    underlying <- members %>% filter(role == "underlying_stock") %>% pull(symbol)
    option_symbol <- members %>% filter(role == "short_call") %>% pull(symbol)

    # Calculate stock cost from activities (using gross_amount)
    stock_activities <- activities %>%
      filter(type == "Trades", action == "Buy", !purrr::map_lgl(symbol, is_option_symbol))

    if (nrow(stock_activities) == 0) {
      log_warn("Income Projection: No stock purchase activities found for group {group_id}")
      return(FALSE)
    }

    # Get shares from stock purchase quantity
    shares <- sum(abs(stock_activities$quantity), na.rm = TRUE)
    stock_cost <- sum(abs(stock_activities$gross_amount), na.rm = TRUE)

    # Calculate option premium from activities (if exists)
    expiry_date <- NULL
    strike_price <- NULL
    premium_received <- 0

    if (length(option_symbol) > 0) {
      option_activities <- activities %>%
        filter(type == "Trades", action == "Sell", symbol == option_symbol[1])

      if (nrow(option_activities) > 0) {
        # Extract strike and expiry from option symbol (e.g., "ALB17Dec27C55.00")
        option_info <- parse_option_details(option_symbol[1])
        strike_price <- option_info$strike
        expiry_date <- option_info$expiry

        # Premium received from activities (using gross_amount)
        premium_received <- sum(abs(option_activities$gross_amount), na.rm = TRUE)
      }
    }

    # Generate dividend projections
    dividend_events <- generate_dividend_events(
      ticker = underlying[1],
      shares = shares,
      end_date = expiry_date
    )

    # Generate option gain projection (if option exists)
    option_events <- tibble::tibble()
    if (!is.null(expiry_date) && !is.null(strike_price)) {
      option_events <- generate_option_gain_event(
        expiry_date = expiry_date,
        strike_price = strike_price,
        shares = shares,
        stock_cost = stock_cost,
        premium_received = premium_received
      )
    }

    # Combine all events
    all_events <- bind_rows(dividend_events, option_events)

    if (nrow(all_events) == 0) {
      log_info("Income Projection: No events to project for group {group_id}")
      log_projection_recalculation(group_id, "initial_creation", 0, 0)
      return(TRUE)
    }

    # Save events to database
    event_ids <- purrr::map_chr(1:nrow(all_events), function(i) {
      event <- all_events[i, ]
      save_cash_flow_event(
        group_id = group_id,
        event_date = event$event_date,
        event_type = event$event_type,
        amount = event$amount,
        status = "projected",
        confidence = event$confidence
      )
    })

    # Log recalculation
    log_projection_recalculation(
      group_id = group_id,
      reason = "initial_creation",
      old_count = 0,
      new_count = nrow(all_events)
    )

    log_info("Income Projection: Generated {nrow(all_events)} projections for group {group_id}")
    return(TRUE)

  }, error = function(e) {
    log_warn("Income Projection: Failed to generate projections for {group_id} - {e$message}")
    return(FALSE)
  })
}

################################################################################
# DIVIDEND PROJECTION
################################################################################

#' Generate dividend event projections
#'
#' Projects future dividend payments based on historical frequency and amounts.
#'
#' @param ticker Stock ticker symbol
#' @param shares Number of shares held
#' @param end_date End date for projections (option expiry)
#' @return Tibble with event_date, event_type, amount, confidence columns
#' @noRd
generate_dividend_events <- function(ticker, shares, end_date = NULL) {

  # Default to 2 years out if no end date
  if (is.null(end_date)) {
    end_date <- Sys.Date() + 730
  }

  # Fetch dividend history
  div_history <- fetch_dividend_history(ticker)

  # Check if stock pays dividends
  if (is.null(div_history) || length(div_history) == 0 || nrow(div_history) < 2) {
    log_debug("Income Projection: No dividend history for {ticker}")
    return(tibble::tibble(
      event_date = as.Date(character(0)),
      event_type = character(0),
      amount = numeric(0),
      confidence = character(0)
    ))
  }

  # Get recent dividend data for frequency calculation
  recent_divs <- tail(div_history, min(6, nrow(div_history)))
  div_dates <- index(recent_divs)
  latest_amount <- as.numeric(tail(div_history, 1))

  # Calculate average days between payments
  days_between <- as.numeric(diff(div_dates))
  avg_days_between <- mean(days_between)

  # Determine confidence based on consistency
  cv <- sd(days_between) / mean(days_between)  # Coefficient of variation
  confidence <- if (cv < 0.05) "high" else if (cv < 0.15) "medium" else "low"

  # Project dividend dates
  projected_dates <- as.Date(character(0))  # Initialize as Date vector
  next_date <- Sys.Date() + round(avg_days_between)

  while (next_date <= end_date) {
    projected_dates <- c(projected_dates, next_date)
    next_date <- next_date + round(avg_days_between)
  }

  if (length(projected_dates) == 0) {
    return(tibble::tibble(
      event_date = as.Date(character(0)),
      event_type = character(0),
      amount = numeric(0),
      confidence = character(0)
    ))
  }

  # Create event tibble
  dividend_events <- tibble::tibble(
    event_date = projected_dates,
    event_type = "dividend",
    amount = latest_amount * shares,
    confidence = confidence
  )

  log_debug("Income Projection: Generated {nrow(dividend_events)} dividend events for {ticker}")
  return(dividend_events)
}

################################################################################
# OPTION GAIN PROJECTION
################################################################################

#' Generate option exercise gain event
#'
#' Calculates the expected gain at option expiration.
#' Gain = Exercise Proceeds - Net Debit Paid
#' Where Net Debit = Stock Cost - Premium Received
#'
#' @param expiry_date Option expiration date
#' @param strike_price Option strike price
#' @param shares Number of shares
#' @param stock_cost Total cost of stock purchase (price * shares)
#' @param premium_received Premium received from selling the call
#' @return Tibble with single event row
#' @noRd
generate_option_gain_event <- function(expiry_date, strike_price, shares,
                                       stock_cost, premium_received) {

  # Calculate exercise proceeds (if assigned)
  exercise_proceeds <- strike_price * shares

  # Calculate net debit
  net_debit <- stock_cost - premium_received

  # Calculate gain
  gain <- exercise_proceeds - net_debit

  # Only create event if there's a positive gain
  if (gain <= 0) {
    log_debug("Income Projection: No positive option gain to project")
    return(tibble::tibble(
      event_date = as.Date(character(0)),
      event_type = character(0),
      amount = numeric(0),
      confidence = character(0)
    ))
  }

  tibble::tibble(
    event_date = expiry_date,
    event_type = "option_gain",
    amount = gain,
    confidence = "high"
  )
}

################################################################################
# OPTION SYMBOL PARSING
################################################################################

#' Parse option symbol to extract strike and expiry
#'
#' Extracts strike price and expiration date from option symbols like:
#' - "ALB17Dec27C55.00" -> strike: 55, expiry: 2027-12-17
#' - "AAPL240119C150" -> strike: 150, expiry: 2024-01-19
#'
#' @param option_symbol Option symbol string
#' @return List with strike (numeric) and expiry (Date)
#' @noRd
parse_option_details <- function(option_symbol) {

  # Default return
  result <- list(strike = NULL, expiry = NULL)

  if (is.na(option_symbol) || option_symbol == "") {
    return(result)
  }

  tryCatch({
    # Extract strike price (last numeric part)
    # Pattern: ends with C or P followed by number (possibly with decimal)
    strike_match <- stringr::str_extract(option_symbol, "[CP](\\d+\\.?\\d*)$")
    if (!is.na(strike_match)) {
      result$strike <- as.numeric(substr(strike_match, 2, nchar(strike_match)))
    }

    # Extract expiry date
    # Two common formats:
    # 1. "17Dec27" (ddMMMYY)
    # 2. "240119" (YYMMDD)

    # Try format 1: ddMMMYY (e.g., "17Dec27")
    date_match1 <- stringr::str_extract(option_symbol, "\\d{1,2}[A-Za-z]{3}\\d{2}")
    if (!is.na(date_match1)) {
      result$expiry <- as.Date(date_match1, format = "%d%b%y")
      return(result)
    }

    # Try format 2: YYMMDD (e.g., "240119")
    date_match2 <- stringr::str_extract(option_symbol, "\\d{6}")
    if (!is.na(date_match2)) {
      result$expiry <- as.Date(date_match2, format = "%y%m%d")
      return(result)
    }

    return(result)

  }, error = function(e) {
    log_warn("Income Projection: Failed to parse option symbol {option_symbol} - {e$message}")
    return(result)
  })
}
