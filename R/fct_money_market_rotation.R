#' Money Market Rotation Strategy Functions
#'
#' Business logic for tracking the money market dividend capture rotation strategy.
#' This is a standalone performance tracker independent of other portfolio modules.
#'
#' @name money-market-rotation
#' @import dplyr
#' @importFrom DBI dbGetQuery
#' @importFrom logger log_info log_debug log_warn log_error
#' @importFrom lubridate as_date floor_date
#' @importFrom glue glue
NULL

################################################################################
# TICKER CONFIGURATION
################################################################################

#' Get money market rotation tickers from config
#'
#' Returns tickers eligible for rotation tracking (excludes ZMMK.TO).
#' Uses the centralized get_cash_equivalent_tickers() function.
#'
#' @return Character vector of eligible tickers (uppercase)
#' @export
get_mm_rotation_tickers <- function() {
  # Use centralized cash equivalent config

  tickers <- get_cash_equivalent_tickers()

  # Exclude ZMMK.TO (Canadian, not part of USD rotation strategy)
  tickers <- tickers[!tickers %in% c("ZMMK.TO")]

  return(tickers)
}

#' Get tickers that are always included (not date-dependent)
#'
#' @return Character vector of always-included tickers
#' @noRd
get_always_included_tickers <- function() {
  all_tickers <- get_mm_rotation_tickers()
  # All except SGOV (which is conditional on first WEEK date)
  all_tickers[!all_tickers %in% c("SGOV")]
}

################################################################################
# DATE FILTERING
################################################################################

#' Get first WEEK transaction date
#'
#' @param conn DuckDB connection
#' @return Date or NA if no WEEK transactions
#' @export
get_first_week_date <- function(conn) {
  result <- tryCatch({
    dbGetQuery(conn, "
      SELECT MIN(trade_date) as first_date
      FROM account_activities
      WHERE UPPER(symbol) = 'WEEK'
        AND action = 'Buy'
        AND type = 'Trades'
    ")
  }, error = function(e) {
    log_error("MM Rotation: Failed to get first WEEK date - {e$message}")
    return(data.frame(first_date = NA))
  })

  if (nrow(result) == 0 || is.na(result$first_date)) {
    return(as.Date(NA))
  }

  as_date(result$first_date)
}

################################################################################
# TRANSACTION FETCHING
################################################################################

#' Fetch money market rotation transactions
#'
#' Gets all buy/sell transactions for eligible tickers.
#' SGOV is included only STRICTLY AFTER first WEEK transaction (same-day excluded).
#'
#' @param conn DuckDB connection
#' @return Data frame of filtered transactions
#' @export
fetch_mm_rotation_transactions <- function(conn) {
  always_tickers <- get_always_included_tickers()
  tickers_sql <- paste0("'", paste(always_tickers, collapse = "', '"), "'")

  query <- glue::glue("
    WITH first_week AS (
      SELECT COALESCE(MIN(trade_date), '9999-12-31'::TIMESTAMP) as first_week_date
      FROM account_activities
      WHERE UPPER(symbol) = 'WEEK'
        AND action = 'Buy'
        AND type = 'Trades'
    )
    SELECT
      aa.activity_id,
      aa.trade_date,
      aa.action,
      UPPER(aa.symbol) as symbol,
      aa.quantity,
      aa.price,
      aa.net_amount,
      aa.commission,
      aa.description
    FROM account_activities aa
    CROSS JOIN first_week fw
    WHERE aa.type = 'Trades'
      AND aa.action IN ('Buy', 'Sell')
      AND (
        UPPER(aa.symbol) IN ({tickers_sql})
        OR (UPPER(aa.symbol) = 'SGOV' AND aa.trade_date > fw.first_week_date)
      )
    ORDER BY aa.trade_date, aa.action
  ")

log_debug("MM Rotation: Fetching transactions with query")

  tryCatch({
    result <- dbGetQuery(conn, query)

    if (nrow(result) == 0) {
      log_info("MM Rotation: No transactions found")
      return(tibble(
        activity_id = character(),
        trade_date = as.POSIXct(character()),
        action = character(),
        symbol = character(),
        quantity = numeric(),
        price = numeric(),
        net_amount = numeric(),
        commission = numeric(),
        description = character()
      ))
    }

    result <- result %>%
      as_tibble() %>%
      mutate(
        trade_date = as.POSIXct(trade_date),
        quantity = as.numeric(quantity),
        price = as.numeric(price),
        net_amount = as.numeric(net_amount),
        commission = as.numeric(commission)
      )

    log_info("MM Rotation: Found {nrow(result)} transactions")
    return(result)

  }, error = function(e) {
    log_error("MM Rotation: Failed to fetch transactions - {e$message}")
    return(tibble(
      activity_id = character(),
      trade_date = as.POSIXct(character()),
      action = character(),
      symbol = character(),
      quantity = numeric(),
      price = numeric(),
      net_amount = numeric(),
      commission = numeric(),
      description = character()
    ))
  })
}

#' Fetch money market rotation dividends
#'
#' Gets all dividend payments for eligible tickers.
#' SGOV dividends included only STRICTLY AFTER first WEEK transaction.
#'
#' @param conn DuckDB connection
#' @return Data frame of dividend payments
#' @export
fetch_mm_rotation_dividends <- function(conn) {
  always_tickers <- get_always_included_tickers()
  tickers_sql <- paste0("'", paste(always_tickers, collapse = "', '"), "'")

  query <- glue::glue("
    WITH first_week AS (
      SELECT COALESCE(MIN(trade_date), '9999-12-31'::TIMESTAMP) as first_week_date
      FROM account_activities
      WHERE UPPER(symbol) = 'WEEK'
        AND action = 'Buy'
        AND type = 'Trades'
    )
    SELECT
      aa.activity_id,
      aa.trade_date as payment_date,
      UPPER(aa.symbol) as symbol,
      aa.net_amount as amount,
      aa.description
    FROM account_activities aa
    CROSS JOIN first_week fw
    WHERE aa.type = 'Dividends'
      AND (
        UPPER(aa.symbol) IN ({tickers_sql})
        OR (UPPER(aa.symbol) = 'SGOV' AND aa.trade_date > fw.first_week_date)
      )
    ORDER BY aa.trade_date
  ")

  tryCatch({
    result <- dbGetQuery(conn, query)

    if (nrow(result) == 0) {
      log_info("MM Rotation: No dividends found")
      return(tibble(
        activity_id = character(),
        payment_date = as.POSIXct(character()),
        symbol = character(),
        amount = numeric(),
        description = character()
      ))
    }

    result <- result %>%
      as_tibble() %>%
      mutate(
        payment_date = as.POSIXct(payment_date),
        amount = as.numeric(amount)
      )

    log_info("MM Rotation: Found {nrow(result)} dividends")
    return(result)

  }, error = function(e) {
    log_error("MM Rotation: Failed to fetch dividends - {e$message}")
    return(tibble(
      activity_id = character(),
      payment_date = as.POSIXct(character()),
      symbol = character(),
      amount = numeric(),
      description = character()
    ))
  })
}

################################################################################
# ROTATION HISTORY
################################################################################

#' Build rotation history from transactions
#'
#' Groups buy/sell transactions into rotation periods.
#' Each rotation starts with a Buy and ends with a Sell.
#'
#' @param transactions Data frame from fetch_mm_rotation_transactions
#' @return Data frame with rotation details (rotation_id, buy/sell pairs, gain/loss)
#' @export
build_rotation_history <- function(transactions) {
  if (nrow(transactions) == 0) {
    return(tibble(
      rotation_id = integer(),
      symbol = character(),
      buy_date = as.POSIXct(character()),
      buy_quantity = numeric(),
      buy_price = numeric(),
      buy_amount = numeric(),
      sell_date = as.POSIXct(character()),
      sell_quantity = numeric(),
      sell_price = numeric(),
      sell_amount = numeric(),
      realized_gain_loss = numeric(),
      holding_days = integer(),
      status = character()
    ))
  }

  rotations <- list()
  rotation_id <- 0
  current_position <- NULL

  # Process transactions in chronological order
  for (i in seq_len(nrow(transactions))) {
    txn <- transactions[i, ]

    if (txn$action == "Buy") {
      # Start new rotation
      rotation_id <- rotation_id + 1
      current_position <- list(
        rotation_id = rotation_id,
        symbol = txn$symbol,
        buy_date = txn$trade_date,
        buy_quantity = txn$quantity,
        buy_price = txn$price,
        buy_amount = abs(txn$net_amount),  # Buy amount is typically negative in net_amount
        sell_date = NA,
        sell_quantity = NA,
        sell_price = NA,
        sell_amount = NA,
        realized_gain_loss = NA,
        holding_days = NA,
        status = "Open"
      )

    } else if (txn$action == "Sell" && !is.null(current_position)) {
      # Close the current rotation
      if (txn$symbol == current_position$symbol) {
        current_position$sell_date <- txn$trade_date
        current_position$sell_quantity <- txn$quantity
        current_position$sell_price <- txn$price
        current_position$sell_amount <- txn$net_amount  # Sell is positive
        current_position$realized_gain_loss <- txn$net_amount - current_position$buy_amount
        current_position$holding_days <- as.integer(
          difftime(txn$trade_date, current_position$buy_date, units = "days")
        )
        current_position$status <- "Closed"

        rotations <- append(rotations, list(current_position))
        current_position <- NULL
      } else {
        # Selling a different symbol - unexpected scenario
        log_warn("MM Rotation: Unexpected sell of {txn$symbol} while holding {current_position$symbol}")
      }
    }
  }

  # Add any open position
  if (!is.null(current_position)) {
    rotations <- append(rotations, list(current_position))
  }

  if (length(rotations) == 0) {
    return(tibble(
      rotation_id = integer(),
      symbol = character(),
      buy_date = as.POSIXct(character()),
      buy_quantity = numeric(),
      buy_price = numeric(),
      buy_amount = numeric(),
      sell_date = as.POSIXct(character()),
      sell_quantity = numeric(),
      sell_price = numeric(),
      sell_amount = numeric(),
      realized_gain_loss = numeric(),
      holding_days = integer(),
      status = character()
    ))
  }

  # Convert list to tibble
  bind_rows(lapply(rotations, as_tibble))
}

################################################################################
# DIVIDEND ATTRIBUTION
################################################################################

#' Attribute dividends to rotation periods
#'
#' Dividends are attributed based on which ticker was held, not payment date.
#' A dividend posting after position close is attributed to the rotation that held it.
#'
#' @param dividends Data frame of dividend payments
#' @param rotations Data frame of rotation history
#' @return Data frame with dividends attributed to correct rotation period
#' @export
attribute_dividends_to_rotations <- function(dividends, rotations) {
  if (nrow(dividends) == 0) {
    return(tibble(
      activity_id = character(),
      payment_date = as.POSIXct(character()),
      symbol = character(),
      amount = numeric(),
      description = character(),
      rotation_id = integer()
    ))
  }

  if (nrow(rotations) == 0) {
    # No rotations yet - can't attribute
    return(dividends %>% mutate(rotation_id = NA_integer_))
  }

  # For each dividend, find the most recent rotation that held that ticker
  dividends_attributed <- dividends %>%
    rowwise() %>%
    mutate(
      rotation_id = {
        # Find rotations matching this symbol
        matching_rotations <- rotations %>%
          filter(symbol == .data$symbol)

        if (nrow(matching_rotations) == 0) {
          NA_integer_
        } else {
          # Find the rotation where dividend date falls within holding period
          # or is the most recent one for that ticker
          for (j in seq_len(nrow(matching_rotations))) {
            rot <- matching_rotations[j, ]

            # If rotation is open, check if dividend is after buy date
            if (rot$status == "Open" && payment_date >= rot$buy_date) {
              rot$rotation_id
              break
            }

            # If rotation is closed, check if dividend is within reasonable range
            # (dividend could post up to a few days after sell)
            if (rot$status == "Closed") {
              # Within holding period or up to 5 days after sell
              if (payment_date >= rot$buy_date &&
                  payment_date <= rot$sell_date + lubridate::days(5)) {
                rot$rotation_id
                break
              }
            }
          }

          # Fallback: attribute to most recent rotation for this ticker
          matching_rotations %>%
            filter(buy_date <= payment_date) %>%
            arrange(desc(buy_date)) %>%
            slice(1) %>%
            pull(rotation_id) %>%
            first() %>%
            ifelse(length(.) == 0, NA_integer_, .)
        }
      }
    ) %>%
    ungroup()

  return(dividends_attributed)
}

################################################################################
# CAPITAL FLOWS
################################################################################

#' Infer capital flows from rotation history
#'
#' Compares expected cash (sell proceeds + dividends) to next buy amount
#' to determine capital additions or withdrawals.
#'
#' @param rotations Data frame from build_rotation_history
#' @param dividends Data frame of dividends with rotation_id
#' @return Data frame with inferred additions/withdrawals
#' @export
infer_capital_flows <- function(rotations, dividends) {
  if (nrow(rotations) == 0) {
    return(tibble(
      rotation_id = integer(),
      date = as.POSIXct(character()),
      type = character(),
      amount = numeric()
    ))
  }

  flows <- list()

  # First rotation is always an addition (initial capital)
  first_rotation <- rotations %>% filter(rotation_id == 1)
  if (nrow(first_rotation) > 0) {
    flows <- append(flows, list(tibble(
      rotation_id = 1,
      date = first_rotation$buy_date,
      type = "Addition",
      amount = first_rotation$buy_amount
    )))
  }

  # For subsequent rotations, calculate expected vs actual
  closed_rotations <- rotations %>% filter(status == "Closed")

  for (i in seq_len(nrow(closed_rotations))) {
    rot <- closed_rotations[i, ]
    next_rot_id <- rot$rotation_id + 1

    # Get next rotation
    next_rot <- rotations %>% filter(rotation_id == next_rot_id)

    if (nrow(next_rot) > 0) {
      # Expected cash = sell proceeds + dividends from this rotation
      rot_dividends <- dividends %>%
        filter(rotation_id == rot$rotation_id) %>%
        summarise(total_divs = sum(amount, na.rm = TRUE)) %>%
        pull(total_divs)

      expected_cash <- rot$sell_amount + rot_dividends
      actual_buy <- next_rot$buy_amount

      difference <- actual_buy - expected_cash

      if (abs(difference) > 1) {  # More than $1 difference
        if (difference > 0) {
          flows <- append(flows, list(tibble(
            rotation_id = next_rot_id,
            date = next_rot$buy_date,
            type = "Addition",
            amount = difference
          )))
        } else {
          flows <- append(flows, list(tibble(
            rotation_id = next_rot_id,
            date = next_rot$buy_date,
            type = "Withdrawal",
            amount = abs(difference)
          )))
        }
      }
    }
  }

  if (length(flows) == 0) {
    return(tibble(
      rotation_id = integer(),
      date = as.POSIXct(character()),
      type = character(),
      amount = numeric()
    ))
  }

  bind_rows(flows)
}

################################################################################
# PERFORMANCE CALCULATIONS
################################################################################

#' Calculate time-weighted return
#'
#' Uses Modified Dietz method for TWR calculation with capital flows.
#'
#' @param rotations Rotation history
#' @param dividends Dividend history with rotation_id
#' @param capital_flows Inferred capital flows
#' @return List with TWR percentage and sub-period details
#' @export
calculate_twr <- function(rotations, dividends, capital_flows) {
  if (nrow(rotations) == 0) {
    return(list(
      twr_pct = 0,
      total_return_dollars = 0,
      total_dividends = 0,
      total_realized_gains = 0,
      total_capital_added = 0,
      total_capital_withdrawn = 0,
      days_active = 0,
      annualized_return = 0
    ))
  }

  # Calculate totals
  total_dividends <- sum(dividends$amount, na.rm = TRUE)

  closed_rotations <- rotations %>% filter(status == "Closed")
  total_realized_gains <- sum(closed_rotations$realized_gain_loss, na.rm = TRUE)

  total_return_dollars <- total_dividends + total_realized_gains

  # Capital flows
  additions <- capital_flows %>%
    filter(type == "Addition") %>%
    summarise(total = sum(amount, na.rm = TRUE)) %>%
    pull(total)

  withdrawals <- capital_flows %>%
    filter(type == "Withdrawal") %>%
    summarise(total = sum(amount, na.rm = TRUE)) %>%
    pull(total)

  # Days active
  first_date <- min(rotations$buy_date, na.rm = TRUE)
  last_date <- max(c(rotations$buy_date, rotations$sell_date), na.rm = TRUE)
  days_active <- as.integer(difftime(Sys.time(), first_date, units = "days"))

  # Simple TWR calculation (Modified Dietz approximation)
  # TWR = (Ending Value - Beginning Value - Cash Flows) / (Beginning Value + Weighted Cash Flows)
  beginning_value <- rotations %>%
    filter(rotation_id == 1) %>%
    pull(buy_amount) %>%
    first()

  if (is.na(beginning_value) || beginning_value == 0) {
    beginning_value <- 1  # Avoid division by zero
  }

  # For current position, estimate current value
  open_rotation <- rotations %>% filter(status == "Open")
  if (nrow(open_rotation) > 0) {
    # Use buy amount as proxy (would need real-time price for actual value)
    ending_value <- open_rotation$buy_amount
  } else {
    # No open position - use last sell amount
    ending_value <- closed_rotations %>%
      filter(rotation_id == max(rotation_id)) %>%
      pull(sell_amount) %>%
      first()

    if (is.na(ending_value)) ending_value <- 0
  }

  # Add dividends to ending value
  ending_value <- ending_value + total_dividends

  # Net cash flows (additions are inflows, withdrawals are outflows)
  net_cash_flows <- additions - withdrawals - beginning_value

  # TWR approximation
  if (beginning_value > 0) {
    twr_pct <- ((ending_value - beginning_value - net_cash_flows) / beginning_value) * 100
  } else {
    twr_pct <- 0
  }

  # Annualized return
  if (days_active > 0) {
    annualized_return <- ((1 + twr_pct / 100) ^ (365 / days_active) - 1) * 100
  } else {
    annualized_return <- 0
  }

  list(
    twr_pct = round(twr_pct, 2),
    total_return_dollars = round(total_return_dollars, 2),
    total_dividends = round(total_dividends, 2),
    total_realized_gains = round(total_realized_gains, 2),
    total_capital_added = round(additions, 2),
    total_capital_withdrawn = round(withdrawals, 2),
    days_active = days_active,
    annualized_return = round(annualized_return, 2)
  )
}

################################################################################
# CURRENT POSITION
################################################################################

#' Get current position summary
#'
#' @param rotations Rotation history
#' @return List with current ticker, shares, cost basis, or NULL if no position
#' @export
get_current_mm_position <- function(rotations) {
  open_rotation <- rotations %>% filter(status == "Open")

  if (nrow(open_rotation) == 0) {
    return(list(
      has_position = FALSE,
      symbol = NA_character_,
      shares = 0,
      cost_basis = 0,
      buy_date = NA,
      holding_days = 0
    ))
  }

  list(
    has_position = TRUE,
    symbol = open_rotation$symbol,
    shares = open_rotation$buy_quantity,
    cost_basis = open_rotation$buy_amount,
    buy_date = open_rotation$buy_date,
    holding_days = as.integer(difftime(Sys.time(), open_rotation$buy_date, units = "days"))
  )
}

################################################################################
# SUMMARY STATISTICS
################################################################################

#' Get rotation summary statistics
#'
#' @param rotations Rotation history
#' @param dividends Dividend history
#' @return List with summary stats
#' @export
get_mm_rotation_summary <- function(rotations, dividends) {
  total_rotations <- nrow(rotations)
  completed_rotations <- nrow(rotations %>% filter(status == "Closed"))

  if (completed_rotations > 0) {
    closed <- rotations %>% filter(status == "Closed")
    avg_holding_days <- mean(closed$holding_days, na.rm = TRUE)
    total_realized_gain <- sum(closed$realized_gain_loss, na.rm = TRUE)
    winning_rotations <- sum(closed$realized_gain_loss > 0, na.rm = TRUE)
    win_rate <- (winning_rotations / completed_rotations) * 100
  } else {
    avg_holding_days <- 0
    total_realized_gain <- 0
    winning_rotations <- 0
    win_rate <- 0
  }

  total_dividends <- sum(dividends$amount, na.rm = TRUE)
  unique_tickers_used <- length(unique(rotations$symbol))

  list(
    total_rotations = total_rotations,
    completed_rotations = completed_rotations,
    avg_holding_days = round(avg_holding_days, 1),
    total_realized_gain = round(total_realized_gain, 2),
    total_dividends = round(total_dividends, 2),
    total_income = round(total_realized_gain + total_dividends, 2),
    win_rate = round(win_rate, 1),
    unique_tickers_used = unique_tickers_used
  )
}

################################################################################
# PERIOD RETURNS
################################################################################

#' Calculate returns for a specific date range
#'
#' @param rotations Rotation history
#' @param dividends Dividend history
#' @param capital_flows Capital flow history
#' @param start_date Start of period
#' @param end_date End of period
#' @return List with period performance metrics
#' @export
calculate_period_returns <- function(rotations, dividends, capital_flows, start_date, end_date) {
  start_dt <- as.POSIXct(start_date)
  end_dt <- as.POSIXct(paste0(as.Date(end_date), " 23:59:59"))

  if (nrow(rotations) == 0) {
    return(list(
      total_dividends = 0,
      total_realized_gains = 0,
      total_return_dollars = 0,
      total_capital_added = 0,
      total_capital_withdrawn = 0,
      twr_pct = 0,
      annualized_return = 0,
      days_in_period = as.integer(difftime(end_dt, start_dt, units = "days"))
    ))
  }

  # Dividends in period
  divs_in_period <- dividends %>%
    filter(payment_date >= start_dt & payment_date <= end_dt)
  total_dividends <- sum(divs_in_period$amount, na.rm = TRUE)

  # Realized gains in period (closed rotations)
  closed_in_period <- rotations %>%
    filter(status == "Closed") %>%
    filter(sell_date >= start_dt & sell_date <= end_dt)
  total_realized_gains <- sum(closed_in_period$realized_gain_loss, na.rm = TRUE)

  total_return_dollars <- total_dividends + total_realized_gains

  # Capital flows in period
  if (nrow(capital_flows) > 0) {
    flows_in_period <- capital_flows %>%
      filter(date >= start_dt & date <= end_dt)

    total_capital_added <- flows_in_period %>%
      filter(type == "Addition") %>%
      summarise(total = sum(amount, na.rm = TRUE)) %>%
      pull(total)

    total_capital_withdrawn <- flows_in_period %>%
      filter(type == "Withdrawal") %>%
      summarise(total = sum(amount, na.rm = TRUE)) %>%
      pull(total)
  } else {
    total_capital_added <- 0
    total_capital_withdrawn <- 0
  }

  # Starting value for period
  position_at_start <- rotations %>%
    filter(buy_date < start_dt) %>%
    filter(is.na(sell_date) | sell_date >= start_dt) %>%
    arrange(desc(buy_date)) %>%
    slice(1)

  if (nrow(position_at_start) > 0) {
    starting_value <- position_at_start$buy_amount
  } else {
    # No position at start of period - starting value is 0
    # The first buy will be captured as capital_added
    starting_value <- 0
  }

  # Ending value for period
  position_at_end <- rotations %>%
    filter(buy_date <= end_dt) %>%
    filter(is.na(sell_date) | sell_date > end_dt) %>%
    arrange(desc(buy_date)) %>%
    slice(1)

  if (nrow(position_at_end) > 0) {
    ending_value <- position_at_end$buy_amount
  } else {
    last_sell_in_period <- rotations %>%
      filter(status == "Closed") %>%
      filter(sell_date >= start_dt & sell_date <= end_dt) %>%
      arrange(desc(sell_date)) %>%
      slice(1)

    if (nrow(last_sell_in_period) > 0) {
      ending_value <- last_sell_in_period$sell_amount
    } else {
      ending_value <- starting_value
    }
  }

  # Calculate TWR
  net_flows <- total_capital_added - total_capital_withdrawn

  if (starting_value > 0) {
    weighted_flows <- net_flows * 0.5
    denominator <- starting_value + weighted_flows

    if (denominator > 0) {
      twr_pct <- ((ending_value - starting_value - net_flows + total_return_dollars) / denominator) * 100
    } else {
      twr_pct <- 0
    }
  } else if (total_capital_added > 0) {
    twr_pct <- (total_return_dollars / total_capital_added) * 100
  } else {
    twr_pct <- 0
  }

  # Annualized return
  days_in_period <- as.integer(difftime(end_dt, start_dt, units = "days"))
  if (days_in_period > 0 && twr_pct != 0) {
    annualized_return <- ((1 + twr_pct / 100) ^ (365 / days_in_period) - 1) * 100
  } else {
    annualized_return <- 0
  }

  list(
    total_dividends = round(total_dividends, 2),
    total_realized_gains = round(total_realized_gains, 2),
    total_return_dollars = round(total_return_dollars, 2),
    total_capital_added = round(total_capital_added, 2),
    total_capital_withdrawn = round(total_capital_withdrawn, 2),
    twr_pct = round(twr_pct, 2),
    annualized_return = round(annualized_return, 2),
    days_in_period = days_in_period
  )
}

#' Get available periods for filter
#'
#' @param rotations Rotation history
#' @return List with period options
#' @export
get_available_periods <- function(rotations) {
  if (nrow(rotations) == 0) {
    return(list(
      choices = c("All Time" = "all"),
      default = "all"
    ))
  }

  all_dates <- c(rotations$buy_date, rotations$sell_date)
  all_dates <- all_dates[!is.na(all_dates)]

  if (length(all_dates) == 0) {
    return(list(
      choices = c("All Time" = "all"),
      default = "all"
    ))
  }

  min_date <- min(all_dates)
  max_date <- max(max(all_dates), Sys.time())

  choices <- c("All Time" = "all", "YTD" = "ytd")

  # Add years
  years <- sort(unique(lubridate::year(all_dates)), decreasing = TRUE)
  for (yr in years) {
    choices <- c(choices, setNames(paste0("y_", yr), as.character(yr)))
  }

  # Add quarters
  for (yr in years) {
    for (q in 4:1) {
      q_start <- as.Date(paste0(yr, "-", (q - 1) * 3 + 1, "-01"))
      q_end <- as.Date(paste0(yr, "-", q * 3, "-01")) - 1
      q_end <- lubridate::ceiling_date(lubridate::`%m+%`(q_start, months(3)), "month") - 1

      if (q_start <= as.Date(max_date) && q_end >= as.Date(min_date)) {
        q_label <- paste0("Q", q, " ", yr)
        choices <- c(choices, setNames(paste0("q_", yr, "_", q), q_label))
      }
    }
  }

  # Add months (last 12)
  current_month <- lubridate::floor_date(Sys.Date(), "month")
  for (i in 0:11) {
    m_date <- lubridate::`%m-%`(current_month, months(i))
    if (m_date >= lubridate::floor_date(as.Date(min_date), "month")) {
      m_label <- format(m_date, "%b %Y")
      choices <- c(choices, setNames(paste0("m_", format(m_date, "%Y_%m")), m_label))
    }
  }

  list(
    choices = choices,
    default = "all"
  )
}

#' Parse period selection to date range
#'
#' @param period_value Value from period selector
#' @param rotations Rotation history (for "all" option)
#' @return List with start_date and end_date
#' @export
parse_period_selection <- function(period_value, rotations) {
  today <- Sys.Date()

  if (period_value == "all") {
    if (nrow(rotations) == 0) {
      return(list(start_date = today, end_date = today))
    }
    all_dates <- c(rotations$buy_date, rotations$sell_date)
    all_dates <- all_dates[!is.na(all_dates)]
    return(list(
      start_date = as.Date(min(all_dates)),
      end_date = today
    ))
  }

  if (period_value == "ytd") {
    return(list(
      start_date = as.Date(paste0(lubridate::year(today), "-01-01")),
      end_date = today
    ))
  }

  if (startsWith(period_value, "y_")) {
    yr <- as.integer(sub("y_", "", period_value))
    return(list(
      start_date = as.Date(paste0(yr, "-01-01")),
      end_date = as.Date(paste0(yr, "-12-31"))
    ))
  }

  if (startsWith(period_value, "q_")) {
    parts <- strsplit(sub("q_", "", period_value), "_")[[1]]
    yr <- as.integer(parts[1])
    q <- as.integer(parts[2])
    q_start <- as.Date(paste0(yr, "-", (q - 1) * 3 + 1, "-01"))
    q_end <- lubridate::ceiling_date(lubridate::`%m+%`(q_start, months(3)), "month") - 1
    return(list(
      start_date = q_start,
      end_date = q_end
    ))
  }

  if (startsWith(period_value, "m_")) {
    parts <- strsplit(sub("m_", "", period_value), "_")[[1]]
    yr <- as.integer(parts[1])
    mo <- as.integer(parts[2])
    m_start <- as.Date(paste0(yr, "-", mo, "-01"))
    m_end <- lubridate::ceiling_date(m_start, "month") - 1
    return(list(
      start_date = m_start,
      end_date = m_end
    ))
  }

  # Fallback
  list(start_date = today, end_date = today)
}

#' Calculate returns by period type for table display
#'
#' @param rotations Rotation history
#' @param dividends Dividend history
#' @param capital_flows Capital flow history
#' @param period_type One of "monthly", "quarterly", "yearly"
#' @return Data frame with period returns
#' @export
calculate_returns_by_period_type <- function(rotations, dividends, capital_flows, period_type = "yearly") {
  if (nrow(rotations) == 0) {
    return(tibble(
      period = character(),
      dividends = numeric(),
      cap_gains = numeric(),
      total_income = numeric(),
      return_pct = numeric()
    ))
  }

  all_dates <- c(rotations$buy_date, rotations$sell_date)
  all_dates <- all_dates[!is.na(all_dates)]

  if (length(all_dates) == 0) {
    return(tibble(
      period = character(),
      dividends = numeric(),
      cap_gains = numeric(),
      total_income = numeric(),
      return_pct = numeric()
    ))
  }

  min_date <- as.Date(min(all_dates))
  max_date <- max(as.Date(max(all_dates)), Sys.Date())

  # Generate period ranges
  if (period_type == "monthly") {
    starts <- seq(lubridate::floor_date(min_date, "month"),
                  lubridate::floor_date(max_date, "month"),
                  by = "month")
    periods <- lapply(starts, function(s) {
      list(
        label = format(s, "%b %Y"),
        start = s,
        end = lubridate::ceiling_date(s, "month") - 1
      )
    })
  } else if (period_type == "quarterly") {
    starts <- seq(lubridate::floor_date(min_date, "quarter"),
                  lubridate::floor_date(max_date, "quarter"),
                  by = "quarter")
    periods <- lapply(starts, function(s) {
      q <- lubridate::quarter(s)
      yr <- lubridate::year(s)
      list(
        label = paste0("Q", q, " ", yr),
        start = s,
        end = lubridate::ceiling_date(s, "quarter") - 1
      )
    })
  } else {
    # yearly
    years <- sort(unique(lubridate::year(all_dates)))
    periods <- lapply(years, function(yr) {
      list(
        label = as.character(yr),
        start = as.Date(paste0(yr, "-01-01")),
        end = as.Date(paste0(yr, "-12-31"))
      )
    })
  }

  # Calculate returns for each period
  results <- lapply(periods, function(p) {
    perf <- calculate_period_returns(rotations, dividends, capital_flows, p$start, p$end)
    tibble(
      period = p$label,
      dividends = perf$total_dividends,
      cap_gains = perf$total_realized_gains,
      total_income = perf$total_return_dollars,
      return_pct = perf$twr_pct
    )
  })

  bind_rows(results)
}

#' Calculate returns by year
#'
#' Breaks down performance metrics by calendar year.
#'
#' @param rotations Rotation history
#' @param dividends Dividend history
#' @param capital_flows Capital flow history
#' @return Data frame with yearly performance metrics
#' @export
calculate_returns_by_year <- function(rotations, dividends, capital_flows) {
  if (nrow(rotations) == 0) {
    return(tibble(
      year = integer(),
      rotations_completed = integer(),
      realized_gains = numeric(),
      dividends_received = numeric(),
      total_income = numeric(),
      capital_added = numeric(),
      capital_withdrawn = numeric(),
      starting_value = numeric(),
      ending_value = numeric(),
      return_pct = numeric()
    ))
  }

  # Get all years with activity
  all_dates <- c(rotations$buy_date, rotations$sell_date)
  all_dates <- all_dates[!is.na(all_dates)]

  if (length(all_dates) == 0) {
    return(tibble(
      year = integer(),
      rotations_completed = integer(),
      realized_gains = numeric(),
      dividends_received = numeric(),
      total_income = numeric(),
      capital_added = numeric(),
      capital_withdrawn = numeric(),
      starting_value = numeric(),
      ending_value = numeric(),
      return_pct = numeric()
    ))
  }

  years <- sort(unique(lubridate::year(all_dates)))

  yearly_data <- lapply(years, function(yr) {
    year_start <- as.POSIXct(paste0(yr, "-01-01 00:00:00"))
    year_end <- as.POSIXct(paste0(yr, "-12-31 23:59:59"))

    # Rotations completed in this year (sell date in year)
    closed_in_year <- rotations %>%
      filter(status == "Closed") %>%
      filter(sell_date >= year_start & sell_date <= year_end)

    rotations_completed <- nrow(closed_in_year)
    realized_gains <- sum(closed_in_year$realized_gain_loss, na.rm = TRUE)

    # Dividends received in this year
    divs_in_year <- dividends %>%
      filter(payment_date >= year_start & payment_date <= year_end)
    dividends_received <- sum(divs_in_year$amount, na.rm = TRUE)

    total_income <- realized_gains + dividends_received

    # Capital flows in this year
    if (nrow(capital_flows) > 0) {
      flows_in_year <- capital_flows %>%
        filter(date >= year_start & date <= year_end)

      capital_added <- flows_in_year %>%
        filter(type == "Addition") %>%
        summarise(total = sum(amount, na.rm = TRUE)) %>%
        pull(total)

      capital_withdrawn <- flows_in_year %>%
        filter(type == "Withdrawal") %>%
        summarise(total = sum(amount, na.rm = TRUE)) %>%
        pull(total)
    } else {
      capital_added <- 0
      capital_withdrawn <- 0
    }

    # Starting value: position value at start of year
    # Find the most recent buy before year start that wasn't sold before year start
    position_at_start <- rotations %>%
      filter(buy_date < year_start) %>%
      filter(is.na(sell_date) | sell_date >= year_start) %>%
      arrange(desc(buy_date)) %>%
      slice(1)

    if (nrow(position_at_start) > 0) {
      starting_value <- position_at_start$buy_amount
    } else {
      # No position at year start - starting value is 0
      # The first buy will be captured as capital_added
      starting_value <- 0
    }

    # Ending value: position value at end of year
    # Find position held at year end
    position_at_end <- rotations %>%
      filter(buy_date <= year_end) %>%
      filter(is.na(sell_date) | sell_date > year_end) %>%
      arrange(desc(buy_date)) %>%
      slice(1)

    if (nrow(position_at_end) > 0) {
      ending_value <- position_at_end$buy_amount
    } else {
      # No position at year end - use last sell amount
      last_sell_in_year <- rotations %>%
        filter(status == "Closed") %>%
        filter(sell_date >= year_start & sell_date <= year_end) %>%
        arrange(desc(sell_date)) %>%
        slice(1)

      if (nrow(last_sell_in_year) > 0) {
        ending_value <- last_sell_in_year$sell_amount
      } else {
        ending_value <- starting_value
      }
    }

    # Calculate return percentage using Modified Dietz
    # Return = (Ending - Starting - Net Flows + Income) / (Starting + Weighted Flows)
    net_flows <- capital_added - capital_withdrawn

    if (starting_value > 0) {
      # Simplified: assume flows occur mid-period
      weighted_flows <- net_flows * 0.5
      denominator <- starting_value + weighted_flows

      if (denominator > 0) {
        return_pct <- ((ending_value - starting_value - net_flows + total_income) / denominator) * 100
      } else {
        return_pct <- 0
      }
    } else if (capital_added > 0) {
      # First year - return based on income vs capital added
      return_pct <- (total_income / capital_added) * 100
    } else {
      return_pct <- 0
    }

    tibble(
      year = yr,
      rotations_completed = rotations_completed,
      realized_gains = round(realized_gains, 2),
      dividends_received = round(dividends_received, 2),
      total_income = round(total_income, 2),
      capital_added = round(capital_added, 2),
      capital_withdrawn = round(capital_withdrawn, 2),
      starting_value = round(starting_value, 2),
      ending_value = round(ending_value, 2),
      return_pct = round(return_pct, 2)
    )
  })

  bind_rows(yearly_data)
}
