#' Position Group P&L Calculations
#'
#' Functions for calculating profit/loss for closed position groups.
#' Includes all transaction types: stock trades, option premiums, dividends,
#' and commissions. Calculates both total return and annualized return.
#'
#' @name group-pnl
#' @import dplyr
#' @importFrom logger log_info log_warn log_error log_debug
NULL

################################################################################
# P&L CALCULATION
################################################################################

#' Calculate P&L for a position group
#'
#' Retrieves all activities linked to a group and calculates comprehensive
#' profit/loss metrics including dividends.
#'
#' Returns:
#' - total_cost: Total money invested (stock purchases + commissions)
#' - total_proceeds: Total money received (stock sales + option premiums + dividends)
#' - total_dividends: Sum of all dividend payments
#' - net_pnl: Total proceeds - total cost
#' - total_return_pct: (net_pnl / total_cost) * 100
#' - annualized_return_pct: Total return annualized based on hold period
#' - hold_days: Number of days position was held
#'
#' @param group_id Group identifier
#' @return Tibble with P&L breakdown or empty tibble if failed
#' @noRd
calculate_group_pnl <- function(group_id) {
  tryCatch({
    # Get all activities for this group
    activities <- get_activities_by_group(group_id)

    if (nrow(activities) == 0) {
      log_warn("P&L Calc: No activities found for group {group_id}")
      return(tibble::tibble())
    }

    # Get group info for dates and strategy type
    group_info <- get_group_by_id(group_id)

    if (nrow(group_info) == 0) {
      log_warn("P&L Calc: Group {group_id} not found")
      return(tibble::tibble())
    }

    strategy_type <- group_info$strategy_type[1]

    # Calculate total cost (money out)
    # Stock purchases: use gross_amount (excluding commissions)
    # Commissions: tracked separately to avoid double-counting
    stock_purchases <- activities %>%
      filter(type == "Trades", action == "Buy", !purrr::map_lgl(symbol, is_option_symbol)) %>%
      summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
      pull(total)

    stock_purchases <- if (length(stock_purchases) == 0) 0 else stock_purchases

    # Total commissions
    total_commissions <- activities %>%
      summarise(total = sum(abs(commission), na.rm = TRUE)) %>%
      pull(total)

    total_commissions <- if (length(total_commissions) == 0) 0 else total_commissions

    # Option sales (premiums received): use gross_amount (excluding commissions)
    option_premiums <- activities %>%
      filter(type == "Trades", action == "Sell", purrr::map_lgl(symbol, is_option_symbol)) %>%
      summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
      pull(total)

    option_premiums <- if (length(option_premiums) == 0) 0 else option_premiums

    # Calculate total proceeds (money in)
    # Stock sales: use gross_amount (excluding commissions)
    stock_sales <- activities %>%
      filter(type == "Trades", action == "Sell", !purrr::map_lgl(symbol, is_option_symbol)) %>%
      summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
      pull(total)

    stock_sales <- if (length(stock_sales) == 0) 0 else stock_sales

    # Dividends received: positive (IMPORTANT: included in P&L)
    total_dividends <- activities %>%
      filter(type == "Dividends") %>%
      summarise(total = sum(abs(net_amount), na.rm = TRUE)) %>%
      pull(total)

    total_dividends <- if (length(total_dividends) == 0) 0 else total_dividends

    # Apply strategy-specific accounting
    # For covered call strategies (non-"Other"): premiums reduce cost basis
    # For "Other" strategy: premiums are proceeds (legacy behavior)
    if (strategy_type != "Other") {
      # Net debit accounting: premiums offset cost
      total_cost <- stock_purchases + total_commissions - option_premiums
      total_proceeds <- stock_sales + total_dividends
    } else {
      # Legacy accounting: premiums as income
      total_cost <- stock_purchases + total_commissions
      total_proceeds <- stock_sales + option_premiums + total_dividends
    }

    # Net P&L = proceeds - cost
    net_pnl <- total_proceeds - total_cost

    # Total return percentage
    total_return_pct <- if (total_cost > 0) {
      (net_pnl / total_cost) * 100
    } else {
      0
    }

    # Calculate hold period in days
    first_trade_date <- activities %>%
      filter(!is.na(trade_date)) %>%
      arrange(trade_date) %>%
      slice(1) %>%
      pull(trade_date) %>%
      as.Date()

    last_trade_date <- activities %>%
      filter(!is.na(trade_date)) %>%
      arrange(desc(trade_date)) %>%
      slice(1) %>%
      pull(trade_date) %>%
      as.Date()

    hold_days <- as.numeric(last_trade_date - first_trade_date)
    hold_days <- if (length(hold_days) == 0 || is.na(hold_days) || hold_days == 0) 1 else hold_days

    # Annualized return percentage
    # Formula: ((1 + total_return)^(365/hold_days) - 1) * 100
    annualized_return_pct <- if (total_cost > 0 && hold_days > 0) {
      total_return_ratio <- 1 + (net_pnl / total_cost)
      annualization_factor <- 365 / hold_days
      ((total_return_ratio ^ annualization_factor) - 1) * 100
    } else {
      0
    }

    # Return P&L breakdown
    pnl_result <- tibble::tibble(
      group_id = group_id,
      total_cost = total_cost,
      total_proceeds = total_proceeds,
      stock_purchases = stock_purchases,
      stock_sales = stock_sales,
      option_premiums = option_premiums,
      total_dividends = total_dividends,
      total_commissions = total_commissions,
      net_pnl = net_pnl,
      total_return_pct = total_return_pct,
      annualized_return_pct = annualized_return_pct,
      hold_days = hold_days,
      first_trade_date = as.character(first_trade_date),
      last_trade_date = as.character(last_trade_date)
    )

    log_info("P&L Calc: Calculated P&L for group {group_id} - Net: ${round(net_pnl, 2)}, Return: {round(total_return_pct, 2)}%, Annualized: {round(annualized_return_pct, 2)}%")

    return(pnl_result)

  }, error = function(e) {
    log_error("P&L Calc: Failed to calculate P&L for group {group_id} - {e$message}")
    return(tibble::tibble())
  })
}

#' Update group with P&L metrics
#'
#' Calculates P&L and saves to position_groups table
#'
#' @param group_id Group identifier
#' @return Logical TRUE if successful
#' @noRd
update_group_pnl <- function(group_id) {
  # Calculate P&L
  pnl <- calculate_group_pnl(group_id)

  if (nrow(pnl) == 0) {
    log_warn("P&L Update: Cannot update group {group_id} - calculation failed")
    return(FALSE)
  }

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Update position_groups table with P&L metrics
    rows_affected <- dbExecute(conn, "
      UPDATE position_groups
      SET total_return_pct = ?,
          total_return_amount = ?,
          annualized_return_pct = ?,
          updated_at = ?
      WHERE group_id = ?
    ", params = list(
      pnl$total_return_pct,
      pnl$net_pnl,
      pnl$annualized_return_pct,
      Sys.time(),
      group_id
    ))

    if (rows_affected > 0) {
      log_info("P&L Update: Updated group {group_id} with P&L metrics")
      return(TRUE)
    } else {
      log_warn("P&L Update: Group {group_id} not found")
      return(FALSE)
    }

  }, error = function(e) {
    log_error("P&L Update: Failed to update group {group_id} - {e$message}")
    return(FALSE)
  })
}

#' Close a position group and calculate final P&L
#'
#' Marks group as closed and calculates/saves final P&L metrics.
#' Removes all projected cash flows (future dividends) since position is closed.
#'
#' @param group_id Group identifier
#' @return Tibble with P&L results or empty tibble if failed
#' @export
close_position_group <- function(group_id) {
  # Calculate P&L first
  pnl <- calculate_group_pnl(group_id)

  if (nrow(pnl) == 0) {
    log_warn("Close Group: Cannot close group {group_id} - P&L calculation failed")
    return(tibble::tibble())
  }

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Delete all projected cash flows (future dividends no longer relevant)
    delete_group_cash_flows(group_id, status_filter = "projected")

    # Create actual cash flow event for final realized P&L
    # Get latest activity date (liquidation/close date)
    latest_activity <- dbGetQuery(conn, "
      SELECT MAX(trade_date) as close_date
      FROM account_activities
      WHERE group_id = ?
    ", params = list(group_id))

    if (nrow(latest_activity) > 0 && !is.na(latest_activity$close_date[1])) {
      close_date <- as.Date(latest_activity$close_date[1])

      # Create actual cash flow event for the realized gain/loss
      save_cash_flow_event(
        group_id = group_id,
        event_date = close_date,
        event_type = "option_gain",
        amount = pnl$net_pnl,
        status = "actual",
        confidence = "high",
        conn = conn
      )

      log_info("Close Group: Created actual cash flow event for final P&L: ${round(pnl$net_pnl, 2)} on {close_date}")
    }

    # Update group status to closed and save P&L
    rows_affected <- dbExecute(conn, "
      UPDATE position_groups
      SET status = 'closed',
          total_return_pct = ?,
          total_return_amount = ?,
          annualized_return_pct = ?,
          updated_at = ?
      WHERE group_id = ?
    ", params = list(
      pnl$total_return_pct,
      pnl$net_pnl,
      pnl$annualized_return_pct,
      Sys.time(),
      group_id
    ))

    if (rows_affected > 0) {
      log_info("Close Group: Closed group {group_id} with final P&L - Net: ${round(pnl$net_pnl, 2)}, Total Return: {round(pnl$total_return_pct, 2)}%, Annualized: {round(pnl$annualized_return_pct, 2)}%")
      return(pnl)
    } else {
      log_warn("Close Group: Group {group_id} not found")
      return(tibble::tibble())
    }

  }, error = function(e) {
    log_error("Close Group: Failed to close group {group_id} - {e$message}")
    return(tibble::tibble())
  })
}

#' Auto-close a position group (when members disappear from positions)
#'
#' Automatically closes groups when their positions are no longer active.
#' P&L calculation skipped for "Other" strategy, calculated for named strategies.
#'
#' @param group_id Group identifier
#' @return Tibble with P&L results (if calculated) or empty tibble
#' @noRd
auto_close_group <- function(group_id) {
  # Get group info to check strategy type
  group_info <- get_group_by_id(group_id)

  if (nrow(group_info) == 0) {
    log_warn("Auto-Close: Group {group_id} not found")
    return(tibble::tibble())
  }

  strategy_type <- group_info$strategy_type[1]

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Always delete projected cash flows (future events no longer relevant)
    delete_group_cash_flows(group_id, status_filter = "projected")

    # If "Other" strategy, skip P&L calculation
    if (strategy_type == "Other") {
      rows_affected <- dbExecute(conn, "
        UPDATE position_groups
        SET status = 'closed',
            updated_at = ?
        WHERE group_id = ?
      ", params = list(Sys.time(), group_id))

      if (rows_affected > 0) {
        log_info("Auto-Close: Closed 'Other' strategy group {group_id} (no P&L calculation)")
        return(tibble::tibble())
      } else {
        log_warn("Auto-Close: Group {group_id} not found")
        return(tibble::tibble())
      }
    }

    # For named strategies, calculate P&L
    pnl <- calculate_group_pnl(group_id)

    if (nrow(pnl) == 0) {
      log_warn("Auto-Close: Cannot close group {group_id} - P&L calculation failed")
      return(tibble::tibble())
    }

    # Create actual cash flow event for final realized P&L
    latest_activity <- dbGetQuery(conn, "
      SELECT MAX(trade_date) as close_date
      FROM account_activities
      WHERE group_id = ?
    ", params = list(group_id))

    if (nrow(latest_activity) > 0 && !is.na(latest_activity$close_date[1])) {
      close_date <- as.Date(latest_activity$close_date[1])

      save_cash_flow_event(
        group_id = group_id,
        event_date = close_date,
        event_type = "option_gain",
        amount = pnl$net_pnl,
        status = "actual",
        confidence = "high",
        conn = conn
      )

      log_info("Auto-Close: Created actual cash flow event for final P&L: ${round(pnl$net_pnl, 2)} on {close_date}")
    }

    # Update group status to closed and save P&L
    rows_affected <- dbExecute(conn, "
      UPDATE position_groups
      SET status = 'closed',
          total_return_pct = ?,
          total_return_amount = ?,
          annualized_return_pct = ?,
          updated_at = ?
      WHERE group_id = ?
    ", params = list(
      pnl$total_return_pct,
      pnl$net_pnl,
      pnl$annualized_return_pct,
      Sys.time(),
      group_id
    ))

    if (rows_affected > 0) {
      log_info("Auto-Close: Closed group {group_id} - Net P&L: ${round(pnl$net_pnl, 2)}, Return: {round(pnl$total_return_pct, 2)}%")
      return(pnl)
    } else {
      log_warn("Auto-Close: Group {group_id} not found")
      return(tibble::tibble())
    }

  }, error = function(e) {
    log_error("Auto-Close: Failed to close group {group_id} - {e$message}")
    return(tibble::tibble())
  })
}
