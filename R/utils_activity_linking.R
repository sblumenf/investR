#' Activity Linking Tracking Utilities
#'
#' Functions for tracking which activities are linked to groups
#' and identifying unlinked activities that may need attention.
#'
#' @name activity-linking-tracking
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @import dplyr
#' @importFrom logger log_info log_warn
NULL

#' Get unlinked activities report
#'
#' Returns all activities that don't have a group_id assigned.
#' Useful for tracking progress during manual linking process.
#'
#' @return Tibble with unlinked activities
#' @export
get_unlinked_activities <- function() {
  con <- get_portfolio_db_connection()
  on.exit(dbDisconnect(con))

  unlinked <- dbGetQuery(con, "
    SELECT
      activity_id,
      account_number,
      symbol,
      action,
      quantity,
      price,
      net_amount,
      trade_date,
      type,
      description
    FROM account_activities
    WHERE group_id IS NULL
      AND COALESCE(ignore_for_grouping, FALSE) = FALSE
    ORDER BY trade_date DESC, symbol
  ") %>% as_tibble()

  return(unlinked)
}

#' Get linking statistics
#'
#' Returns summary statistics about activity linking status.
#'
#' @return List with counts and percentages
#' @export
get_linking_stats <- function() {
  con <- get_portfolio_db_connection()
  on.exit(dbDisconnect(con))

  stats <- dbGetQuery(con, "
    SELECT
      COUNT(*) as total_activities,
      SUM(CASE WHEN group_id IS NOT NULL THEN 1 ELSE 0 END) as linked_count,
      SUM(CASE WHEN group_id IS NULL AND COALESCE(ignore_for_grouping, FALSE) = FALSE THEN 1 ELSE 0 END) as unlinked_count,
      SUM(CASE WHEN COALESCE(ignore_for_grouping, FALSE) = TRUE THEN 1 ELSE 0 END) as ignored_count
    FROM account_activities
  ") %>% as_tibble()

  stats <- stats %>%
    mutate(
      linked_pct = round(100 * linked_count / total_activities, 1),
      unlinked_pct = round(100 * unlinked_count / total_activities, 1),
      ignored_pct = round(100 * ignored_count / total_activities, 1)
    )

  return(as.list(stats))
}

#' Get unlinked activities by symbol
#'
#' Groups unlinked activities by symbol to make manual review easier.
#'
#' @return Tibble with symbol and count of unlinked activities
#' @export
get_unlinked_by_symbol <- function() {
  con <- get_portfolio_db_connection()
  on.exit(dbDisconnect(con))

  by_symbol <- dbGetQuery(con, "
    SELECT
      symbol,
      account_number,
      COUNT(*) as unlinked_count,
      MIN(trade_date) as earliest_trade,
      MAX(trade_date) as latest_trade
    FROM account_activities
    WHERE group_id IS NULL
      AND COALESCE(ignore_for_grouping, FALSE) = FALSE
    GROUP BY symbol, account_number
    ORDER BY symbol
  ") %>% as_tibble()

  return(by_symbol)
}

#' Check for activities that should be linked but aren't
#'
#' Identifies activities whose symbols match group members but aren't linked.
#' These are likely candidates for manual review.
#'
#' @return Tibble with orphaned activities
#' @export
get_orphaned_activities <- function() {
  con <- get_portfolio_db_connection()
  on.exit(dbDisconnect(con))

  orphaned <- dbGetQuery(con, "
    SELECT DISTINCT
      aa.activity_id,
      aa.symbol,
      aa.account_number,
      aa.action,
      aa.trade_date,
      pg.group_id,
      pg.group_name
    FROM account_activities aa
    JOIN position_group_members pgm
      ON aa.symbol = pgm.symbol
      AND aa.account_number = pgm.account_number
    JOIN position_groups pg
      ON pgm.group_id = pg.group_id
    WHERE aa.group_id IS NULL
      AND COALESCE(aa.ignore_for_grouping, FALSE) = FALSE
    ORDER BY aa.trade_date DESC, aa.symbol
  ") %>% as_tibble()

  return(orphaned)
}

#' Link multiple activities to a group
#'
#' Batch operation to link multiple activities to the same group.
#'
#' @param activity_ids Character vector of activity IDs
#' @param group_id Group identifier
#' @return Logical TRUE if successful
#' @export
link_activities_to_group <- function(activity_ids, group_id) {
  if (length(activity_ids) == 0) return(TRUE)
  con <- get_portfolio_db_connection()
  on.exit(dbDisconnect(con))

  tryCatch({
    # Verify group exists
    group_check <- dbGetQuery(con, "SELECT COUNT(*) as count FROM position_groups WHERE group_id = ?", params = list(group_id))
    if (group_check$count == 0) {
      log_warn("Cannot link activities: group {group_id} does not exist")
      return(FALSE)
    }

    # Build parameterized UPDATE query with IN clause
    placeholders <- paste(rep("?", length(activity_ids)), collapse = ", ")
    query <- sprintf("UPDATE account_activities SET group_id = ? WHERE activity_id IN (%s)", placeholders)
    dbExecute(con, query, params = c(list(group_id), as.list(activity_ids)))

    log_info("Linked {length(activity_ids)} activities to group {group_id}")

    # Reconcile projected dividends for any dividend activities in the batch
    placeholders <- paste(rep("?", length(activity_ids)), collapse = ", ")
    div_sql <- sprintf("
      SELECT group_id, type, transaction_date
      FROM account_activities
      WHERE activity_id IN (%s)
        AND type = 'Dividends'
    ", placeholders)
    dividends <- dbGetQuery(con, div_sql, params = as.list(activity_ids))

    if (nrow(dividends) > 0) {
      # Group by month and reconcile once per unique group+month combination
      dividend_months <- dividends %>%
        as_tibble() %>%
        mutate(
          transaction_date = as.Date(as.POSIXct(transaction_date, origin = "1970-01-01")),
          year = lubridate::year(transaction_date),
          month = lubridate::month(transaction_date)
        ) %>%
        distinct(group_id, year, month, .keep_all = TRUE)

      # Reconcile each unique month
      for (i in seq_len(nrow(dividend_months))) {
        delete_projected_cash_flows_by_month(
          group_id = dividend_months$group_id[i],
          event_type = "dividend",
          event_date = dividend_months$transaction_date[i],
          conn = con
        )
      }

      log_info("Reconciled projected dividends for {nrow(dividend_months)} month(s) in group {group_id}")

      # Create actual cash flow records for linked dividends
      # ONLY for named strategies (not "Other" or "Legacy Covered Call")
      # "Other" and "Legacy Covered Call" pull dividends directly from account_activities
      group_info <- dbGetQuery(con, "SELECT strategy_type FROM position_groups WHERE group_id = ?", params = list(group_id))

      if (nrow(group_info) > 0 && !group_info$strategy_type[1] %in% c("Other", "Legacy Covered Call")) {
        # Get full dividend data including amounts
        placeholders_full <- paste(rep("?", length(activity_ids)), collapse = ", ")
        div_sql_full <- sprintf("
          SELECT activity_id, group_id, transaction_date, net_amount
          FROM account_activities
          WHERE activity_id IN (%s)
            AND type = 'Dividends'
        ", placeholders_full)
        dividend_details <- dbGetQuery(con, div_sql_full, params = as.list(activity_ids))

        if (nrow(dividend_details) > 0) {
          for (i in seq_len(nrow(dividend_details))) {
            div <- dividend_details[i, ]
            save_cash_flow_event(
              group_id = div$group_id,
              event_date = as.Date(div$transaction_date),
              event_type = "dividend",
              amount = abs(div$net_amount),
              status = "actual",
              confidence = "high"
            )
          }
          log_info("Created {nrow(dividend_details)} actual cash flow record(s) for linked dividends")
        }
      } else {
        log_debug("Activity Linking: Skipping dividend cash flow creation for Other/Legacy Covered Call strategy (pulled from account_activities)")
      }
    }

    # Create cash flow records for option trades in covered call strategies
    # Get group strategy type
    group_info <- dbGetQuery(con, "SELECT strategy_type FROM position_groups WHERE group_id = ?", params = list(group_id))

    # Strategies that track option premiums as cash flows
    covered_call_strategies <- c(
      "Legacy Covered Call",
      "Zero-Dividend Stocks",
      "Dividend Aristocrats",
      "Dynamic Covered Calls",
      "Collar Strategy"
    )

    if (nrow(group_info) > 0 && group_info$strategy_type[1] %in% covered_call_strategies) {
      # Query option trades from linked activities
      placeholders_opt <- paste(rep("?", length(activity_ids)), collapse = ", ")
      opt_sql <- sprintf("
        SELECT activity_id, group_id, trade_date, action, symbol, net_amount
        FROM account_activities
        WHERE activity_id IN (%s)
          AND type = 'Trades'
      ", placeholders_opt)
      option_trades <- dbGetQuery(con, opt_sql, params = as.list(activity_ids))

      # Filter to option trades
      if (nrow(option_trades) > 0) {
        # For Legacy Covered Call ONLY: use role-based logic (not underlying stock = option trade)
        # This handles cases where the symbol field is empty but the trade is clearly an option
        if (group_info$strategy_type[1] == "Legacy Covered Call") {
          underlying_result <- dbGetQuery(con, "
            SELECT symbol FROM position_group_members
            WHERE group_id = ? AND role = 'underlying_stock'
          ", params = list(group_id))

          underlying_symbol <- if (nrow(underlying_result) > 0) underlying_result$symbol[1] else ""

          option_trades <- option_trades %>%
            as_tibble() %>%
            filter(symbol != underlying_symbol)
        } else {
          # For all other strategies: require symbol to look like an option
          option_trades <- option_trades %>%
            as_tibble() %>%
            filter(purrr::map_lgl(symbol, is_option_symbol))
        }

        if (nrow(option_trades) > 0) {
          # For named strategies (not "Other" or "Legacy Covered Call"), need to detect initial vs roll premiums
          # Initial premiums are cost reductions, not cash flows. Roll premiums ARE cash flows.
          earliest_stock_buy_date <- NULL
          if (!group_info$strategy_type[1] %in% c("Other", "Legacy Covered Call")) {
            # Get earliest stock buy date for this group (filter out options properly)
            stock_buys <- dbGetQuery(con, "
              SELECT trade_date, symbol
              FROM account_activities
              WHERE group_id = ?
                AND type = 'Trades'
                AND action = 'Buy'
            ", params = list(group_id)) %>%
              as_tibble() %>%
              filter(!purrr::map_lgl(symbol, is_option_symbol))

            if (nrow(stock_buys) > 0) {
              earliest_stock_buy_date <- as.Date(min(stock_buys$trade_date))
            }
          }

          for (i in seq_len(nrow(option_trades))) {
            opt <- option_trades[i, ]

            # For named strategies, skip initial premium (cost reduction, not cash flow)
            # Only "Other" and "Legacy Covered Call" strategies record initial premiums as actual cash flows
            if (!group_info$strategy_type[1] %in% c("Other", "Legacy Covered Call") &&
                opt$action == "Sell" &&
                !is.null(earliest_stock_buy_date)) {
              option_sell_date <- as.Date(opt$trade_date)
              if (option_sell_date == earliest_stock_buy_date) {
                # Initial premium - skip creating cash flow
                log_debug("Activity Linking: Skipping initial premium cash flow for {group_info$strategy_type[1]} (cost reduction)")
                next
              }
            }

            # Sell = positive income, Buy = negative cost
            amount <- if (opt$action == "Sell") {
              abs(opt$net_amount)
            } else {
              -abs(opt$net_amount)
            }

            save_cash_flow_event(
              group_id = opt$group_id,
              event_date = as.Date(opt$trade_date),
              event_type = "option_premium",
              amount = amount,
              status = "actual",
              confidence = "high",
              conn = con
            )
          }
          log_info("Created actual cash flow record(s) for option premiums")
        }
      }
    }

    # Detect option rolls in the batch of linked activities
    # After all activities are linked and cash flows created, check if any form roll patterns
    tryCatch({
      # Get ALL activities in the group (both newly linked and previously linked)
      # This is critical because a roll pattern might involve:
      # - An old activity already in the group (e.g., EXP from a previous link)
      # - A new activity just linked (e.g., new option Sell)
      group_activities <- dbGetQuery(con, "
        SELECT * FROM account_activities
        WHERE group_id = ?
        ORDER BY trade_date
      ", params = list(group_id)) %>% as_tibble()

      # Get just the activities we linked in this batch (to know which are "new")
      newly_linked <- dbGetQuery(con, sprintf("
        SELECT * FROM account_activities
        WHERE activity_id IN (%s)
      ", placeholders), params = activity_ids) %>% as_tibble()

      # Find option sell activities in the newly linked batch (potential new options in a roll)
      option_sells <- newly_linked %>%
        filter(
          type == "Trades",
          action == "Sell",
          purrr::map_lgl(symbol, is_option_symbol)
        )

      # For each newly linked option sell, check if there's a closing event in the ENTIRE group
      if (nrow(option_sells) > 0) {
        for (i in seq_len(nrow(option_sells))) {
          sell_activity <- option_sells[i, ]

          # Check for roll pattern with this sell against ALL group activities
          roll_info <- detect_option_roll(sell_activity, group_activities)

          if (roll_info$is_roll) {
            log_info("Activity Linking: Detected option roll in batch - {roll_info$old_symbol} → {roll_info$new_symbol}")

            # Update the group member to reflect the new option (pass parent connection)
            update_result <- update_group_option_member(
              group_id = group_id,
              old_symbol = roll_info$old_symbol,
              new_symbol = roll_info$new_symbol,
              conn = con
            )

            # Regenerate projections if member update succeeded (pass parent connection)
            if (update_result) {
              regenerate_projections_after_roll(
                group_id = group_id,
                new_option_symbol = roll_info$new_symbol,
                conn = con
              )
            }
          }
        }
      }
    }, error = function(e) {
      log_warn("Activity Linking: Roll detection failed in batch linking - {e$message}")
      # Don't fail the entire linking operation if roll detection fails
    })

    return(TRUE)

  }, error = function(e) {
    log_warn("Failed to link activities to group {group_id}: {e$message}")
    return(FALSE)
  })
}

#' Unlink activity from group
#'
#' Sets group_id to NULL for an activity (useful for corrections).
#'
#' @param activity_id Activity identifier
#' @return Logical TRUE if successful
#' @export
unlink_activity <- function(activity_id) {
  con <- get_portfolio_db_connection()
  on.exit(dbDisconnect(con))
  tryCatch({
    dbExecute(con, "UPDATE account_activities SET group_id = NULL WHERE activity_id = ?", params = list(activity_id))
    log_info("Unlinked activity {activity_id}")
    return(TRUE)
  }, error = function(e) {
    log_warn("Failed to unlink activity {activity_id}: {e$message}")
    return(FALSE)
  })
}

#' Mark activity to be ignored for grouping
#'
#' Sets ignore_for_grouping flag for activities that should not be linked to groups.
#' This is a convenience wrapper around the batch function.
#'
#' @param activity_id Activity identifier
#' @return Logical TRUE if successful
#' @export
ignore_activity <- function(activity_id) {
  batch_ignore_activities(activity_id)
}

#' Batch ignore activities
#'
#' Efficiently sets ignore_for_grouping flag for multiple activities.
#'
#' @param activity_ids Character vector of activity IDs
#' @return Logical TRUE if successful
#' @export
batch_ignore_activities <- function(activity_ids) {
  if (length(activity_ids) == 0) return(TRUE)
  con <- get_portfolio_db_connection()
  on.exit(dbDisconnect(con))
  tryCatch({
    query <- "UPDATE account_activities SET ignore_for_grouping = TRUE WHERE activity_id IN (?)"
    dbExecute(con, query, params = list(activity_ids))
    log_info("Marked {length(activity_ids)} activities as ignored.")
    return(TRUE)
  }, error = function(e) {
    log_warn("Failed to batch ignore activities: {e$message}")
    return(FALSE)
  })
}


#' Unignore activity
#'
#' Clears ignore_for_grouping flag (useful for corrections).
#'
#' @param activity_id Activity identifier
#' @return Logical TRUE if successful
#' @export
unignore_activity <- function(activity_id) {
  con <- get_portfolio_db_connection()
  on.exit(dbDisconnect(con))
  tryCatch({
    dbExecute(con, "UPDATE account_activities SET ignore_for_grouping = FALSE WHERE activity_id = ?", params = list(activity_id))
    log_info("Cleared ignore flag for activity {activity_id}")
    return(TRUE)
  }, error = function(e) {
    log_warn("Failed to unignore activity {activity_id}: {e$message}")
    return(FALSE)
  })
}

#' Print linking summary report
#'
#' Displays a formatted summary of linking status.
#'
#' @export
print_linking_summary <- function() {
  stats <- get_linking_stats()

  cat("\n")
  cat("===== ACTIVITY LINKING SUMMARY =====\n")
  cat(sprintf("Total Activities:   %d\n", stats$total_activities))
  cat(sprintf("Linked:            %d (%s%%)\n", stats$linked_count, stats$linked_pct))
  cat(sprintf("Unlinked:          %d (%s%%)\n", stats$unlinked_count, stats$unlinked_pct))
  cat(sprintf("Ignored:           %d (%s%%)\n", stats$ignored_count, stats$ignored_pct))
  cat("====================================\n")
  cat("\n")

  if (stats$unlinked_count > 0) {
    by_symbol <- get_unlinked_by_symbol()
    cat("Unlinked activities by symbol:\n")
    print(by_symbol, n = Inf)
  }
}
