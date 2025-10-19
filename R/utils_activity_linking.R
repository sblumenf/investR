#' Activity Linking Tracking Utilities
#'
#' Functions for tracking which activities are linked to groups
#' and identifying unlinked activities that may need attention.
#'
#' @name activity-linking-tracking
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom duckdb duckdb
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
  on.exit(dbDisconnect(con, shutdown = TRUE))

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
  on.exit(dbDisconnect(con, shutdown = TRUE))

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
  on.exit(dbDisconnect(con, shutdown = TRUE))

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
  on.exit(dbDisconnect(con, shutdown = TRUE))

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
  on.exit(dbDisconnect(con, shutdown = TRUE))

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
          transaction_date = as.Date(transaction_date),
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
    }

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
  on.exit(dbDisconnect(con, shutdown = TRUE))
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
  on.exit(dbDisconnect(con, shutdown = TRUE))
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
  on.exit(dbDisconnect(con, shutdown = TRUE))
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
