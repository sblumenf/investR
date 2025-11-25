#' Auto-Link Cash Equivalent Activities
#'
#' Automatically links ZMMK.to and SGOV activities to their respective
#' cash equivalent groups by account number.
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbExecute
#' @importFrom duckdb duckdb
#' @importFrom logger log_info
#' @importFrom shiny showNotification
#' @noRd
auto_link_cash_equivalents <- function() {
  con <- get_portfolio_db_connection()
  on.exit(dbDisconnect(con, shutdown = TRUE))

  # Get unlinked cash equivalent activities
  unlinked_cash <- dbGetQuery(con, "
    SELECT activity_id, symbol, account_number
    FROM account_activities
    WHERE UPPER(symbol) IN ('ZMMK.TO', 'SGOV')
      AND group_id IS NULL
      AND COALESCE(ignore_for_grouping, FALSE) = FALSE
  ")

  if (nrow(unlinked_cash) == 0) {
    return(0)
  }

  # Get existing cash equivalent groups by symbol and account
  cash_groups <- dbGetQuery(con, "
    SELECT pg.group_id, pg.account_number, pgm.symbol
    FROM position_groups pg
    INNER JOIN position_group_members pgm ON pg.group_id = pgm.group_id
    WHERE UPPER(pgm.symbol) IN ('ZMMK.TO', 'SGOV')
      AND pg.group_name LIKE '%Cash Equivalent%'
  ")

  linked_count <- 0

  # Link each unlinked activity to its group
  for (i in seq_len(nrow(unlinked_cash))) {
    activity <- unlinked_cash[i, ]

    # Find matching group (same symbol and account, case-insensitive)
    matching_group <- cash_groups[
      toupper(cash_groups$symbol) == toupper(activity$symbol) &
      cash_groups$account_number == activity$account_number,
    ]

    if (nrow(matching_group) > 0) {
      group_id <- matching_group$group_id[1]

      # Link activity to group
      dbExecute(con, sprintf("
        UPDATE account_activities
        SET group_id = '%s'
        WHERE activity_id = '%s'
      ", group_id, activity$activity_id))

      linked_count <- linked_count + 1
    }
  }

  if (linked_count > 0) {
    log_info(sprintf("Auto-linked %d cash equivalent activities", linked_count))
    showNotification(
      sprintf("Auto-linked %d cash equivalent transaction%s (ZMMK.TO/SGOV)",
              linked_count,
              ifelse(linked_count == 1, "", "s")),
      type = "message",
      duration = 5
    )
  }

  return(linked_count)
}
