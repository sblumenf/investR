# Fix cash flow events for closed position groups
#
# Issue: Closed groups have both option_premium events (from individual trades)
# and option_gain events (final P&L). This causes cash flow projections to show
# large losses instead of the correct net gain/loss.
#
# Solution: Delete all old cash flow events and create a single option_gain event
# with the final P&L from the position_groups table.

library(DBI)
library(dplyr)
library(logger)
library(investR)

# Get database connection
conn <- get_portfolio_db_connection()

tryCatch({

  # Get all closed groups
  closed_groups <- dbGetQuery(conn, "
    SELECT group_id, group_name, total_return_amount
    FROM position_groups
    WHERE status = 'closed'
      AND total_return_amount IS NOT NULL
  ")

  log_info("Found {nrow(closed_groups)} closed groups to fix")

  for (i in seq_len(nrow(closed_groups))) {
    group <- closed_groups[i, ]
    group_id <- group$group_id
    group_name <- group$group_name
    final_pnl <- group$total_return_amount

    # Get existing cash flows
    existing_cf <- dbGetQuery(conn, "
      SELECT event_id, event_date, event_type, amount, status
      FROM position_group_cash_flows
      WHERE group_id = ?
      ORDER BY event_date
    ", params = list(group_id))

    if (nrow(existing_cf) == 0) {
      log_info("Group {group_name}: No cash flows found, skipping")
      next
    }

    # Check if already fixed (only one option_gain event)
    if (nrow(existing_cf) == 1 &&
        existing_cf$event_type[1] == "option_gain" &&
        abs(existing_cf$amount[1] - final_pnl) < 0.01) {
      log_info("Group {group_name}: Already fixed, skipping")
      next
    }

    log_info("Group {group_name}: Fixing {nrow(existing_cf)} cash flow events")
    log_info("  Current total: ${round(sum(existing_cf$amount), 2)}")
    log_info("  Should be: ${round(final_pnl, 2)}")

    # Delete all existing cash flows
    delete_group_cash_flows(group_id, status_filter = NULL)

    # Get the latest activity date for this group
    latest_activity <- dbGetQuery(conn, "
      SELECT MAX(trade_date) as close_date
      FROM account_activities
      WHERE group_id = ?
    ", params = list(group_id))

    if (nrow(latest_activity) > 0 && !is.na(latest_activity$close_date[1])) {
      close_date <- as.Date(latest_activity$close_date[1])

      # Create single option_gain event with final P&L
      save_cash_flow_event(
        group_id = group_id,
        event_date = close_date,
        event_type = "option_gain",
        amount = final_pnl,
        status = "actual",
        confidence = "high",
        conn = conn
      )

      log_info("  ✓ Fixed: Created option_gain event for ${round(final_pnl, 2)} on {close_date}")
    } else {
      log_warn("  ✗ Could not find close date for group {group_name}")
    }
  }

  log_info("✓ Cash flow fix complete for {nrow(closed_groups)} groups")

}, error = function(e) {
  log_error("Failed to fix cash flows: {e$message}")
  stop(e)
}, finally = {
  dbDisconnect(conn, shutdown = TRUE)
})
