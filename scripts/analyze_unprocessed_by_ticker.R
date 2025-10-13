#!/usr/bin/env Rscript
#
# Analyze Unprocessed Activities by Ticker
#
# Shows comprehensive details for each ticker with unprocessed activities,
# including: activity details, existing groups, current positions, and recommendations.
#

library(DBI)
library(dplyr)
library(investR)
devtools::load_all()

conn <- get_portfolio_db_connection()

# Get all tickers with unprocessed activities
tickers_query <- dbGetQuery(conn, "
  SELECT DISTINCT symbol
  FROM account_activities
  WHERE group_id IS NULL
    AND COALESCE(ignore_for_grouping, FALSE) = FALSE
  ORDER BY symbol
")

tickers <- tickers_query$symbol

cat(sprintf("\n==========================================================\n"))
cat(sprintf("UNPROCESSED ACTIVITIES ANALYSIS\n"))
cat(sprintf("Total tickers to review: %d\n", length(tickers)))
cat(sprintf("==========================================================\n\n"))

for (ticker in tickers) {
  cat(sprintf("\n\n"))
  cat(sprintf("##########################################################\n"))
  cat(sprintf("### TICKER: %s\n", ticker))
  cat(sprintf("##########################################################\n\n"))

  # 1. Get all unprocessed activities for this ticker
  activities <- dbGetQuery(conn, "
    SELECT
      activity_id,
      account_number,
      trade_date,
      transaction_date,
      action,
      symbol,
      description,
      quantity,
      price,
      gross_amount,
      commission,
      net_amount,
      type,
      currency
    FROM account_activities
    WHERE symbol = ?
      AND group_id IS NULL
      AND COALESCE(ignore_for_grouping, FALSE) = FALSE
    ORDER BY trade_date, transaction_date
  ", params = list(ticker)) %>% as_tibble()

  cat(sprintf("--- UNPROCESSED ACTIVITIES (%d) ---\n", nrow(activities)))
  print(activities, n = Inf)
  cat("\n")

  # 2. Check for existing groups containing this ticker or related ticker
  # Extract base ticker (for options)
  base_ticker <- parse_option_symbol(ticker)
  if (is.na(base_ticker)) {
    base_ticker <- ticker
  }

  # Find groups with this ticker (exact match or underlying)
  groups_with_ticker <- dbGetQuery(conn, "
    SELECT DISTINCT
      pg.group_id,
      pg.group_name,
      pg.strategy_type,
      pg.status,
      pg.account_number,
      pg.created_at
    FROM position_groups pg
    JOIN position_group_members pgm ON pg.group_id = pgm.group_id
    WHERE pgm.symbol = ?
       OR pgm.symbol LIKE ?
    ORDER BY pg.created_at DESC
  ", params = list(ticker, paste0(base_ticker, "%"))) %>% as_tibble()

  if (nrow(groups_with_ticker) > 0) {
    cat(sprintf("--- EXISTING GROUPS WITH %s OR %s (%d) ---\n", ticker, base_ticker, nrow(groups_with_ticker)))
    print(groups_with_ticker, n = Inf)

    # Show members for each group
    for (i in 1:nrow(groups_with_ticker)) {
      gid <- groups_with_ticker$group_id[i]
      members <- dbGetQuery(conn, "
        SELECT symbol, role, allocated_quantity
        FROM position_group_members
        WHERE group_id = ?
        ORDER BY role
      ", params = list(gid)) %>% as_tibble()

      cat(sprintf("\n  Members of '%s':\n", groups_with_ticker$group_name[i]))
      print(members, n = Inf)
    }
    cat("\n")
  } else {
    cat(sprintf("--- NO EXISTING GROUPS FOUND FOR %s ---\n\n", ticker))
  }

  # 3. Check current positions (if any)
  current_pos <- dbGetQuery(conn, "
    SELECT
      symbol,
      open_quantity,
      current_price,
      current_market_value,
      average_entry_price,
      total_cost,
      snapshot_timestamp
    FROM positions_history
    WHERE symbol = ?
       OR symbol LIKE ?
    ORDER BY snapshot_timestamp DESC
    LIMIT 5
  ", params = list(ticker, paste0(base_ticker, "%"))) %>% as_tibble()

  if (nrow(current_pos) > 0) {
    cat(sprintf("--- CURRENT/RECENT POSITIONS FOR %s OR %s ---\n", ticker, base_ticker))
    print(current_pos, n = Inf)
    cat("\n")
  } else {
    cat(sprintf("--- NO CURRENT POSITIONS FOR %s ---\n\n", ticker))
  }

  # 4. Check all activities for this ticker (including processed ones for context)
  all_activities <- dbGetQuery(conn, "
    SELECT
      activity_id,
      trade_date,
      action,
      quantity,
      price,
      net_amount,
      type,
      group_id,
      COALESCE(ignore_for_grouping, FALSE) as ignored
    FROM account_activities
    WHERE symbol = ?
    ORDER BY trade_date
  ", params = list(ticker)) %>% as_tibble()

  linked_count <- sum(!is.na(all_activities$group_id))
  ignored_count <- sum(all_activities$ignored)
  unlinked_count <- sum(is.na(all_activities$group_id) & !all_activities$ignored)

  cat(sprintf("--- ALL ACTIVITIES FOR %s (Total: %d, Linked: %d, Ignored: %d, Unlinked: %d) ---\n",
              ticker, nrow(all_activities), linked_count, ignored_count, unlinked_count))
  if (nrow(all_activities) > nrow(activities)) {
    cat("(Showing linked/ignored activities for context)\n")
    print(all_activities, n = Inf)
    cat("\n")
  }

  # 5. Get projected and actual cash flows for related groups
  cat("--- PROJECTED/ACTUAL CASH FLOWS FOR RELATED GROUPS ---\n")
  if (nrow(groups_with_ticker) > 0) {
    for (i in 1:nrow(groups_with_ticker)) {
      gid <- groups_with_ticker$group_id[i]
      gname <- groups_with_ticker$group_name[i]

      cash_flows <- dbGetQuery(conn, "
        SELECT
          event_id,
          event_type,
          event_date,
          amount,
          status,
          confidence,
          created_at
        FROM position_group_cash_flows
        WHERE group_id = ?
        ORDER BY event_date
      ", params = list(gid)) %>% as_tibble()

      if (nrow(cash_flows) > 0) {
        cat(sprintf("\n  Cash flows for '%s' (%s):\n", gname, gid))
        print(cash_flows, n = Inf)

        # Summary by status
        summary <- cash_flows %>%
          group_by(event_type, status) %>%
          summarise(
            count = n(),
            total_amount = sum(amount, na.rm = TRUE),
            .groups = "drop"
          )
        cat("\n  Summary:\n")
        print(summary)
      } else {
        cat(sprintf("  No cash flows recorded for '%s'\n", gname))
      }
    }
    cat("\n")
  } else {
    cat("No related groups found\n\n")
  }

  # 6. Get actual cash flows (activity-based) for this ticker
  cat("--- ACTUAL CASH FLOWS (from activities) ---\n")
  cash_flows <- dbGetQuery(conn, "
    SELECT
      activity_id,
      trade_date,
      action,
      type,
      quantity,
      price,
      net_amount,
      group_id,
      description
    FROM account_activities
    WHERE symbol = ?
      AND type IN ('Trades', 'Dividends', 'Other')
    ORDER BY trade_date
  ", params = list(ticker)) %>% as_tibble()

  if (nrow(cash_flows) > 0) {
    print(cash_flows, n = Inf)

    # Summarize cash flows by type
    cat("\nCash Flow Summary:\n")
    summary <- cash_flows %>%
      group_by(type, action) %>%
      summarise(
        count = n(),
        total_cash = sum(net_amount, na.rm = TRUE),
        .groups = "drop"
      )
    print(summary)
  } else {
    cat("No cash flow activities found\n")
  }
  cat("\n")

  # 7. Provide recommendation
  cat("--- RECOMMENDATION ---\n")
  if (is_option_symbol(ticker)) {
    cat(sprintf("• This is an OPTION symbol (%s)\n", ticker))
    cat(sprintf("• Underlying ticker: %s\n", base_ticker))
    if (nrow(groups_with_ticker) > 0) {
      cat("• Existing group(s) found - consider linking to existing group\n")
    } else {
      cat("• No existing groups - may need to create new group or link to underlying stock group\n")
    }
  } else {
    cat(sprintf("• This is a STOCK symbol (%s)\n", ticker))
    if (nrow(groups_with_ticker) > 0) {
      cat("• Existing group(s) found - consider linking to existing group\n")
    } else {
      cat("• No existing groups - may need to create new group\n")
    }
  }

  if (nrow(current_pos) > 0) {
    cat("• Current position exists\n")
  } else {
    cat("• No current position (may be closed)\n")
  }

  cat("\n")
  cat(sprintf("==========================================================\n"))
  cat("READY FOR YOUR INSTRUCTIONS FOR THIS TICKER\n")
  cat(sprintf("==========================================================\n"))
  cat("\n")

  # Pause for review (comment out if running automated)
  # readline(prompt = "Press [enter] to continue to next ticker...")
}

dbDisconnect(conn, shutdown = TRUE)

cat("\n\n")
cat("==========================================================\n")
cat("ANALYSIS COMPLETE\n")
cat(sprintf("Reviewed %d tickers\n", length(tickers)))
cat("==========================================================\n")
