#' Cash Flow Projection Functions
#'
#' Functions for querying and combining actual and projected cash flows
#' across position groups. Supports monthly aggregation and filtering
#' for dashboard visualization.
#'
#' @name cash-flow-projection
#' @import dplyr
#' @importFrom DBI dbGetQuery dbConnect dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom tibble tibble as_tibble
#' @importFrom lubridate floor_date year month
#' @importFrom purrr map_chr
#' @importFrom stringr str_extract
#' @importFrom logger log_info log_warn log_debug
NULL

################################################################################
# ACTUAL CASH FLOWS FROM ACTIVITIES
################################################################################

#' Get actual cash flows from activities table
#'
#' Retrieves all linked activities that represent actual cash flows
#' (dividends and option premiums). Parses option symbols to extract
#' underlying ticker.
#'
#' @return Tibble with columns: event_date, ticker, type, amount, group_id, source, description
#' @noRd
get_actual_cash_flows_from_activities <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Query activities that are linked to groups
    # Exclude ignored activities (bootstrap, corrections, etc.)
    result <- dbGetQuery(conn, "
      SELECT
        aa.trade_date,
        aa.symbol,
        aa.action,
        aa.type,
        aa.net_amount,
        aa.description,
        aa.group_id,
        pg.group_name,
        pgm.role,
        pg.strategy_type
      FROM account_activities aa
      INNER JOIN position_groups pg ON aa.group_id = pg.group_id
      LEFT JOIN position_group_members pgm
        ON aa.group_id = pgm.group_id
        AND aa.symbol = pgm.symbol
      WHERE aa.group_id IS NOT NULL
        AND aa.type IN ('Dividends', 'Trades', 'Other')
        AND COALESCE(aa.ignore_for_grouping, FALSE) = FALSE
      ORDER BY aa.trade_date ASC
    ") %>% as_tibble()

    if (nrow(result) == 0) {
      log_debug("Cash Flow Projection: No actual cash flows found in activities")
      return(tibble(
        event_date = as.Date(character()),
        ticker = character(),
        type = character(),
        amount = numeric(),
        group_id = character(),
        group_name = character(),
        source = character(),
        description = character()
      ))
    }

    # Process each activity to determine if it's a cash flow
    cash_flows <- result %>%
      mutate(
        # Parse ticker from symbol (for options, extract underlying)
        ticker = map_chr(symbol, function(sym) {
          if (is_option_symbol(sym)) {
            parsed <- parse_option_symbol(sym)
            if (!is.na(parsed)) parsed else sym
          } else {
            sym
          }
        }),
        # Determine cash flow type based on strategy
        cash_flow_type = case_when(
          # Dividends count for "Other" and "Legacy Covered Call" strategies ONLY (named strategies use position_group_cash_flows)
          type == "Dividends" & strategy_type %in% c("Other", "Legacy Covered Call") ~ "dividend",
          # Option premiums count for "Other" and "Legacy Covered Call" strategy groups ONLY
          # Named strategies (Dividend Aristocrats, Zero-Dividend Growth, etc.) should NOT have option premiums as actual cash flows
          # because the premium is already accounted for in the projected option_gain calculation
          type %in% c("Trades", "Other") & is_option_symbol(symbol) & action == "Sell" & strategy_type %in% c("Other", "Legacy Covered Call") ~ "option_premium",
          TRUE ~ NA_character_
        )
      ) %>%
      # Keep only rows that are actual cash flows
      filter(!is.na(cash_flow_type)) %>%
      # Select and rename columns
      transmute(
        event_date = as.Date(trade_date),
        ticker = ticker,
        type = cash_flow_type,
        # Use net_amount as-is (already has correct sign)
        amount = net_amount,
        group_id = group_id,
        group_name = group_name,
        source = "actual",
        description = description
      )

    log_debug("Cash Flow Projection: Retrieved {nrow(cash_flows)} actual cash flow events")
    return(cash_flows)

  }, error = function(e) {
    log_warn("Cash Flow Projection: Failed to get actual cash flows - {e$message}")
    return(tibble(
      event_date = as.Date(character()),
      ticker = character(),
      type = character(),
      amount = numeric(),
      group_id = character(),
      group_name = character(),
      source = character(),
      description = character()
    ))
  })
}

################################################################################
# PROJECTED CASH FLOWS FROM CASH_FLOWS TABLE
################################################################################

#' Get projected cash flows from position_group_cash_flows table
#'
#' Retrieves projected dividends and option gains, joined with group
#' information to extract ticker symbols.
#'
#' @return Tibble with columns: event_date, ticker, type, amount, group_id, source, confidence
#' @noRd
get_projected_cash_flows_from_database <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_income_projection_schema(conn)

    # Query projected and actual cash flows with group info
    # Actual cash flows from this table represent:
    # 1. Roll premiums for long-term covered call strategies
    # 2. Final P&L for closed positions
    result <- dbGetQuery(conn, "
      SELECT
        cf.event_date,
        cf.event_type,
        cf.amount,
        cf.confidence,
        cf.status,
        cf.group_id,
        pg.group_name,
        pg.strategy_type
      FROM position_group_cash_flows cf
      INNER JOIN position_groups pg ON cf.group_id = pg.group_id
      WHERE cf.status IN ('projected', 'actual')
      ORDER BY cf.event_date ASC
    ") %>% as_tibble()

    if (nrow(result) == 0) {
      log_debug("Cash Flow Projection: No projected cash flows found")
      return(tibble(
        event_date = as.Date(character()),
        ticker = character(),
        type = character(),
        amount = numeric(),
        group_id = character(),
        group_name = character(),
        source = character(),
        confidence = character()
      ))
    }

    # Filter out "Other" and "Legacy Covered Call" strategy projections
    # These strategies pull cash flows from account_activities only
    result <- result %>%
      filter(!strategy_type %in% c("Other", "Legacy Covered Call"))

    # Get all group members to extract tickers
    all_group_ids <- unique(result$group_id)
    group_members <- get_members_for_groups(all_group_ids)

    # Extract ticker for each group (use underlying_stock role)
    group_tickers <- group_members %>%
      filter(role == "underlying_stock") %>%
      select(group_id, ticker = symbol) %>%
      distinct()

    # Join tickers and format
    cash_flows <- result %>%
      left_join(group_tickers, by = "group_id") %>%
      filter(!is.na(ticker)) %>%  # Only keep flows with known tickers
      transmute(
        event_date = as.Date(event_date),
        ticker = ticker,
        type = event_type,
        amount = amount,
        group_id = group_id,
        group_name = group_name,
        source = status,  # Use status from database (projected or actual)
        confidence = confidence
      )

    log_debug("Cash Flow Projection: Retrieved {nrow(cash_flows)} cash flow events ({sum(result$status == 'actual')} actual, {sum(result$status == 'projected')} projected)")
    return(cash_flows)

  }, error = function(e) {
    log_warn("Cash Flow Projection: Failed to get projected cash flows - {e$message}")
    return(tibble(
      event_date = as.Date(character()),
      ticker = character(),
      type = character(),
      amount = numeric(),
      group_id = character(),
      group_name = character(),
      source = character(),
      confidence = character()
    ))
  })
}

################################################################################
# DATE RANGE CALCULATION
################################################################################

#' Determine date range for cash flow projection
#'
#' Calculates the earliest activity date and latest option expiry date
#' to determine the full range of historical and projected cash flows.
#'
#' @return List with start_date and end_date (Date objects)
#' @noRd
get_cash_flow_date_range <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Get earliest activity date
    activity_result <- dbGetQuery(conn, "
      SELECT MIN(trade_date) as earliest_date
      FROM account_activities
      WHERE group_id IS NOT NULL
    ")

    earliest_activity <- if (!is.null(activity_result$earliest_date) &&
                            !is.na(activity_result$earliest_date)) {
      as.Date(activity_result$earliest_date)
    } else {
      # Default to 1 year ago if no activities
      Sys.Date() - 365
    }

    # Get latest option expiry from current positions
    positions <- get_latest_positions()

    if (nrow(positions) > 0) {
      # Extract expiry dates from option symbols
      option_expiries <- positions %>%
        filter(is_option_symbol(symbol)) %>%
        mutate(expiry = map_chr(symbol, function(sym) {
          parsed <- parse_option_details(sym)
          if (!is.null(parsed$expiry)) {
            as.character(parsed$expiry)
          } else {
            NA_character_
          }
        })) %>%
        filter(!is.na(expiry)) %>%
        pull(expiry) %>%
        as.Date()

      latest_expiry <- if (length(option_expiries) > 0) {
        max(option_expiries)
      } else {
        # Default to 2 years out if no options
        Sys.Date() + 730
      }
    } else {
      # Default to 2 years out if no positions
      latest_expiry <- Sys.Date() + 730
    }

    log_debug("Cash Flow Projection: Date range from {earliest_activity} to {latest_expiry}")

    return(list(
      start_date = earliest_activity,
      end_date = latest_expiry
    ))

  }, error = function(e) {
    log_warn("Cash Flow Projection: Failed to calculate date range - {e$message}")
    # Return default range
    return(list(
      start_date = Sys.Date() - 365,
      end_date = Sys.Date() + 730
    ))
  })
}

################################################################################
# COMBINED CASH FLOWS
################################################################################

#' Get combined actual and projected cash flows
#'
#' Combines actual cash flows from activities with projected cash flows
#' from the cash_flows table. Returns both transaction-level detail and
#' monthly aggregates for visualization.
#'
#' @return List with two tibbles:
#'   - transactions: Detail rows with columns: event_date, month_label, ticker, type, amount, source
#'   - monthly_aggregates: Grouped by month and type for charting
#' @noRd
get_combined_cash_flows <- function() {
  # Get actual and projected flows
  actuals <- get_actual_cash_flows_from_activities()
  projections <- get_projected_cash_flows_from_database()

  # Combine datasets
  # Actuals have description column, projections have confidence
  # Standardize to common schema
  actuals_standardized <- actuals %>%
    mutate(confidence = NA_character_) %>%
    select(event_date, ticker, type, amount, group_id, group_name, source)

  projections_standardized <- projections %>%
    select(event_date, ticker, type, amount, group_id, group_name, source)

  all_flows <- bind_rows(actuals_standardized, projections_standardized)

  if (nrow(all_flows) == 0) {
    log_info("Cash Flow Projection: No cash flows found (actual or projected)")
    return(list(
      transactions = tibble(
        event_date = as.Date(character()),
        month_label = character(),
        ticker = character(),
        type = character(),
        amount = numeric(),
        group_name = character(),
        source = character(),
        is_current_month = logical()
      ),
      monthly_aggregates = tibble(
        month_label = character(),
        type = character(),
        total_amount = numeric()
      )
    ))
  }

  # Add month label and current month flag
  current_month_start <- floor_date(Sys.Date(), "month")

  transactions <- all_flows %>%
    mutate(
      month_label = format(floor_date(event_date, "month"), "%Y-%m"),
      is_current_month = floor_date(event_date, "month") == current_month_start
    ) %>%
    arrange(event_date, ticker)

  # Create monthly aggregates for charting
  monthly_aggregates <- transactions %>%
    group_by(month_label, type) %>%
    summarise(
      total_amount = sum(amount, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(month_label, type)

  log_info("Cash Flow Projection: Combined {nrow(transactions)} transactions into {nrow(monthly_aggregates)} monthly aggregates")

  return(list(
    transactions = transactions,
    monthly_aggregates = monthly_aggregates
  ))
}
