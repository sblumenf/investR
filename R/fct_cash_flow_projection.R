#' Cash Flow Projection Functions
#'
#' Functions for querying and combining actual and projected cash flows
#' across position groups. Supports monthly aggregation and filtering
#' for dashboard visualization.
#'
#' @name cash-flow-projection
#' @import dplyr
#' @importFrom DBI dbGetQuery dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
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
  on.exit(dbDisconnect(conn), add = TRUE)

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
        AND (COALESCE(aa.ignore_for_grouping, FALSE) = FALSE OR aa.type = 'Dividends')
        AND pg.status != 'ignored'
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

    # For CSP positions, get correct option symbols from members table
    # Activities may have truncated symbols (e.g., "26Dec25P11.00" instead of "BITO26Dec25P11.50")
    csp_groups <- result %>%
      filter(grepl("Cash-Secured Put", strategy_type, ignore.case = TRUE)) %>%
      pull(group_id) %>%
      unique()

    csp_member_symbols <- tibble(group_id = character(), member_symbol = character())
    if (length(csp_groups) > 0) {
      csp_member_symbols <- dbGetQuery(conn, paste0("
        SELECT group_id, symbol as member_symbol
        FROM position_group_members
        WHERE role = 'short_put'
          AND group_id IN ('", paste(csp_groups, collapse = "','"), "')
      ")) %>% as_tibble()
    }

    # For covered call groups, get underlying stock symbols for ticker extraction
    # This handles cases where the activity symbol is empty but the group knows the ticker
    covered_call_groups <- result %>%
      filter(strategy_type %in% c("Other", "Legacy Covered Call")) %>%
      pull(group_id) %>%
      unique()

    underlying_symbols <- tibble(group_id = character(), underlying_ticker = character())
    if (length(covered_call_groups) > 0) {
      underlying_symbols <- dbGetQuery(conn, paste0("
        SELECT group_id, symbol as underlying_ticker
        FROM position_group_members
        WHERE role = 'underlying_stock'
          AND group_id IN ('", paste(covered_call_groups, collapse = "','"), "')
      ")) %>% as_tibble()
    }

    # Process each activity to determine if it's a cash flow
    cash_flows <- result %>%
      # Join CSP member symbols to get correct option symbol
      left_join(csp_member_symbols, by = "group_id") %>%
      # Join underlying stock symbols for covered call groups
      left_join(underlying_symbols, by = "group_id") %>%
      mutate(
        # For CSPs, use member symbol if available
        # For covered calls with empty symbol, use underlying ticker
        # Otherwise use activity symbol
        effective_symbol = case_when(
          grepl("Cash-Secured Put", strategy_type, ignore.case = TRUE) & !is.na(member_symbol) ~ member_symbol,
          strategy_type %in% c("Other", "Legacy Covered Call") & (symbol == "" | is.na(symbol)) & !is.na(underlying_ticker) ~ underlying_ticker,
          TRUE ~ symbol
        ),
        # Parse ticker from effective symbol (for options, extract underlying)
        ticker = map_chr(effective_symbol, function(sym) {
          if (is_option_symbol(sym)) {
            parsed <- parse_option_symbol(sym)
            if (!is.na(parsed)) parsed else sym
          } else {
            sym
          }
        }),
        # Determine cash flow type based on strategy
        cash_flow_type = case_when(
          # Money market dividends get their own type for distinct coloring
          type == "Dividends" & strategy_type == "Money Market / Cash Equivalent" ~ "mm_dividend",
          # Equity dividends for "Other" and "Legacy Covered Call" strategies
          # Named strategies (Covered Calls, CSPs, etc.) use position_group_cash_flows for projections
          type == "Dividends" & strategy_type %in% c("Other", "Legacy Covered Call") ~ "dividend",
          # Option premiums for Legacy Covered Call - use group context, not just symbol parsing
          # Any Trades activity that is NOT the underlying stock is an option trade
          type %in% c("Trades", "Other") &
            strategy_type == "Legacy Covered Call" &
            (is.na(role) | role != "underlying_stock") ~ "option_premium",
          # Option premiums for Other and CSP strategies - require symbol to look like an option
          type %in% c("Trades", "Other") &
            strategy_type %in% c("Other", "S&P 500 Cash-Secured Puts") &
            is_option_symbol(symbol) ~ "option_premium",
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
# UNGROUPED DIVIDENDS (including ignored)
################################################################################

#' Get dividends that are not assigned to any group
#'
#' Retrieves dividend transactions that have no group_id assigned.
#' These are typically ignored dividends or dividends from positions
#' not tracked in position groups.
#'
#' @return Tibble with columns: event_date, ticker, type, amount, group_id, group_name, source, description
#' @noRd
get_ungrouped_dividends <- function() {
 conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    result <- dbGetQuery(conn, "
      SELECT
        aa.trade_date,
        aa.symbol,
        aa.net_amount,
        aa.description
      FROM account_activities aa
      WHERE aa.type = 'Dividends'
        AND aa.group_id IS NULL
      ORDER BY aa.trade_date ASC
    ") %>% as_tibble()

    if (nrow(result) == 0) {
      log_debug("Cash Flow Projection: No ungrouped dividends found")
      return(empty_cash_flow_tibble())
    }

    # Format as cash flow entries
    cash_flows <- result %>%
      transmute(
        event_date = as.Date(trade_date),
        ticker = symbol,
        type = "dividend",
        amount = net_amount,
        group_id = NA_character_,
        group_name = "Ungrouped",
        source = "actual",
        description = description
      )

    log_debug("Cash Flow Projection: Found {nrow(cash_flows)} ungrouped dividend events")
    return(cash_flows)

  }, error = function(e) {
    log_warn("Cash Flow Projection: Failed to get ungrouped dividends - {e$message}")
    return(empty_cash_flow_tibble())
  })
}

################################################################################
# CASH EQUIVALENT CAPITAL GAINS
################################################################################

#' Get capital gains/losses from cash equivalent position closures
#'
#' For Money Market / Cash Equivalent strategy positions, calculates realized
#' capital gains/losses when positions are sold. Uses FIFO matching of buys to sells.
#'
#' @return Tibble with columns: event_date, ticker, type, amount, group_id, group_name, source, description
#' @noRd
get_cash_equivalent_capital_gains <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    # Get cash equivalent tickers
    ce_tickers <- get_cash_equivalent_tickers()
    if (length(ce_tickers) == 0) {
      log_debug("Cash Flow Projection: No cash equivalent tickers configured")
      return(empty_cash_flow_tibble())
    }

    ce_tickers_sql <- paste0("'", paste(ce_tickers, collapse = "','"), "'")

    # Query all buy/sell transactions for cash equivalent tickers
    # in Money Market / Cash Equivalent strategy groups
    result <- dbGetQuery(conn, paste0("
      SELECT
        aa.trade_date,
        aa.symbol,
        aa.action,
        aa.quantity,
        aa.price,
        aa.net_amount,
        aa.group_id,
        pg.group_name
      FROM account_activities aa
      INNER JOIN position_groups pg ON aa.group_id = pg.group_id
      WHERE aa.type = 'Trades'
        AND aa.action IN ('Buy', 'Sell')
        AND UPPER(aa.symbol) IN (", ce_tickers_sql, ")
        AND pg.strategy_type = 'Money Market / Cash Equivalent'
        AND pg.status != 'ignored'
        AND COALESCE(aa.ignore_for_grouping, FALSE) = FALSE
      ORDER BY aa.symbol, aa.trade_date ASC
    ")) %>% as_tibble()

    if (nrow(result) == 0) {
      log_debug("Cash Flow Projection: No cash equivalent trades found")
      return(empty_cash_flow_tibble())
    }

    # Calculate capital gains using FIFO matching per symbol
    capital_gains <- result %>%
      group_by(symbol) %>%
      group_modify(~ calculate_fifo_gains(.x)) %>%
      ungroup()

    if (nrow(capital_gains) == 0) {
      log_debug("Cash Flow Projection: No closed cash equivalent positions found")
      return(empty_cash_flow_tibble())
    }

    # Format as cash flow entries
    cash_flows <- capital_gains %>%
      transmute(
        event_date = as.Date(sell_date),
        ticker = symbol,
        type = "capital_gain",
        amount = capital_gain,
        group_id = group_id,
        group_name = group_name,
        source = "actual",
        description = paste0("Capital gain/loss on ", symbol, " sale")
      )

    log_debug("Cash Flow Projection: Found {nrow(cash_flows)} cash equivalent capital gain events")
    return(cash_flows)

  }, error = function(e) {
    log_warn("Cash Flow Projection: Failed to get cash equivalent capital gains - {e$message}")
    return(empty_cash_flow_tibble())
  })
}

#' Calculate FIFO capital gains for a single symbol
#'
#' @param trades Data frame of buy/sell trades for one symbol, ordered by date
#' @return Data frame with sell_date, capital_gain, group_id, group_name, symbol
#' @noRd
calculate_fifo_gains <- function(trades) {
  if (nrow(trades) == 0) return(tibble())

  # Separate buys and sells
  buys <- trades %>% filter(action == "Buy") %>% arrange(trade_date)
  sells <- trades %>% filter(action == "Sell") %>% arrange(trade_date)

  if (nrow(sells) == 0) {
    # No sells = no realized gains
    return(tibble())
  }

  gains <- list()
  buy_queue <- list()

  # Build initial buy queue
 for (i in seq_len(nrow(buys))) {
    buy_queue[[length(buy_queue) + 1]] <- list(
      date = buys$trade_date[i],
      quantity = buys$quantity[i],
      price = buys$price[i],
      remaining = buys$quantity[i]
    )
  }

  # Process each sell using FIFO
  for (i in seq_len(nrow(sells))) {
    sell_qty <- sells$quantity[i]
    sell_price <- sells$price[i]
    sell_date <- sells$trade_date[i]
    sell_group_id <- sells$group_id[i]
    sell_group_name <- sells$group_name[i]
    symbol <- sells$symbol[i]

    total_cost_basis <- 0
    qty_matched <- 0

    # Match against buys in FIFO order
    for (j in seq_along(buy_queue)) {
      if (sell_qty <= 0) break
      if (is.null(buy_queue[[j]]) || buy_queue[[j]]$remaining <= 0) next

      match_qty <- min(buy_queue[[j]]$remaining, sell_qty)
      total_cost_basis <- total_cost_basis + (match_qty * buy_queue[[j]]$price)
      buy_queue[[j]]$remaining <- buy_queue[[j]]$remaining - match_qty
      sell_qty <- sell_qty - match_qty
      qty_matched <- qty_matched + match_qty
    }

    if (qty_matched > 0) {
      sell_proceeds <- qty_matched * sell_price
      capital_gain <- sell_proceeds - total_cost_basis

      gains[[length(gains) + 1]] <- tibble(
        sell_date = sell_date,
        capital_gain = capital_gain,
        group_id = sell_group_id,
        group_name = sell_group_name,
        symbol = symbol
      )
    }
  }

  if (length(gains) == 0) return(tibble())
  bind_rows(gains)
}

#' Create empty cash flow tibble with standard schema
#' @noRd
empty_cash_flow_tibble <- function() {
  tibble(
    event_date = as.Date(character()),
    ticker = character(),
    type = character(),
    amount = numeric(),
    group_id = character(),
    group_name = character(),
    source = character(),
    description = character()
  )
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
  on.exit(dbDisconnect(conn), add = TRUE)

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
        AND pg.status != 'ignored'
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

    # Extract ticker for each group
    # For Covered Calls: use underlying_stock symbol directly
    # For CSPs: parse ticker from the short_put option symbol (no underlying_stock member)
    underlying_tickers <- group_members %>%
      filter(role == "underlying_stock") %>%
      select(group_id, ticker = symbol)

    # For CSPs, extract ticker from short_put option symbol
    csp_tickers <- group_members %>%
      filter(role == "short_put") %>%
      mutate(ticker = purrr::map_chr(symbol, ~ {
        parsed <- parse_option_symbol(.x)
        if (!is.null(parsed)) parsed else NA_character_
      })) %>%
      filter(!is.na(ticker)) %>%
      select(group_id, ticker)

    # Combine both sources, prioritizing underlying_stock
    group_tickers <- bind_rows(underlying_tickers, csp_tickers) %>%
      distinct(group_id, .keep_all = TRUE)

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
  on.exit(dbDisconnect(conn), add = TRUE)

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
  ce_capital_gains <- get_cash_equivalent_capital_gains()
  ungrouped_divs <- get_ungrouped_dividends()

  # Combine datasets
  # Actuals have description column, projections have confidence
  # Standardize to common schema
  actuals_standardized <- actuals %>%
    mutate(confidence = NA_character_) %>%
    select(event_date, ticker, type, amount, group_id, group_name, source)

  projections_standardized <- projections %>%
    select(event_date, ticker, type, amount, group_id, group_name, source)

  ce_gains_standardized <- ce_capital_gains %>%
    select(event_date, ticker, type, amount, group_id, group_name, source)

  ungrouped_divs_standardized <- ungrouped_divs %>%
    select(event_date, ticker, type, amount, group_id, group_name, source)

  all_flows <- bind_rows(actuals_standardized, projections_standardized, ce_gains_standardized, ungrouped_divs_standardized)

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
