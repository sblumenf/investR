#' Position Group Metrics Calculations
#'
#' Functions for calculating forward-looking metrics for open position groups.
#' These are distinct from P&L calculations (which are for closed groups with
#' realized returns). Open group metrics include cost basis, cash collected,
#' projected income, and target returns.
#'
#' @name group-metrics
#' @import dplyr
#' @importFrom logger log_info log_warn log_error log_debug
#' @importFrom tidyr unnest
NULL

################################################################################
# SHARED METRICS CALCULATION LOGIC
################################################################################

#' Calculate metrics from activities and cash flows (DRY helper)
#'
#' Shared calculation logic used by multiple functions to avoid duplication.
#' Computes cost basis, projected income, hold period, and annualized returns.
#'
#' @param activities Tibble of account_activities for the group
#' @param cash_flows Tibble of position_group_cash_flows for the group
#' @param strategy_type String: strategy type from position_groups
#' @param group_id String: group identifier (for logging)
#' @return Tibble with calculated metrics
#' @noRd
calculate_metrics_core <- function(activities, cash_flows, strategy_type, group_id = NULL) {
  # If no activities, return empty metrics
  if (nrow(activities) == 0) {
    return(tibble::tibble(
      cost_basis = 0,
      cash_collected = 0,
      projected_income = 0,
      target_total_return = 0,
      pct_recovered = 0,
      days_held = 1,
      projected_annualized_return_pct = 0,
      first_trade_date = as.character(NA)
    ))
  }

  # Detect Cash-Secured Put strategy
  is_csp <- grepl("Cash-Secured Put", strategy_type, ignore.case = TRUE)

  # Calculate stock purchases (excluding commissions for gross amount)
  stock_purchases <- activities %>%
    filter(type == "Trades", action == "Buy", !purrr::map_lgl(symbol, is_option_symbol)) %>%
    summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
    pull(total)
  stock_purchases <- if (length(stock_purchases) == 0) 0 else stock_purchases

  # Calculate total commissions
  total_commissions <- activities %>%
    summarise(total = sum(abs(commission), na.rm = TRUE)) %>%
    pull(total)
  total_commissions <- if (length(total_commissions) == 0) 0 else total_commissions

  # Calculate option premiums (excluding commissions for gross amount)
  option_sells <- activities %>%
    filter(type == "Trades", action == "Sell", purrr::map_lgl(symbol, is_option_symbol))

  option_premiums <- option_sells %>%
    summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
    pull(total)
  option_premiums <- if (length(option_premiums) == 0) 0 else option_premiums

  # Calculate dividends received
  total_dividends <- activities %>%
    filter(type == "Dividends") %>%
    summarise(total = sum(abs(net_amount), na.rm = TRUE)) %>%
    pull(total)
  total_dividends <- if (length(total_dividends) == 0) 0 else total_dividends

  # Apply strategy-specific accounting
  # For CSP: cost_basis = cash collateral (strike × 100 × contracts)
  # For covered call strategies (non-"Other"): premiums reduce cost basis
  # For "Other" strategy: premiums are income (legacy behavior)
  csp_expiration_date <- NULL  # Track expiration for CSP days_held calculation

  if (is_csp) {
    # For Cash-Secured Puts: calculate cash collateral from group members
    # The "capital at risk" is strike × 100 × number of contracts
    # Use members table (has complete option symbol) instead of activities (may be truncated)
    cash_collateral <- 0
    
    # Get members to find the short_put with correct option symbol
    members <- if (!is.null(group_id)) get_group_members(group_id) else tibble::tibble()
    put_members <- members %>% filter(role == "short_put")
    
    if (nrow(put_members) > 0) {
      # Get strike and expiry from members (has complete symbol)
      option_symbol <- put_members$symbol[1]
      option_details <- parse_option_details(option_symbol)
      
      if (!is.null(option_details$strike) && !is.na(option_details$strike)) {
        # Get quantity from activities (members doesn't store quantity)
        option_qty <- if (nrow(option_sells) > 0) sum(abs(option_sells$quantity)) else 1
        cash_collateral <- option_details$strike * 100 * option_qty
      }
      
      # Capture expiration date for days_held calculation
      if (!is.null(option_details$expiry) && !is.na(option_details$expiry)) {
        csp_expiration_date <- option_details$expiry
      }
    }

    # If we couldn't parse strike from members, fall back to a reasonable estimate
    if (cash_collateral == 0 && option_premiums > 0) {
      log_warn("Group Metrics: Could not parse strike for CSP {group_id}, using premium as fallback")
      cash_collateral <- option_premiums * 10  # Rough estimate
    }

    cost_basis <- cash_collateral
    cash_collected <- option_premiums  # Premium is income for CSP
    log_debug("GROUP_METRICS_CALC: CSP detected - cash_collateral={cash_collateral}, premium={option_premiums}, expiry={csp_expiration_date}")
  } else if (strategy_type != "Other") {
    cost_basis <- stock_purchases + total_commissions - option_premiums
    cash_collected <- total_dividends
  } else {
    cost_basis <- stock_purchases + total_commissions
    cash_collected <- option_premiums + total_dividends
  }

  # Calculate projected income (future cash flows)
  projected_cash_flows <- cash_flows %>%
    filter(status == "projected")

  projected_income <- if (nrow(projected_cash_flows) > 0) {
    sum(projected_cash_flows$amount, na.rm = TRUE)
  } else {
    0
  }

  # Calculate derived metrics
  target_total_return <- cash_collected + projected_income
  pct_recovered <- if (cost_basis > 0) (cash_collected / cost_basis) * 100 else 0

  # Calculate hold period in days
  first_trade_date <- activities %>%
    filter(!is.na(trade_date)) %>%
    arrange(trade_date) %>%
    slice(1) %>%
    pull(trade_date) %>%
    as.POSIXct(origin = "1970-01-01", tz = "UTC") %>%
    as.Date()

  # For covered call strategies (non-"Other"), use expected hold period until option expiration
  # For "Other" strategies, no projected returns (no expected close date)
  # For CSP: use option expiration date parsed from symbol
  if (is_csp && !is.null(csp_expiration_date)) {
    # CSP: use expiration date from option symbol
    days_held <- as.numeric(csp_expiration_date - first_trade_date)
    log_debug("GROUP_METRICS_CALC: CSP using expiration - first={first_trade_date}, expiry={csp_expiration_date}, days={days_held}")
  } else if (strategy_type != "Other" && nrow(projected_cash_flows) > 0) {
    # Use last projected event date as end date (option expiration)
    last_event_date <- projected_cash_flows %>%
      arrange(desc(event_date)) %>%
      slice(1) %>%
      pull(event_date) %>%
      as.Date()

    days_held <- as.numeric(last_event_date - first_trade_date)
    log_debug("GROUP_METRICS_CALC: Using last_event_date - first={first_trade_date}, last={last_event_date}, days={days_held}")
  } else if (strategy_type == "Other") {
    # For "Other" strategy, use days held so far but don't calculate projected returns
    days_held <- as.numeric(Sys.Date() - first_trade_date)
    log_debug("Group Metrics: Other strategy - first_trade: {first_trade_date}, today: {Sys.Date()}, days_held: {days_held}")
  } else {
    # Fallback for strategies with no projected cash flows
    days_held <- as.numeric(Sys.Date() - first_trade_date)
  }

  days_held <- if (length(days_held) == 0 || is.na(days_held) || days_held <= 0) 1 else days_held

  # Calculate projected annualized return
  # Formula: ((1 + return_ratio)^(365/days_held) - 1) * 100
  # return_ratio = income / cost_basis (the gain as a percentage)
  # Note: "Other" strategy does NOT calculate projected returns (no expected close date)
  projected_annualized_return_pct <- if (strategy_type == "Other") {
    NA_real_  # No projected return for "Other" strategy
  } else if (is_csp && cost_basis > 0 && days_held > 0) {
    # For CSP: use premium collected as the income (already received)
    # projected_income may be 0 since CSPs don't project option_gain
    csp_income <- if (projected_income > 0) projected_income else cash_collected
    return_ratio <- csp_income / cost_basis
    annualization_factor <- 365 / days_held
    result <- ((1 + return_ratio) ^ annualization_factor - 1) * 100
    log_debug("GROUP_METRICS_CALC: CSP - income={csp_income}, collateral={cost_basis}, days={days_held}, result={result}%")
    result
  } else if (cost_basis > 0 && days_held > 0) {
    return_ratio <- projected_income / cost_basis
    annualization_factor <- 365 / days_held
    result <- ((1 + return_ratio) ^ annualization_factor - 1) * 100
    log_debug("GROUP_METRICS_CALC: proj_inc={projected_income}, cost={cost_basis}, days={days_held}, result={result}%")
    result
  } else {
    0
  }

  # Return metrics tibble
  tibble::tibble(
    cost_basis = cost_basis,
    cash_collected = cash_collected,
    projected_income = projected_income,
    target_total_return = target_total_return,
    pct_recovered = pct_recovered,
    days_held = days_held,
    projected_annualized_return_pct = projected_annualized_return_pct,
    first_trade_date = as.character(first_trade_date)
  )
}

################################################################################
# OPEN GROUP METRICS
################################################################################

#' Calculate metrics for an open position group
#'
#' Computes forward-looking metrics based on historical activities and
#' projected cash flows. Does not use current market prices.
#'
#' Returns:
#' - cost_basis: Total money invested (stock purchases + commissions)
#' - cash_collected: Money received to date (premiums + dividends)
#' - projected_income: Expected future cash flows (dividends + option gains)
#' - target_total_return: cash_collected + projected_income
#' - pct_recovered: (cash_collected / cost_basis) * 100
#' - days_held: Days since first trade
#' - projected_annualized_return_pct: Annualized return if projections realize
#'
#' @param group_id Group identifier
#' @return Tibble with metrics or empty tibble if failed
#' @noRd
calculate_open_group_metrics <- function(group_id) {
  tryCatch({
    # Get all activities for this group
    activities <- get_activities_by_group(group_id)

    if (nrow(activities) == 0) {
      log_debug("Group Metrics: No activities found for group {group_id}")
      return(tibble::tibble())
    }

    # Get group info for strategy type
    group_info <- get_group_by_id(group_id)

    if (nrow(group_info) == 0) {
      log_warn("Group Metrics: Group {group_id} not found")
      return(tibble::tibble())
    }

    strategy_type <- group_info$strategy_type[1]
    cash_flows <- get_group_cash_flows(group_id)

    # Use shared calculation logic (DRY)
    metrics <- calculate_metrics_core(activities, cash_flows, strategy_type, group_id)

    # Add group_id to result
    metrics <- metrics %>%
      mutate(group_id = group_id) %>%
      select(group_id, everything())

    log_debug("Group Metrics: Calculated metrics for group {group_id} - Cost: ${round(metrics$cost_basis, 2)}, Collected: ${round(metrics$cash_collected, 2)}, Projected: ${round(metrics$projected_income, 2)}")

    return(metrics)

  }, error = function(e) {
    log_error("Group Metrics: Failed to calculate metrics for group {group_id} - {e$message}")
    return(tibble::tibble())
  })
}

################################################################################
# UNIFIED GROUP SUMMARY
################################################################################

#' Get summary metrics for any group (open or closed)
#'
#' Wrapper function that routes to appropriate calculation based on group status.
#' For closed groups: calls calculate_group_pnl()
#' For open groups: calls calculate_open_group_metrics()
#'
#' @param group_id Group identifier
#' @return Tibble with metrics and status indicator
#' @noRd
get_group_summary_for_card <- function(group_id) {
  # Get group info to check status
  group_info <- get_group_by_id(group_id)

  if (nrow(group_info) == 0) {
    log_warn("Group Summary: Group {group_id} not found")
    return(tibble::tibble())
  }

  status <- group_info$status[1]

  if (status == "closed") {
    # Use P&L calculations for closed groups
    pnl <- calculate_group_pnl(group_id)

    if (nrow(pnl) == 0) {
      return(tibble::tibble())
    }

    # Add status column
    pnl %>%
      mutate(status = "closed")

  } else {
    # Use metrics calculations for open groups
    metrics <- calculate_open_group_metrics(group_id)

    if (nrow(metrics) == 0) {
      return(tibble::tibble())
    }

    # Add status column
    metrics %>%
      mutate(status = "open")
  }
}

################################################################################
# DASHBOARD AGGREGATIONS
################################################################################

#' Calculate dashboard summary metrics
#'
#' Aggregates metrics across all groups for dashboard display.
#' Separates open and closed groups with appropriate calculations for each.
#' Returns both aggregated summaries and individual group metrics for reuse.
#'
#' @param status_filter Optional filter: "open", "closed", or NULL for all
#' @return List with open_metrics, closed_metrics, open_groups_detail, closed_groups_detail
#' @noRd
calculate_dashboard_metrics <- function(status_filter = NULL) {
  tryCatch({
    # Get all groups based on filter
    if (is.null(status_filter)) {
      all_groups <- get_all_groups(include_closed = TRUE)
    } else if (status_filter == "open") {
      all_groups <- get_all_groups(include_closed = FALSE)
    } else if (status_filter == "closed") {
      all_groups <- get_all_groups(include_closed = TRUE) %>%
        filter(status == "closed")
    } else {
      log_warn("Dashboard Metrics: Invalid status_filter '{status_filter}'")
      return(list(open_metrics = tibble::tibble(), closed_metrics = tibble::tibble()))
    }

    if (nrow(all_groups) == 0) {
      log_debug("Dashboard Metrics: No groups found")
      return(list(
        open_metrics = tibble::tibble(),
        closed_metrics = tibble::tibble(),
        open_groups_detail = tibble::tibble(),
        closed_groups_detail = tibble::tibble()
      ))
    }

    # Split into open and closed
    open_groups <- all_groups %>% filter(status == "open")
    closed_groups <- all_groups %>% filter(status == "closed")

    # Calculate open group metrics using batch queries
    open_metrics <- if (nrow(open_groups) > 0) {
      # Batch fetch all activities and cash flows for open groups
      open_group_ids <- open_groups$group_id
      all_activities <- get_activities_for_groups(open_group_ids)
      all_cash_flows <- get_cash_flows_for_groups(open_group_ids)

      # Calculate metrics for each group using in-memory data
      # Keep group metadata (group_id, strategy_type, etc.) for later reuse
      metrics_df <- open_groups %>%
        mutate(
          metrics = purrr::map2(group_id, strategy_type, function(gid, strat_type) {
            # Filter activities for this group
            group_activities <- all_activities %>% filter(group_id == gid)

            # Filter cash flows for this group
            group_cash_flows <- all_cash_flows %>% filter(group_id == gid)

            # Use shared calculation logic (DRY)
            calculate_metrics_core(group_activities, group_cash_flows, strat_type, gid)
          })
        ) %>%
        tidyr::unnest(metrics)

      # Store detailed metrics for reuse (DRY principle)
      open_groups_detail <- metrics_df

      # Calculate aggregated summary
      if (nrow(metrics_df) > 0) {
        list(
          summary = tibble::tibble(
            count = nrow(open_groups),
            total_cost_basis = sum(metrics_df$cost_basis, na.rm = TRUE),
            total_cash_collected = sum(metrics_df$cash_collected, na.rm = TRUE),
            total_projected_income = sum(metrics_df$projected_income, na.rm = TRUE),
            avg_projected_return_pct = mean(metrics_df$projected_annualized_return_pct, na.rm = TRUE),
            avg_pct_recovered = mean(metrics_df$pct_recovered, na.rm = TRUE)
          ),
          detail = open_groups_detail
        )
      } else {
        list(
          summary = tibble::tibble(
            count = 0,
            total_cost_basis = 0,
            total_cash_collected = 0,
            total_projected_income = 0,
            avg_projected_return_pct = 0,
            avg_pct_recovered = 0
          ),
          detail = tibble::tibble()
        )
      }
    } else {
      list(
        summary = tibble::tibble(
          count = 0,
          total_cost_basis = 0,
          total_cash_collected = 0,
          total_projected_income = 0,
          avg_projected_return_pct = 0,
          avg_pct_recovered = 0
        ),
        detail = tibble::tibble()
      )
    }

    # Calculate closed group metrics
    closed_metrics <- if (nrow(closed_groups) > 0) {
      list(
        summary = closed_groups %>%
          summarise(
            count = n(),
            total_realized_pnl = sum(total_return_amount, na.rm = TRUE),
            avg_total_return_pct = mean(total_return_pct, na.rm = TRUE),
            avg_annualized_return_pct = mean(annualized_return_pct, na.rm = TRUE)
          ),
        detail = closed_groups %>%
          arrange(desc(annualized_return_pct))
      )
    } else {
      list(
        summary = tibble::tibble(
          count = 0,
          total_realized_pnl = 0,
          avg_total_return_pct = 0,
          avg_annualized_return_pct = 0
        ),
        detail = tibble::tibble()
      )
    }

    log_info("Dashboard Metrics: Calculated - {open_metrics$summary$count} open, {closed_metrics$summary$count} closed")

    return(list(
      open_metrics = open_metrics$summary,
      closed_metrics = closed_metrics$summary,
      open_groups_detail = open_metrics$detail,
      closed_groups_detail = closed_metrics$detail
    ))

  }, error = function(e) {
    log_error("Dashboard Metrics: Failed to calculate - {e$message}")
    return(list(
      open_metrics = tibble::tibble(),
      closed_metrics = tibble::tibble(),
      open_groups_detail = tibble::tibble(),
      closed_groups_detail = tibble::tibble()
    ))
  })
}

#' Get strategy breakdown for dashboard
#'
#' Groups by strategy_type and calculates counts and average returns.
#' Accepts pre-calculated group details to avoid redundant calculations (DRY).
#'
#' @param open_groups_detail Tibble with open group metrics (from calculate_dashboard_metrics)
#' @param closed_groups_detail Tibble with closed group data (from calculate_dashboard_metrics)
#' @return Tibble with strategy_type, count, avg_return columns
#' @noRd
get_strategy_breakdown <- function(open_groups_detail = NULL, closed_groups_detail = NULL) {
  tryCatch({
    # Combine open and closed groups with their respective return metrics
    all_groups <- bind_rows(
      # Open groups: use calculated projected_annualized_return_pct
      if (!is.null(open_groups_detail) && nrow(open_groups_detail) > 0) {
        open_groups_detail %>%
          select(group_id, strategy_type, status, projected_annualized_return_pct) %>%
          rename(return_pct = projected_annualized_return_pct)
      } else {
        tibble::tibble()
      },
      # Closed groups: use stored annualized_return_pct
      if (!is.null(closed_groups_detail) && nrow(closed_groups_detail) > 0) {
        closed_groups_detail %>%
          select(group_id, strategy_type, status, annualized_return_pct) %>%
          rename(return_pct = annualized_return_pct)
      } else {
        tibble::tibble()
      }
    )

    if (nrow(all_groups) == 0) {
      return(tibble::tibble())
    }

    # Group by strategy and calculate averages
    strategy_summary <- all_groups %>%
      group_by(strategy_type) %>%
      summarise(
        count = n(),
        avg_return_pct = mean(return_pct, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_return_pct))

    return(strategy_summary)

  }, error = function(e) {
    log_error("Strategy Breakdown: Failed to calculate - {e$message}")
    return(tibble::tibble())
  })
}

################################################################################
# MARKET DATA ENRICHMENT
################################################################################

#' Enrich group with current market data
#'
#' Fetches current stock prices and calculates market-based metrics for display.
#' Returns current price, days to expiration, strike price, and ITM/OTM status.
#'
#' @param group_id Group identifier
#' @param members Tibble from get_group_members()
#' @param activities Tibble from get_activities_by_group()
#' @param latest_positions Tibble from get_latest_positions() (optional, will fetch if NULL)
#' @return List with market data metrics or NULL values if unavailable
#' @noRd
enrich_group_with_market_data <- function(group_id, members, activities, latest_positions = NULL) {
  tryCatch({
    # Initialize result with NULL values (fallback)
    result <- list(
      current_stock_price = NULL,
      days_to_expiration = NULL,
      strike_price = NULL,
      expiration_date = NULL,
      itm_otm_amount = NULL
    )

    # Find the underlying stock, cash equivalent, or short_put member
    stock_member <- members %>%
      filter(role %in% c("underlying_stock", "cash_equivalent")) %>%
      slice(1)

    # For CSPs: no underlying stock, need to parse ticker from put option
    is_csp <- nrow(stock_member) == 0 && any(members$role == "short_put")

    if (nrow(stock_member) == 0 && !is_csp) {
      log_debug("Market Data: No underlying stock or cash equivalent found for group {group_id}")
      return(result)
    }

    # Determine stock symbol to fetch price for
    if (is_csp) {
      put_member <- members %>% filter(role == "short_put") %>% slice(1)
      stock_symbol <- parse_option_symbol(put_member$symbol)
      log_debug("Market Data: CSP detected, fetching price for underlying {stock_symbol}")
    } else if (stock_member$role[1] == "cash_equivalent") {
      stock_symbol <- stock_member$symbol
      log_debug("Market Data: Processing cash equivalent {stock_symbol} for group {group_id}")
    } else {
      stock_symbol <- stock_member$symbol
    }

    # Always fetch fresh real-time price from API for portfolio groups
    # Bypasses stale database cache that only updates when trades occur
    log_debug("Market Data: Fetching fresh price for {stock_symbol} from market API")

    current_price <- NULL
    quote_data <- tryCatch({
      fetch_current_quote(stock_symbol)
    }, error = function(e) {
      log_warn("Market Data: Failed to fetch quote for {stock_symbol} - {e$message}")
      NULL
    })

    # fetch_current_quote returns Yahoo-format dataframe with 'Last' column
    if (!is.null(quote_data) && nrow(quote_data) > 0 && !is.na(quote_data$Last[1])) {
      current_price <- quote_data$Last[1]
      log_debug("Market Data: Fetched fresh price for {stock_symbol}: ${current_price}")
    } else {
      log_debug("Market Data: Current price not available for {stock_symbol} from API")
      return(result)
    }

    result$current_stock_price <- current_price

    # Parse option information (if strategy involves options)
    option_members <- members %>%
      filter(role %in% c("short_call", "long_put", "short_put"))

    if (nrow(option_members) > 0) {
      # Parse first option for expiration and strike
      option_symbol <- option_members$symbol[1]
      option_details <- parse_option_details(option_symbol)

      if (!is.null(option_details$expiry)) {
        result$expiration_date <- option_details$expiry
        result$days_to_expiration <- as.numeric(option_details$expiry - Sys.Date())
      }

      if (!is.null(option_details$strike)) {
        result$strike_price <- option_details$strike

        # Calculate ITM/OTM amount (for short calls)
        if (!is.null(current_price)) {
          result$itm_otm_amount <- current_price - option_details$strike
        }
      }
    }

    log_debug("Market Data: Enriched group {group_id} - Price: ${current_price}, Days to exp: {result$days_to_expiration}")

    return(result)

  }, error = function(e) {
    log_error("Market Data: Failed to enrich group {group_id} - {e$message}")
    # Return empty result on error (graceful degradation)
    return(list(
      current_stock_price = NULL,
      days_to_expiration = NULL,
      strike_price = NULL,
      expiration_date = NULL,
      itm_otm_amount = NULL
    ))
  })
}
