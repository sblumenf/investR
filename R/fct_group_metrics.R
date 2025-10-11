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

    # Calculate cost basis (money out)
    stock_purchases <- activities %>%
      filter(type == "Trades", action == "Buy", !purrr::map_lgl(symbol, is_option_symbol)) %>%
      summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
      pull(total)

    stock_purchases <- if (length(stock_purchases) == 0) 0 else stock_purchases

    total_commissions <- activities %>%
      summarise(total = sum(abs(commission), na.rm = TRUE)) %>%
      pull(total)

    total_commissions <- if (length(total_commissions) == 0) 0 else total_commissions

    cost_basis <- stock_purchases + total_commissions

    # Calculate cash collected to date (money in, realized)
    option_premiums <- activities %>%
      filter(type == "Trades", action == "Sell", purrr::map_lgl(symbol, is_option_symbol)) %>%
      summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
      pull(total)

    option_premiums <- if (length(option_premiums) == 0) 0 else option_premiums

    actual_dividends <- activities %>%
      filter(type == "Dividends") %>%
      summarise(total = sum(abs(net_amount), na.rm = TRUE)) %>%
      pull(total)

    actual_dividends <- if (length(actual_dividends) == 0) 0 else actual_dividends

    cash_collected <- option_premiums + actual_dividends

    # Calculate projected income (future cash flows)
    projected_cash_flows <- get_group_cash_flows(group_id) %>%
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
      as.Date()

    days_held <- as.numeric(Sys.Date() - first_trade_date)
    days_held <- if (length(days_held) == 0 || is.na(days_held) || days_held == 0) 1 else days_held

    # Calculate projected annualized return
    # Formula: ((1 + total_return_ratio)^(365/days_held) - 1) * 100
    projected_annualized_return_pct <- if (cost_basis > 0 && days_held > 0) {
      total_return_ratio <- 1 + (target_total_return / cost_basis)
      annualization_factor <- 365 / days_held
      ((total_return_ratio ^ annualization_factor) - 1) * 100
    } else {
      0
    }

    # Return metrics
    metrics <- tibble::tibble(
      group_id = group_id,
      cost_basis = cost_basis,
      cash_collected = cash_collected,
      projected_income = projected_income,
      target_total_return = target_total_return,
      pct_recovered = pct_recovered,
      days_held = days_held,
      projected_annualized_return_pct = projected_annualized_return_pct,
      first_trade_date = as.character(first_trade_date)
    )

    log_debug("Group Metrics: Calculated metrics for group {group_id} - Cost: ${round(cost_basis, 2)}, Collected: ${round(cash_collected, 2)}, Projected: ${round(projected_income, 2)}")

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
          metrics = purrr::map(group_id, function(gid) {
            # Filter activities for this group
            group_activities <- all_activities %>% filter(group_id == gid)

            # Filter cash flows for this group
            group_cash_flows <- all_cash_flows %>% filter(group_id == gid)

            # Calculate metrics inline
            if (nrow(group_activities) == 0) {
              return(tibble::tibble(
                cost_basis = 0, cash_collected = 0, projected_income = 0,
                pct_recovered = 0, projected_annualized_return_pct = 0
              ))
            }

            # Cost basis
            stock_purchases <- group_activities %>%
              filter(type == "Trades", action == "Buy", !purrr::map_lgl(symbol, is_option_symbol)) %>%
              summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
              pull(total)
            stock_purchases <- if (length(stock_purchases) == 0) 0 else stock_purchases

            commissions <- group_activities %>%
              summarise(total = sum(abs(commission), na.rm = TRUE)) %>%
              pull(total)
            commissions <- if (length(commissions) == 0) 0 else commissions

            cost_basis <- stock_purchases + commissions

            # Cash collected
            premiums <- group_activities %>%
              filter(type == "Trades", action == "Sell", purrr::map_lgl(symbol, is_option_symbol)) %>%
              summarise(total = sum(abs(gross_amount), na.rm = TRUE)) %>%
              pull(total)
            premiums <- if (length(premiums) == 0) 0 else premiums

            dividends <- group_activities %>%
              filter(type == "Dividends") %>%
              summarise(total = sum(abs(net_amount), na.rm = TRUE)) %>%
              pull(total)
            dividends <- if (length(dividends) == 0) 0 else dividends

            cash_collected <- premiums + dividends

            # Projected income
            projected_income <- group_cash_flows %>%
              filter(status == "projected") %>%
              summarise(total = sum(amount, na.rm = TRUE)) %>%
              pull(total)
            projected_income <- if (length(projected_income) == 0) 0 else projected_income

            # Calculated metrics
            pct_recovered <- if (cost_basis > 0) (cash_collected / cost_basis) * 100 else 0

            first_date <- group_activities %>%
              filter(!is.na(trade_date)) %>%
              arrange(trade_date) %>%
              slice(1) %>%
              pull(trade_date) %>%
              as.Date()
            days_held <- as.numeric(Sys.Date() - first_date)
            days_held <- if (length(days_held) == 0 || is.na(days_held) || days_held == 0) 1 else days_held

            target_return <- cash_collected + projected_income
            projected_ann_return <- if (cost_basis > 0 && days_held > 0) {
              ((1 + (target_return / cost_basis)) ^ (365 / days_held) - 1) * 100
            } else {
              0
            }

            tibble::tibble(
              cost_basis = cost_basis,
              cash_collected = cash_collected,
              projected_income = projected_income,
              target_total_return = target_return,
              pct_recovered = pct_recovered,
              days_held = days_held,
              projected_annualized_return_pct = projected_ann_return,
              first_trade_date = as.character(first_date)
            )
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
        detail = closed_groups
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
