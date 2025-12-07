#' Portfolio Expected Return Calculations
#'
#' Functions for calculating portfolio-wide expected annualized returns.
#' Provides capital-weighted returns for both open (expected) and closed (realized) positions.
#'
#' @name portfolio-expected-return
#' @import dplyr
#' @importFrom logger log_info log_warn log_error log_debug
#' @importFrom tibble tibble
NULL

# Strategy type display mapping
STRATEGY_DISPLAY_MAP <- list(
  "Covered Call" = "Covered Calls",
  "Dynamic Covered Calls" = "Covered Calls",
  "Legacy Covered Call" = "Covered Calls",
  "Zero-Dividend" = "Covered Calls",
  "CSP" = "Cash Secured Puts",
  "Cash-Secured Put" = "Cash Secured Puts",
  "Collar" = "Collars",
  "Put Calendar Spread" = "Put Calendar Spreads",
  "Dividend Aristocrats" = "Dividend Aristocrats",
  "Other" = "Other"
)

#' Map strategy type to display category
#'
#' @param strategy_type Raw strategy type from database
#' @param ticker Optional ticker to identify money market positions
#' @return Display category string
#' @noRd
map_strategy_to_display <- function(strategy_type, ticker = NULL) {
  # Check for money market first
  if (!is.null(ticker) && ticker %in% CASH_EQUIVALENT_TICKERS) {
    return("Money Market")
  }

  # Use mapping or default to "Other"
  display <- STRATEGY_DISPLAY_MAP[[strategy_type]]
  if (is.null(display)) {
    return("Other")
  }

  display
}

#' Get money market yield for a ticker
#'
#' Returns the current SEC yield for money market funds.
#' SGOV: Fetches from Yahoo Finance dividend yield
#' ZMMK: Uses SGOV yield as proxy (similar treasury money market)
#' Falls back to configured default if API fails.
#'
#' @param ticker Money market ticker ("SGOV" or "ZMMK.TO")
#' @return Numeric yield as decimal (0.05 = 5%) or NA if unavailable
#' @noRd
get_money_market_yield <- function(ticker) {
  # Normalize ticker (strip .TO suffix for comparison)
  ticker_normalized <- gsub("\\.TO$", "", ticker)

  # Default fallback yield
  default_yield <- ARISTOCRATS_CONFIG$sgov_yield_default %||% 0.045

  tryCatch({
    # For both SGOV and ZMMK, use SGOV yield as the proxy
    # (both are short-term Treasury money market funds)
    sgov_yield <- get_reinvestment_rate()

    if (!is.na(sgov_yield) && sgov_yield > 0 && sgov_yield < 0.15) {
      log_debug("Money Market Yield: {ticker} using SGOV yield = {round(sgov_yield * 100, 2)}%")
      return(sgov_yield)
    }

    log_warn("Money Market Yield: Invalid SGOV yield ({sgov_yield}), using default {default_yield * 100}%")
    return(default_yield)

  }, error = function(e) {
    log_warn("Money Market Yield: Failed to fetch yield for {ticker}, using default - {e$message}")
    return(default_yield)
  })
}

#' Calculate capital at risk for a position
#'
#' Determines the capital at risk based on position type:
#' - CSPs: strike * 100 * contracts (cash collateral)
#' - Covered Calls/Collars: stock cost basis
#' - Dividend Aristocrats: stock cost basis
#' - Money Market: current market value
#'
#' @param group_detail Row from calculate_dashboard_metrics detail
#' @param strategy_type Strategy type string
#' @param ticker Ticker symbol
#' @return Numeric capital at risk
#' @noRd
calculate_capital_at_risk <- function(group_detail, strategy_type, ticker = NULL) {
  # Money market: use cost_basis (which should equal current value)
  if (!is.null(ticker) && ticker %in% CASH_EQUIVALENT_TICKERS) {
    return(group_detail$cost_basis)
  }

  # CSP: cost_basis already calculated as collateral in calculate_metrics_core()
  # Other strategies: cost_basis is stock purchase + commissions
  cost_basis <- group_detail$cost_basis

  if (is.na(cost_basis) || cost_basis <= 0) {
    log_debug("Capital at Risk: Invalid cost_basis for group, returning 0")
    return(0)
  }

  return(cost_basis)
}

#' Calculate Portfolio Expected Annualized Returns
#'
#' Calculates capital-weighted annualized returns for the entire portfolio,
#' separating open positions (expected) from closed positions (realized).
#' Handles money market funds (SGOV, ZMMK) using SEC yield as expected return.
#'
#' @param include_breakdown Logical, if TRUE returns strategy breakdown (default TRUE)
#' @return List with expected_return, realized_return, strategy_breakdown, exclusions
#' @export
calculate_portfolio_expected_returns <- function(include_breakdown = TRUE) {
  tryCatch({
    log_info("Portfolio Expected Return: Starting calculation")

    # Get dashboard metrics (pre-calculated group-level data)
    dashboard_data <- calculate_dashboard_metrics(status_filter = NULL)

    open_detail <- dashboard_data$open_groups_detail
    closed_detail <- dashboard_data$closed_groups_detail

    log_info("Portfolio Expected Return: Found {nrow(open_detail)} open, {nrow(closed_detail)} closed groups")

    # Initialize result structure
    result <- list(
      expected_return_pct = NA_real_,
      realized_return_pct = NA_real_,
      total_open_capital = 0,
      total_closed_capital = 0,
      strategy_breakdown = tibble::tibble(),
      exclusions = list(
        count = 0,
        capital = 0,
        pct_of_portfolio = 0,
        reasons = character()
      ),
      calculation_timestamp = Sys.time(),
      positions_included = 0,
      positions_excluded = 0
    )

    # =========================================================================
    # PROCESS OPEN POSITIONS (Expected Returns)
    # =========================================================================
    open_results <- process_open_positions(open_detail, include_breakdown)

    result$expected_return_pct <- open_results$weighted_return_pct
    result$total_open_capital <- open_results$total_capital
    result$positions_included <- result$positions_included + open_results$included_count

    # Track exclusions
    if (open_results$excluded_count > 0) {
      result$exclusions$count <- result$exclusions$count + open_results$excluded_count
      result$exclusions$capital <- result$exclusions$capital + open_results$excluded_capital
      result$exclusions$reasons <- c(result$exclusions$reasons, open_results$exclusion_reasons)
      result$positions_excluded <- result$positions_excluded + open_results$excluded_count
    }

    # =========================================================================
    # PROCESS CLOSED POSITIONS (Realized Returns)
    # =========================================================================
    closed_results <- process_closed_positions(closed_detail, include_breakdown)

    result$realized_return_pct <- closed_results$weighted_return_pct
    result$total_closed_capital <- closed_results$total_capital
    result$positions_included <- result$positions_included + closed_results$included_count

    # Track exclusions
    if (closed_results$excluded_count > 0) {
      result$exclusions$count <- result$exclusions$count + closed_results$excluded_count
      result$exclusions$capital <- result$exclusions$capital + closed_results$excluded_capital
      result$exclusions$reasons <- c(result$exclusions$reasons, closed_results$exclusion_reasons)
      result$positions_excluded <- result$positions_excluded + closed_results$excluded_count
    }

    # =========================================================================
    # BUILD STRATEGY BREAKDOWN
    # =========================================================================
    if (include_breakdown) {
      result$strategy_breakdown <- bind_rows(
        open_results$strategy_breakdown,
        closed_results$strategy_breakdown
      ) %>%
        arrange(status, desc(total_capital))
    }

    # Calculate exclusion percentage
    total_capital <- result$total_open_capital + result$total_closed_capital + result$exclusions$capital
    if (total_capital > 0) {
      result$exclusions$pct_of_portfolio <- result$exclusions$capital / total_capital
    }

    # De-duplicate exclusion reasons
    result$exclusions$reasons <- unique(result$exclusions$reasons)

    log_info("Portfolio Expected Return: Calculation complete - Expected={round(result$expected_return_pct, 2)}%, Realized={round(result$realized_return_pct, 2)}%")

    return(result)

  }, error = function(e) {
    log_error("Portfolio Expected Return: Calculation failed - {e$message}")
    return(list(
      expected_return_pct = NA_real_,
      realized_return_pct = NA_real_,
      total_open_capital = 0,
      total_closed_capital = 0,
      strategy_breakdown = tibble::tibble(),
      exclusions = list(
        count = 0,
        capital = 0,
        pct_of_portfolio = 0,
        reasons = c(paste("Calculation error:", e$message))
      ),
      calculation_timestamp = Sys.time(),
      positions_included = 0,
      positions_excluded = 0,
      error = e$message
    ))
  })
}

#' Process open positions for expected return calculation
#'
#' @param open_detail Tibble from calculate_dashboard_metrics open_groups_detail
#' @param include_breakdown Whether to build strategy breakdown
#' @return List with calculation results
#' @noRd
process_open_positions <- function(open_detail, include_breakdown = TRUE) {
  # Initialize result
  result <- list(
    weighted_return_pct = NA_real_,
    total_capital = 0,
    included_count = 0,
    excluded_count = 0,
    excluded_capital = 0,
    exclusion_reasons = character(),
    strategy_breakdown = tibble::tibble()
  )

  if (is.null(open_detail) || nrow(open_detail) == 0) {
    log_debug("Portfolio Expected Return: No open positions to process")
    return(result)
  }

  # Get group members to identify tickers
  group_ids <- open_detail$group_id
  all_members <- get_members_for_groups(group_ids)

  # Process each open position
  processed <- lapply(seq_len(nrow(open_detail)), function(i) {
    row <- open_detail[i, ]
    group_id <- row$group_id
    strategy_type <- row$strategy_type

    # Get ticker from members
    members <- all_members %>% filter(group_id == !!group_id)
    ticker <- extract_ticker_from_members(members)

    # Get capital at risk
    capital <- calculate_capital_at_risk(row, strategy_type, ticker)

    # Get expected return
    # Money market: use yield
    # Other strategies: use projected_annualized_return_pct
    if (!is.null(ticker) && ticker %in% CASH_EQUIVALENT_TICKERS) {
      return_pct <- get_money_market_yield(ticker) * 100  # Convert to percentage
    } else {
      return_pct <- row$projected_annualized_return_pct
    }

    # Map to display category
    display_category <- map_strategy_to_display(strategy_type, ticker)

    # Check for exclusion
    should_exclude <- FALSE
    exclusion_reason <- NULL

    if (is.na(return_pct)) {
      should_exclude <- TRUE
      exclusion_reason <- paste0("No projected return (", strategy_type, ")")
    } else if (capital <= 0) {
      should_exclude <- TRUE
      exclusion_reason <- "Zero or negative capital"
    }

    list(
      group_id = group_id,
      ticker = ticker,
      strategy_type = strategy_type,
      display_category = display_category,
      capital = capital,
      return_pct = return_pct,
      excluded = should_exclude,
      exclusion_reason = exclusion_reason
    )
  })

  # Separate included and excluded
  included <- processed[!sapply(processed, function(x) x$excluded)]
  excluded <- processed[sapply(processed, function(x) x$excluded)]

  result$included_count <- length(included)
  result$excluded_count <- length(excluded)
  result$excluded_capital <- if (length(excluded) > 0) {
    sum(unlist(sapply(excluded, function(x) x$capital)))
  } else {
    0
  }
  result$exclusion_reasons <- unique(unlist(lapply(excluded, function(x) x$exclusion_reason)))

  if (length(included) == 0) {
    log_debug("Portfolio Expected Return: All open positions excluded")
    return(result)
  }

  # Calculate weighted average return
  capitals <- sapply(included, function(x) x$capital)
  returns <- sapply(included, function(x) x$return_pct)

  result$total_capital <- sum(capitals)
  result$weighted_return_pct <- sum(returns * capitals) / sum(capitals)

  # Build strategy breakdown
  if (include_breakdown && length(included) > 0) {
    breakdown_df <- tibble::tibble(
      display_category = sapply(included, function(x) x$display_category),
      capital = sapply(included, function(x) x$capital),
      return_pct = sapply(included, function(x) x$return_pct)
    )

    result$strategy_breakdown <- breakdown_df %>%
      group_by(display_category) %>%
      summarise(
        status = "open",
        position_count = n(),
        total_capital = sum(capital),
        weighted_return_pct = sum(return_pct * capital) / sum(capital),
        .groups = "drop"
      ) %>%
      rename(strategy_type = display_category) %>%
      mutate(pct_of_portfolio = total_capital / result$total_capital)
  }

  return(result)
}

#' Process closed positions for realized return calculation
#'
#' Uses Capital-Days Weighted Return methodology for meaningful annualized returns.
#' Instead of averaging individual annualized returns (which inflates short-term trades),
#' this calculates: (Total Profit / Total Capital-Days) * 365
#' This gives a true measure of "return per dollar-day invested, annualized."
#'
#' @param closed_detail Tibble from calculate_dashboard_metrics closed_groups_detail
#' @param include_breakdown Whether to build strategy breakdown
#' @return List with calculation results
#' @noRd
process_closed_positions <- function(closed_detail, include_breakdown = TRUE) {
  # Initialize result
  result <- list(
    weighted_return_pct = NA_real_,
    total_capital = 0,
    included_count = 0,
    excluded_count = 0,
    excluded_capital = 0,
    exclusion_reasons = character(),
    strategy_breakdown = tibble::tibble()
  )

  if (is.null(closed_detail) || nrow(closed_detail) == 0) {
    log_debug("Portfolio Expected Return: No closed positions to process")
    return(result)
  }

  # Get group members to identify tickers
  group_ids <- closed_detail$group_id
  all_members <- get_members_for_groups(group_ids)

  # Process each closed position
  processed <- lapply(seq_len(nrow(closed_detail)), function(i) {
    row <- closed_detail[i, ]
    group_id <- row$group_id
    strategy_type <- row$strategy_type

    # Get ticker from members
    members <- all_members %>% filter(group_id == !!group_id)
    ticker <- extract_ticker_from_members(members)

    # For closed positions, estimate capital from total_return_amount and total_return_pct
    capital <- if (!is.na(row$total_return_amount) && !is.na(row$total_return_pct) && row$total_return_pct != 0) {
      abs(row$total_return_amount / (row$total_return_pct / 100))
    } else if ("total_cost" %in% names(row) && !is.na(row$total_cost)) {
      abs(row$total_cost)
    } else {
      0
    }

    # Calculate days_held from total_return_pct and annualized_return_pct
    # Formula: annualized = total * (365 / days) => days = total * 365 / annualized
    days_held <- if (!is.na(row$total_return_pct) && !is.na(row$annualized_return_pct) && row$annualized_return_pct != 0) {
      abs(365 * row$total_return_pct / row$annualized_return_pct)
    } else {
      NA_real_
    }

    # Get actual dollar profit (total_return_amount)
    profit_amount <- row$total_return_amount

    # Calculate capital-days (capital * days_held)
    capital_days <- if (!is.na(days_held) && capital > 0) {
      capital * days_held
    } else {
      NA_real_
    }

    # Map to display category
    display_category <- map_strategy_to_display(strategy_type, ticker)

    # Check for exclusion - need valid profit, capital, and days for capital-days calculation
    should_exclude <- FALSE
    exclusion_reason <- NULL

    if (is.na(profit_amount)) {
      should_exclude <- TRUE
      exclusion_reason <- "No realized return amount"
    } else if (capital <= 0) {
      should_exclude <- TRUE
      exclusion_reason <- "Zero or negative capital"
    } else if (is.na(days_held) || days_held <= 0) {
      should_exclude <- TRUE
      exclusion_reason <- "Cannot determine holding period"
    } else if (is.na(capital_days)) {
      should_exclude <- TRUE
      exclusion_reason <- "Cannot calculate capital-days"
    }

    list(
      group_id = group_id,
      ticker = ticker,
      strategy_type = strategy_type,
      display_category = display_category,
      capital = capital,
      days_held = days_held,
      profit_amount = profit_amount,
      capital_days = capital_days,
      excluded = should_exclude,
      exclusion_reason = exclusion_reason
    )
  })

  # Separate included and excluded
  included <- processed[!sapply(processed, function(x) x$excluded)]
  excluded <- processed[sapply(processed, function(x) x$excluded)]

  result$included_count <- length(included)
  result$excluded_count <- length(excluded)
  result$excluded_capital <- if (length(excluded) > 0) {
    sum(unlist(sapply(excluded, function(x) x$capital)))
  } else {
    0
  }
  result$exclusion_reasons <- unique(unlist(lapply(excluded, function(x) x$exclusion_reason)))

  if (length(included) == 0) {
    log_debug("Portfolio Expected Return: All closed positions excluded")
    return(result)
  }

  # Calculate Capital-Days Weighted Annualized Return
  # Formula: (Total Profit / Total Capital-Days) * 365 * 100
  total_profit <- sum(sapply(included, function(x) x$profit_amount))
  total_capital_days <- sum(sapply(included, function(x) x$capital_days))
  capitals <- sapply(included, function(x) x$capital)

  result$total_capital <- sum(capitals)

  if (total_capital_days > 0) {
    daily_return_rate <- total_profit / total_capital_days
    result$weighted_return_pct <- daily_return_rate * 365 * 100
  } else {
    result$weighted_return_pct <- NA_real_
  }

  # Build strategy breakdown using capital-days weighting per strategy
  if (include_breakdown && length(included) > 0) {
    breakdown_df <- tibble::tibble(
      display_category = sapply(included, function(x) x$display_category),
      capital = sapply(included, function(x) x$capital),
      profit_amount = sapply(included, function(x) x$profit_amount),
      capital_days = sapply(included, function(x) x$capital_days)
    )

    result$strategy_breakdown <- breakdown_df %>%
      group_by(display_category) %>%
      summarise(
        status = "closed",
        position_count = n(),
        total_capital = sum(capital),
        # Capital-days weighted annualized return per strategy
        weighted_return_pct = if (sum(capital_days) > 0) {
          (sum(profit_amount) / sum(capital_days)) * 365 * 100
        } else {
          NA_real_
        },
        .groups = "drop"
      ) %>%
      rename(strategy_type = display_category) %>%
      mutate(pct_of_portfolio = total_capital / result$total_capital)
  }

  return(result)
}

#' Extract ticker from group members
#'
#' @param members Tibble of group members
#' @return Ticker string or NULL
#' @noRd
extract_ticker_from_members <- function(members) {
  if (nrow(members) == 0) {
    return(NULL)
  }

  # Try underlying_stock first
  stock <- members %>% filter(role == "underlying_stock")
  if (nrow(stock) > 0) {
    symbol <- stock$symbol[1]
    if (is_option_symbol(symbol)) {
      return(parse_option_symbol(symbol))
    }
    return(symbol)
  }

  # Try cash_equivalent
  cash_eq <- members %>% filter(role == "cash_equivalent")
  if (nrow(cash_eq) > 0) {
    return(cash_eq$symbol[1])
  }

  # Try short_put (CSP)
  put <- members %>% filter(role == "short_put")
  if (nrow(put) > 0) {
    return(parse_option_symbol(put$symbol[1]))
  }

  # Fallback: parse from first member
  return(parse_option_symbol(members$symbol[1]))
}
