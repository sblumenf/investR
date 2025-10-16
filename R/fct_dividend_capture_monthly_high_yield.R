#' Monthly High-Yield Dividend Capture Analysis Functions
#'
#' Core business logic for analyzing dividend capture opportunities in monthly
#' high-yield dividend ETFs. Strategy: buy at close before ex-dividend date, sell at open
#' on ex-dividend date.
#'
#' @name dividend-capture-monthly-high-yield
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom zoo index
#' @importFrom quantmod Op Cl
#' @importFrom purrr map_dfr compact
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multisession
#' @importFrom logger log_info log_success log_warn
#' @importFrom lubridate days
NULL

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Estimate next ex-dividend date based on historical pattern
#'
#' Calculates average days between dividends and projects next ex-div date
#'
#' @param dividends xts object with dividend amounts and dates
#' @return Date of estimated next ex-dividend, or NA if cannot calculate
#' @noRd
estimate_next_ex_dividend_date <- function(dividends) {
  if (is.null(dividends) || length(dividends) < 2) {
    return(as.Date(NA))
  }

  # Get dividend dates
  div_dates <- index(dividends)

  # Calculate days between consecutive dividends
  date_diffs <- diff(as.numeric(div_dates))

  if (length(date_diffs) == 0) {
    return(as.Date(NA))
  }

  # Use median to be robust against outliers
  median_days_between <- median(date_diffs, na.rm = TRUE)

  # Project next ex-div date from last dividend
  last_div_date <- tail(div_dates, 1)
  next_ex_div <- last_div_date + lubridate::days(median_days_between)

  return(as.Date(next_ex_div))
}

################################################################################
# ANALYSIS FUNCTIONS
################################################################################
# Note: analyze_dividend_events() is reused from utils_dividend_capture.R (DRY)

#' Calculate aggregate statistics from trade results
#'
#' @param trade_results Tibble with individual trade results
#' @param ticker Character ETF ticker
#' @param current_price Numeric current price
#' @param yield_pct Character yield percentage string (e.g., "15.5%")
#' @param next_ex_div_date Date estimated next ex-dividend date
#' @param annual_sofr Numeric annual SOFR rate (passed from batch function)
#' @return Tibble row with aggregate statistics
#' @noRd
calculate_monthly_high_yield_statistics <- function(trade_results, ticker, current_price, yield_pct, next_ex_div_date, annual_sofr) {
  config <- get_monthly_high_yield_config()

  # Use SOFR rate passed from batch function (avoids redundant API calls)
  daily_sofr <- annual_sofr / config$trading_days_per_year
  risk_free_per_trade <- daily_sofr * 100  # Convert to percentage

  # Basic statistics
  total_events <- nrow(trade_results)
  profitable_trades <- sum(trade_results$profitable)
  losing_trades <- sum(!trade_results$profitable)
  success_rate <- mean(trade_results$profitable) * 100

  # Return statistics
  avg_return <- mean(trade_results$pct_return)
  median_return <- median(trade_results$pct_return)
  std_deviation <- sd(trade_results$pct_return)
  best_return <- max(trade_results$pct_return)
  worst_return <- min(trade_results$pct_return)

  # Dividend statistics
  avg_dividend <- mean(trade_results$dividend)
  avg_overnight_move <- mean(trade_results$overnight_move)
  drop_ratio <- if (avg_dividend != 0) {
    (avg_overnight_move / avg_dividend) * 100
  } else {
    0
  }

  # Recent performance (last 10 trades)
  if (total_events >= 10) {
    recent <- tail(trade_results, 10)
    recent_success_rate <- mean(recent$profitable) * 100
    recent_avg_return <- mean(recent$pct_return)
  } else {
    recent_success_rate <- success_rate
    recent_avg_return <- avg_return
  }

  # Annual projections
  monthly_return <- avg_return / 100
  simple_annual_return <- avg_return * config$months_per_year
  compound_annual_return <- calculate_compound_annual_return(
    periodic_return = monthly_return,
    periods_per_year = config$months_per_year
  ) * 100  # Convert to percentage

  # Income projections per $10,000
  avg_profit_per_10k <- config$investment_amount * (avg_return / 100)
  annual_income_per_10k <- avg_profit_per_10k * config$months_per_year

  # Risk-adjusted returns
  sharpe_ratio <- calculate_sharpe_ratio(
    returns = trade_results$pct_return / 100,  # Convert to decimal
    risk_free_rate = daily_sofr
  )

  annual_sharpe <- calculate_annual_sharpe(
    sharpe_ratio = sharpe_ratio,
    periods_per_year = config$months_per_year
  )

  sortino_ratio <- calculate_sortino_ratio(
    returns = trade_results$pct_return / 100,  # Convert to decimal
    risk_free_rate = daily_sofr
  )

  annual_sortino <- calculate_annual_sortino(
    sortino_ratio = sortino_ratio,
    periods_per_year = config$months_per_year
  )

  # Date range
  date_range <- paste(
    format(min(trade_results$div_date), "%Y-%m-%d"),
    "to",
    format(max(trade_results$div_date), "%Y-%m-%d")
  )

  # Days until next ex-dividend
  days_until_next_ex_div <- if (!is.na(next_ex_div_date)) {
    as.numeric(next_ex_div_date - Sys.Date())
  } else {
    NA_real_
  }

  # Return tibble row
  tibble(
    ticker = ticker,
    current_price = current_price,
    yield_pct = yield_pct,
    next_ex_div_date = next_ex_div_date,
    days_until_next_ex_div = days_until_next_ex_div,
    total_events = total_events,
    date_range = date_range,
    success_rate = success_rate,
    profitable_trades = profitable_trades,
    losing_trades = losing_trades,
    avg_return = avg_return,
    median_return = median_return,
    std_deviation = std_deviation,
    best_return = best_return,
    worst_return = worst_return,
    avg_dividend = avg_dividend,
    avg_overnight_move = avg_overnight_move,
    drop_ratio = drop_ratio,
    recent_success_rate = recent_success_rate,
    recent_avg_return = recent_avg_return,
    simple_annual_return = simple_annual_return,
    compound_annual_return = compound_annual_return,
    avg_profit_per_10k = avg_profit_per_10k,
    annual_income_per_10k = annual_income_per_10k,
    sharpe_ratio = sharpe_ratio,
    annual_sharpe = annual_sharpe,
    sortino_ratio = sortino_ratio,
    annual_sortino = annual_sortino,
    sofr_annual = annual_sofr * 100  # Store as percentage
  )
}

#' Analyze a single monthly high-yield dividend ETF candidate
#'
#' Phase 2 analysis function. Performs detailed backtesting on ETF that passed
#' Phase 1 screening. Fetches full dividend and price history, backtests all
#' historical dividend events, and calculates comprehensive statistics.
#'
#' @param ticker Character ETF ticker symbol
#' @param current_price Numeric current price from screening
#' @param yield_pct Character yield percentage string from screening
#' @param annual_sofr Numeric annual SOFR rate (passed from batch function)
#' @return Tibble row with analysis results, or NULL on error
#' @export
analyze_monthly_high_yield_etf <- function(ticker, current_price, yield_pct, annual_sofr) {
  config <- get_monthly_high_yield_config()

  tryCatch({
    log_debug("{ticker}: Analyzing candidate (Yield: {yield_pct})")

    # Fetch dividend history for backtesting
    dividends <- fetch_dividend_history(
      ticker = ticker,
      from = "1900-01-01"
    )

    if (is.null(dividends) || length(dividends) == 0) {
      log_warn("{ticker}: No dividend history for backtesting")
      return(NULL)
    }

    # Check minimum events threshold
    if (length(dividends) < config$min_dividend_events) {
      log_debug("{ticker}: Insufficient dividend events ({length(dividends)} < {config$min_dividend_events})")
      return(NULL)
    }

    # Fetch price history for backtesting
    price_history <- fetch_price_history(
      ticker = ticker,
      from = "1900-01-01",
      auto_adjust = FALSE
    )

    if (is.null(price_history) || nrow(price_history) == 0) {
      log_warn("{ticker}: No price history for backtesting")
      return(NULL)
    }

    # Estimate next ex-dividend date
    next_ex_div_date <- estimate_next_ex_dividend_date(dividends)

    # Backtest all historical dividend events (reuse from utils_dividend_capture.R - DRY)
    trade_results <- analyze_dividend_events(price_history, dividends)

    if (nrow(trade_results) == 0) {
      log_warn("{ticker}: Backtesting produced no results")
      return(NULL)
    }

    # Final check: minimum events threshold
    if (nrow(trade_results) < config$min_dividend_events) {
      log_debug("{ticker}: Insufficient backtest results ({nrow(trade_results)} < {config$min_dividend_events})")
      return(NULL)
    }

    # Calculate comprehensive statistics
    stats <- calculate_monthly_high_yield_statistics(
      trade_results,
      ticker,
      current_price,
      yield_pct,
      next_ex_div_date,
      annual_sofr
    )

    # Apply quality filters (DRY - uses shared function)
    if (should_filter_dividend_opportunity(
      stats = stats,
      ticker = ticker,
      min_success_rate = config$min_success_rate,
      exclude_negative_returns = config$exclude_negative_returns
    )) {
      return(NULL)  # Filtered out - won't appear in results
    }

    log_success("{ticker}: Analysis complete - {nrow(trade_results)} historical events backtested")
    return(stats)

  }, error = function(e) {
    log_warn("{ticker}: Analysis failed - {e$message}")
    return(NULL)
  })
}

#' Batch analyze all monthly high-yield dividend ETFs
#'
#' Analyzes monthly high-yield dividend ETFs using efficient two-phase filtering:
#' - Phase 1: Lightweight screening via web scraping (yield data from stockanalysis.com)
#' - Phase 2: Heavy backtesting only on qualifying candidates
#'
#' ETF list is fetched dynamically from stockanalysis.com (cached for 24 hours).
#'
#' @param max_workers Number of parallel workers (default from config)
#' @param force_refresh Logical. If TRUE, refreshes ticker list from web
#' @param min_yield Numeric. Minimum yield percentage filter (default from config: 8.0)
#'
#' @return Tibble with all results sorted by next ex-div date and annual Sortino ratio
#'
#' @export
batch_analyze_monthly_high_yield_etfs <- function(max_workers = NULL, force_refresh = FALSE, min_yield = NULL) {
  config <- get_monthly_high_yield_config()

  if (is.null(max_workers)) {
    max_workers <- config$max_workers
  }

  if (is.null(min_yield)) {
    min_yield <- config$min_yield_pct
  }

  ############################################################################
  # PHASE 1: LIGHTWEIGHT SCREENING
  ############################################################################

  log_info("Starting two-phase analysis of monthly high-yield dividend ETFs...")
  log_info("Phase 1: Screening for yield >= {min_yield}%")
  log_info("Using {max_workers} parallel workers")

  # Fetch ETF list with yield data from stockanalysis.com (already filtered by yield)
  etf_data <- fetch_monthly_high_yield_etfs(force_refresh = force_refresh, min_yield = min_yield)

  if (nrow(etf_data) == 0) {
    log_warn("Phase 1: No ETFs found with yield >= {min_yield}%")
    return(tibble())
  }

  log_info("Phase 1: Fetching current prices for {nrow(etf_data)} ETFs...")

  # Setup parallel processing for screening
  oplan <- plan(multisession, workers = max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Capture quote source setting to pass to workers
  quote_source <- get_quote_source()
  log_info("Quote source for this analysis: {toupper(quote_source)}")

  # Fetch current prices in parallel (lightweight screening)
  screening_results <- future_map(seq_len(nrow(etf_data)), function(i) {
    row <- etf_data[i, ]
    ticker <- row$ticker

    # Set quote source in this worker to match main process
    options(investR.quote_source = quote_source)

    tryCatch({
      # Fetch current quote (lightweight - just price)
      quote <- fetch_current_quote(ticker, fields = c("Last Trade (Price Only)"))

      if (is.null(quote)) {
        return(NULL)
      }

      # Extract price
      price <- as.numeric(quote[[2]])

      if (is.na(price) || price <= 0) {
        return(NULL)
      }

      # Return screening data
      tibble(
        ticker = ticker,
        price = price,
        yield_pct = row$yield_pct,
        yield_numeric = row$yield_numeric
      )

    }, error = function(e) {
      return(NULL)
    })
  }, .options = furrr_options(seed = TRUE))

  # Combine screening results (remove NULLs)
  candidates <- screening_results %>%
    compact() %>%
    bind_rows()

  log_info("Phase 1 complete: Retrieved data for {nrow(candidates)}/{nrow(etf_data)} ETFs")

  if (nrow(candidates) == 0) {
    log_warn("Phase 1: No ETFs passed screening")
    return(tibble())
  }

  log_success("Phase 1: Found {nrow(candidates)} qualifying candidates")

  ############################################################################
  # PHASE 2: DETAILED BACKTESTING
  ############################################################################

  log_info("Phase 2: Backtesting {nrow(candidates)} candidates...")

  # Fetch SOFR rate ONCE (not per ETF - this is the key optimization!)
  annual_sofr <- fetch_sofr_rate()
  log_info("Using SOFR rate: {round(annual_sofr * 100, 2)}%")

  # Analyze candidates in parallel with full backtesting
  analysis_results <- future_map(seq_len(nrow(candidates)), function(i) {
    row <- candidates[i, ]

    log_info("Analyzing {row$ticker} ({i}/{nrow(candidates)})")

    analyze_monthly_high_yield_etf(
      ticker = row$ticker,
      current_price = row$price,
      yield_pct = row$yield_pct,
      annual_sofr = annual_sofr  # Pass SOFR rate to avoid redundant API calls
    )
  }, .options = furrr_options(seed = TRUE))

  # Combine results (remove NULLs from failed backtests)
  results_df <- analysis_results %>%
    compact() %>%
    bind_rows()

  if (nrow(results_df) > 0) {
    # Remove opportunities with past ex-dividend dates (not actionable)
    results_df <- results_df %>%
      filter(!is.na(days_until_next_ex_div), days_until_next_ex_div > 0)

    # Sort by days until next ex-div (ascending), then annual Sortino (descending)
    results_df <- results_df %>%
      arrange(days_until_next_ex_div, desc(annual_sortino))
  }

  log_success("Analysis complete: {nrow(results_df)}/{nrow(candidates)} candidates passed backtesting")
  log_success("Total: {nrow(results_df)} ETFs analyzed from {nrow(etf_data)} screened")

  return(results_df)
}
