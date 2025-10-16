#' Weekly Dividend Capture Analysis Functions
#'
#' Core business logic for analyzing dividend capture opportunities in weekly
#' dividend ETFs. Strategy: buy at close before ex-dividend date, sell at open
#' on ex-dividend date.
#'
#' @name dividend-capture-weekly
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom zoo index
#' @importFrom quantmod Op Cl
#' @importFrom purrr map_dfr compact
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multisession
#' @importFrom logger log_info log_success
#' @importFrom lubridate wday
NULL

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Map ex-dividend day to buy day
#'
#' @param ex_div_day Character day of week for ex-dividend
#' @return Character day of week for buying
#' @noRd
map_ex_div_to_buy_day <- function(ex_div_day) {
  day_map <- get_dividend_capture_config("day_map")
  buy_day <- day_map[[ex_div_day]]

  if (is.null(buy_day)) {
    return("Unknown")
  }

  return(buy_day)
}


################################################################################
# ANALYSIS FUNCTIONS
################################################################################
# Note: analyze_dividend_events() moved to utils_dividend_capture.R for reuse

#' Calculate aggregate statistics from trade results
#'
#' @param trade_results Tibble with individual trade results
#' @param ticker Character ETF ticker
#' @param current_price Numeric current price
#' @param annual_sofr Numeric annual SOFR rate (passed from batch function)
#' @return Tibble row with aggregate statistics
#' @noRd
calculate_weekly_statistics <- function(trade_results, ticker, current_price, annual_sofr) {
  config <- get_dividend_capture_config()

  # Determine most common ex-dividend day
  ex_dividend_day <- get_most_common_value(trade_results$ex_div_day)
  buy_day <- map_ex_div_to_buy_day(ex_dividend_day)

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
  weekly_return <- avg_return / 100
  simple_annual_return <- avg_return * config$weeks_per_year
  compound_annual_return <- calculate_compound_annual_return(
    periodic_return = weekly_return,
    periods_per_year = config$weeks_per_year
  ) * 100  # Convert to percentage

  # Income projections per $10,000
  avg_profit_per_10k <- config$investment_amount * (avg_return / 100)
  annual_income_per_10k <- avg_profit_per_10k * config$weeks_per_year

  # Risk-adjusted returns
  sharpe_ratio <- calculate_sharpe_ratio(
    returns = trade_results$pct_return / 100,  # Convert to decimal
    risk_free_rate = daily_sofr
  )

  annual_sharpe <- calculate_annual_sharpe(
    sharpe_ratio = sharpe_ratio,
    periods_per_year = config$weeks_per_year
  )

  sortino_ratio <- calculate_sortino_ratio(
    returns = trade_results$pct_return / 100,  # Convert to decimal
    risk_free_rate = daily_sofr
  )

  annual_sortino <- calculate_annual_sortino(
    sortino_ratio = sortino_ratio,
    periods_per_year = config$weeks_per_year
  )

  # Date range
  date_range <- paste(
    format(min(trade_results$div_date), "%Y-%m-%d"),
    "to",
    format(max(trade_results$div_date), "%Y-%m-%d")
  )

  # Return tibble row
  tibble(
    ticker = ticker,
    current_price = current_price,
    ex_dividend_day = ex_dividend_day,
    buy_day = buy_day,
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

#' Analyze a single weekly dividend ETF candidate
#'
#' Phase 2 analysis function. Performs detailed backtesting on ETF that passed
#' Phase 1 screening. Fetches full dividend and price history, backtests all
#' historical dividend events, and calculates comprehensive statistics.
#'
#' @param ticker Character ETF ticker symbol
#' @param current_price Numeric current price from screening
#' @param annual_sofr Numeric annual SOFR rate (passed from batch function)
#' @return Tibble row with analysis results, or NULL on error
#' @export
analyze_weekly_etf <- function(ticker, current_price, annual_sofr) {
  config <- get_dividend_capture_config()

  tryCatch({
    log_debug("{ticker}: Analyzing candidate")

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

    # Backtest all historical dividend events
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
    stats <- calculate_weekly_statistics(trade_results, ticker, current_price, annual_sofr)

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

#' Batch analyze all weekly dividend ETFs
#'
#' Analyzes weekly dividend ETFs using efficient two-phase filtering:
#' - Phase 1: Lightweight screening via current quotes (price only)
#' - Phase 2: Heavy backtesting only on qualifying candidates
#'
#' ETF list is fetched dynamically from stockanalysis.com (cached for 24 hours).
#'
#' @param max_workers Number of parallel workers (default from config)
#' @param force_refresh Logical. If TRUE, refreshes ticker list from web
#'
#' @return Tibble with all results sorted by buy day and annual Sortino ratio
#'
#' @export
batch_analyze_weekly_etfs <- function(max_workers = NULL, force_refresh = FALSE) {
  config <- get_dividend_capture_config()

  if (is.null(max_workers)) {
    max_workers <- config$max_workers
  }

  ############################################################################
  # PHASE 1: LIGHTWEIGHT SCREENING
  ############################################################################

  log_info("Starting two-phase analysis of weekly dividend ETFs...")
  log_info("Using {max_workers} parallel workers")

  # Fetch current ETF list dynamically
  etf_list <- fetch_weekly_dividend_tickers(force_refresh = force_refresh)

  if (length(etf_list) == 0) {
    log_warn("Phase 1: No ETFs found")
    return(tibble())
  }

  log_info("Phase 1: Fetching current prices for {length(etf_list)} ETFs...")

  # Setup parallel processing for screening
  oplan <- plan(multisession, workers = max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Capture quote source setting to pass to workers
  quote_source <- get_quote_source()
  log_info("Quote source for this analysis: {toupper(quote_source)}")

  # Fetch current prices in parallel (lightweight screening)
  screening_results <- future_map(etf_list, function(ticker) {
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
        price = price
      )

    }, error = function(e) {
      return(NULL)
    })
  }, .options = furrr_options(seed = TRUE))

  # Combine screening results (remove NULLs)
  candidates <- screening_results %>%
    compact() %>%
    bind_rows()

  log_info("Phase 1 complete: Retrieved data for {nrow(candidates)}/{length(etf_list)} ETFs")

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

    analyze_weekly_etf(
      ticker = row$ticker,
      current_price = row$price,
      annual_sofr = annual_sofr  # Pass SOFR rate to avoid redundant API calls
    )
  }, .options = furrr_options(seed = TRUE))

  # Combine results (remove NULLs from failed backtests)
  results_df <- analysis_results %>%
    compact() %>%
    bind_rows()

  if (nrow(results_df) > 0) {
    results_df <- results_df %>%
      mutate(day_order = get_day_order(buy_day)) %>%
      arrange(day_order, desc(annual_sortino)) %>%
      select(-day_order)
  }

  log_success("Analysis complete: {nrow(results_df)}/{nrow(candidates)} candidates passed backtesting")
  log_success("Total: {nrow(results_df)} ETFs analyzed from {length(etf_list)} screened")

  return(results_df)
}
