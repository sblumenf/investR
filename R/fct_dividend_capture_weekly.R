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
#' @return Tibble row with aggregate statistics
#' @noRd
calculate_weekly_statistics <- function(trade_results, ticker, current_price) {
  config <- get_dividend_capture_config()

  # Determine most common ex-dividend day
  ex_dividend_day <- get_most_common_value(trade_results$ex_div_day)
  buy_day <- map_ex_div_to_buy_day(ex_dividend_day)

  # Get SOFR rate for Sharpe calculations
  annual_sofr <- fetch_sofr_rate()
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

#' Analyze a single weekly dividend ETF
#'
#' Main analysis function for one ETF. Fetches data, analyzes all historical
#' dividend events, and calculates aggregate statistics.
#'
#' @param ticker Character ETF ticker symbol
#' @return Tibble row with analysis results, or NULL on error
#' @export
analyze_weekly_etf <- function(ticker) {
  # Fetch price history
  price_history <- fetch_price_history(
    ticker = ticker,
    from = "1900-01-01",
    auto_adjust = FALSE
  )

  if (is.null(price_history) || nrow(price_history) == 0) {
    return(NULL)
  }

  # Fetch dividend history
  dividends <- fetch_dividend_history(
    ticker = ticker,
    from = "1900-01-01"
  )

  if (is.null(dividends) || length(dividends) == 0) {
    return(NULL)
  }

  # Get current price
  current_price <- as.numeric(tail(Cl(price_history), 1))

  if (is.na(current_price) || current_price <= 0) {
    return(NULL)
  }

  # Analyze all dividend events
  trade_results <- analyze_dividend_events(price_history, dividends)

  if (nrow(trade_results) == 0) {
    return(NULL)
  }

  # Check minimum events threshold
  config <- get_dividend_capture_config()
  if (nrow(trade_results) < config$min_dividend_events) {
    return(NULL)
  }

  # Calculate statistics
  stats <- calculate_weekly_statistics(trade_results, ticker, current_price)

  return(stats)
}

#' Batch analyze all weekly dividend ETFs
#'
#' Analyzes all 32 weekly dividend ETFs in parallel and returns combined results.
#'
#' @param max_workers Number of parallel workers (default from config)
#' @return Tibble with all results sorted by success rate
#' @export
batch_analyze_weekly_etfs <- function(max_workers = NULL) {
  if (is.null(max_workers)) {
    max_workers <- get_dividend_capture_config("max_workers")
  }

  # Get ETF list
  etf_list <- WEEKLY_ETFS$ticker

  log_info("Analyzing {length(etf_list)} weekly dividend ETFs...")
  log_info("Using {max_workers} parallel workers")

  # Setup parallel processing
  oplan <- plan(multisession, workers = max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Process in parallel
  results <- future_map(etf_list, function(ticker) {
    analyze_weekly_etf(ticker)
  }, .options = furrr_options(seed = TRUE))

  # Combine results (remove NULLs)
  results_df <- results %>%
    compact() %>%
    bind_rows()

  if (nrow(results_df) > 0) {
    results_df <- results_df %>%
      mutate(day_order = get_day_order(buy_day)) %>%
      arrange(day_order, desc(annual_sortino)) %>%
      select(-day_order)
  }

  log_success("Analysis complete. {nrow(results_df)} ETFs analyzed successfully.")

  return(results_df)
}
