#' Monthly Dividend Capture Analysis Functions
#'
#' Core business logic for analyzing dividend capture opportunities in monthly
#' dividend ETFs. Strategy: buy at close before ex-dividend date, sell at open
#' on ex-dividend date.
#'
#' @name dividend-capture-monthly
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom zoo index
#' @importFrom quantmod Op Cl
#' @importFrom purrr map_dfr compact
#' @importFrom furrr future_map2 furrr_options
#' @importFrom future plan multisession
#' @importFrom logger log_info log_success
#' @importFrom lubridate day mday
NULL

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Determine schedule pattern from dividend dates
#'
#' Analyzes ex-dividend dates to determine if they occur at beginning,
#' middle, or end of month.
#'
#' @param div_dates Vector of dividend dates
#' @return Character string describing schedule pattern
#' @noRd
determine_schedule_pattern <- function(div_dates) {
  if (length(div_dates) == 0) {
    return("Unknown")
  }

  # Get day of month for each dividend
  days_of_month <- mday(div_dates)

  # Categorize: 1-10 = Beginning, 11-20 = Mid, 21+ = End
  beginning_count <- sum(days_of_month <= 10)
  mid_count <- sum(days_of_month >= 11 & days_of_month <= 20)
  end_count <- sum(days_of_month >= 21)

  # Determine most common pattern
  max_count <- max(beginning_count, mid_count, end_count)

  if (max_count == 0) {
    return("Unknown")
  }

  if (beginning_count == max_count) {
    return("Beginning of Month")
  } else if (end_count == max_count) {
    return("End of Month")
  } else {
    return("Mid-Month")
  }
}

################################################################################
# ANALYSIS FUNCTIONS
################################################################################
# Note: analyze_dividend_events() is in utils_dividend_capture.R for reuse

#' Calculate aggregate statistics from trade results for monthly strategy
#'
#' @param trade_results Tibble with individual trade results
#' @param ticker Character ETF ticker
#' @param current_price Numeric current price
#' @param schedule_type Character schedule type from config (if known)
#' @param last_dividend_info List with last dividend date and amount
#' @return Tibble row with aggregate statistics
#' @noRd
calculate_monthly_statistics <- function(trade_results, ticker, current_price, schedule_type = NULL, last_dividend_info = NULL, annual_sofr) {
  config <- get_dividend_capture_monthly_config()

  # Determine schedule pattern from actual dividend dates
  observed_schedule <- determine_schedule_pattern(trade_results$div_date)

  # Use provided schedule type if available, otherwise use observed
  final_schedule <- if (!is.null(schedule_type)) schedule_type else observed_schedule

  # Use provided SOFR rate for Sharpe calculations
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

  # Annual projections (monthly frequency)
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

  # Return tibble row
  tibble(
    ticker = ticker,
    current_price = current_price,
    schedule_type = final_schedule,
    observed_schedule = observed_schedule,
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
    sofr_annual = annual_sofr * 100,  # Store as percentage
    # Last dividend fields
    last_ex_date = if (!is.null(last_dividend_info)) as.character(last_dividend_info$last_ex_date) else NA_character_,
    last_dividend_amount = if (!is.null(last_dividend_info)) last_dividend_info$last_dividend_amount else NA_real_,
    dividend_count = if (!is.null(last_dividend_info)) last_dividend_info$dividend_count else 0,
    days_since_last_dividend = if (!is.null(last_dividend_info) && !is.na(last_dividend_info$last_ex_date)) {
      as.numeric(difftime(Sys.Date(), last_dividend_info$last_ex_date, units = "days"))
    } else NA_real_
  )
}

#' Analyze a single monthly dividend ETF
#'
#' Main analysis function for one ETF. Fetches data, analyzes all historical
#' dividend events, and calculates aggregate statistics.
#'
#' @param ticker Character ETF ticker symbol
#' @param schedule_type Optional schedule type from config
#' @return Tibble row with analysis results, or NULL on error
#' @export
analyze_monthly_etf <- function(ticker, schedule_type = NULL, annual_sofr) {
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

  # Extract last dividend payment info
  last_dividend_info <- tryCatch({
    if (length(dividends) > 0) {
      list(
        last_ex_date = as.Date(tail(index(dividends), 1)),
        last_dividend_amount = as.numeric(tail(dividends, 1)),
        dividend_count = length(dividends)
      )
    } else {
      list(
        last_ex_date = NA,
        last_dividend_amount = NA,
        dividend_count = 0
      )
    }
  }, error = function(e) {
    log_warn("{ticker}: Failed to extract last dividend - {e$message}")
    list(
      last_ex_date = NA,
      last_dividend_amount = NA,
      dividend_count = 0
    )
  })

  # Analyze all dividend events (uses shared function)
  trade_results <- analyze_dividend_events(price_history, dividends)

  if (nrow(trade_results) == 0) {
    return(NULL)
  }

  # Check minimum events threshold
  config <- get_dividend_capture_monthly_config()
  if (nrow(trade_results) < config$min_dividend_events) {
    return(NULL)
  }

  # Calculate statistics
  stats <- calculate_monthly_statistics(trade_results, ticker, current_price, schedule_type, last_dividend_info, annual_sofr)

  # Apply quality filters (DRY - uses shared function)
  if (should_filter_dividend_opportunity(
    stats = stats,
    ticker = ticker,
    min_success_rate = config$min_success_rate,
    exclude_negative_returns = config$exclude_negative_returns
  )) {
    return(NULL)  # Filtered out - won't appear in results
  }

  return(stats)
}

#' Batch analyze all monthly dividend ETFs
#'
#' Analyzes all 76 monthly dividend ETFs in parallel and returns combined results.
#'
#' @param max_workers Number of parallel workers (default from config)
#' @return Tibble with all results sorted by annual Sharpe ratio
#' @export
batch_analyze_monthly_etfs <- function(max_workers = NULL) {
  if (is.null(max_workers)) {
    max_workers <- get_dividend_capture_monthly_config("max_workers")
  }

  # Extract vectors before parallel processing
  ticker_list <- MONTHLY_ETFS$ticker
  schedule_list <- MONTHLY_ETFS$schedule_type

  log_info("Analyzing {length(ticker_list)} monthly dividend ETFs...")
  log_info("Using {max_workers} parallel workers")

  # Fetch SOFR rate ONCE (not per ETF - this is the key optimization!)
  annual_sofr <- fetch_sofr_rate()
  log_info("Using SOFR rate: {round(annual_sofr * 100, 2)}%")

  # Setup parallel processing
  oplan <- plan(multisession, workers = max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Process in parallel - iterate over both vectors
  results <- future_map2(
    ticker_list,
    schedule_list,
    function(ticker, schedule) {
      analyze_monthly_etf(ticker, schedule, annual_sofr)
    },
    .options = furrr_options(seed = TRUE)
  )

  # Combine results (remove NULLs)
  results_df <- results %>%
    compact() %>%
    bind_rows()

  if (nrow(results_df) > 0) {
    # Sort by annual Sortino ratio (descending)
    results_df <- results_df %>%
      arrange(desc(annual_sortino))
  }

  log_success("Analysis complete. {nrow(results_df)} ETFs analyzed successfully.")

  return(results_df)
}
