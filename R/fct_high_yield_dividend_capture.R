#' High-Yield Dividend Capture Analysis Functions
#'
#' Core business logic for analyzing high-yield dividend capture opportunities
#' across customizable stock universes. Strategy: buy at close before ex-dividend
#' date, sell at open on ex-dividend date, focusing on stocks with high yield
#' (adjustable threshold) and announced ex-dividend dates in next 1-2 business days.
#'
#' Implementation uses efficient two-phase filtering:
#' - Phase 1: Lightweight screening via Yahoo quotes (price, yield, ex-div date)
#' - Phase 2: Heavy backtesting only on qualifying candidates
#'
#' This is a universe-agnostic implementation. Universe-specific configurations
#' (ticker sources, defaults) are provided via separate config objects.
#'
#' @name high-yield-dividend-capture
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom zoo index
#' @importFrom quantmod Op Cl yahooQF getQuote
#' @importFrom purrr map_dfr compact
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multisession
#' @importFrom logger log_info log_success log_warn log_debug
#' @importFrom lubridate wday with_tz now
NULL

################################################################################
# DATA FETCHING
################################################################################

################################################################################
# PHASE 1: SCREENING & FILTERING
################################################################################

#' Calculate business days between two dates
#'
#' Counts weekdays (Monday-Friday) between two dates, excluding weekends.
#' Does not account for market holidays.
#'
#' @param from_date Start date
#' @param to_date End date
#' @return Integer number of business days
#' @noRd
calculate_business_days <- function(from_date, to_date) {
  if (is.na(from_date) || is.na(to_date)) {
    return(NA_integer_)
  }

  if (to_date <= from_date) {
    return(0L)
  }

  # Get sequence of dates
  date_seq <- seq(from_date + 1, to_date, by = "day")

  # Count weekdays (1=Monday, 7=Sunday)
  weekdays <- wday(date_seq)
  business_days <- sum(weekdays >= 2 & weekdays <= 6)

  return(as.integer(business_days))
}

#' Filter stocks by yield and ex-dividend date criteria
#'
#' Applies high-yield and imminent ex-dividend date filters to screening data.
#' Uses business days (weekdays) for ex-dividend date filtering.
#'
#' @param screening_data Tibble with columns: ticker, price, yield_pct, ex_div_date
#' @param min_yield Numeric minimum yield threshold (percentage)
#' @param max_days_until_ex_div Numeric maximum business days until ex-dividend (default 2)
#' @return Tibble with qualifying stocks
#' @noRd
filter_by_yield_and_exdiv <- function(screening_data,
                                       min_yield,
                                       max_days_until_ex_div = 2) {

  if (nrow(screening_data) == 0) {
    return(tibble())
  }

  today <- Sys.Date()

  # Calculate business days for each stock
  screening_data %>%
    mutate(
      business_days_until = sapply(ex_div_date, function(ex_date) {
        calculate_business_days(today, ex_date)
      })
    ) %>%
    filter(
      # Yield filter
      !is.na(yield_pct),
      yield_pct >= min_yield,

      # Ex-dividend date filter (using business days)
      !is.na(ex_div_date),
      ex_div_date > today,  # Must be future date
      !is.na(business_days_until),
      business_days_until >= 1,  # At least 1 business day away
      business_days_until <= max_days_until_ex_div  # Within business day window
    ) %>%
    mutate(
      days_until_ex_div = business_days_until
    ) %>%
    select(-business_days_until)
}

################################################################################
# PHASE 2: DETAILED ANALYSIS
################################################################################

#' Calculate aggregate statistics for high-yield stock
#'
#' @param trade_results Tibble with individual trade results
#' @param ticker Character stock ticker
#' @param current_price Numeric current price
#' @param current_yield Numeric current annualized yield
#' @param ex_div_date Date of next ex-dividend date
#' @param days_until_ex_div Numeric days until ex-dividend
#' @param universe_config Universe configuration object
#' @return Tibble row with aggregate statistics
#' @noRd
calculate_high_yield_statistics <- function(trade_results,
                                            ticker,
                                            current_price,
                                            current_yield,
                                            ex_div_date,
                                            days_until_ex_div,
                                            universe_config,
                                            annual_sofr) {

  # Use provided SOFR rate for Sharpe calculations
  daily_sofr <- annual_sofr / universe_config$trading_days_per_year

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

  # Determine dividend frequency
  intervals <- as.numeric(diff(trade_results$div_date))
  avg_interval <- mean(intervals, na.rm = TRUE)

  # Estimate payments per year
  if (!is.na(avg_interval) && avg_interval > 0) {
    payments_per_year <- 365 / avg_interval
  } else {
    payments_per_year <- 12  # Default to monthly
  }

  # Annual projections
  periodic_return <- avg_return / 100
  simple_annual_return <- avg_return * payments_per_year
  compound_annual_return <- calculate_compound_annual_return(
    periodic_return = periodic_return,
    periods_per_year = payments_per_year
  ) * 100  # Convert to percentage

  # Income projections per $10,000
  avg_profit_per_10k <- (avg_return / 100) * universe_config$investment_amount
  annual_income_per_10k <- avg_profit_per_10k * payments_per_year

  # Risk-adjusted returns (Sharpe and Sortino ratios)
  excess_returns <- trade_results$pct_return - (daily_sofr * 100)
  sharpe_ratio <- if (std_deviation != 0) {
    mean(excess_returns) / std_deviation
  } else {
    0
  }
  annual_sharpe <- sharpe_ratio * sqrt(payments_per_year)

  downside_returns <- excess_returns[excess_returns < 0]
  downside_deviation <- if (length(downside_returns) > 0) {
    sqrt(mean(downside_returns^2))
  } else {
    std_deviation
  }

  sortino_ratio <- if (downside_deviation != 0) {
    mean(excess_returns) / downside_deviation
  } else {
    0
  }
  annual_sortino <- sortino_ratio * sqrt(payments_per_year)

  # Date range
  date_range <- sprintf(
    "%s to %s",
    format(min(trade_results$div_date), "%Y-%m-%d"),
    format(max(trade_results$div_date), "%Y-%m-%d")
  )

  # Return tibble row
  tibble(
    ticker = ticker,
    current_price = current_price,
    current_yield = current_yield,
    predicted_ex_date = ex_div_date,
    days_until_ex_div = days_until_ex_div,
    prediction_confidence = "Announced",  # From Yahoo, not predicted
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
    payments_per_year = payments_per_year,
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

#' Analyze a single high-yield candidate stock
#'
#' Performs detailed backtesting analysis on a stock that passed screening filters.
#' Fetches full dividend and price history, backtests all historical dividend events,
#' and calculates comprehensive statistics.
#'
#' @param ticker Character stock ticker symbol
#' @param current_price Numeric current price from screening
#' @param current_yield Numeric current yield from screening (percentage)
#' @param ex_div_date Date next ex-dividend date from screening
#' @param days_until_ex_div Numeric days until ex-dividend from screening
#' @param universe_config Universe configuration object
#' @return Tibble row with analysis results, or NULL on error
#' @export
analyze_high_yield_candidate <- function(ticker,
                                         current_price,
                                         current_yield,
                                         ex_div_date,
                                         days_until_ex_div,
                                         universe_config,
                                         annual_sofr) {
  tryCatch({

    log_debug("{ticker}: Analyzing candidate (Yield: {round(current_yield, 2)}%, Ex-div: {ex_div_date})")

    # Fetch full dividend history for backtesting
    dividends <- fetch_dividend_history(
      ticker = ticker,
      from = "1900-01-01"
    )

    if (is.null(dividends) || length(dividends) == 0) {
      log_warn("{ticker}: No dividend history for backtesting")
      return(NULL)
    }

    # Check minimum events threshold
    if (length(dividends) < universe_config$min_dividend_events) {
      log_debug("{ticker}: Insufficient dividend events for backtesting ({length(dividends)} < {universe_config$min_dividend_events})")
      return(NULL)
    }

    # Fetch full price history for backtesting
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
    if (nrow(trade_results) < universe_config$min_dividend_events) {
      log_debug("{ticker}: Insufficient backtest results ({nrow(trade_results)} < {universe_config$min_dividend_events})")
      return(NULL)
    }

    # Calculate comprehensive statistics
    stats <- calculate_high_yield_statistics(
      trade_results,
      ticker,
      current_price,
      current_yield,
      ex_div_date,
      days_until_ex_div,
      universe_config,
      annual_sofr
    )

    # Apply quality filters (DRY - uses shared function)
    if (should_filter_dividend_opportunity(
      stats = stats,
      ticker = ticker,
      min_success_rate = universe_config$min_success_rate,
      exclude_negative_returns = universe_config$exclude_negative_returns
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

################################################################################
# BATCH PROCESSING
################################################################################

#' Batch analyze high-yield stocks
#'
#' Analyzes stocks for high-yield dividend capture opportunities using efficient
#' two-phase filtering approach. Universe-agnostic function that accepts a
#' configuration object specifying the stock universe and parameters.
#'
#' Phase 1: Lightweight screening of all stocks via Yahoo quotes
#' Phase 2: Detailed backtesting only on qualifying candidates
#'
#' @param universe_config List configuration object from get_{universe}_high_yield_config()
#' @param min_yield Numeric minimum yield threshold (NULL = use config default)
#' @param max_workers Number of parallel workers (NULL = use config default)
#' @param stock_limit Numeric maximum stocks to analyze (NULL = analyze all, for testing only)
#' @return Tibble with all results sorted by annual Sortino ratio
#' @export
batch_analyze_high_yield_stocks <- function(universe_config,
                                            min_yield = NULL,
                                            max_workers = NULL,
                                            stock_limit = NULL) {

  # Use config defaults if not specified
  if (is.null(min_yield)) {
    min_yield <- universe_config$default_yield_threshold
  }

  if (is.null(max_workers)) {
    max_workers <- universe_config$max_workers
  }

  # Fetch stock universe using config's ticker fetcher
  stock_list <- universe_config$ticker_fetcher()

  if (length(stock_list) == 0) {
    log_warn("No tickers fetched from {universe_config$universe_name}")
    return(tibble())
  }

  # Apply stock limit if specified (for testing)
  if (!is.null(stock_limit) && stock_limit < length(stock_list)) {
    stock_list <- head(stock_list, stock_limit)
    log_info("Limited to {stock_limit} stocks for analysis (testing mode)")
  }

  log_info("Starting two-phase analysis of {length(stock_list)} {universe_config$universe_name} stocks...")
  log_info("Phase 1: Screening for yield >= {min_yield}% and ex-div dates in next {universe_config$max_days_until_ex_div} business days")
  log_info("Using {max_workers} parallel workers")

  ############################################################################
  # PHASE 1: LIGHTWEIGHT SCREENING
  ############################################################################

  log_info("Phase 1: Fetching quotes for {length(stock_list)} stocks...")

  # Setup parallel processing for screening
  oplan <- plan(multisession, workers = max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Capture quote source setting to pass to workers
  quote_source <- get_quote_source()
  log_info("Quote source for this analysis: {toupper(quote_source)}")

  # Fetch screening data in parallel
  screening_results <- future_map(stock_list, function(ticker) {
    # Set quote source in this worker to match main process
    options(investR.quote_source = quote_source)

    tryCatch({
      # Fetch quote with price, yield, and ex-dividend date
      quote <- fetch_current_quote(
        ticker,
        fields = c("Last Trade (Price Only)", "Dividend Yield", "Ex-Dividend Date")
      )

      if (is.null(quote)) {
        return(NULL)
      }

      # Extract fields (column positions depend on yahooQF output)
      price <- as.numeric(quote[[2]])
      yield_decimal <- as.numeric(quote[[3]])
      ex_div_date <- as.Date(quote[[4]])

      # Validate data
      if (is.na(price) || is.na(yield_decimal)) {
        return(NULL)
      }

      # Convert yield to percentage
      yield_pct <- yield_decimal * 100

      # Return tibble row
      tibble(
        ticker = ticker,
        price = price,
        yield_pct = yield_pct,
        ex_div_date = ex_div_date
      )

    }, error = function(e) {
      return(NULL)
    })
  }, .options = furrr_options(seed = TRUE))

  # Combine results (remove NULLs and non-dataframe failures)
  screening_data <- screening_results %>%
    compact() %>%
    keep(~ is.data.frame(.x)) %>%
    bind_rows()

  log_info("Phase 1 complete: Retrieved data for {nrow(screening_data)}/{length(stock_list)} stocks")

  # Filter by yield and ex-dividend date
  candidates <- filter_by_yield_and_exdiv(
    screening_data,
    min_yield = min_yield,
    max_days_until_ex_div = universe_config$max_days_until_ex_div
  )

  if (nrow(candidates) == 0) {
    log_warn("Phase 1: No stocks meet criteria (yield >= {min_yield}%, ex-div in {universe_config$max_days_until_ex_div} business days)")
    log_info("Try lowering yield threshold")
    return(tibble())
  }

  log_success("Phase 1: Found {nrow(candidates)} qualifying candidates ({round(nrow(candidates)/length(stock_list)*100, 2)}%)")
  log_info("Candidates: {paste(candidates$ticker, collapse=', ')}")

  ############################################################################
  # PHASE 2: DETAILED BACKTESTING
  ############################################################################

  log_info("Phase 2: Backtesting {nrow(candidates)} candidates...")

  # Fetch SOFR rate ONCE (not per stock - this is the key optimization!)
  annual_sofr <- fetch_sofr_rate()
  log_info("Using SOFR rate: {round(annual_sofr * 100, 2)}%")

  # Analyze candidates in parallel
  # Note: quote_source already captured above, reuse it
  analysis_results <- future_map(seq_len(nrow(candidates)), function(i) {
    # Set quote source in this worker to match main process
    options(investR.quote_source = quote_source)

    row <- candidates[i, ]

    log_info("Analyzing {row$ticker} ({i}/{nrow(candidates)})")

    analyze_high_yield_candidate(
      ticker = row$ticker,
      current_price = row$price,
      current_yield = row$yield_pct,
      ex_div_date = row$ex_div_date,
      days_until_ex_div = row$days_until_ex_div,
      universe_config = universe_config,
      annual_sofr = annual_sofr
    )
  }, .options = furrr_options(seed = TRUE))

  # Combine results (remove NULLs and non-dataframe failures)
  results_df <- analysis_results %>%
    compact() %>%
    keep(~ is.data.frame(.x)) %>%
    bind_rows()

  if (nrow(results_df) > 0) {
    # Sort by annual Sortino ratio (descending)
    results_df <- results_df %>%
      arrange(desc(annual_sortino))
  }

  log_success("Analysis complete: {nrow(results_df)}/{nrow(candidates)} candidates passed backtesting")
  log_success("Total: {nrow(results_df)} opportunities found from {length(stock_list)} stocks analyzed")

  return(results_df)
}
