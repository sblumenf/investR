#' Put Calendar Spread Analysis Functions
#'
#' Core business logic for analyzing put calendar spread opportunities on
#' dividend aristocrats and other high-quality stocks.
#'
#' Strategy: Sell near-term put (front month), buy longer-dated put (back month)
#' at the same strike price. Profits from differential time decay (theta).
#'
#' @name put-calendar-spread
#' @import dplyr
#' @importFrom purrr map_dfr compact possibly
#' @importFrom tibble tibble
#' @importFrom logger log_info log_warn log_success log_debug log_error
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multisession
NULL

################################################################################
# CALENDAR SPREAD METRICS CALCULATION FUNCTIONS
################################################################################

#' Calculate net debit for calendar spread
#'
#' The net debit is the cost to enter the position:
#' Buy long-dated put (pay ask) - Sell short-dated put (receive bid)
#'
#' @param short_bid Bid price of short-dated put (premium received)
#' @param long_ask Ask price of long-dated put (premium paid)
#' @return Net debit per share (positive = cost to enter)
#' @noRd
calculate_calendar_net_debit <- function(short_bid, long_ask) {
  if (!is.numeric(short_bid) || short_bid < 0) {
    stop("short_bid must be non-negative")
  }
  if (!is.numeric(long_ask) || long_ask < 0) {
    stop("long_ask must be non-negative")
  }

  # Net debit = what we pay - what we receive
  # Positive value means we pay to enter (normal for calendar spreads)
  net_debit <- long_ask - short_bid

  net_debit
}

#' Calculate calendar spread return metrics
#'
#' Calculates return on investment and annualized return based on
#' the net debit and estimated maximum profit.
#'
#' @param net_debit Net debit paid to enter position
#' @param estimated_max_profit Estimated maximum profit
#' @param days_to_front_expiry Days until front-month expiration
#' @return List with roi, annualized_return, and profit targets
#' @noRd
calculate_calendar_return_metrics <- function(net_debit, estimated_max_profit,
                                              days_to_front_expiry) {
  if (!is.numeric(net_debit) || net_debit <= 0) {
    stop("net_debit must be positive for a calendar spread")
  }
  if (!is.numeric(days_to_front_expiry) || days_to_front_expiry <= 0) {
    stop("days_to_front_expiry must be positive")
  }

  # ROI = max_profit / net_debit
  roi <- estimated_max_profit / net_debit

  # Annualize based on front month expiry (when position is typically closed)
  annualized_return <- calculate_annualized_return(
    total_return = roi,
    days = days_to_front_expiry,
    days_per_year = PUT_CALENDAR_SPREAD_CONFIG$days_per_year
  )

  # Profit targets (% of debit)
  profit_target_15pct <- net_debit * PUT_CALENDAR_SPREAD_CONFIG$profit_target_conservative
profit_target_20pct <- net_debit * PUT_CALENDAR_SPREAD_CONFIG$profit_target_standard
  profit_target_25pct <- net_debit * PUT_CALENDAR_SPREAD_CONFIG$profit_target_aggressive

  list(
    roi = roi,
    annualized_return = annualized_return,
    profit_target_15pct = profit_target_15pct,
    profit_target_20pct = profit_target_20pct,
    profit_target_25pct = profit_target_25pct
  )
}

#' Calculate all calendar spread metrics (orchestrator)
#'
#' Comprehensive metric calculation for put calendar spreads.
#' Combines both legs' data into a single result row.
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @param short_put Short-dated put option (tibble row)
#' @param long_put Long-dated put option (tibble row)
#' @param stock_data Stock data list from get_stock_data()
#' @return Tibble row with all calculated metrics
#' @noRd
calculate_calendar_spread_metrics <- function(ticker, current_price, short_put,
                                              long_put, stock_data) {
  # Validate inputs
  validate_ticker(ticker)
  validate_price(current_price, "current_price")

  # Extract option data
  strike <- short_put$Strike  # Same strike for both legs
  short_bid <- short_put$Bid
  short_days <- short_put$days_to_expiry
  short_expiration <- short_put$expiration

  # For long put, use Ask if available, otherwise estimate from Bid
  long_ask <- if ("Ask" %in% names(long_put) && !is.na(long_put$Ask)) {
    long_put$Ask
  } else {
    # Estimate ask as bid + typical spread (5-10%)
    long_put$Bid * 1.05
  }
  long_days <- long_put$days_to_expiry
  long_expiration <- long_put$expiration

  # Calculate net debit (per share)
  net_debit_per_share <- calculate_calendar_net_debit(short_bid, long_ask)

  # Per contract values
  shares <- PUT_CALENDAR_SPREAD_CONFIG$shares_per_contract
  net_debit <- net_debit_per_share * shares
  max_loss <- net_debit  # Max loss is the debit paid

  # Estimated max profit (conservative: 50-100% of debit)
  # Max profit occurs when stock is at strike at front expiration
  estimated_max_profit <- net_debit * 0.75  # Conservative estimate

  # Calculate return metrics
  returns <- calculate_calendar_return_metrics(
    net_debit = net_debit,
    estimated_max_profit = estimated_max_profit,
    days_to_front_expiry = short_days
  )

  # Days spread and ratio
  days_spread <- long_days - short_days
  expiry_ratio <- long_days / short_days

  # Greeks (if available in options data)
  short_theta <- if ("Theta" %in% names(short_put)) short_put$Theta else NA
  long_theta <- if ("Theta" %in% names(long_put)) long_put$Theta else NA
  net_theta <- if (!is.na(short_theta) && !is.na(long_theta)) {
    abs(short_theta) - abs(long_theta)  # Should be positive (short decays faster)
  } else {
    NA
  }

  short_vega <- if ("Vega" %in% names(short_put)) short_put$Vega else NA
  long_vega <- if ("Vega" %in% names(long_put)) long_put$Vega else NA
  net_vega <- if (!is.na(short_vega) && !is.na(long_vega)) {
    long_vega - short_vega  # Should be positive (long has more vega)
  } else {
    NA
  }

  short_delta <- if ("Delta" %in% names(short_put)) short_put$Delta else NA
  long_delta <- if ("Delta" %in% names(long_put)) long_put$Delta else NA
  net_delta <- if (!is.na(short_delta) && !is.na(long_delta)) {
    long_delta - short_delta  # Should be near zero
  } else {
    NA
  }

  # IV ratio (if available)
  short_iv <- if ("IV" %in% names(short_put)) short_put$IV else NA
  long_iv <- if ("IV" %in% names(long_put)) long_put$IV else NA
  iv_ratio <- if (!is.na(short_iv) && !is.na(long_iv) && long_iv > 0) {
    short_iv / long_iv
  } else {
    NA
  }

  # Open interest (minimum of both legs)
  short_oi <- short_put$OI
  long_oi <- long_put$OI
  min_open_interest <- min(short_oi, long_oi, na.rm = TRUE)

  # Debit to profit ratio (for scoring)
  debit_profit_ratio <- if (estimated_max_profit > 0) {
    net_debit / estimated_max_profit
  } else {
    NA
  }

  # Calculate opportunity score
  score <- calculate_calendar_score(
    iv_rank = NA,  # Would need to calculate separately
    iv_ratio = iv_ratio,
    net_theta = net_theta,
    net_vega = net_vega,
    debit_profit_ratio = debit_profit_ratio,
    open_interest = min_open_interest,
    is_range_bound = TRUE,  # Assume true for dividend aristocrats
    dividend_safe = TRUE    # Would need to check ex-div dates
  )

  # Assemble final result
  tibble(
    ticker = ticker,
    company_name = stock_data$company_name,
    current_price = current_price,
    strike = strike,
    # Short leg (sell this put)
    short_expiration = as.character(short_expiration),
    short_days_to_expiry = short_days,
    short_bid = short_bid,
    short_oi = short_oi,
    # Long leg (buy this put)
    long_expiration = as.character(long_expiration),
    long_days_to_expiry = long_days,
    long_ask = long_ask,
    long_oi = long_oi,
    # Spread characteristics
    days_spread = days_spread,
    expiry_ratio = expiry_ratio,
    # Financials
    net_debit = net_debit,
    max_loss = max_loss,
    estimated_max_profit = estimated_max_profit,
    # Returns
    roi = returns$roi,
    annualized_return = returns$annualized_return,
    profit_target_15pct = returns$profit_target_15pct,
    profit_target_20pct = returns$profit_target_20pct,
    profit_target_25pct = returns$profit_target_25pct,
    # Greeks
    net_theta = net_theta,
    net_vega = net_vega,
    net_delta = net_delta,
    iv_ratio = iv_ratio,
    # Liquidity
    min_open_interest = min_open_interest,
    # Scoring
    opportunity_score = score,
    # Stock data
    max_drawdown = stock_data$max_drawdown,
    current_yield = stock_data$current_yield,
    # Flags
    is_calendar_spread = TRUE,
    is_aristocrat = TRUE
  )
}

################################################################################
# CALENDAR SPREAD OPTION SELECTION
################################################################################

#' Select optimal calendar spread put pair
#'
#' Finds matching put options at the same strike for both legs.
#' Short leg: near-term (20-45 DTE), Long leg: further-dated (50-120 DTE)
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @param options_df Tibble of available put options
#' @param strike_pct Target strike as % of current price (default 0.95)
#' @param short_min_days Minimum days for short leg
#' @param short_max_days Maximum days for short leg
#' @param long_min_days Minimum days for long leg
#' @param long_max_days Maximum days for long leg
#' @param min_expiry_ratio Minimum ratio of long/short DTE
#' @return List with short_put, long_put, and strike, or NULL if no valid pair
#' @noRd
select_optimal_calendar_puts <- function(
  ticker,
  current_price,
  options_df,
  strike_pct = 0.95,
  short_min_days = 20,
  short_max_days = 45,
  long_min_days = 50,
  long_max_days = 120,
  min_expiry_ratio = 1.5
) {
  # Calculate target strike (ATM/slightly OTM)
  target_strike <- current_price * strike_pct

  log_info("{ticker}: Looking for calendar spread near strike ${sprintf('%.2f', target_strike)} ({sprintf('%.0f%%', strike_pct*100)} of ${sprintf('%.2f', current_price)})")

  # Get minimum thresholds from config
  min_bid <- PUT_CALENDAR_SPREAD_CONFIG$min_option_bid
  min_oi <- PUT_CALENDAR_SPREAD_CONFIG$min_open_interest

  # Step 1: Filter options by liquidity
  liquid_options <- options_df %>%
    filter(Bid >= min_bid, OI >= min_oi)

  if (nrow(liquid_options) == 0) {
    log_warn("{ticker}: No options meet liquidity requirements (bid >= {min_bid}, OI >= {min_oi})")
    return(NULL)
  }

  # Step 2: Find available strikes near target
  # Look within 10% of target strike
  strike_range_min <- target_strike * 0.90
  strike_range_max <- target_strike * 1.05

  available_strikes <- liquid_options %>%
    filter(Strike >= strike_range_min, Strike <= strike_range_max) %>%
    distinct(Strike) %>%
    pull(Strike)

  if (length(available_strikes) == 0) {
    log_warn("{ticker}: No strikes available in range ${sprintf('%.2f', strike_range_min)} - ${sprintf('%.2f', strike_range_max)}")
    return(NULL)
  }

  log_info("{ticker}: Found {length(available_strikes)} potential strikes in range")

  # Step 3: For each strike, try to find valid short/long pair
  for (strike in sort(available_strikes, decreasing = FALSE)) {
    # Get options at this strike
    strike_options <- liquid_options %>%
      filter(Strike == strike)

    # Find short-dated options (front month)
    short_candidates <- strike_options %>%
      filter(days_to_expiry >= short_min_days, days_to_expiry <= short_max_days) %>%
      arrange(days_to_expiry)  # Prefer shorter expiry for front month

    if (nrow(short_candidates) == 0) {
      next
    }

    # Find long-dated options (back month)
    long_candidates <- strike_options %>%
      filter(days_to_expiry >= long_min_days, days_to_expiry <= long_max_days) %>%
      arrange(desc(days_to_expiry))  # Prefer longer expiry for back month

    if (nrow(long_candidates) == 0) {
      next
    }

    # Step 4: Find a valid pair that meets the expiry ratio requirement
    for (i in seq_len(nrow(short_candidates))) {
      short_put <- short_candidates[i, ]

      for (j in seq_len(nrow(long_candidates))) {
        long_put <- long_candidates[j, ]

        # Check expiry ratio
        ratio <- long_put$days_to_expiry / short_put$days_to_expiry

        if (ratio >= min_expiry_ratio) {
          # Valid pair found!
          log_info("{ticker}: Found calendar pair at strike ${sprintf('%.2f', strike)} - Short: {short_put$days_to_expiry} DTE, Long: {long_put$days_to_expiry} DTE (ratio: {sprintf('%.1f', ratio)})")

          return(list(
            short_put = short_put,
            long_put = long_put,
            strike = strike
          ))
        }
      }
    }
  }

  log_warn("{ticker}: No valid calendar spread pair found meeting all criteria")
  return(NULL)
}

################################################################################
# SINGLE STOCK ANALYZER FOR CALENDAR SPREADS
################################################################################

#' Analyze a single stock for put calendar spread opportunity
#'
#' Analyzes a single stock for put calendar spread opportunities.
#' Finds optimal short/long put pair at same strike.
#'
#' @param ticker Stock ticker symbol
#' @param strike_pct Target strike as % of current price
#' @param short_min_days Minimum days for short leg
#' @param short_max_days Maximum days for short leg
#' @param long_min_days Minimum days for long leg
#' @param long_max_days Maximum days for long leg
#' @param result_flags Named list of additional flags to add to results
#' @param return_failure_reason If TRUE, return failure reason instead of NULL
#' @return Tibble row with analysis results, or NULL if no opportunity found
#' @keywords internal
#' @export
analyze_single_stock_calendar <- function(
  ticker,
  strike_pct = PUT_CALENDAR_SPREAD_CONFIG$strike_pct,
  short_min_days = PUT_CALENDAR_SPREAD_CONFIG$short_expiry_min_days,
  short_max_days = PUT_CALENDAR_SPREAD_CONFIG$short_expiry_max_days,
  long_min_days = PUT_CALENDAR_SPREAD_CONFIG$long_expiry_min_days,
  long_max_days = PUT_CALENDAR_SPREAD_CONFIG$long_expiry_max_days,
  result_flags = list(),
  return_failure_reason = FALSE
) {
  validate_ticker(ticker)

  log_info("{ticker}: Starting calendar spread analysis")

  # Get stock data
  stock_data <- get_stock_data(ticker)
  if (is.null(stock_data)) {
    log_warn("{ticker}: Failed to get stock data")
    if (return_failure_reason) {
      return(list(failure_reason = "Failed to get stock data"))
    }
    return(NULL)
  }

  current_price <- stock_data$current_price

  # Skip expensive stocks
  if (current_price > PUT_CALENDAR_SPREAD_CONFIG$max_stock_price) {
    log_info("{ticker}: Skipping - price ${sprintf('%.2f', current_price)} exceeds max ${PUT_CALENDAR_SPREAD_CONFIG$max_stock_price}")
    if (return_failure_reason) {
      return(list(failure_reason = sprintf("Price $%.2f exceeds max $%.0f",
                                           current_price,
                                           PUT_CALENDAR_SPREAD_CONFIG$max_stock_price)))
    }
    return(NULL)
  }

  # Get put options chain
  options_chain <- get_options_chain_puts(ticker, current_price)
  if (is.null(options_chain) || nrow(options_chain) == 0) {
    log_warn("{ticker}: No put options available")
    if (return_failure_reason) {
      return(list(failure_reason = "No put options available"))
    }
    return(NULL)
  }

  # Select optimal calendar spread pair
  selection <- select_optimal_calendar_puts(
    ticker = ticker,
    current_price = current_price,
    options_df = options_chain,
    strike_pct = strike_pct,
    short_min_days = short_min_days,
    short_max_days = short_max_days,
    long_min_days = long_min_days,
    long_max_days = long_max_days,
    min_expiry_ratio = PUT_CALENDAR_SPREAD_CONFIG$min_expiry_ratio
  )

  if (is.null(selection)) {
    log_info("{ticker}: No suitable calendar spread found")
    if (return_failure_reason) {
      return(list(failure_reason = "No calendar spread pair meets criteria"))
    }
    return(NULL)
  }

  # Calculate metrics
  result <- calculate_calendar_spread_metrics(
    ticker = ticker,
    current_price = current_price,
    short_put = selection$short_put,
    long_put = selection$long_put,
    stock_data = stock_data
  )

  # Add any additional result flags
  for (flag_name in names(result_flags)) {
    result[[flag_name]] <- result_flags[[flag_name]]
  }

  log_success("{ticker}: Calendar spread found - Strike: ${sprintf('%.2f', selection$strike)}, Net Debit: ${sprintf('%.0f', result$net_debit)}, Ann. Return: {sprintf('%.1f%%', result$annualized_return*100)}")

  result
}

################################################################################
# MAIN ANALYSIS FUNCTIONS
################################################################################

#' Generic put calendar spread analyzer for any stock universe
#'
#' Orchestrates the complete calendar spread analysis workflow for any stock universe.
#' Uses parallel processing for efficiency.
#'
#' @param stock_universe Character vector of tickers to analyze
#' @param strategy_name Name for logging
#' @param strike_pct Target strike as % of current price
#' @param short_min_days Minimum days for short leg
#' @param short_max_days Maximum days for short leg
#' @param long_min_days Minimum days for long leg
#' @param long_max_days Maximum days for long leg
#' @param max_workers Number of parallel workers
#' @param result_flags Named list of additional flags to add to results
#' @return Tibble with all opportunities sorted by opportunity score
#' @export
analyze_calendar_spread_generic <- function(
  stock_universe,
  strategy_name,
  strike_pct = PUT_CALENDAR_SPREAD_CONFIG$strike_pct,
  short_min_days = PUT_CALENDAR_SPREAD_CONFIG$short_expiry_min_days,
  short_max_days = PUT_CALENDAR_SPREAD_CONFIG$short_expiry_max_days,
  long_min_days = PUT_CALENDAR_SPREAD_CONFIG$long_expiry_min_days,
  long_max_days = PUT_CALENDAR_SPREAD_CONFIG$long_expiry_max_days,
  max_workers = PUT_CALENDAR_SPREAD_CONFIG$max_workers,
  result_flags = list()
) {
  # Validate inputs
  if (length(stock_universe) == 0) {
    stop("stock_universe cannot be empty")
  }

  # Log analysis start
  log_info("=" %>% rep(60) %>% paste(collapse = ""))
  log_info("{strategy_name}")
  log_info("=" %>% rep(60) %>% paste(collapse = ""))
  log_info("Analyzing {length(stock_universe)} stocks")
  log_info("Parameters: strike={sprintf('%.0f%%', strike_pct*100)}, short={short_min_days}-{short_max_days} DTE, long={long_min_days}-{long_max_days} DTE")
  log_info("Workers: {max_workers}")

  # Setup parallel processing
  oplan <- setup_parallel_processing(max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Safe analyzer with error handling
  safe_analyze <- possibly(
    analyze_single_stock_calendar,
    otherwise = NULL
  )

  # Process stocks in parallel
  results <- future_map(
    stock_universe,
    ~safe_analyze(
      ticker = .x,
      strike_pct = strike_pct,
      short_min_days = short_min_days,
      short_max_days = short_max_days,
      long_min_days = long_min_days,
      long_max_days = long_max_days,
      result_flags = result_flags
    ),
    .options = furrr_options(seed = TRUE),
    .progress = TRUE
  )

  # Combine results (remove NULLs and non-dataframe failures)
  results_df <- results %>%
    compact() %>%
    keep(~ is.data.frame(.x)) %>%
    bind_rows()

  # Sort by opportunity score (descending), then annualized return
  if (nrow(results_df) > 0) {
    results_df <- results_df %>%
      arrange(desc(opportunity_score), desc(annualized_return))
  }

  # Log completion
  log_info("-" %>% rep(60) %>% paste(collapse = ""))
  log_success("Analysis complete! Found {nrow(results_df)} calendar spread opportunities")
  log_info("-" %>% rep(60) %>% paste(collapse = ""))

  results_df
}

#' Analyze dividend aristocrats for put calendar spread opportunities
#'
#' Main entry point for put calendar spread analysis on dividend aristocrats.
#' Finds optimal short/long put pairs to capitalize on theta decay differential.
#'
#' @param limit Optional limit on number of stocks to analyze
#' @param strike_pct Target strike as % of current price (default 0.95 = ATM)
#' @param short_min_days Minimum days for short leg (default 20)
#' @param short_max_days Maximum days for short leg (default 45)
#' @param long_min_days Minimum days for long leg (default 50)
#' @param long_max_days Maximum days for long leg (default 120)
#' @param max_workers Number of parallel workers
#' @return Tibble with all opportunities sorted by opportunity score
#' @export
#' @examples
#' \dontrun{
#'   # Analyze all dividend aristocrats
#'   results <- analyze_put_calendar_spread()
#'
#'   # Analyze with custom parameters
#'   results <- analyze_put_calendar_spread(
#'     strike_pct = 0.90,
#'     short_max_days = 30,
#'     long_min_days = 60,
#'     max_workers = 4
#'   )
#'
#'   # Quick test with limit
#'   results <- analyze_put_calendar_spread(limit = 5)
#' }
analyze_put_calendar_spread <- function(
  limit = NULL,
  strike_pct = PUT_CALENDAR_SPREAD_CONFIG$strike_pct,
  short_min_days = PUT_CALENDAR_SPREAD_CONFIG$short_expiry_min_days,
  short_max_days = PUT_CALENDAR_SPREAD_CONFIG$short_expiry_max_days,
  long_min_days = PUT_CALENDAR_SPREAD_CONFIG$long_expiry_min_days,
  long_max_days = PUT_CALENDAR_SPREAD_CONFIG$long_expiry_max_days,
  max_workers = PUT_CALENDAR_SPREAD_CONFIG$max_workers
) {
  # Get dividend aristocrats (reuse existing function!)
  aristocrats <- get_dividend_aristocrats()

  # Apply limit if specified
  if (!is.null(limit)) {
    aristocrats <- head(aristocrats, limit)
    log_info("Limiting analysis to first {limit} stocks")
  }

  # Call generic analyzer
  analyze_calendar_spread_generic(
    stock_universe = aristocrats,
    strategy_name = "Put Calendar Spread - Dividend Aristocrats",
    strike_pct = strike_pct,
    short_min_days = short_min_days,
    short_max_days = short_max_days,
    long_min_days = long_min_days,
    long_max_days = long_max_days,
    max_workers = max_workers,
    result_flags = list(
      is_calendar_spread = TRUE,
      is_aristocrat = TRUE
    )
  )
}
