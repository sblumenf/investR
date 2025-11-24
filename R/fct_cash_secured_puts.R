#' Cash-Secured Puts Analysis Functions
#'
#' Core business logic for analyzing cash-secured put opportunities on
#' dividend aristocrats and other high-quality stocks.
#'
#' Strategy: Sell puts on dividend aristocrats, collect premium income,
#' and potentially acquire quality stocks at a discount if assigned.
#'
#' @name cash-secured-puts
#' @import dplyr
#' @importFrom purrr map_dfr compact
#' @importFrom tibble tibble
#' @importFrom logger log_info log_warn log_success log_debug log_error
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multisession
NULL

################################################################################
# PUT METRICS CALCULATION FUNCTIONS
################################################################################

#' Calculate cash flow components for cash-secured put
#'
#' Calculates the cash required, premium received, and net outlay for
#' a cash-secured put position.
#'
#' @param strike Option strike price
#' @param bid_price Option bid price
#' @return List with cash_required, premium_received, and net_outlay
#' @noRd
calculate_put_cash_flows <- function(strike, bid_price) {
  validate_price(strike, "strike")
  validate_price(bid_price, "bid_price")

  cash_required <- strike * CASH_SECURED_PUTS_CONFIG$shares_per_contract
  premium_received <- bid_price * CASH_SECURED_PUTS_CONFIG$shares_per_contract
  net_outlay <- cash_required - premium_received

  list(
    cash_required = cash_required,
    premium_received = premium_received,
    net_outlay = net_outlay
  )
}

#' Calculate put protection metrics
#'
#' Calculates breakeven price and downside protection percentage for put options.
#' For puts: breakeven is strike - premium, and we measure how far stock can
#' fall before we lose money.
#'
#' @param current_price Current stock price
#' @param strike Option strike price
#' @param bid_price Option bid price
#' @return List with breakeven_price and downside_protection_pct
#' @noRd
calculate_put_protection_metrics <- function(current_price, strike, bid_price) {
  validate_price(current_price, "current_price")
  validate_price(strike, "strike")
  validate_price(bid_price, "bid_price")

  # For puts: breakeven = strike - premium
  # We effectively buy stock at (strike - premium) if assigned
  breakeven_price <- strike - bid_price

  # Downside protection: how far stock can fall before loss
  # Protection % = (current_price - breakeven) / current_price
  downside_protection_pct <- (current_price - breakeven_price) / current_price

  list(
    breakeven_price = breakeven_price,
    downside_protection_pct = downside_protection_pct
  )
}

#' Calculate put return metrics
#'
#' Calculates return on cash and annualized return for put options.
#' Assumes the put is not assigned (stock stays above strike).
#'
#' @param premium_received Premium income from selling put
#' @param cash_required Cash collateral required
#' @param days_to_expiry Days until expiration
#' @return List with return_on_cash and annualized_return
#' @noRd
calculate_put_return_metrics <- function(premium_received, cash_required, days_to_expiry) {
  if (!is.numeric(cash_required) || cash_required <= 0) {
    stop("cash_required must be positive")
  }
  if (!is.numeric(days_to_expiry) || days_to_expiry <= 0) {
    stop("days_to_expiry must be positive")
  }

  # Return on cash = premium / cash_required
  return_on_cash <- premium_received / cash_required

  # Annualize the return
  annualized_return <- calculate_annualized_return(
    total_return = return_on_cash,
    days = days_to_expiry,
    days_per_year = CASH_SECURED_PUTS_CONFIG$days_per_year
  )

  list(
    return_on_cash = return_on_cash,
    annualized_return = annualized_return
  )
}

#' Calculate all cash-secured put metrics (orchestrator)
#'
#' Comprehensive metric calculation for put options. Delegates to
#' specialized functions for cash flows, protection, and returns.
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @param option_row Selected put option (tibble row)
#' @param stock_data Stock data list from get_stock_data()
#' @param warning_flag Warning flag from option selection
#' @return Tibble row with all calculated metrics
#' @noRd
calculate_put_metrics <- function(ticker, current_price, option_row,
                                  stock_data, warning_flag) {
  # Validate inputs
  validate_ticker(ticker)
  validate_price(current_price, "current_price")
  validate_columns(option_row, c("Strike", "Bid", "days_to_expiry", "expiration", "OI"),
                  "option_row")

  # Extract option data
  strike <- option_row$Strike
  bid_price <- option_row$Bid
  days_to_expiry <- option_row$days_to_expiry
  expiration <- option_row$expiration
  open_interest <- option_row$OI

  # Calculate components using modular functions
  cash_flows <- calculate_put_cash_flows(strike, bid_price)

  protection <- calculate_put_protection_metrics(current_price, strike, bid_price)

  returns <- calculate_put_return_metrics(
    cash_flows$premium_received,
    cash_flows$cash_required,
    days_to_expiry
  )

  # Option value decomposition (for puts: ITM when strike > current_price)
  intrinsic_value <- max(0, strike - current_price)
  extrinsic_value <- bid_price - intrinsic_value

  # Assemble final result
  tibble(
    ticker = ticker,
    company_name = stock_data$company_name,
    current_price = current_price,
    strike = strike,
    expiration = as.character(expiration),
    days_to_expiry = days_to_expiry,
    bid_price = bid_price,
    open_interest = open_interest,
    # Cash flows
    cash_required = cash_flows$cash_required,
    premium_received = cash_flows$premium_received,
    net_outlay = cash_flows$net_outlay,
    # Returns
    return_on_cash = returns$return_on_cash,
    annualized_return = returns$annualized_return,
    # Risk metrics
    max_drawdown = stock_data$max_drawdown,
    current_yield = stock_data$current_yield,
    # Protection
    breakeven_price = protection$breakeven_price,
    downside_protection_pct = protection$downside_protection_pct,
    # Option values
    intrinsic_value = intrinsic_value,
    extrinsic_value = extrinsic_value,
    annual_dividend = stock_data$annual_dividend,
    # Flags
    warning_flag = warning_flag,
    is_put = TRUE,
    is_aristocrat = TRUE
  )
}

################################################################################
# PUT OPTION SELECTION
################################################################################

#' Select optimal put option based on strategy parameters
#'
#' Filters put options for OTM/ATM opportunities with sufficient liquidity.
#' For puts: ITM when strike > current_price, so we want strikes >= threshold
#' to get OTM/ATM puts (slightly below current price).
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @param options_df Tibble of available put options
#' @param strike_threshold_pct Minimum strike as % of current price (default 0.95)
#' @param min_days Minimum days to expiry
#' @param max_days Maximum days to expiry
#' @param expiry_month Optional month number (1-12) to filter
#' @return List with optimal option row and warning_flag, or NULL if no options
#' @noRd
select_optimal_put <- function(ticker,
                               current_price,
                               options_df,
                               strike_threshold_pct = 0.95,
                               min_days = NULL,
                               max_days = NULL,
                               expiry_month = NULL) {

  warning_flag <- FALSE

  # Calculate strike threshold (for puts: target strike for OTM)
  # User expects: 85% threshold = strikes near 85% of price (15% OTM)
  strike_threshold <- current_price * strike_threshold_pct
  log_info("{ticker}: Strike filter: near ${sprintf('%.2f', strike_threshold)} ({sprintf('%.0f%%', strike_threshold_pct*100)} of ${sprintf('%.2f', current_price)})")

  # Filter by strike (puts: want strikes AT OR BELOW threshold, but above deep OTM)
  # For OTM puts, select strikes in range: [threshold * 0.90, threshold * 1.05]
  # This targets strikes near the threshold, not everything above it
  strike_min <- strike_threshold * 0.90  # 10% below threshold
  strike_max <- strike_threshold * 1.05  # 5% above threshold (allow slight ITM for liquidity)

  filtered_options <- options_df %>%
    filter(Strike >= strike_min & Strike <= strike_max)
  log_info("{ticker}: After strike filter (${sprintf('%.2f', strike_min)} - ${sprintf('%.2f', strike_max)}): {nrow(filtered_options)} options")

  # Filter by minimum bid price
  min_bid <- CASH_SECURED_PUTS_CONFIG$min_option_bid
  if (min_bid > 0) {
    filtered_options <- filtered_options %>%
      filter(Bid >= min_bid)
    log_info("{ticker}: After bid filter (>=${min_bid}): {nrow(filtered_options)} options")
  }

  # Filter by minimum open interest for liquidity
  min_oi <- CASH_SECURED_PUTS_CONFIG$min_open_interest
  if (min_oi > 0) {
    filtered_options <- filtered_options %>%
      filter(OI >= min_oi)
    log_info("{ticker}: After OI filter (>={min_oi}): {nrow(filtered_options)} options")
  }

  # Filter by expiry month if specified
  if (!is.null(expiry_month)) {
    filtered_options <- filtered_options %>%
      filter(lubridate::month(expiration) == expiry_month)
    log_info("{ticker}: After month filter: {nrow(filtered_options)} options")
  }

  # Filter by days to expiry range
  if (!is.null(min_days)) {
    filtered_options <- filtered_options %>%
      filter(days_to_expiry >= min_days)
  }
  if (!is.null(max_days)) {
    filtered_options <- filtered_options %>%
      filter(days_to_expiry <= max_days)
  }

  log_info("{ticker}: After days filter: {nrow(filtered_options)} options")

  # Return NULL if no options meet criteria
  if (nrow(filtered_options) == 0) {
    log_warn("{ticker}: No put options meet filtering criteria")
    return(NULL)
  }

  # Select optimal: strike closest to threshold, then longest dated
  # Priority: (1) closest to target threshold, (2) longest dated, (3) highest liquidity
  optimal_option <- filtered_options %>%
    mutate(distance_from_threshold = abs(Strike - strike_threshold)) %>%
    arrange(distance_from_threshold, desc(expiration), desc(OI)) %>%
    slice(1)

  log_info("{ticker}: Selected optimal put - Strike: ${sprintf('%.2f', optimal_option$Strike)}, Expiry: {optimal_option$expiration}, Premium: ${sprintf('%.2f', optimal_option$Bid)}")

  list(
    option = optimal_option,
    warning_flag = warning_flag
  )
}

################################################################################
# SINGLE STOCK ANALYZER FOR PUTS
################################################################################

#' Analyze a single stock for cash-secured put opportunity
#'
#' Analyzes a single stock for cash-secured put opportunities using
#' dividend aristocrats strategy parameters.
#'
#' @param ticker Stock ticker symbol
#' @param strike_threshold_pct Minimum strike as % of current price
#' @param min_days Minimum days to expiry
#' @param max_days Maximum days to expiry
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param result_flags Named list of additional flags to add to results
#' @param return_failure_reason If TRUE, return failure reason instead of NULL
#' @return Tibble row with analysis results, or NULL if no opportunity found
#' @keywords internal
#' @export
analyze_single_stock_put <- function(ticker,
                                     strike_threshold_pct,
                                     min_days = NULL,
                                     max_days = NULL,
                                     expiry_month = NULL,
                                     result_flags = list(),
                                     return_failure_reason = FALSE) {
  validate_ticker(ticker)

  log_info("{ticker}: Starting put analysis (strike>={sprintf('%.0f%%', strike_threshold_pct*100)}, days={min_days %||% 0}-{max_days %||% 'max'})")

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
  if (current_price > CASH_SECURED_PUTS_CONFIG$max_stock_price) {
    log_info("{ticker}: Skipping - price ${sprintf('%.2f', current_price)} exceeds max ${CASH_SECURED_PUTS_CONFIG$max_stock_price}")
    if (return_failure_reason) {
      return(list(failure_reason = sprintf("Price $%.2f exceeds max $%.0f", current_price, CASH_SECURED_PUTS_CONFIG$max_stock_price)))
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

  # Select optimal put
  selection_result <- select_optimal_put(
    ticker = ticker,
    current_price = current_price,
    options_df = options_chain,
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month
  )

  if (is.null(selection_result)) {
    log_info("{ticker}: No suitable put options found")
    if (return_failure_reason) {
      return(list(failure_reason = "No put options meet filtering criteria"))
    }
    return(NULL)
  }

  # Calculate metrics
  result <- calculate_put_metrics(
    ticker = ticker,
    current_price = current_price,
    option_row = selection_result$option,
    stock_data = stock_data,
    warning_flag = selection_result$warning_flag
  )

  # Add any additional result flags
  for (flag_name in names(result_flags)) {
    result[[flag_name]] <- result_flags[[flag_name]]
  }

  log_success("{ticker}: Put opportunity found - Ann. Return: {sprintf('%.1f%%', result$annualized_return*100)}, Premium: ${sprintf('%.0f', result$premium_received)}")

  result
}

################################################################################
# OPTIONS CHAIN FETCHING FOR PUTS
################################################################################

#' Get put options chain for a stock
#'
#' Fetches put options chain using the existing fetch_options_chain() function
#' and processes all available expirations. Mirrors get_options_chain() from
#' aristocrats analysis but for puts.
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @return Tibble of put options with expiration and days_to_expiry
#' @noRd
get_options_chain_puts <- function(ticker, current_price) {
  validate_ticker(ticker)
  validate_price(current_price, "current_price")

  tryCatch({
    log_info("{ticker}: Fetching put options chain (source={toupper(getOption('investR.quote_source', 'questrade'))})")

    # Fetch complete options chain (returns Yahoo format with both calls and puts)
    opt_chain <- fetch_options_chain(ticker, expiration = NULL)

    if (is.null(opt_chain) || length(opt_chain) == 0) {
      log_warn("{ticker}: fetch_options_chain() returned NULL or empty")
      return(tibble())
    }

    log_info("{ticker}: Raw options chain has {length(opt_chain)} expiration dates")

    # Process each expiration with error handling
    process_exp <- possibly(function(exp_date) {
      exp_data <- opt_chain[[exp_date]]

      if (is.null(exp_data) || !"puts" %in% names(exp_data)) {
        return(tibble())
      }

      puts <- exp_data$puts

      if (is.null(puts) || nrow(puts) == 0) {
        return(tibble())
      }

      # Filter for valid puts with bids and strikes
      valid_puts <- puts %>%
        filter(!is.na(Strike)) %>%
        filter(!is.na(Bid) & Bid > CASH_SECURED_PUTS_CONFIG$min_option_bid) %>%
        mutate(
          expiration = as.Date(exp_date, format = "%b.%d.%Y"),
          days_to_expiry = as.integer(difftime(expiration, Sys.Date(), units = "days")),
          # For puts: intrinsic value when strike > current price (ITM)
          intrinsic_value = pmax(0, Strike - current_price),
          time_value = Bid - intrinsic_value
        )

      valid_puts
    }, otherwise = tibble())

    all_options <- map_dfr(names(opt_chain), process_exp)

    log_info("{ticker}: Processed options chain - {nrow(all_options)} puts with valid bids (Bid > {sprintf('%.2f', CASH_SECURED_PUTS_CONFIG$min_option_bid)})")

    all_options

  }, error = function(e) {
    log_warn("{ticker}: Error fetching put options - {truncate_error(e$message)}")
    tibble()
  })
}

################################################################################
# PARALLEL PROCESSING FOR PUTS
################################################################################

#' Process stocks in parallel for put analysis
#'
#' @param stock_universe Character vector of ticker symbols
#' @param strike_threshold_pct Minimum strike as % of current price
#' @param min_days Minimum days to expiry
#' @param max_days Maximum days to expiry
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param result_flags Named list of flags to add to results
#' @return List of analysis results
#' @noRd
process_stocks_parallel_put <- function(stock_universe,
                                        strike_threshold_pct,
                                        min_days = NULL,
                                        max_days = NULL,
                                        expiry_month = NULL,
                                        result_flags = list()) {
  log_info("Processing stocks in parallel for put opportunities...")

  # Capture quote source setting to pass to workers
  quote_source <- get_quote_source()
  log_info("Quote source for this analysis: {toupper(quote_source)}")

  # Process in parallel
  results <- future_map(stock_universe, function(ticker) {
    # Ensure package is loaded in worker
    if (!"investR" %in% loadedNamespaces()) {
      suppressPackageStartupMessages(loadNamespace("investR"))
    }

    # Set quote source in this worker to match main process
    options(investR.quote_source = quote_source)

    # Wrap analysis to capture failure reason
    tryCatch({
      result <- analyze_single_stock_put(
        ticker = ticker,
        strike_threshold_pct = strike_threshold_pct,
        min_days = min_days,
        max_days = max_days,
        expiry_month = expiry_month,
        result_flags = result_flags,
        return_failure_reason = TRUE
      )

      # Check if result is a failure reason or actual result
      if (!is.null(result) && !is.null(result$failure_reason)) {
        list(ticker = ticker, status = "failed", reason = result$failure_reason, result = NULL)
      } else if (is.null(result)) {
        list(ticker = ticker, status = "failed", reason = "Analysis returned NULL", result = NULL)
      } else {
        list(ticker = ticker, status = "success", result = result)
      }
    }, error = function(e) {
      list(ticker = ticker, status = "error", error = e$message, result = NULL)
    })
  }, .options = furrr_options(
    seed = TRUE,
    packages = "investR"
  ))

  # Log summary
  log_info("\n=== Worker Results Summary ===")
  success_count <- sum(sapply(results, function(r) r$status == "success"))
  failed_count <- sum(sapply(results, function(r) r$status == "failed"))
  error_count <- sum(sapply(results, function(r) r$status == "error"))

  log_info("Successful: {success_count}, Failed: {failed_count}, Errors: {error_count}")

  # Log failed tickers with reasons
  if (failed_count > 0) {
    failed_results <- results[sapply(results, function(r) r$status == "failed")]
    log_warn("\nFailed tickers breakdown:")

    # Group by failure reason
    failure_reasons <- list()
    for (fail in failed_results) {
      reason <- fail$reason %||% "Unknown"
      if (is.null(failure_reasons[[reason]])) {
        failure_reasons[[reason]] <- c()
      }
      failure_reasons[[reason]] <- c(failure_reasons[[reason]], fail$ticker)
    }

    # Log each reason group
    for (reason in names(failure_reasons)) {
      tickers <- failure_reasons[[reason]]
      log_warn("  [{reason}]: {paste(tickers, collapse=', ')}")
    }
  }

  # Log error tickers with error messages
  if (error_count > 0) {
    error_results <- results[sapply(results, function(r) r$status == "error")]
    log_error("\nError tickers breakdown:")

    # Group by error message
    error_messages <- list()
    for (err in error_results) {
      message <- err$error %||% "Unknown error"
      if (is.null(error_messages[[message]])) {
        error_messages[[message]] <- c()
      }
      error_messages[[message]] <- c(error_messages[[message]], err$ticker)
    }

    # Log each error group
    for (message in names(error_messages)) {
      tickers <- error_messages[[message]]
      log_error("  [{message}]: {paste(tickers, collapse=', ')}")
    }
  }

  # Extract actual results
  actual_results <- lapply(results, function(r) r$result)

  return(actual_results)
}

################################################################################
# MAIN ANALYSIS FUNCTIONS
################################################################################

#' Analyze dividend aristocrats for cash-secured put opportunities
#'
#' Analyzes dividend aristocrats for ATM/OTM cash-secured put opportunities.
#' Targets high-quality dividend stocks with put options priced above fair value.
#'
#' @param limit Optional limit on number of stocks to analyze
#' @param strike_threshold_pct Minimum strike as % of current price (default 0.95)
#'        Higher % = more conservative (closer to ATM)
#'        Lower % = more aggressive (deeper OTM)
#' @param min_days Minimum days to expiry (default 45)
#' @param max_days Maximum days to expiry (default 120)
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers
#' @return Tibble with all opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze all dividend aristocrats
#'   results <- analyze_cash_secured_puts()
#'
#'   # Analyze with custom parameters
#'   results <- analyze_cash_secured_puts(
#'     strike_threshold_pct = 0.90,
#'     min_days = 60,
#'     max_workers = 4
#'   )
#' }
analyze_cash_secured_puts <- function(
  limit = NULL,
  strike_threshold_pct = CASH_SECURED_PUTS_CONFIG$strike_threshold_pct,
  min_days = CASH_SECURED_PUTS_CONFIG$min_days,
  max_days = CASH_SECURED_PUTS_CONFIG$max_days,
  expiry_month = NULL,
  max_workers = CASH_SECURED_PUTS_CONFIG$max_workers
) {
  # Get dividend aristocrats (reuse existing function!)
  aristocrats <- get_dividend_aristocrats()

  # Apply limit if specified
  if (!is.null(limit)) {
    aristocrats <- head(aristocrats, limit)
    log_info("Limiting analysis to first {limit} stocks")
  }

  # Call generic analyzer
  analyze_puts_generic(
    stock_universe = aristocrats,
    strategy_name = "Cash-Secured Puts - Dividend Aristocrats",
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    max_workers = max_workers,
    result_flags = list(
      is_put = TRUE,
      is_aristocrat = TRUE
    )
  )
}

#' Generic cash-secured puts analysis orchestrator
#'
#' Orchestrates the complete cash-secured put analysis workflow for any stock universe.
#' Mirrors analyze_covered_calls_generic() but for puts.
#'
#' @param stock_universe Character vector of tickers to analyze
#' @param strategy_name Name for logging
#' @param strike_threshold_pct Minimum strike as % of current price
#' @param min_days Minimum days to expiry
#' @param max_days Maximum days to expiry
#' @param expiry_month Target expiry month (1-12, NULL for any)
#' @param max_workers Number of parallel workers
#' @param result_flags Named list of additional flags to add to results
#' @return Tibble with all opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze custom stock universe
#'   custom_stocks <- c("AAPL", "MSFT", "JNJ", "PG")
#'   results <- analyze_puts_generic(
#'     stock_universe = custom_stocks,
#'     strategy_name = "Custom Tech Puts"
#'   )
#' }
analyze_puts_generic <- function(
  stock_universe,
  strategy_name,
  strike_threshold_pct = 0.95,
  min_days = NULL,
  max_days = NULL,
  expiry_month = NULL,
  max_workers = 10,
  result_flags = list()
) {
  # Validate inputs
  if (length(stock_universe) == 0) {
    stop("stock_universe cannot be empty")
  }

  # Log analysis start
  log_analysis_header_generic(strategy_name)
  log_analysis_params_generic(
    n_stocks = length(stock_universe),
    strategy_name = strategy_name,
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    max_workers = max_workers
  )

  # Setup parallel processing
  oplan <- setup_parallel_processing(max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Process stocks in parallel
  results <- process_stocks_parallel_put(
    stock_universe = stock_universe,
    strike_threshold_pct = strike_threshold_pct,
    min_days = min_days,
    max_days = max_days,
    expiry_month = expiry_month,
    result_flags = result_flags
  )

  # Finalize and sort results
  results_df <- finalize_results(results)

  # Log completion
  log_analysis_footer(nrow(results_df))

  results_df
}
