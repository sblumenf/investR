#' Collar Strategy Analysis Functions
#'
#' Core business logic for analyzing collar (synthetic bond) opportunities.
#' A collar = Long stock + Short ATM call + Long ATM put
#' This creates a position with locked-in value at expiration.
#'
#' @name collar-analysis
#' @importFrom logger log_info log_warn log_debug log_success
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multisession
#' @importFrom purrr compact
#' @importFrom dplyr bind_rows arrange desc filter mutate select %>%
#' @importFrom tidyr tibble
NULL

################################################################################
# CORE ANALYSIS FUNCTIONS
################################################################################

#' Analyze single ticker for collar opportunity
#'
#' Analyzes a single stock or ETF for collar strategy opportunity.
#' Returns NULL if no suitable opportunity found.
#'
#' @param ticker Stock or ETF ticker symbol
#' @param target_days Target days to expiry (finds closest match, default 300)
#' @param strike_adjustment_pct Strike adjustment as decimal (0 = ATM, 0.05 = 5% above, -0.05 = 5% below)
#' @return Tibble row with analysis results or NULL
#' @export
#' @examples
#' \dontrun{
#'   result <- analyze_collar_single("AAPL")
#'   result <- analyze_collar_single("AAPL", target_days = 180, strike_adjustment_pct = 0.05)
#' }
analyze_collar_single <- function(ticker,
                                  target_days = 300,
                                  strike_adjustment_pct = 0) {

  validate_ticker(ticker)

  tryCatch({

    # 1. Get stock data
    log_debug("Analyzing {ticker}...")

    quote <- fetch_current_quote(ticker, fields = c("Last Trade (Price Only)", "Name"))
    current_price <- as.numeric(quote$Last)
    company_name <- as.character(quote$Name)

    if (is.na(current_price) || current_price <= 0) {
      log_debug("{ticker}: Invalid price data")
      return(NULL)
    }

    # Filter by maximum stock price
    if (current_price > COLLAR_CONFIG$max_stock_price) {
      log_debug("{ticker}: Price ${current_price} exceeds max ${COLLAR_CONFIG$max_stock_price}")
      return(NULL)
    }

    # 2. Get dividend history (for dividend projection)
    end_date <- Sys.Date()
    start_date <- end_date - lubridate::years(COLLAR_CONFIG$history_years)
    dividends <- fetch_dividend_history(ticker, from = start_date, to = end_date)

    # Calculate current dividend yield
    has_dividends <- FALSE
    if (!is.null(dividends) && length(dividends) > 0 && nrow(dividends) >= 2) {
      latest_dividend <- as.numeric(tail(dividends, 1))
      recent_divs <- tail(dividends, min(6, nrow(dividends)))
      div_dates <- index(recent_divs)
      days_between <- as.numeric(diff(div_dates))
      avg_days_between <- mean(days_between)
      annual_dividend <- latest_dividend * (365 / avg_days_between)
      current_yield <- annual_dividend / current_price
      has_dividends <- (current_yield > 0)
    }

    # 3. Get options chain
    log_debug("{ticker}: Fetching options chain...")
    opt_chain <- fetch_options_chain(ticker, expiration = NULL)

    if (is.null(opt_chain) || length(opt_chain) == 0) {
      log_debug("{ticker}: No options data available")
      return(NULL)
    }

    # 4. Find expiration closest to target_days
    exp_dates <- names(opt_chain)

    # Convert to Date objects and calculate days to expiry
    exp_date_objs <- as.Date(exp_dates, format = "%b.%d.%Y")
    days_to_expiry_vec <- as.integer(difftime(exp_date_objs, Sys.Date(), units = "days"))

    # Filter out NAs from failed date parsing
    valid_indices <- which(!is.na(days_to_expiry_vec))

    if (length(valid_indices) == 0) {
      log_debug("{ticker}: All expiration dates failed to parse")
      return(NULL)
    }

    # Find expiration closest to target (among valid dates only)
    valid_days <- days_to_expiry_vec[valid_indices]
    closest_valid_idx <- which.min(abs(valid_days - target_days))
    closest_idx <- valid_indices[closest_valid_idx]

    selected_exp <- exp_dates[closest_idx]
    selected_days <- days_to_expiry_vec[closest_idx]

    log_debug("{ticker}: Using expiration {selected_exp} ({selected_days} days, target: {target_days})")

    # 5. Get calls and puts for selected expiration
    exp_data <- opt_chain[[selected_exp]]

    if (is.null(exp_data) || !"calls" %in% names(exp_data) || !"puts" %in% names(exp_data)) {
      log_debug("{ticker}: Missing calls or puts data")
      return(NULL)
    }

    calls <- exp_data$calls
    puts <- exp_data$puts

    if (is.null(calls) || nrow(calls) == 0 || is.null(puts) || nrow(puts) == 0) {
      log_debug("{ticker}: Empty calls or puts data")
      return(NULL)
    }

    # 6. Find strike (with optional adjustment from ATM)
    strikes <- unique(calls$Strike)

    # Filter out NA strikes
    valid_strikes <- strikes[!is.na(strikes)]

    if (length(valid_strikes) == 0) {
      log_debug("{ticker}: All strikes are NA")
      return(NULL)
    }

    # Calculate adjusted target price
    adjusted_target_price <- current_price * (1 + strike_adjustment_pct)

    # Find strike closest to adjusted target (among valid strikes)
    strike_idx <- which.min(abs(valid_strikes - adjusted_target_price))
    atm_strike <- valid_strikes[strike_idx]

    if (length(atm_strike) == 0 || is.na(atm_strike)) {
      log_debug("{ticker}: Failed to determine ATM strike")
      return(NULL)
    }

    if (strike_adjustment_pct == 0) {
      log_debug("{ticker}: ATM strike: ${atm_strike} (current price: ${current_price})")
    } else {
      adjustment_pct_display <- sprintf("%+.0f%%", strike_adjustment_pct * 100)
      log_debug("{ticker}: Strike: ${atm_strike} ({adjustment_pct_display} adj, current price: ${current_price})")
    }

    # 7. Get ATM call and put
    atm_call <- calls %>% filter(Strike == atm_strike)
    atm_put <- puts %>% filter(Strike == atm_strike)

    if (nrow(atm_call) == 0 || nrow(atm_put) == 0) {
      log_debug("{ticker}: No ATM call or put found")
      return(NULL)
    }

    # Extract bid/ask prices
    call_bid <- atm_call$Bid[1]
    call_ask <- atm_call$Ask[1]
    put_bid <- atm_put$Bid[1]
    put_ask <- atm_put$Ask[1]

    # Validate prices (check length first to handle numeric(0))
    if (length(call_bid) == 0 || length(call_ask) == 0 || length(put_bid) == 0 || length(put_ask) == 0) {
      log_debug("{ticker}: Zero-length price vectors")
      return(NULL)
    }

    if (is.na(call_bid) || is.na(put_ask) || call_bid <= 0 || put_ask <= 0) {
      log_debug("{ticker}: Invalid option prices")
      return(NULL)
    }

    # 8. Calculate net credit (sell call at bid, buy put at ask)
    net_credit <- call_bid - put_ask

    # Filter: Only keep positions with net credit > 0
    if (net_credit <= COLLAR_CONFIG$min_net_credit) {
      log_debug("{ticker}: Net credit ${net_credit} too low (need >${COLLAR_CONFIG$min_net_credit})")
      return(NULL)
    }

    log_debug("{ticker}: Net credit: ${net_credit}")

    # 9. Use expiration date and days to expiry from earlier selection
    expiration <- as.Date(selected_exp, format = "%b.%d.%Y")

    if (is.na(expiration)) {
      log_debug("{ticker}: Failed to parse expiration date: {selected_exp}")
      return(NULL)
    }

    days_to_expiry <- selected_days

    # 10. Calculate investment amounts
    capital_required <- current_price * COLLAR_CONFIG$shares_per_contract
    premium_received <- net_credit * COLLAR_CONFIG$shares_per_contract
    net_investment <- capital_required - premium_received  # Actual cash outlay

    # 11. Project dividends (if applicable)
    dividend_income <- 0
    reinvestment_income <- 0

    if (has_dividends) {
      reinvest_rate <- get_reinvestment_rate()
      div_projection <- calculate_dividend_projections(
        dividends,
        days_to_expiry,
        reinvest_rate
      )
      dividend_income <- div_projection$dividend_income
      reinvestment_income <- div_projection$reinvestment_income
    }

    # 12. Calculate returns
    # At expiration, synthetic bond pays exactly the strike price
    final_value <- atm_strike * COLLAR_CONFIG$shares_per_contract
    max_profit <- final_value - net_investment + dividend_income + reinvestment_income
    total_return <- max_profit / net_investment
    annualized_return <- calculate_annualized_return(total_return, days_to_expiry, COLLAR_CONFIG$days_per_year)

    # 13. Get option volume and open interest
    call_volume <- if (length(atm_call$Vol) > 0 && !is.na(atm_call$Vol[1])) as.integer(atm_call$Vol[1]) else 0
    put_volume <- if (length(atm_put$Vol) > 0 && !is.na(atm_put$Vol[1])) as.integer(atm_put$Vol[1]) else 0
    call_oi <- if (length(atm_call$OI) > 0 && !is.na(atm_call$OI[1])) as.integer(atm_call$OI[1]) else 0
    put_oi <- if (length(atm_put$OI) > 0 && !is.na(atm_put$OI[1])) as.integer(atm_put$OI[1]) else 0

    # 14. Assemble result
    result <- tibble(
      ticker = ticker,
      company_name = company_name,
      beta = NA_real_,  # Could fetch from stock info if needed
      current_price = current_price,
      strike = atm_strike,
      expiration = as.character(expiration),
      days_to_expiry = days_to_expiry,
      call_bid = call_bid,
      call_ask = call_ask,
      put_bid = put_bid,
      put_ask = put_ask,
      net_credit = net_credit,
      premium_received = premium_received,
      investment = capital_required,
      capital_required = capital_required,
      net_investment = net_investment,
      net_outlay = net_investment,
      final_value = final_value,
      dividend_income = dividend_income,
      reinvestment_income = reinvestment_income,
      max_profit = max_profit,
      net_profit = max_profit,
      total_return = total_return,
      annualized_return = annualized_return,
      call_volume = call_volume,
      put_volume = put_volume,
      call_oi = call_oi,
      put_oi = put_oi,
      open_interest = call_oi  # For compatibility
    )

    log_success("{ticker}: Annualized return: {sprintf('%.2f%%', annualized_return * 100)}")

    return(result)

  }, error = function(e) {
    log_warn("{ticker}: Error analyzing - {truncate_error(e$message)}")
    return(NULL)
  })
}

#' Analyze S&P 500 stocks for collar opportunities
#'
#' @param dividend_filter Filter by dividend status: "all", "dividend", or "zero"
#' @param target_days Target days to expiry (finds closest match, default 300)
#' @param strike_adjustment_pct Strike adjustment as decimal (0 = ATM, 0.05 = 5% above)
#' @param limit Optional limit on number of stocks to analyze (for testing)
#' @param max_workers Number of parallel workers
#' @return Tibble with all opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze all S&P 500 stocks
#'   results <- analyze_collar_stocks()
#'
#'   # Analyze with custom parameters
#'   results <- analyze_collar_stocks(target_days = 180, strike_adjustment_pct = 0.05)
#'
#'   # Analyze only dividend-paying stocks
#'   results <- analyze_collar_stocks(dividend_filter = "dividend")
#' }
analyze_collar_stocks <- function(dividend_filter = "all",
                                  target_days = 300,
                                  strike_adjustment_pct = 0,
                                  limit = NULL,
                                  max_workers = COLLAR_CONFIG$max_workers) {

  log_analysis_header_generic("Collar Strategy - S&P 500 Stocks")

  # Get stock universe based on dividend filter
  log_info("Fetching S&P 500 stock universe (filter: {dividend_filter})...")

  stock_universe <- switch(dividend_filter,
    "dividend" = get_dividend_paying_sp500(max_workers = max_workers),
    "zero" = get_zero_dividend_stocks(max_workers = max_workers),
    get_sp500_stocks()  # default "all"
  )

  if (length(stock_universe) == 0) {
    stop("No stocks found in S&P 500. This may indicate an issue with data fetching.")
  }

  # Apply limit if specified
  if (!is.null(limit)) {
    log_info("Limiting analysis to first {limit} stocks (testing mode)")
    stock_universe <- head(stock_universe, limit)
  }

  log_info("Analyzing {length(stock_universe)} stocks...")

  # Get reinvestment rate
  reinvest_rate <- get_reinvestment_rate()
  log_info("SGOV yield for reinvestment: {sprintf('%.2f%%', reinvest_rate * 100)}")

  # Setup parallel processing
  oplan <- setup_parallel_processing(max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Capture quote source setting to pass to workers
  quote_source <- get_quote_source()
  log_info("Quote source for this analysis: {toupper(quote_source)}")

  # Process stocks in parallel
  log_info("Processing stocks in parallel (workers: {max_workers})...")

  results <- future_map(stock_universe, function(ticker) {
    if (!"investR" %in% loadedNamespaces()) {
      suppressPackageStartupMessages(loadNamespace("investR"))
    }

    # Set quote source in this worker to match main process
    options(investR.quote_source = quote_source)

    investR::analyze_collar_single(ticker, target_days, strike_adjustment_pct)
  }, .options = furrr_options(seed = TRUE, packages = "investR"))

  # Filter out NULLs and combine
  results_df <- compact(results) %>% bind_rows()

  # Sort by annualized return
  if (nrow(results_df) > 0) {
    results_df <- results_df %>% arrange(desc(annualized_return))
  }

  # Log completion
  log_analysis_footer(nrow(results_df))

  return(results_df)
}

#' Analyze liquid ETFs for collar opportunities
#'
#' @param min_market_cap Minimum market capitalization (default from config)
#' @param min_avg_volume Minimum average daily volume (default from config)
#' @param target_days Target days to expiry (finds closest match, default 300)
#' @param strike_adjustment_pct Strike adjustment as decimal (0 = ATM, 0.05 = 5% above)
#' @param limit Optional limit on number of ETFs to analyze (for testing)
#' @param max_workers Number of parallel workers
#' @return Tibble with all opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze all liquid ETFs
#'   results <- analyze_collar_etfs()
#'
#'   # Analyze with custom parameters
#'   results <- analyze_collar_etfs(target_days = 180, strike_adjustment_pct = 0.05)
#'
#'   # Analyze with custom filters
#'   results <- analyze_collar_etfs(min_market_cap = 5e9, min_avg_volume = 5e6)
#' }
analyze_collar_etfs <- function(min_market_cap = COLLAR_CONFIG$min_market_cap,
                                min_avg_volume = COLLAR_CONFIG$min_avg_volume,
                                target_days = 300,
                                strike_adjustment_pct = 0,
                                limit = NULL,
                                max_workers = COLLAR_CONFIG$max_workers) {

  log_analysis_header_generic("Collar Strategy - Liquid ETFs")

  # Get ETF universe
  etf_universe <- get_liquid_etfs(min_market_cap, min_avg_volume)

  if (length(etf_universe) == 0) {
    stop("No ETFs found meeting liquidity criteria.")
  }

  # Apply limit if specified
  if (!is.null(limit)) {
    log_info("Limiting analysis to first {limit} ETFs (testing mode)")
    etf_universe <- head(etf_universe, limit)
  }

  log_info("Analyzing {length(etf_universe)} ETFs...")

  # Get reinvestment rate
  reinvest_rate <- get_reinvestment_rate()
  log_info("SGOV yield for reinvestment: {sprintf('%.2f%%', reinvest_rate * 100)}")

  # Setup parallel processing
  oplan <- setup_parallel_processing(max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Capture quote source setting to pass to workers
  quote_source <- get_quote_source()
  log_info("Quote source for this analysis: {toupper(quote_source)}")

  # Process ETFs in parallel
  log_info("Processing ETFs in parallel (workers: {max_workers})...")

  results <- future_map(etf_universe, function(ticker) {
    if (!"investR" %in% loadedNamespaces()) {
      suppressPackageStartupMessages(loadNamespace("investR"))
    }

    # Set quote source in this worker to match main process
    options(investR.quote_source = quote_source)

    investR::analyze_collar_single(ticker, target_days, strike_adjustment_pct)
  }, .options = furrr_options(seed = TRUE, packages = "investR"))

  # Filter out NULLs and combine
  results_df <- compact(results) %>% bind_rows()

  # Sort by annualized return
  if (nrow(results_df) > 0) {
    results_df <- results_df %>% arrange(desc(annualized_return))
  }

  # Log completion
  log_analysis_footer(nrow(results_df))

  return(results_df)
}

#' Analyze custom ticker list for collar opportunities
#'
#' @param list_type Type of custom list: "overbought", "oversold", or "most_shorted"
#' @param target_days Target days to expiry (finds closest match, default 300)
#' @param strike_adjustment_pct Strike adjustment as decimal (0 = ATM, 0.05 = 5% above)
#' @param max_workers Number of parallel workers
#' @return Tibble with all opportunities sorted by annualized return
#' @export
#' @examples
#' \dontrun{
#'   # Analyze overbought stocks
#'   results <- analyze_collar_custom_list("overbought")
#'
#'   # Analyze with custom parameters
#'   results <- analyze_collar_custom_list("most_shorted", target_days = 180)
#' }
analyze_collar_custom_list <- function(list_type,
                                       target_days = 300,
                                       strike_adjustment_pct = 0,
                                       max_workers = COLLAR_CONFIG$max_workers) {

  # Get display name for logging
  list_name <- switch(list_type,
    "overbought" = "Overbought Stocks",
    "oversold" = "Oversold Stocks",
    "most_shorted" = "Most Shorted Stocks",
    "leveraged_2x" = "2x Leveraged ETFs",
    "leveraged_3x" = "3x Leveraged ETFs",
    stop("Invalid list_type. Must be 'overbought', 'oversold', 'most_shorted', 'leveraged_2x', or 'leveraged_3x'")
  )

  log_analysis_header_generic(paste("Collar Strategy -", list_name))

  # Fetch ticker list (web scraping with caching)
  ticker_list <- switch(list_type,
    "overbought" = fetch_overbought_tickers(),
    "oversold" = fetch_oversold_tickers(),
    "most_shorted" = fetch_most_shorted_tickers(),
    "leveraged_2x" = fetch_2x_leveraged_etfs(),
    "leveraged_3x" = fetch_3x_leveraged_etfs()
  )

  if (length(ticker_list) == 0) {
    log_warn("No tickers found for {list_name}")
    return(tibble())
  }

  log_info("Analyzing {length(ticker_list)} stocks...")

  # Get reinvestment rate
  reinvest_rate <- get_reinvestment_rate()
  log_info("SGOV yield for reinvestment: {sprintf('%.2f%%', reinvest_rate * 100)}")

  # Setup parallel processing
  oplan <- setup_parallel_processing(max_workers)
  on.exit(plan(oplan), add = TRUE)

  # Capture quote source setting to pass to workers
  quote_source <- get_quote_source()
  log_info("Quote source for this analysis: {toupper(quote_source)}")

  # Process stocks in parallel
  log_info("Processing stocks in parallel (workers: {max_workers})...")

  results <- future_map(ticker_list, function(ticker) {
    if (!"investR" %in% loadedNamespaces()) {
      suppressPackageStartupMessages(loadNamespace("investR"))
    }

    # Set quote source in this worker to match main process
    options(investR.quote_source = quote_source)

    investR::analyze_collar_single(ticker, target_days, strike_adjustment_pct)
  }, .options = furrr_options(seed = TRUE, packages = "investR"))

  # Filter out NULLs and combine
  results_df <- compact(results) %>% bind_rows()

  # Sort by annualized return
  if (nrow(results_df) > 0) {
    results_df <- results_df %>% arrange(desc(annualized_return))
  }

  # Log completion
  log_analysis_footer(nrow(results_df))

  return(results_df)
}

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Get reinvestment rate for dividend reinvestment
#'
#' Fetches current SGOV yield or uses fallback
#'
#' @return Annual yield as decimal (0.05 = 5%)
#' @noRd
get_reinvestment_rate <- function() {
  rate <- tryCatch({
    sgov <- fetch_current_quote("SGOV", fields = "Dividend Yield")
    yield <- as.numeric(sgov$`Dividend Yield`)

    if (!is.na(yield) && yield > 0 && yield < COLLAR_CONFIG$max_sgov_yield_sanity) {
      return(yield)
    }

    COLLAR_CONFIG$sgov_yield_default
  }, error = function(e) {
    COLLAR_CONFIG$sgov_yield_default
  })

  return(rate)
}

#' Truncate error messages for logging
#'
#' @param message Error message
#' @param max_length Maximum length (default from config)
#' @return Truncated message
#' @noRd
truncate_error <- function(message, max_length = COLLAR_CONFIG$error_truncate_length) {
  if (nchar(message) > max_length) {
    paste0(substr(message, 1, max_length), "...")
  } else {
    message
  }
}

################################################################################
# IV SKEW SCREENER FUNCTIONS
################################################################################

#' Fetch IWB (iShares Russell 1000 ETF) holdings
#'
#' Downloads and parses the iShares Russell 1000 ETF holdings CSV, returning
#' equity ticker symbols only. Falls back to the cached CSV at
#' inst/cache/IWB_holdings.csv on download failure.
#'
#' @return Character vector of ticker symbols
#' @export
#' @examples
#' \dontrun{
#'   tickers <- fetch_iwb_holdings()
#' }
fetch_iwb_holdings <- function() {
  fallback_path <- system.file("cache", "IWB_holdings.csv", package = "investR")
  if (!nzchar(fallback_path)) {
    fallback_path <- file.path("inst", "cache", "IWB_holdings.csv")
  }

  parse_iwb_csv <- function(path) {
    df <- suppressWarnings(readr::read_csv(
      path,
      skip = 9,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      name_repair = "minimal"
    ))
    # Keep only rows where Asset Class is Equity
    equity_rows <- df[!is.na(df[["Asset Class"]]) & df[["Asset Class"]] == "Equity", ]
    tickers <- equity_rows[["Ticker"]]
    tickers <- tickers[!is.na(tickers) & nzchar(trimws(tickers))]
    trimws(tickers)
  }

  # Try to download fresh CSV
  holdings_url <- COLLAR_CONFIG$iv_skew_holdings_url
  tickers <- tryCatch({
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    httr::GET(
      holdings_url,
      httr::user_agent("Mozilla/5.0"),
      httr::write_disk(tmp, overwrite = TRUE),
      httr::timeout(30)
    )
    result <- parse_iwb_csv(tmp)
    if (length(result) == 0) stop("Parsed empty ticker list from downloaded CSV")
    log_info("Fetched {length(result)} Russell 1000 equity tickers from iShares")
    result
  }, error = function(e) {
    log_warn("IWB holdings download failed ({e$message}); falling back to cached CSV")
    if (!file.exists(fallback_path)) {
      stop("IWB holdings download failed and no fallback CSV found at: ", fallback_path)
    }
    result <- parse_iwb_csv(fallback_path)
    if (length(result) == 0) stop("Fallback CSV parsed empty ticker list")
    log_info("Loaded {length(result)} Russell 1000 equity tickers from fallback cache")
    result
  })

  tickers
}

#' Compute IV skew ratio for a single ticker
#'
#' Fetches ATM call IV and ATM put IV at the closest expiry to 45 days from
#' today (within the 45-60 day window) and returns their ratio.
#'
#' @param ticker Stock ticker symbol
#' @return List with ticker, call_iv, put_iv, iv_ratio, current_price,
#'   expiry_date; or NULL if no valid data found
#' @export
#' @examples
#' \dontrun{
#'   result <- compute_iv_skew_ratio("AAPL")
#' }
compute_iv_skew_ratio <- function(ticker) {
  tryCatch({
    screening_target <- COLLAR_CONFIG$iv_skew_screening_target_days
    screening_max <- COLLAR_CONFIG$iv_skew_screening_max_days

    # 1. Get current price
    quote <- fetch_current_quote(ticker, fields = c("Last Trade (Price Only)"))
    if (is.null(quote)) {
      log_debug("{ticker}: No quote data, skipping")
      return(NULL)
    }
    current_price <- as.numeric(quote$Last)
    if (is.na(current_price) || current_price <= 0) {
      log_debug("{ticker}: No valid price, skipping")
      return(NULL)
    }

    # 2. Get options structure to find expiry dates
    opt_structure <- fetch_options_chain(ticker, expiration = NULL)
    if (is.null(opt_structure) || length(opt_structure) == 0) {
      log_debug("{ticker}: No options chain available, skipping")
      return(NULL)
    }

    # 3. Find expiry closest to screening_target days, within [screening_target, screening_max]
    today <- Sys.Date()
    expiry_dates <- as.Date(names(opt_structure), format = "%b.%d.%Y")
    days_to_expiry_vec <- as.integer(difftime(expiry_dates, today, units = "days"))

    valid_idx <- which(
      !is.na(days_to_expiry_vec) &
      days_to_expiry_vec >= screening_target &
      days_to_expiry_vec <= screening_max
    )

    if (length(valid_idx) == 0) {
      log_debug("{ticker}: No expiry in {screening_target}-{screening_max} day window, skipping")
      return(NULL)
    }

    # Pick closest to target within window
    window_days <- days_to_expiry_vec[valid_idx]
    best_in_window <- valid_idx[which.min(abs(window_days - screening_target))]
    selected_expiry_str <- names(opt_structure)[best_in_window]

    # 4. Extract ATM call and put from the options chain
    exp_data <- opt_structure[[selected_expiry_str]]
    if (is.null(exp_data)) {
      log_debug("{ticker}: No data for expiry {selected_expiry_str}, skipping")
      return(NULL)
    }

    calls <- exp_data$calls
    puts  <- exp_data$puts

    if (is.null(calls) || nrow(calls) == 0 || is.null(puts) || nrow(puts) == 0) {
      log_debug("{ticker}: Empty calls or puts at {selected_expiry_str}, skipping")
      return(NULL)
    }

    # Find ATM strike
    valid_strikes <- unique(calls$Strike[!is.na(calls$Strike)])
    if (length(valid_strikes) == 0) {
      log_debug("{ticker}: No valid strikes, skipping")
      return(NULL)
    }
    atm_strike <- valid_strikes[which.min(abs(valid_strikes - current_price))]

    atm_call <- calls[calls$Strike == atm_strike, ]
    atm_put  <- puts[puts$Strike == atm_strike, ]

    if (nrow(atm_call) == 0 || nrow(atm_put) == 0) {
      log_debug("{ticker}: ATM call or put not found at strike {atm_strike}, skipping")
      return(NULL)
    }

    # 5. Extract IV values (column may be named IV or impliedVolatility)
    get_iv <- function(opt_row) {
      if ("IV" %in% names(opt_row)) return(as.numeric(opt_row$IV[1]))
      if ("impliedVolatility" %in% names(opt_row)) return(as.numeric(opt_row$impliedVolatility[1]))
      NA_real_
    }

    call_iv <- get_iv(atm_call)
    put_iv  <- get_iv(atm_put)

    if (is.na(call_iv) || is.na(put_iv) || put_iv == 0) {
      log_debug("{ticker}: Invalid IV (call_iv={call_iv}, put_iv={put_iv}), skipping")
      return(NULL)
    }

    iv_ratio <- call_iv / put_iv

    list(
      ticker        = ticker,
      call_iv       = call_iv,
      put_iv        = put_iv,
      iv_ratio      = iv_ratio,
      current_price = current_price,
      expiry_date   = as.character(as.Date(selected_expiry_str, format = "%b.%d.%Y"))
    )
  }, error = function(e) {
    log_debug("{ticker}: Error computing IV skew - {truncate_error(e$message)}")
    return(NULL)
  })
}

#' Analyze Russell 1000 universe for IV skew collar opportunities
#'
#' Screens the Russell 1000 equity universe for stocks where ATM call IV is
#' high relative to ATM put IV, then runs standard collar analysis on the top N
#' results.
#'
#' @param strike_adjustment_pct Strike adjustment as decimal (0 = ATM)
#' @param max_workers Number of parallel workers
#' @return Tibble with collar opportunities sorted by annualized_return
#' @export
#' @examples
#' \dontrun{
#'   results <- analyze_collar_iv_skew(strike_adjustment_pct = 0)
#' }
analyze_collar_iv_skew <- function(strike_adjustment_pct = 0,
                                   max_workers = COLLAR_CONFIG$max_workers) {

  log_analysis_header_generic("Collar Strategy - IV Skew Screener (Russell 1000)")

  # 1. Fetch holdings universe
  log_info("Fetching Russell 1000 holdings...")
  ticker_universe <- fetch_iwb_holdings()
  log_info("Screening {length(ticker_universe)} tickers for IV skew...")

  # 2. Setup parallel processing
  oplan <- setup_parallel_processing(max_workers)
  on.exit(plan(oplan), add = TRUE)

  quote_source <- get_quote_source()

  # 3. Compute IV skew ratio for each ticker in parallel
  skew_results <- future_map(ticker_universe, function(ticker) {
    if (!"investR" %in% loadedNamespaces()) {
      suppressPackageStartupMessages(loadNamespace("investR"))
    }
    options(investR.quote_source = quote_source)
    investR::compute_iv_skew_ratio(ticker)
  }, .options = furrr_options(seed = TRUE, packages = "investR"))

  skew_results <- compact(skew_results)
  log_info("{length(skew_results)} tickers had valid IV data out of {length(ticker_universe)} screened")

  if (length(skew_results) == 0) {
    log_warn("No tickers returned valid IV skew data")
    return(tibble::tibble())
  }

  # 4. Sort by iv_ratio descending, take top N
  top_n <- COLLAR_CONFIG$iv_skew_top_n
  skew_df <- dplyr::bind_rows(
    lapply(skew_results, function(x) {
      tibble::tibble(
        ticker        = x$ticker,
        call_iv       = x$call_iv,
        put_iv        = x$put_iv,
        iv_ratio      = x$iv_ratio,
        current_price = x$current_price,
        expiry_date   = x$expiry_date
      )
    })
  )
  skew_df <- dplyr::arrange(skew_df, dplyr::desc(iv_ratio))
  top_tickers <- head(skew_df$ticker, top_n)

  log_info("Top {length(top_tickers)} tickers by IV skew ratio selected for collar analysis")

  # 5. Run collar analysis on top N in parallel
  results <- future_map(top_tickers, function(ticker) {
    if (!"investR" %in% loadedNamespaces()) {
      suppressPackageStartupMessages(loadNamespace("investR"))
    }
    options(investR.quote_source = quote_source)
    investR::analyze_collar_single(ticker, COLLAR_CONFIG$iv_skew_screening_target_days, strike_adjustment_pct)
  }, .options = furrr_options(seed = TRUE, packages = "investR"))

  results_df <- compact(results) %>% bind_rows()

  if (nrow(results_df) > 0) {
    results_df <- results_df %>% arrange(desc(annualized_return))
  }

  log_info("{nrow(results_df)} collar opportunities found among top {length(top_tickers)} IV-skew stocks")
  log_analysis_footer(nrow(results_df))

  return(results_df)
}
