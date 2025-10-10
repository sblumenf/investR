#' Aristocrats Strategy Analysis Functions
#'
#' Core business logic for fetching data, calculating metrics,
#' and analyzing dividend aristocrats covered call opportunities
#'
#' @name aristocrats-analysis
#' @import dplyr
#' @importFrom purrr map_dfr compact possibly
#' @importFrom tibble tibble
#' @importFrom stringr str_extract
#' @importFrom rvest read_html html_nodes html_attr html_table
#' @importFrom quantmod Cl dailyReturn
#' @importFrom zoo index
#' @importFrom lubridate years
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multisession
#' @importFrom logger log_info log_warn log_success log_debug log_error
#' @importFrom utils head tail
#' @importFrom stats na.omit
NULL

################################################################################
# METRICS CALCULATION FUNCTIONS
################################################################################

#' Calculate dividend projections for holding period
#'
#' Projects future dividend payments based on historical payment frequency
#'
#' @param dividends xts object with dividend history
#' @param days_to_expiry Number of days until option expiration
#' @param reinvest_rate Reinvestment rate for dividend income
#' @return List with dividend_income and reinvestment_income
#' @noRd
calculate_dividend_projections <- function(dividends,
                                          days_to_expiry,
                                          reinvest_rate = get_reinvestment_rate()) {

  # Default values
  dividend_income <- 0
  reinvestment_income <- 0

  # Note: For empty xts objects, nrow() returns NULL, so check length() first
  if (is.null(dividends) || length(dividends) == 0 || nrow(dividends) < 2) {
    return(list(
      dividend_income = 0,
      reinvestment_income = 0
    ))
  }

  # Get latest dividend amount
  latest_dividend <- as.numeric(tail(dividends, 1))

  # Calculate average days between payments from last 6 dividends
  # Safe to use nrow() here because we already checked length() > 0 above
  recent_divs <- tail(dividends, min(6, nrow(dividends)))

  if (!is.null(recent_divs) && length(recent_divs) >= 2 && nrow(recent_divs) >= 2) {
    div_dates <- index(recent_divs)
    days_between <- as.numeric(diff(div_dates))
    avg_days_between <- mean(days_between)

    # Project dividend payments during holding period
    expected_payments <- days_to_expiry / avg_days_between
    dividend_income <- latest_dividend * expected_payments * ARISTOCRATS_CONFIG$shares_per_contract

    # Calculate reinvestment income
    # Assume dividends received evenly throughout holding period
    avg_days_to_invest <- days_to_expiry / 2
    years_to_invest <- avg_days_to_invest / ARISTOCRATS_CONFIG$days_per_year
    reinvestment_income <- dividend_income * ((1 + reinvest_rate)^years_to_invest - 1)
  }

  list(
    dividend_income = dividend_income,
    reinvestment_income = reinvestment_income
  )
}

#' Calculate option value components
#'
#' @param current_price Current stock price
#' @param strike Option strike price
#' @param bid_price Option bid price
#' @return List with intrinsic_value and extrinsic_value
#' @noRd
calculate_option_values <- function(current_price, strike, bid_price) {
  validate_price(current_price, "current_price")
  validate_price(strike, "strike")
  validate_price(bid_price, "bid_price")

  intrinsic_value <- max(0, current_price - strike)
  extrinsic_value <- bid_price - intrinsic_value

  list(
    intrinsic_value = intrinsic_value,
    extrinsic_value = extrinsic_value
  )
}

#' Calculate protection metrics
#'
#' @param current_price Current stock price
#' @param bid_price Option bid price
#' @return List with breakeven_price and downside_protection_pct
#' @noRd
calculate_protection_metrics <- function(current_price, bid_price) {
  validate_price(current_price, "current_price")
  validate_price(bid_price, "bid_price")

  breakeven_price <- current_price - bid_price
  downside_protection_pct <- (current_price - breakeven_price) / current_price

  list(
    breakeven_price = breakeven_price,
    downside_protection_pct = downside_protection_pct
  )
}

#' Calculate return metrics
#'
#' @param net_profit Total profit amount
#' @param net_outlay Capital required (after premium received)
#' @param days_to_expiry Days until expiration
#' @return List with total_return and annualized_return
#' @noRd
calculate_return_metrics <- function(net_profit, net_outlay, days_to_expiry) {
  if (!is.numeric(net_outlay)) stop("net_outlay must be numeric")
  if (days_to_expiry <= 0) stop("days_to_expiry must be positive")

  total_return <- if (net_outlay > 0) net_profit / net_outlay else 0
  annualized_return <- calculate_annualized_return(
    total_return = total_return,
    days = days_to_expiry,
    days_per_year = ARISTOCRATS_CONFIG$days_per_year
  )

  list(
    total_return = total_return,
    annualized_return = annualized_return
  )
}

#' Calculate cash flows for covered call position
#'
#' @param current_price Current stock price
#' @param strike Option strike price
#' @param bid_price Option bid price
#' @param dividend_income Projected dividend income
#' @param reinvestment_income Projected reinvestment income
#' @return List with all cash flow components
#' @noRd
calculate_cash_flows <- function(current_price, strike, bid_price,
                                dividend_income, reinvestment_income) {

  investment <- current_price * ARISTOCRATS_CONFIG$shares_per_contract
  premium_received <- bid_price * ARISTOCRATS_CONFIG$shares_per_contract
  net_outlay <- investment - premium_received
  exercise_proceeds <- strike * ARISTOCRATS_CONFIG$shares_per_contract

  # Net profit = all income minus what we paid
  net_profit <- premium_received + dividend_income +
                reinvestment_income + exercise_proceeds - investment

  list(
    investment = investment,
    premium_received = premium_received,
    net_outlay = net_outlay,
    exercise_proceeds = exercise_proceeds,
    net_profit = net_profit
  )
}

#' Calculate all covered call metrics (orchestrator)
#'
#' Comprehensive metric calculation that delegates to specialized functions
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @param option_row Selected option (tibble row)
#' @param stock_data Stock data list from get_stock_data()
#' @param warning_flag Warning flag from option selection
#' @return Tibble row with all calculated metrics
#' @noRd
calculate_metrics <- function(ticker, current_price, option_row,
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
  option_vals <- calculate_option_values(current_price, strike, bid_price)

  dividend_proj <- calculate_dividend_projections(
    stock_data$dividends,
    days_to_expiry
  )

  cash_flows <- calculate_cash_flows(
    current_price, strike, bid_price,
    dividend_proj$dividend_income,
    dividend_proj$reinvestment_income
  )

  return_metrics <- calculate_return_metrics(
    cash_flows$net_profit,
    cash_flows$net_outlay,
    days_to_expiry
  )

  protection <- calculate_protection_metrics(current_price, bid_price)

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
    investment = cash_flows$investment,
    premium_received = cash_flows$premium_received,
    dividend_income = dividend_proj$dividend_income,
    reinvestment_income = dividend_proj$reinvestment_income,
    exercise_proceeds = cash_flows$exercise_proceeds,
    net_profit = cash_flows$net_profit,
    net_outlay = cash_flows$net_outlay,
    # Returns
    total_return = return_metrics$total_return,
    annualized_return = return_metrics$annualized_return,
    # Risk metrics
    max_drawdown = stock_data$max_drawdown,
    current_yield = stock_data$current_yield,
    # Protection
    breakeven_price = protection$breakeven_price,
    downside_protection_pct = protection$downside_protection_pct,
    # Additional values
    intrinsic_value = option_vals$intrinsic_value,
    extrinsic_value = option_vals$extrinsic_value,
    annual_dividend = stock_data$annual_dividend,
    # Flags
    warning_flag = warning_flag,
    is_aristocrat = TRUE
  )
}

################################################################################
# DATA FETCHING FUNCTIONS
################################################################################

#' Fetch Dividend Aristocrats with fallback sources
#'
#' @return Character vector of ticker symbols
#' @noRd
get_dividend_aristocrats <- function() {
  log_info("Fetching Dividend Aristocrats list...")

  # Try 1: StockAnalysis.com
  aristocrats <- tryCatch({
    url <- ARISTOCRATS_CONFIG$urls$stockanalysis
    page <- read_html(url)

    ticker_links <- page %>%
      html_nodes("a[href*='/stocks/']") %>%
      html_attr("href")

    tickers <- ticker_links %>%
      str_extract("/stocks/([A-Z\\.]+)/", group = 1) %>%
      na.omit() %>%
      unique() %>%
      as.character()

    if (length(tickers) >= ARISTOCRATS_CONFIG$min_aristocrats) {
      log_success("Found {length(tickers)} Dividend Aristocrats from StockAnalysis.com")
      return(tickers)
    }

    NULL
  }, error = function(e) {
    log_warn("StockAnalysis.com failed: {e$message}")
    NULL
  })

  if (!is.null(aristocrats)) return(aristocrats)

  # Try 2: Wikipedia
  aristocrats <- tryCatch({
    url <- ARISTOCRATS_CONFIG$urls$wikipedia
    tables <- read_html(url) %>% html_table(fill = TRUE)

    for (table in tables) {
      ticker_col <- find_ticker_column(names(table))

      if (!is.null(ticker_col)) {
        tickers <- table[[ticker_col]] %>%
          na.omit() %>%
          unique() %>%
          as.character()

        if (length(tickers) >= ARISTOCRATS_CONFIG$min_aristocrats) {
          log_success("Found {length(tickers)} Dividend Aristocrats from Wikipedia (fallback)")
          return(tickers)
        }
      }
    }

    NULL
  }, error = function(e) {
    log_error("Wikipedia failed: {e$message}")
    NULL
  })

  if (is.null(aristocrats)) {
    stop("ERROR: Unable to fetch Dividend Aristocrats from any source")
  }

  aristocrats
}

#' Get comprehensive stock data for a ticker
#'
#' @param ticker Stock ticker symbol
#' @return List with price, history, dividends, max_drawdown, current_yield
#' @noRd
get_stock_data <- function(ticker) {
  validate_ticker(ticker)

  tryCatch({
    # Get current price and info
    quote <- fetch_current_quote(ticker, fields = c("Last Trade (Price Only)", "Name"))
    current_price <- as.numeric(quote$Last)
    company_name <- as.character(quote$Name)

    if (is.na(current_price) || current_price <= 0) {
      log_debug("{ticker}: Invalid price data")
      return(NULL)
    }

    validate_price(current_price, "current_price")

    # Get history for max drawdown
    end_date <- Sys.Date()
    start_date <- end_date - years(ARISTOCRATS_CONFIG$history_years)

    history <- fetch_price_history(ticker,
                                   from = start_date,
                                   to = end_date,
                                   auto_adjust = FALSE)

    if (is.null(history) || nrow(history) == 0) {
      log_debug("{ticker}: No price history available")
      return(NULL)
    }

    # Calculate max drawdown
    close_prices <- Cl(history)
    max_drawdown <- calculate_max_drawdown(close_prices)

    # Get dividends
    dividends <- fetch_dividend_history(ticker, from = start_date, to = end_date)

    # Calculate current yield
    current_yield <- 0
    annual_dividend <- 0
    # Note: For empty xts objects, nrow() returns NULL, so check length() first
    if (!is.null(dividends) && length(dividends) > 0 && nrow(dividends) >= ARISTOCRATS_CONFIG$min_dividend_quarters) {
      recent_divs <- tail(dividends, ARISTOCRATS_CONFIG$min_dividend_quarters)
      annual_dividend <- sum(recent_divs)
      current_yield <- annual_dividend / current_price
    }

    list(
      ticker = ticker,
      current_price = current_price,
      company_name = company_name,
      history = history,
      dividends = dividends,
      max_drawdown = max_drawdown,
      current_yield = current_yield,
      annual_dividend = annual_dividend
    )

  }, error = function(e) {
    log_warn("{ticker}: {truncate_error(e$message)}")
    NULL
  })
}

#' Get options chain for all available expirations
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @return Tibble with all ITM call options across expirations
#' @noRd
get_options_chain <- function(ticker, current_price) {
  validate_ticker(ticker)
  validate_price(current_price, "current_price")

  tryCatch({
    log_debug("Fetching options chain for {ticker}...")

    opt_chain <- fetch_options_chain(ticker, expiration = NULL)

    if (is.null(opt_chain) || length(opt_chain) == 0) {
      return(tibble())
    }

    log_debug("Found {length(opt_chain)} expiration dates for {ticker}")

    # Process each expiration with error handling
    process_exp <- possibly(function(exp_date) {
      exp_data <- opt_chain[[exp_date]]

      if (is.null(exp_data) || !"calls" %in% names(exp_data)) {
        return(tibble())
      }

      calls <- exp_data$calls

      if (is.null(calls) || nrow(calls) == 0) {
        return(tibble())
      }

      # Filter for ITM calls with valid bids
      itm_calls <- calls %>%
        filter(Strike < current_price | ITM == TRUE) %>%
        filter(!is.na(Bid) & Bid > ARISTOCRATS_CONFIG$min_option_bid) %>%
        mutate(
          expiration = as.Date(exp_date, format = "%b.%d.%Y"),
          days_to_expiry = as.integer(difftime(expiration, Sys.Date(), units = "days")),
          time_value = Bid - pmax(current_price - Strike, 0)
        )

      itm_calls
    }, otherwise = tibble())

    all_options <- map_dfr(names(opt_chain), process_exp)
    all_options

  }, error = function(e) {
    log_warn("{ticker}: Error fetching options - {truncate_error(e$message)}")
    tibble()
  })
}

#' Select optimal ITM call option based on strategy parameters
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @param options_df Tibble of available options
#' @param strike_threshold_pct Maximum strike as % of current price
#' @param min_days Minimum days to expiry (NULL = no minimum)
#' @param max_days Maximum days to expiry (NULL = longest available)
#' @param expiry_month Optional month number (1-12) to filter expiration dates
#' @param target_days Target days to expiry (NULL = use longest available, numeric = find closest match)
#' @return List with optimal option row and warning_flag
#' @noRd
select_optimal_option <- function(ticker, current_price, options_df,
                                 strike_threshold_pct = ARISTOCRATS_CONFIG$strike_threshold_pct,
                                 min_days = NULL,
                                 max_days = NULL,
                                 expiry_month = NULL,
                                 target_days = NULL) {

  validate_ticker(ticker)
  validate_price(current_price, "current_price")
  validate_percentage(strike_threshold_pct, "strike_threshold_pct")
  if (!is.data.frame(options_df)) stop("options_df must be a tibble")

  warning_flag <- FALSE

  # Calculate strike threshold
  strike_threshold <- current_price * strike_threshold_pct

  # Filter by strike
  filtered_options <- options_df %>%
    filter(Strike <= strike_threshold)

  # Filter by minimum open interest
  min_oi <- tryCatch(ARISTOCRATS_CONFIG$min_open_interest, error = function(e) 0)
  if (min_oi > 0) {
    filtered_options <- filtered_options %>%
      filter(OI >= min_oi)

    if (nrow(filtered_options) == 0) {
      log_debug("{ticker}: No options with OI >= {min_oi}")
      return(NULL)
    }
  }

  # Filter by expiry month if specified
  if (!is.null(expiry_month)) {
    filtered_options <- filtered_options %>%
      filter(lubridate::month(expiration) == expiry_month)

    if (nrow(filtered_options) == 0) {
      log_debug("{ticker}: No options expiring in month {expiry_month}")
      return(NULL)
    }
  }

  # Filter by days to expiry range
  if (!is.null(min_days)) {
    filtered_options <- filtered_options %>%
      filter(days_to_expiry >= min_days)

    if (nrow(filtered_options) == 0) {
      log_debug("{ticker}: No options with >= {min_days} days to expiry")
      return(NULL)
    }
  }

  if (!is.null(max_days)) {
    filtered_options <- filtered_options %>%
      filter(days_to_expiry <= max_days)

    if (nrow(filtered_options) == 0) {
      log_debug("{ticker}: No options with <= {max_days} days to expiry")
      return(NULL)
    }
  }

  # Fallback if no options meet threshold
  if (nrow(filtered_options) == 0) {
    filtered_options <- options_df
    warning_flag <- TRUE
  }

  if (nrow(filtered_options) == 0) {
    return(NULL)
  }

  # Select optimal option based on target_days or longest dated
  if (!is.null(target_days)) {
    # Dynamic strategy: Find option closest to target_days
    optimal_option <- filtered_options %>%
      mutate(days_diff = abs(days_to_expiry - target_days)) %>%
      arrange(days_diff, desc(OI)) %>%
      slice(1)
  } else {
    # Original strategy: Select longest dated option (prioritize time value)
    optimal_option <- filtered_options %>%
      arrange(desc(expiration), desc(OI)) %>%
      slice(1)
  }

  # Log concerning selections
  if (optimal_option$days_to_expiry < ARISTOCRATS_CONFIG$short_expiry_warning_days) {
    log_warn("{ticker}: Selected {optimal_option$days_to_expiry}-day option (only {nrow(filtered_options)} deep ITM options available)")
  }

  list(
    option = optimal_option,
    warning_flag = warning_flag
  )
}

################################################################################
# ANALYSIS FUNCTIONS
################################################################################

#' Setup parallel processing environment
#'
#' @param max_workers Number of parallel workers
#' @return Original plan (for cleanup via on.exit)
#' @noRd
#' @importFrom future plan multisession
setup_parallel_processing <- function(max_workers) {
  oplan <- plan(multisession, workers = max_workers)
  return(oplan)
}

#' Finalize analysis results
#'
#' Combines parallel results, removes NULLs, and sorts by annualized return.
#' Returns properly structured empty tibble with expected schema when no results.
#'
#' @param results List of results from parallel processing
#' @return Tibble with sorted results (or empty tibble with correct schema)
#' @noRd
#' @importFrom purrr compact
#' @importFrom dplyr bind_rows arrange desc
#' @importFrom tibble tibble
finalize_results <- function(results) {
  results_df <- results %>%
    compact() %>%
    bind_rows()

  # If no results, return properly structured empty tibble with expected schema
  # This ensures UI filtering code doesn't crash when accessing columns like 'expiration'
  if (nrow(results_df) == 0) {
    return(tibble(
      ticker = character(),
      company_name = character(),
      current_price = numeric(),
      strike = numeric(),
      expiration = character(),
      days_to_expiry = numeric(),
      bid_price = numeric(),
      open_interest = numeric(),
      investment = numeric(),
      premium_received = numeric(),
      dividend_income = numeric(),
      reinvestment_income = numeric(),
      exercise_proceeds = numeric(),
      net_profit = numeric(),
      net_outlay = numeric(),
      total_return = numeric(),
      annualized_return = numeric(),
      max_drawdown = numeric(),
      current_yield = numeric(),
      breakeven_price = numeric(),
      downside_protection_pct = numeric(),
      intrinsic_value = numeric(),
      extrinsic_value = numeric(),
      annual_dividend = numeric(),
      warning_flag = logical()
    ))
  }

  # Sort by annualized return (descending)
  results_df <- results_df %>%
    arrange(desc(annualized_return))

  return(results_df)
}

#' Log analysis completion footer
#'
#' @param n_results Number of successful results
#' @noRd
#' @importFrom logger log_info
log_analysis_footer <- function(n_results) {
  log_info("")
  log_info(strrep("=", 60))
  log_info("Analysis complete. {n_results} stocks analyzed successfully.")
  log_info(strrep("=", 60))
}

#' Analyze dividend aristocrats for covered call opportunities
#'
#' Analyzes dividend aristocrats for deep ITM covered call opportunities.
#' Uses shared generic analysis functions for processing.
#'
#' @param limit Optional limit on number of stocks to analyze
#' @param strike_threshold_pct Maximum strike as % of current price
#' @param target_days Target days to expiry (NULL = longest available)
#' @param max_workers Number of parallel workers
#' @return Tibble with all opportunities sorted by annualized return
#' @export
analyze_aristocrats <- function(limit = NULL,
                               strike_threshold_pct = ARISTOCRATS_CONFIG$strike_threshold_pct,
                               target_days = ARISTOCRATS_CONFIG$target_days,
                               max_workers = ARISTOCRATS_CONFIG$max_workers) {

  # Get aristocrats list (strategy-specific)
  aristocrats <- get_dividend_aristocrats()

  # Apply limit if specified
  if (!is.null(limit)) {
    aristocrats <- head(aristocrats, limit)
    log_info("Limiting analysis to first {limit} stocks")
  }

  # Call generic covered call analyzer
  analyze_covered_calls_generic(
    stock_universe = aristocrats,
    strategy_name = "Dividend Aristocrats",
    strike_threshold_pct = strike_threshold_pct,
    target_days = target_days,
    max_workers = max_workers,
    result_flags = list(is_aristocrat = TRUE)
  )
}