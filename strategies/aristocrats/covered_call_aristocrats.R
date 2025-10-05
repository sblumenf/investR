#!/usr/bin/env Rscript
################################################################################
# Covered Call Aristocrat Strategy - Enhanced R Implementation
#
# This is the refactored version with:
# - Modular design with separate config, utils, and metrics modules
# - Comprehensive input validation
# - Professional logging with logger package
# - Full test coverage with testthat
#
# Version: 2.0
# Author: R Implementation Team
################################################################################

# Check for required packages
required_packages <- c("dplyr", "tidyr", "purrr", "readr", "tibble", "stringr",
                       "rvest", "quantmod", "lubridate", "furrr", "future", "logger")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("ERROR: Missing required packages:\n")
  cat(paste("  -", missing_packages, collapse = "\n"), "\n\n")
  cat("Install missing packages with:\n")
  cat(sprintf("  install.packages(c(%s))\n\n",
              paste0('"', paste(missing_packages, collapse = '", "'), '"')))
  quit(status = 1)
}

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(stringr)
  library(rvest)
  library(quantmod)
  library(lubridate)
  library(furrr)
  library(future)
  library(logger)
})

# Load modules - determine script directory
get_script_dir <- function() {
  # Try multiple methods to get script location
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg)
    return(dirname(script_path))
  }

  # Fallback to working directory + strategies
  if (file.exists("strategies/config.R")) {
    return("strategies")
  }

  # Last resort - assume we're already in strategies/
  return(".")
}

source_dir <- get_script_dir()

source(file.path(source_dir, "config.R"))
source(file.path(source_dir, "utils.R"))
source(file.path(source_dir, "metrics.R"))

# Configure logging
log_threshold(INFO)
log_layout(layout_glue_colors)

################################################################################
# DATA FETCHING FUNCTIONS
################################################################################

#' Fetch Dividend Aristocrats with fallback sources
#'
#' @return Character vector of ticker symbols
get_dividend_aristocrats <- function() {
  log_info("Fetching Dividend Aristocrats list...")

  # Try 1: StockAnalysis.com
  aristocrats <- tryCatch({
    url <- CONFIG$urls$stockanalysis
    page <- read_html(url)

    ticker_links <- page %>%
      html_nodes("a[href*='/stocks/']") %>%
      html_attr("href")

    tickers <- ticker_links %>%
      str_extract("/stocks/([A-Z\\.]+)/", group = 1) %>%
      na.omit() %>%
      unique() %>%
      as.character()

    if (length(tickers) >= CONFIG$min_aristocrats) {
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
    url <- CONFIG$urls$wikipedia
    tables <- read_html(url) %>% html_table(fill = TRUE)

    for (table in tables) {
      ticker_col <- find_ticker_column(names(table))

      if (!is.null(ticker_col)) {
        tickers <- table[[ticker_col]] %>%
          na.omit() %>%
          unique() %>%
          as.character()

        if (length(tickers) >= CONFIG$min_aristocrats) {
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
get_stock_data <- function(ticker) {
  validate_ticker(ticker)

  tryCatch({
    # Get current price and info
    quote <- getQuote(ticker, what = yahooQF(c("Last Trade (Price Only)", "Name")))
    current_price <- as.numeric(quote$Last)
    company_name <- as.character(quote$Name)

    if (is.na(current_price) || current_price <= 0) {
      log_debug("{ticker}: Invalid price data")
      return(NULL)
    }

    validate_price(current_price, "current_price")

    # Get history for max drawdown
    end_date <- Sys.Date()
    start_date <- end_date - years(CONFIG$history_years)

    history <- getSymbols(ticker,
                         src = "yahoo",
                         from = start_date,
                         to = end_date,
                         auto.assign = FALSE,
                         warnings = FALSE)

    if (is.null(history) || nrow(history) == 0) {
      log_debug("{ticker}: No price history available")
      return(NULL)
    }

    # Calculate max drawdown
    close_prices <- Cl(history)
    daily_returns <- dailyReturn(close_prices, type = "arithmetic")
    cumulative_returns <- cumprod(1 + daily_returns)
    running_max <- cummax(cumulative_returns)
    drawdown <- (cumulative_returns - running_max) / running_max
    max_drawdown <- min(drawdown, na.rm = TRUE)

    # Get dividends
    dividends <- tryCatch({
      getDividends(ticker, from = start_date, to = end_date, auto.assign = FALSE)
    }, error = function(e) NULL)

    # Calculate current yield
    current_yield <- 0
    annual_dividend <- 0
    if (!is.null(dividends) && nrow(dividends) >= CONFIG$min_dividend_quarters) {
      recent_divs <- tail(dividends, CONFIG$min_dividend_quarters)
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
get_options_chain <- function(ticker, current_price) {
  validate_ticker(ticker)
  validate_price(current_price, "current_price")

  tryCatch({
    log_debug("Fetching options chain for {ticker}...")

    opt_chain <- getOptionChain(ticker, NULL)

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
        filter(!is.na(Bid) & Bid > CONFIG$min_option_bid) %>%
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
#' @param target_days Target days to expiry (NULL = longest available)
#' @return List with optimal option row and warning_flag
select_optimal_option <- function(ticker, current_price, options_df,
                                 strike_threshold_pct = CONFIG$strike_threshold_pct,
                                 target_days = CONFIG$target_days) {

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

  # Fallback if no options meet threshold
  if (nrow(filtered_options) == 0) {
    filtered_options <- options_df
    warning_flag <- TRUE
  }

  if (nrow(filtered_options) == 0) {
    return(NULL)
  }

  # Select based on target_days
  if (!is.null(target_days)) {
    optimal_option <- filtered_options %>%
      mutate(days_diff = abs(days_to_expiry - target_days)) %>%
      arrange(days_diff, desc(OI)) %>%
      slice(1)
  } else {
    optimal_option <- filtered_options %>%
      arrange(desc(expiration), desc(OI)) %>%
      slice(1)
  }

  # Log concerning selections
  if (optimal_option$days_to_expiry < CONFIG$short_expiry_warning_days) {
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

#' Analyze a single stock for covered call opportunity
#'
#' @param ticker Stock ticker symbol
#' @param strike_threshold_pct Maximum strike as % of current price
#' @param target_days Target days to expiry
#' @return Tibble row with analysis results or NULL
analyze_single_stock <- function(ticker,
                                strike_threshold_pct = CONFIG$strike_threshold_pct,
                                target_days = CONFIG$target_days) {

  validate_ticker(ticker)

  # Get stock data
  stock_data <- get_stock_data(ticker)
  if (is.null(stock_data)) {
    return(NULL)
  }

  # Get options chain
  options_df <- get_options_chain(ticker, stock_data$current_price)
  if (nrow(options_df) == 0) {
    log_debug("{ticker}: No options data available")
    return(NULL)
  }

  # Select optimal option
  selection <- select_optimal_option(ticker, stock_data$current_price, options_df,
                                    strike_threshold_pct, target_days)
  if (is.null(selection)) {
    log_debug("{ticker}: No suitable options found")
    return(NULL)
  }

  # Calculate metrics
  metrics <- calculate_metrics(ticker, stock_data$current_price,
                              selection$option, stock_data, selection$warning_flag)

  # Filter negative returns
  if (metrics$annualized_return <= CONFIG$negative_return_threshold) {
    log_debug("{ticker}: Negative return: {sprintf('%.2f%%', metrics$annualized_return * 100)}")
    return(NULL)
  }

  log_success("{ticker}: Annualized return: {sprintf('%.2f%%', metrics$annualized_return * 100)}")

  metrics
}

#' Analyze dividend aristocrats for covered call opportunities
#'
#' @param limit Optional limit on number of stocks to analyze
#' @param strike_threshold_pct Maximum strike as % of current price
#' @param target_days Target days to expiry (NULL = longest available)
#' @param max_workers Number of parallel workers
#' @return Tibble with all opportunities sorted by annualized return
analyze_aristocrats <- function(limit = NULL,
                               strike_threshold_pct = CONFIG$strike_threshold_pct,
                               target_days = CONFIG$target_days,
                               max_workers = CONFIG$max_workers) {

  log_info("")
  log_info(strrep("=", 60))
  log_info("Deep ITM Covered Calls Analysis - Dividend Aristocrats")
  log_info("Timestamp: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")
  log_info(strrep("=", 60))

  # Get aristocrats
  aristocrats <- get_dividend_aristocrats()

  # Apply limit
  if (!is.null(limit)) {
    aristocrats <- head(aristocrats, limit)
    log_info("Limiting analysis to first {limit} stocks")
  }

  log_info("Analyzing {length(aristocrats)} Dividend Aristocrats...")
  log_info("Strike threshold: {sprintf('%.0f%%', strike_threshold_pct * 100)}, Target days: {ifelse(is.null(target_days), 'longest', target_days)}, Workers: {max_workers}")

  # Set up parallel processing
  plan(multisession, workers = max_workers)

  # Analyze in parallel
  log_info("Processing stocks in parallel...")

  results <- future_map(aristocrats, function(ticker) {
    log_info("Analyzing {ticker}...")
    analyze_single_stock(ticker, strike_threshold_pct, target_days)
  }, .options = furrr_options(seed = TRUE))

  # Close workers
  plan(sequential)

  # Combine and sort results
  results_df <- results %>%
    compact() %>%
    bind_rows()

  if (nrow(results_df) > 0) {
    results_df <- results_df %>%
      arrange(desc(annualized_return))
  }

  log_info("")
  log_info(strrep("=", 60))
  log_info("Analysis complete. {nrow(results_df)} stocks analyzed successfully.")
  log_info(strrep("=", 60))

  results_df
}

################################################################################
# OUTPUT FUNCTIONS
################################################################################

#' Display top opportunities in formatted output
#'
#' @param results_df Tibble with analysis results
#' @param top_n Number of top opportunities to display
display_results <- function(results_df, top_n = CONFIG$default_top_n) {
  if (!is.data.frame(results_df)) stop("results_df must be a tibble")
  if (top_n <= 0) stop("top_n must be positive")

  if (nrow(results_df) == 0) {
    log_info("No opportunities found.")
    return(invisible())
  }

  log_info("")
  log_info("Top {min(top_n, nrow(results_df))} Opportunities (by Annualized Return):")
  log_info(strrep("=", 60))

  top_results <- head(results_df, top_n)

  for (i in seq_len(nrow(top_results))) {
    result <- top_results[i, ]

    log_info("")
    log_info("{i}. {result$ticker}")
    log_info("   Current Price: ${sprintf('%.2f', result$current_price)}")
    log_info("   Strike: ${sprintf('%.2f', result$strike)}")
    log_info("   Expiration: {result$expiration} ({result$days_to_expiry} days)")
    log_info("   Premium: ${sprintf('%.2f', result$premium_received)}")
    log_info("   Dividend Income: ${sprintf('%.2f', result$dividend_income)}")
    log_info("   Total Return: {sprintf('%.2f%%', result$total_return * 100)}")
    log_info("   Annualized Return: {sprintf('%.2f%%', result$annualized_return * 100)}")
    log_info("   Downside Protection: {sprintf('%.2f%%', result$downside_protection_pct * 100)}")
    log_info("   Max Drawdown: {sprintf('%.2f%%', result$max_drawdown * 100)}")
    log_info("   Current Yield: {sprintf('%.2f%%', result$current_yield * 100)}")

    if (result$warning_flag) {
      log_warn("   Warning: No options meeting {sprintf('%.0f%%', CONFIG$strike_threshold_pct * 100)} threshold")
    }
  }

  log_info("")
}

#' Save results to CSV file
#'
#' @param results_df Tibble with analysis results
#' @param output_dir Directory to save results
save_results <- function(results_df, output_dir = CONFIG$output_dir) {
  if (!is.data.frame(results_df)) stop("results_df must be a tibble")

  if (nrow(results_df) == 0) {
    log_info("No results to save.")
    return(invisible())
  }

  # Create directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Generate filename
  timestamp <- format_timestamp()
  filename <- file.path(output_dir, sprintf("covered_call_aristocrats_%s.csv", timestamp))

  # Save
  write_csv(results_df, filename)
  log_success("Results saved to: {filename}")
}

################################################################################
# MAIN EXECUTION
################################################################################

if (!interactive()) {
  # Parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)

  limit <- NULL
  strike_threshold <- CONFIG$strike_threshold_pct
  target_days <- CONFIG$target_days
  max_workers <- CONFIG$max_workers

  if (length(args) > 0) {
    for (i in seq_along(args)) {
      if (args[i] == "--limit" && i < length(args)) {
        limit <- as.integer(args[i + 1])
      } else if (args[i] == "--strike-threshold" && i < length(args)) {
        strike_threshold <- as.numeric(args[i + 1])
      } else if (args[i] == "--target-days" && i < length(args)) {
        target_days <- as.integer(args[i + 1])
      } else if (args[i] == "--workers" && i < length(args)) {
        max_workers <- as.integer(args[i + 1])
      } else if (args[i] == "--help") {
        cat("Usage: Rscript covered_call_aristocrats_v2.R [options]\n")
        cat("\nOptions:\n")
        cat("  --limit N              Limit analysis to first N stocks (for testing)\n")
        cat("  --strike-threshold X   Strike threshold as decimal (default 0.8 = 80%)\n")
        cat("  --target-days N        Target days to expiry (default NULL = longest)\n")
        cat("  --workers N            Number of parallel workers (default 10)\n")
        cat("  --help                 Show this help message\n")
        cat("\nExample:\n")
        cat("  Rscript covered_call_aristocrats_v2.R --limit 5 --workers 5\n\n")
        quit(status = 0)
      }
    }
  }

  # Run analysis
  results <- analyze_aristocrats(
    limit = limit,
    strike_threshold_pct = strike_threshold,
    target_days = target_days,
    max_workers = max_workers
  )

  # Display and save
  display_results(results)
  save_results(results)
}