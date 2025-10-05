#!/usr/bin/env Rscript
################################################################################
# Covered Call Aristocrat Strategy - R Implementation
#
# This script replicates the Python covered call aristocrat strategy using
# tidyverse syntax. It identifies optimal deep ITM covered call opportunities
# on dividend aristocrat stocks.
#
# Strategy: Buy 100 shares + Sell 1 deep ITM call option (strike ≤ 80% of price)
#
# Required Packages:
#   install.packages(c("tidyverse", "rvest", "quantmod", "lubridate", "furrr", "future"))
################################################################################

# Check for required packages
required_packages <- c("dplyr", "tidyr", "purrr", "readr", "tibble", "stringr",
                       "rvest", "quantmod", "lubridate", "furrr", "future")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("ERROR: Missing required packages:\n")
  cat(paste("  -", missing_packages, collapse = "\n"), "\n\n")
  cat("Install missing packages with:\n")
  cat(sprintf("  install.packages(c(%s))\n\n", paste0('"', paste(missing_packages, collapse = '", "'), '"')))
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
})

# Configuration Parameters
STRIKE_THRESHOLD_PCT <- 0.8  # Maximum strike as % of current price (default 80%)
TARGET_DAYS <- NULL          # Target days to expiry (NULL = use longest available)
MAX_WORKERS <- 10            # Number of parallel workers

################################################################################
# 1. GET DIVIDEND ARISTOCRATS LIST
################################################################################

#' Fetch Dividend Aristocrats with fallback sources
#'
#' Tries StockAnalysis.com first, falls back to Wikipedia
#'
#' @return Character vector of ticker symbols
get_dividend_aristocrats <- function() {
  cat("Fetching Dividend Aristocrats list...\n")

  # Try 1: StockAnalysis.com
  aristocrats <- tryCatch({
    url <- "https://stockanalysis.com/list/dividend-aristocrats/"
    page <- read_html(url)

    # Extract ticker links from HTML
    ticker_links <- page %>%
      html_nodes("a[href*='/stocks/']") %>%
      html_attr("href")

    # Extract tickers from URLs like "/stocks/TICKER/"
    tickers <- ticker_links %>%
      str_extract("/stocks/([A-Z\\.]+)/", group = 1) %>%
      na.omit() %>%
      unique() %>%
      as.character()

    # Sanity check - should have at least 50 aristocrats
    if (length(tickers) >= 50) {
      cat(sprintf("✓ Found %d Dividend Aristocrats from StockAnalysis.com\n", length(tickers)))
      return(tickers)
    }

    NULL
  }, error = function(e) {
    cat(sprintf("StockAnalysis.com failed: %s\n", e$message))
    NULL
  })

  # Return if successful
  if (!is.null(aristocrats)) return(aristocrats)

  # Try 2: Wikipedia
  aristocrats <- tryCatch({
    url <- "https://en.wikipedia.org/wiki/S%26P_500_Dividend_Aristocrats"

    # Read all tables from Wikipedia
    tables <- read_html(url) %>%
      html_table(fill = TRUE)

    # Find table with ticker symbols - check multiple possible column names
    for (table in tables) {
      # Try different column name variations
      ticker_col <- NULL
      if ("Ticker symbol" %in% names(table)) {
        ticker_col <- "Ticker symbol"
      } else if ("Symbol" %in% names(table)) {
        ticker_col <- "Symbol"
      } else if ("Ticker" %in% names(table)) {
        ticker_col <- "Ticker"
      }

      if (!is.null(ticker_col)) {
        tickers <- table[[ticker_col]] %>%
          na.omit() %>%
          unique() %>%
          as.character()

        if (length(tickers) >= 50) {
          cat(sprintf("✓ Found %d Dividend Aristocrats from Wikipedia (fallback)\n", length(tickers)))
          return(tickers)
        }
      }
    }

    NULL
  }, error = function(e) {
    cat(sprintf("Wikipedia failed: %s\n", e$message))
    NULL
  })

  if (is.null(aristocrats)) {
    stop("ERROR: Unable to fetch Dividend Aristocrats from any source")
  }

  aristocrats
}

################################################################################
# 2. FETCH STOCK DATA
################################################################################

#' Get comprehensive stock data for a ticker
#'
#' Fetches current price, 5-year history, dividends, and calculates max drawdown
#'
#' @param ticker Stock ticker symbol
#' @return List with price, history, dividends, max_drawdown, current_yield
get_stock_data <- function(ticker) {
  tryCatch({
    # Get current price and info
    quote <- getQuote(ticker, what = yahooQF(c("Last Trade (Price Only)", "Name")))
    current_price <- as.numeric(quote$Last)
    company_name <- as.character(quote$Name)

    if (is.na(current_price) || current_price <= 0) {
      return(NULL)
    }

    # Get 5 years of price history for max drawdown calculation
    end_date <- Sys.Date()
    start_date <- end_date - years(5)

    history <- getSymbols(ticker,
                         src = "yahoo",
                         from = start_date,
                         to = end_date,
                         auto.assign = FALSE,
                         warnings = FALSE)

    if (is.null(history) || nrow(history) == 0) {
      return(NULL)
    }

    # Calculate max drawdown from 5-year history
    close_prices <- Cl(history)
    daily_returns <- dailyReturn(close_prices, type = "arithmetic")
    cumulative_returns <- cumprod(1 + daily_returns)
    running_max <- cummax(cumulative_returns)
    drawdown <- (cumulative_returns - running_max) / running_max
    max_drawdown <- min(drawdown, na.rm = TRUE)

    # Get dividend history
    dividends <- tryCatch({
      getDividends(ticker,
                  from = start_date,
                  to = end_date,
                  auto.assign = FALSE)
    }, error = function(e) NULL)

    # Calculate current yield from last 4 dividends
    current_yield <- 0
    annual_dividend <- 0
    if (!is.null(dividends) && nrow(dividends) >= 4) {
      recent_divs <- tail(dividends, 4)
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
    cat(sprintf("  %s: Error - %s\n", ticker, substr(e$message, 1, 50)))
    NULL
  })
}

################################################################################
# 3. FETCH OPTIONS CHAIN
################################################################################

#' Get options chain for all available expirations
#'
#' Fetches call options for all expirations and filters for ITM calls
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @return Tibble with all ITM call options across expirations
get_options_chain <- function(ticker, current_price) {
  tryCatch({
    cat(sprintf("    Fetching options chain for %s...\n", ticker))

    # Get all available option expirations
    opt_chain <- getOptionChain(ticker, NULL)

    if (is.null(opt_chain) || length(opt_chain) == 0) {
      return(tibble())
    }

    cat(sprintf("    Found %d expiration dates for %s\n", length(opt_chain), ticker))

    # Process each expiration - use possibly to handle errors gracefully
    process_exp <- possibly(function(exp_date) {
      exp_data <- opt_chain[[exp_date]]

      # Check if calls data exists
      if (is.null(exp_data) || !"calls" %in% names(exp_data)) {
        return(tibble())
      }

      calls <- exp_data$calls

      if (is.null(calls) || nrow(calls) == 0) {
        return(tibble())
      }

      # Filter for ITM calls (strike < current_price or ITM == TRUE)
      # Also filter out rows with zero or missing bid prices
      itm_calls <- calls %>%
        filter(Strike < current_price | ITM == TRUE) %>%
        filter(!is.na(Bid) & Bid > 0) %>%
        mutate(
          # Parse expiration date from format like "Oct.03.2025"
          expiration = as.Date(exp_date, format = "%b.%d.%Y"),
          days_to_expiry = as.integer(difftime(expiration, Sys.Date(), units = "days")),
          # Time value = bid - max(intrinsic_value, 0)
          time_value = Bid - pmax(current_price - Strike, 0)
        )

      itm_calls
    }, otherwise = tibble())

    all_options <- map_dfr(names(opt_chain), process_exp)

    all_options

  }, error = function(e) {
    cat(sprintf("  %s: Error fetching options - %s\n", ticker, substr(e$message, 1, 50)))
    tibble()
  })
}

################################################################################
# 4. SELECT OPTIMAL OPTION
################################################################################

#' Select optimal ITM call option based on strategy parameters
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @param options_df Tibble of available options
#' @param strike_threshold_pct Maximum strike as % of current price
#' @param target_days Target days to expiry (NULL = longest available)
#' @return List with optimal option row and warning_flag
select_optimal_option <- function(ticker, current_price, options_df,
                                 strike_threshold_pct = 0.8, target_days = NULL) {

  warning_flag <- FALSE

  # Calculate strike threshold
  strike_threshold <- current_price * strike_threshold_pct

  # Filter: strike <= strike_threshold
  filtered_options <- options_df %>%
    filter(Strike <= strike_threshold)

  # If no options found, use all ITM options as fallback
  if (nrow(filtered_options) == 0) {
    filtered_options <- options_df
    warning_flag <- TRUE
  }

  # Check if we have any options
  if (nrow(filtered_options) == 0) {
    return(NULL)
  }

  # Select based on target_days
  if (!is.null(target_days)) {
    # Sort by: closest to target_days, then OI descending
    optimal_option <- filtered_options %>%
      mutate(days_diff = abs(days_to_expiry - target_days)) %>%
      arrange(days_diff, desc(OI)) %>%
      slice(1)
  } else {
    # Sort by: expiration descending, then OI descending
    optimal_option <- filtered_options %>%
      arrange(desc(expiration), desc(OI)) %>%
      slice(1)
  }

  # Log concerning selections (very short expiry)
  if (optimal_option$days_to_expiry < 14) {
    cat(sprintf("  ⚠️  %s: Selected %d-day option (only %d deep ITM options available)\n",
                ticker, optimal_option$days_to_expiry, nrow(filtered_options)))
  }

  list(
    option = optimal_option,
    warning_flag = warning_flag
  )
}

################################################################################
# 5. CALCULATE ALL METRICS
################################################################################

#' Calculate all covered call metrics
#'
#' Implements the complete metric calculation from the conversion plan
#'
#' @param ticker Stock ticker symbol
#' @param current_price Current stock price
#' @param option_row Selected option (tibble row)
#' @param stock_data Stock data list from get_stock_data()
#' @param warning_flag Warning flag from option selection
#' @return Tibble row with all calculated metrics
calculate_metrics <- function(ticker, current_price, option_row, stock_data, warning_flag) {

  # Extract option data
  strike <- option_row$Strike
  bid_price <- option_row$Bid
  days_to_expiry <- option_row$days_to_expiry
  expiration <- option_row$expiration
  open_interest <- option_row$OI

  # Core values
  investment <- current_price * 100
  premium_received <- bid_price * 100
  net_outlay <- investment - premium_received

  # Intrinsic and extrinsic value
  intrinsic_value <- max(0, current_price - strike)
  extrinsic_value <- bid_price - intrinsic_value

  # Project dividends for holding period
  dividend_income <- 0
  reinvestment_income <- 0

  if (!is.null(stock_data$dividends) && nrow(stock_data$dividends) >= 2) {
    # Get latest dividend amount and payment frequency
    divs <- stock_data$dividends
    latest_dividend <- as.numeric(tail(divs, 1))

    # Calculate average days between payments from last 6 dividends
    recent_divs <- tail(divs, min(6, nrow(divs)))
    if (nrow(recent_divs) >= 2) {
      div_dates <- index(recent_divs)
      days_between <- as.numeric(diff(div_dates))
      avg_days_between <- mean(days_between)

      # Project dividend payments during holding period
      expected_payments <- days_to_expiry / avg_days_between
      dividend_income <- (latest_dividend * expected_payments * 100)

      # Calculate reinvestment income (assume SGOV 5% yield, dividends received evenly)
      avg_days_to_invest <- days_to_expiry / 2
      years_to_invest <- avg_days_to_invest / 365
      reinvest_rate <- 0.05  # SGOV yield
      reinvestment_income <- dividend_income * ((1 + reinvest_rate)^years_to_invest - 1)
    }
  }

  # Calculate proceeds from exercise
  exercise_proceeds <- strike * 100

  # Net profit = all income minus what we paid
  net_profit <- premium_received + dividend_income + reinvestment_income +
                exercise_proceeds - investment

  # Returns
  total_return <- if (net_outlay > 0) net_profit / net_outlay else 0

  # Annualized return = (1 + total_return)^(365/days) - 1
  years <- days_to_expiry / 365
  annualized_return <- if (years > 0) (1 + total_return)^(1/years) - 1 else 0

  # Protection metrics
  breakeven_price <- current_price - bid_price
  downside_protection_pct <- (current_price - breakeven_price) / current_price

  # Return comprehensive metrics as tibble row
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
    investment = investment,
    premium_received = premium_received,
    dividend_income = dividend_income,
    reinvestment_income = reinvestment_income,
    exercise_proceeds = exercise_proceeds,
    net_profit = net_profit,
    net_outlay = net_outlay,
    # Returns
    total_return = total_return,
    annualized_return = annualized_return,
    # Risk metrics
    max_drawdown = stock_data$max_drawdown,
    current_yield = stock_data$current_yield,
    # Protection
    breakeven_price = breakeven_price,
    downside_protection_pct = downside_protection_pct,
    # Additional values
    intrinsic_value = intrinsic_value,
    extrinsic_value = extrinsic_value,
    annual_dividend = stock_data$annual_dividend,
    # Flags
    warning_flag = warning_flag,
    is_aristocrat = TRUE
  )
}

################################################################################
# 6. ANALYZE SINGLE STOCK
################################################################################

#' Analyze a single stock for covered call opportunity
#'
#' @param ticker Stock ticker symbol
#' @param strike_threshold_pct Maximum strike as % of current price
#' @param target_days Target days to expiry
#' @return Tibble row with analysis results or NULL
analyze_single_stock <- function(ticker,
                                strike_threshold_pct = 0.8,
                                target_days = NULL) {

  # Get stock data
  stock_data <- get_stock_data(ticker)
  if (is.null(stock_data)) {
    return(NULL)
  }

  # Get options chain
  options_df <- get_options_chain(ticker, stock_data$current_price)
  if (nrow(options_df) == 0) {
    cat(sprintf("  %s: No options data available\n", ticker))
    return(NULL)
  }

  # Select optimal option
  selection <- select_optimal_option(ticker, stock_data$current_price, options_df,
                                    strike_threshold_pct, target_days)
  if (is.null(selection)) {
    cat(sprintf("  %s: No suitable options found\n", ticker))
    return(NULL)
  }

  # Calculate all metrics
  metrics <- calculate_metrics(ticker, stock_data$current_price,
                              selection$option, stock_data, selection$warning_flag)

  # Filter out negative returns
  if (metrics$annualized_return <= 0) {
    cat(sprintf("  %s: Negative return: %.2f%%\n", ticker, metrics$annualized_return * 100))
    return(NULL)
  }

  cat(sprintf("  ✓ %s: Annualized return: %.2f%%\n", ticker, metrics$annualized_return * 100))

  metrics
}

################################################################################
# 7. MAIN ANALYSIS FUNCTION (PARALLEL)
################################################################################

#' Analyze dividend aristocrats for covered call opportunities
#'
#' Main entry point that orchestrates the entire analysis with parallel processing
#'
#' @param limit Optional limit on number of stocks to analyze (for testing)
#' @param strike_threshold_pct Maximum strike as % of current price
#' @param target_days Target days to expiry (NULL = longest available)
#' @param max_workers Number of parallel workers
#' @return Tibble with all opportunities sorted by annualized return
analyze_aristocrats <- function(limit = NULL,
                               strike_threshold_pct = 0.8,
                               target_days = NULL,
                               max_workers = 10) {

  cat("\n")
  cat(strrep("=", 60), "\n", sep = "")
  cat("Deep ITM Covered Calls Analysis - Dividend Aristocrats\n")
  cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat(strrep("=", 60), "\n\n", sep = "")

  # Get dividend aristocrats
  aristocrats <- get_dividend_aristocrats()

  # Apply limit if specified
  if (!is.null(limit)) {
    aristocrats <- head(aristocrats, limit)
    cat(sprintf("Limiting analysis to first %d stocks\n\n", limit))
  }

  cat(sprintf("Analyzing %d Dividend Aristocrats...\n", length(aristocrats)))
  cat(sprintf("Strike threshold: %.0f%%, Target days: %s, Workers: %d\n\n",
              strike_threshold_pct * 100,
              ifelse(is.null(target_days), "longest", as.character(target_days)),
              max_workers))

  # Set up parallel processing
  plan(multisession, workers = max_workers)

  # Analyze in parallel with progress
  cat("Processing stocks in parallel...\n\n")

  results <- future_map(aristocrats, function(ticker) {
    cat(sprintf("Analyzing %s...\n", ticker))
    analyze_single_stock(ticker, strike_threshold_pct, target_days)
  }, .options = furrr_options(seed = TRUE))

  # Close parallel workers
  plan(sequential)

  # Combine results and filter NULLs
  results_df <- results %>%
    compact() %>%
    bind_rows()

  # Sort by annualized return descending
  if (nrow(results_df) > 0) {
    results_df <- results_df %>%
      arrange(desc(annualized_return))
  }

  cat("\n")
  cat(strrep("=", 60), "\n", sep = "")
  cat(sprintf("Analysis complete. %d stocks analyzed successfully.\n", nrow(results_df)))
  cat(strrep("=", 60), "\n\n", sep = "")

  results_df
}

################################################################################
# 8. OUTPUT FORMATTING
################################################################################

#' Display top opportunities in formatted output
#'
#' @param results_df Tibble with analysis results
#' @param top_n Number of top opportunities to display
display_results <- function(results_df, top_n = 10) {

  if (nrow(results_df) == 0) {
    cat("No opportunities found.\n")
    return(invisible())
  }

  cat(sprintf("\nTop %d Opportunities (by Annualized Return):\n", min(top_n, nrow(results_df))))
  cat(strrep("=", 60), "\n", sep = "")

  top_results <- head(results_df, top_n)

  for (i in seq_len(nrow(top_results))) {
    result <- top_results[i, ]

    cat(sprintf("\n%d. %s\n", i, result$ticker))
    cat(sprintf("   Current Price: $%.2f\n", result$current_price))
    cat(sprintf("   Strike: $%.2f\n", result$strike))
    cat(sprintf("   Expiration: %s (%d days)\n", result$expiration, result$days_to_expiry))
    cat(sprintf("   Premium: $%.2f\n", result$premium_received))
    cat(sprintf("   Dividend Income: $%.2f\n", result$dividend_income))
    cat(sprintf("   Total Return: %.2f%%\n", result$total_return * 100))
    cat(sprintf("   Annualized Return: %.2f%%\n", result$annualized_return * 100))
    cat(sprintf("   Downside Protection: %.2f%%\n", result$downside_protection_pct * 100))
    cat(sprintf("   Max Drawdown: %.2f%%\n", result$max_drawdown * 100))
    cat(sprintf("   Current Yield: %.2f%%\n", result$current_yield * 100))

    if (result$warning_flag) {
      cat("   ⚠️  Warning: No options meeting 80% threshold\n")
    }
  }

  cat("\n")
}

#' Save results to CSV file
#'
#' @param results_df Tibble with analysis results
#' @param output_dir Directory to save results
save_results <- function(results_df, output_dir = "strategies") {

  if (nrow(results_df) == 0) {
    cat("No results to save.\n")
    return(invisible())
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Generate filename with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(output_dir, sprintf("covered_call_aristocrats_%s.csv", timestamp))

  # Save to CSV
  write_csv(results_df, filename)

  cat(sprintf("Results saved to: %s\n", filename))
}

################################################################################
# MAIN EXECUTION
################################################################################

if (!interactive()) {
  # Parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)

  # Default parameters
  limit <- NULL
  strike_threshold <- STRIKE_THRESHOLD_PCT
  target_days <- TARGET_DAYS
  max_workers <- MAX_WORKERS

  # Simple argument parsing
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
        cat("Usage: Rscript covered_call_aristocrats.R [options]\n")
        cat("\nOptions:\n")
        cat("  --limit N              Limit analysis to first N stocks (for testing)\n")
        cat("  --strike-threshold X   Strike threshold as decimal (default 0.8 = 80%)\n")
        cat("  --target-days N        Target days to expiry (default NULL = longest)\n")
        cat("  --workers N            Number of parallel workers (default 10)\n")
        cat("  --help                 Show this help message\n")
        cat("\nExample:\n")
        cat("  Rscript covered_call_aristocrats.R --limit 5 --workers 5\n\n")
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

  # Display and save results
  display_results(results, top_n = 10)
  save_results(results)
}