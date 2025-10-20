#' Portfolio Risk Analysis Functions
#'
#' Functions for analyzing portfolio-level risk using correlated Monte Carlo simulations.
#' Implements Cholesky decomposition for correlation and calculates portfolio VaR/CVaR.
#'
#' @name portfolio-risk
#' @importFrom stats cor quantile rnorm
#' @importFrom dplyr %>% filter select mutate group_by summarize arrange desc bind_rows
#' @importFrom tibble tibble
#' @importFrom PerformanceAnalytics VaR ES
NULL

#' Analyze portfolio risk using correlated Monte Carlo
#'
#' Main function for portfolio-level risk analysis. Fetches all open positions,
#' runs correlated Monte Carlo simulations, and calculates portfolio metrics.
#'
#' @param simulation_paths Number of Monte Carlo paths (default 10000, min 100, max 100000)
#' @param lookback_days Number of days for correlation calculation (default 252 = 1 year, min 30, max 1000)
#'
#' @return List with portfolio risk metrics
#' @export
analyze_portfolio_risk <- function(simulation_paths = 10000, lookback_days = 252) {

  # Validate inputs
  if (!is.numeric(simulation_paths) || simulation_paths < 100 || simulation_paths > 100000) {
    stop("simulation_paths must be between 100 and 100,000")
  }
  if (!is.numeric(lookback_days) || lookback_days < 30 || lookback_days > 1000) {
    stop("lookback_days must be between 30 and 1,000")
  }

  log_info("Portfolio Risk: Starting analysis with {simulation_paths} paths")

  # Get all open portfolio groups
  all_groups <- get_all_groups()
  open_groups <- all_groups %>% filter(status == "open")

  if (nrow(open_groups) == 0) {
    log_warn("Portfolio Risk: No open positions found")
    return(list(
      error = "No open positions in portfolio",
      total_positions = 0
    ))
  }

  log_info("Portfolio Risk: Found {nrow(open_groups)} open positions")

  # Extract position details for each group
  positions <- extract_portfolio_positions(open_groups)

  if (nrow(positions) == 0 || all(is.na(positions$ticker))) {
    log_warn("Portfolio Risk: Unable to extract position details from ANY groups. This likely means:")
    log_warn("  1. latest_positions table is empty or stale")
    log_warn("  2. Market API calls are failing for all tickers")
    log_warn("  3. Position group members/activities data is malformed")
    log_warn("  Check the detailed warnings above for specific group failures.")
    return(list(
      error = "Unable to extract position details from open groups",
      total_positions = nrow(open_groups)
    ))
  }

  # Filter out positions without expiration (except cash equivalents)
  cash_equivalent_tickers <- c("ZMMK.TO", "SGOV")
  positions_with_expiry <- positions %>%
    filter(!is.na(days_to_expiry) | ticker %in% cash_equivalent_tickers)

  excluded_count <- nrow(positions) - nrow(positions_with_expiry)
  if (excluded_count > 0) {
    excluded_tickers <- positions %>%
      filter(is.na(days_to_expiry) & !(ticker %in% cash_equivalent_tickers)) %>%
      pull(ticker) %>%
      unique()
    log_warn("Portfolio Risk: Excluding {excluded_count} positions without expiration: {paste(excluded_tickers, collapse=', ')}")
  }

  # Use filtered positions for analysis
  positions <- positions_with_expiry

  # Get unique tickers for correlation calculation
  tickers <- unique(positions$ticker[!is.na(positions$ticker)])

  log_info("Portfolio Risk: Analyzing {length(tickers)} unique tickers across {nrow(positions)} positions")

  # Calculate correlation matrix for tickers
  ticker_correlation_matrix <- calculate_correlation_matrix(tickers, lookback_days)

  # Expand correlation matrix to match positions (not unique tickers)
  # Each position gets correlation based on its ticker
  correlation_matrix <- expand_correlation_to_positions(positions, ticker_correlation_matrix)

  # Run correlated Monte Carlo simulation
  mc_results <- run_correlated_monte_carlo(
    positions = positions,
    correlation_matrix = correlation_matrix,
    simulation_paths = simulation_paths
  )

  # Calculate portfolio VaR and CVaR
  var_95 <- quantile(mc_results$portfolio_pnl, 0.05, na.rm = TRUE)
  var_99 <- quantile(mc_results$portfolio_pnl, 0.01, na.rm = TRUE)
  cvar_95 <- mean(mc_results$portfolio_pnl[mc_results$portfolio_pnl <= var_95], na.rm = TRUE)
  cvar_99 <- mean(mc_results$portfolio_pnl[mc_results$portfolio_pnl <= var_99], na.rm = TRUE)

  # Calculate position contributions to risk and return
  position_contributions <- calculate_position_contributions(
    positions = positions,
    portfolio_pnl = mc_results$portfolio_pnl,
    position_pnl_matrix = mc_results$position_pnl_matrix,
    portfolio_var = var_95
  )

  # Apply stress scenarios to entire portfolio
  stress_results <- run_portfolio_stress_tests(
    positions = positions,
    correlation_matrix = correlation_matrix
  )

  # Calculate concentration metrics
  concentration <- calculate_concentration(positions)

  # Calculate portfolio value
  total_value <- sum(positions$current_value, na.rm = TRUE)

  # Determine overall risk level
  var_pct <- abs(var_95) / total_value
  risk_level <- if (var_pct < 0.05) {
    "Low"
  } else if (var_pct < 0.10) {
    "Moderate"
  } else {
    "High"
  }

  log_info("Portfolio Risk: Analysis complete. VaR 95% = {format_currency(var_95)}, Risk Level = {risk_level}")

  list(
    # Summary metrics
    total_positions = nrow(positions),
    total_value = total_value,
    var_95 = var_95,
    var_99 = var_99,
    cvar_95 = cvar_95,
    cvar_99 = cvar_99,
    risk_level = risk_level,

    # Detailed results
    positions = positions,
    correlation_matrix = correlation_matrix,
    position_contributions = position_contributions,
    stress_results = stress_results,
    concentration = concentration,

    # Monte Carlo details
    simulation_paths = simulation_paths,
    portfolio_pnl = mc_results$portfolio_pnl,

    # Return distribution statistics
    expected_return = mean(mc_results$portfolio_pnl),
    median_return = median(mc_results$portfolio_pnl),
    sd_return = sd(mc_results$portfolio_pnl),
    prob_loss = mean(mc_results$portfolio_pnl < 0),
    percentile_5 = quantile(mc_results$portfolio_pnl, 0.05),
    percentile_25 = quantile(mc_results$portfolio_pnl, 0.25),
    percentile_75 = quantile(mc_results$portfolio_pnl, 0.75),
    percentile_95 = quantile(mc_results$portfolio_pnl, 0.95)
  )
}

#' Extract position details from portfolio groups
#'
#' Wraps each position extraction in error handling to prevent single failures
#' from crashing entire portfolio analysis.
#'
#' @param open_groups Tibble of open portfolio groups
#' @return Tibble with position details (positions that failed are skipped with warnings)
#' @noRd
extract_portfolio_positions <- function(open_groups) {

  # Batch fetch data
  group_ids <- open_groups$group_id
  all_members <- get_members_for_groups(group_ids)
  all_activities <- get_activities_for_groups(group_ids)
  latest_positions <- get_latest_positions()

  # Log data availability
  log_info("Portfolio Risk: Fetched {nrow(all_members)} members, {nrow(all_activities)} activities, {nrow(latest_positions)} current positions")

  if (nrow(latest_positions) == 0) {
    log_warn("Portfolio Risk: latest_positions is EMPTY - all groups will need to fetch prices from API")
  }

  # Track successes and failures
  successful_positions <- list()
  failed_count <- 0

  for (i in seq_len(nrow(open_groups))) {
    group <- open_groups[i, ]
    group_id <- group$group_id

    # Wrap entire position extraction in tryCatch
    position <- tryCatch({
      # Get members and activities for this group
      members <- all_members %>% filter(group_id == !!group_id)
      activities <- all_activities %>% filter(group_id == !!group_id)

      # Extract ticker (underlying stock symbol, not option)
      ticker <- NULL
      if (nrow(members) > 0) {
        underlying <- members %>% filter(role == "underlying_stock")
        if (nrow(underlying) > 0) {
          ticker <- underlying$symbol[1]
          # Validate: if this is actually an option symbol, parse it
          if (is_option_symbol(ticker)) {
            ticker <- parse_option_symbol(ticker)
          }
        } else {
          # No underlying stock member, try to parse from first member
          if (nrow(members) > 0) {
            ticker <- parse_option_symbol(members$symbol[1])
          }
        }
      }

      # Skip if we couldn't determine ticker
      if (is.null(ticker) || is.na(ticker) || ticker == "") {
        log_warn("Portfolio Risk: Skipping group {group_id} - could not determine ticker")
        return(NULL)
      }

      # Get current price and value from market data
      market_data <- enrich_group_with_market_data(group_id, members, activities, latest_positions)
      current_price <- market_data$current_stock_price

      # Skip if no current price available
      if (is.null(current_price) || is.na(current_price)) {
        log_warn("Portfolio Risk: Skipping group {group_id} ({ticker}) - no current price available")
        return(NULL)
      }

      # Extract option details
      strike <- market_data$strike_price
      expiration <- market_data$expiration_date

      # Extract premium from option activities (sum all option trades for rolled positions)
      option_activities <- activities %>%
        filter(type == "Trades") %>%
        filter(purrr::map_lgl(symbol, is_option_symbol))

      premium_received <- if (nrow(option_activities) > 0) {
        # Sum all option net amounts (sells are positive, buys are negative)
        sum(option_activities$net_amount, na.rm = TRUE)
      } else {
        0
      }

      # Calculate shares from group's stock purchase activities (single source of truth)
      # This handles positions split across multiple groups without needing allocated_quantity
      stock_purchases <- activities %>%
        filter(type == "Trades", action == "Buy") %>%
        filter(!purrr::map_lgl(symbol, is_option_symbol))

      shares <- if (nrow(stock_purchases) > 0) {
        sum(abs(stock_purchases$quantity), na.rm = TRUE)
      } else {
        100  # Default fallback
      }

      # Calculate weighted average purchase price from stock purchases
      # This is the cost basis per share for P&L calculations
      purchase_price <- if (nrow(stock_purchases) > 0) {
        total_cost <- sum(abs(stock_purchases$gross_amount), na.rm = TRUE)
        total_shares <- sum(abs(stock_purchases$quantity), na.rm = TRUE)
        if (total_shares > 0) {
          total_cost / total_shares
        } else {
          current_price  # Fallback if shares calculation fails
        }
      } else {
        current_price  # Fallback if no stock purchases found
      }

      # Get symbol_id from latest_positions for sector lookup
      stock_member <- members %>% filter(role == "underlying_stock")
      symbol_id <- NA_integer_
      if (nrow(stock_member) > 0) {
        stock_symbol <- stock_member$symbol[1]
        stock_position <- latest_positions %>% filter(symbol == stock_symbol)
        if (nrow(stock_position) > 0 &&
            !is.null(stock_position$symbol_id) &&
            !is.na(stock_position$symbol_id[1])) {
          symbol_id <- stock_position$symbol_id[1]
        }
      }

      current_value <- shares * current_price

      # Log if position has no expiration
      if (is.null(expiration)) {
        log_warn("Portfolio Risk: Position {group_id} ({ticker}) has no expiration date - will be excluded from simulation unless cash equivalent")
      }

      tibble(
        group_id = group_id,
        ticker = ticker,
        symbol_id = symbol_id,
        current_price = current_price,
        purchase_price = purchase_price,
        strike = strike,
        expiration = expiration,
        premium_received = abs(premium_received),  # Take absolute value
        shares = shares,
        current_value = current_value,
        days_to_expiry = if (!is.null(expiration)) as.numeric(difftime(expiration, Sys.Date(), units = "days")) else NA_real_
      )

    }, error = function(e) {
      # Log the error and skip this position
      log_warn("Portfolio Risk: Failed to extract position for group {group_id}: {e$message}")
      failed_count <<- failed_count + 1
      return(NULL)
    })

    # Add to successful list if not NULL
    if (!is.null(position)) {
      successful_positions[[length(successful_positions) + 1]] <- position
    }
  }

  # Log summary
  total_groups <- nrow(open_groups)
  successful_count <- length(successful_positions)
  log_info("Portfolio Risk: Successfully extracted {successful_count} of {total_groups} positions ({failed_count} failed)")

  # Return combined positions
  if (length(successful_positions) > 0) {
    bind_rows(successful_positions)
  } else {
    # Return empty tibble with correct structure
    tibble(
      group_id = character(0),
      ticker = character(0),
      symbol_id = integer(0),
      current_price = numeric(0),
      purchase_price = numeric(0),
      strike = numeric(0),
      expiration = as.Date(character(0)),
      premium_received = numeric(0),
      shares = numeric(0),
      current_value = numeric(0),
      days_to_expiry = numeric(0)
    )
  }
}

#' Calculate correlation matrix from historical price data
#'
#' @param tickers Character vector of ticker symbols
#' @param lookback_days Number of days for correlation calculation
#' @return Correlation matrix
#' @noRd
calculate_correlation_matrix <- function(tickers, lookback_days = 252) {

  if (length(tickers) == 0) {
    return(matrix(1, nrow = 1, ncol = 1))
  }

  if (length(tickers) == 1) {
    mat <- matrix(1, nrow = 1, ncol = 1)
    rownames(mat) <- colnames(mat) <- tickers[1]
    return(mat)
  }

  # Fetch historical prices for all tickers
  from_date <- Sys.Date() - lubridate::days(as.integer(lookback_days + 50)) # Extra buffer

  log_info("Portfolio Risk: Fetching price history for correlation ({length(tickers)} tickers)")

  price_data <- lapply(tickers, function(ticker) {
    tryCatch({
      hist <- fetch_price_history(ticker, from = from_date, auto_adjust = TRUE)
      if (!is.null(hist) && nrow(hist) > 0) {
        data.frame(
          date = index(hist),
          ticker = ticker,
          close = as.numeric(Cl(hist))
        )
      } else {
        NULL
      }
    }, error = function(e) {
      log_warn("Portfolio Risk: Failed to fetch price history for {ticker}: {e$message}")
      NULL
    })
  })

  # Remove NULLs and combine
  price_data <- price_data[!sapply(price_data, is.null)]

  # Get list of tickers that successfully fetched data
  successful_tickers <- unique(sapply(price_data, function(x) x$ticker[1]))

  if (length(price_data) == 0) {
    # Fallback: identity matrix for ORIGINAL tickers
    log_warn("Portfolio Risk: No price data available, using identity correlation matrix")
    mat <- diag(length(tickers))
    rownames(mat) <- colnames(mat) <- tickers
    return(mat)
  }

  # Log if some tickers failed
  failed_tickers <- setdiff(tickers, successful_tickers)
  if (length(failed_tickers) > 0) {
    log_warn("Portfolio Risk: Failed to fetch price history for {length(failed_tickers)} tickers: {paste(failed_tickers, collapse = ', ')}")
    log_info("Portfolio Risk: Continuing with {length(successful_tickers)} tickers that have price data")
  }

  all_prices <- bind_rows(price_data)

  # Pivot to wide format (dates x tickers)
  price_matrix <- all_prices %>%
    tidyr::pivot_wider(names_from = ticker, values_from = close) %>%
    arrange(date)

  # Calculate log returns
  returns_matrix <- price_matrix %>%
    select(-date) %>%
    as.matrix() %>%
    apply(2, function(x) diff(log(x)))

  # Calculate correlation matrix (only for tickers with data)
  cor_matrix <- cor(returns_matrix, use = "pairwise.complete.obs")

  # Handle NAs - use market average correlation (0.3 for equities) instead of 0
  # This prevents understating portfolio risk
  na_count <- sum(is.na(cor_matrix)) - sum(is.na(diag(cor_matrix)))
  if (na_count > 0) {
    log_warn("Portfolio Risk: {na_count} correlations could not be estimated, using 0.3 (equity market average)")
    cor_matrix[is.na(cor_matrix)] <- 0.3
  }

  # Ensure diagonal is 1
  diag(cor_matrix) <- 1

  # Check if matrix is positive definite (required for Cholesky)
  eigenvalues <- eigen(cor_matrix, only.values = TRUE)$values
  if (any(eigenvalues <= 0)) {
    log_warn("Portfolio Risk: Correlation matrix is not positive definite, applying shrinkage")
    # Simple shrinkage: blend with identity matrix
    shrinkage_factor <- 0.1
    cor_matrix <- (1 - shrinkage_factor) * cor_matrix + shrinkage_factor * diag(nrow(cor_matrix))
  }

  cor_matrix
}

#' Expand ticker correlation matrix to position-level correlation matrix
#'
#' Maps the ticker-level correlation matrix to a position-level matrix where
#' positions with the same ticker have correlation 1.0, and positions with
#' different tickers use the ticker correlation.
#'
#' @param positions Tibble of position details
#' @param ticker_correlation_matrix Correlation matrix for unique tickers
#' @return Correlation matrix matching number of positions
#' @noRd
expand_correlation_to_positions <- function(positions, ticker_correlation_matrix) {
  n_positions <- nrow(positions)
  tickers <- positions$ticker
  unique_tickers <- unique(tickers)
  n_tickers <- length(unique_tickers)

  log_debug("Portfolio Risk: Expanding correlation from {n_tickers} tickers to {n_positions} positions using vectorized matrix operations")

  # Check for duplicate tickers (multiple positions on same ticker)
  ticker_counts <- table(tickers)
  duplicated_tickers <- names(ticker_counts)[ticker_counts > 1]
  if (length(duplicated_tickers) > 0) {
    log_debug("Portfolio Risk: Found {length(duplicated_tickers)} tickers with multiple positions: {paste(duplicated_tickers, collapse=', ')}")
  }

  # VECTORIZED APPROACH: Use indicator matrix for efficient computation
  # Create indicator matrix: positions × tickers
  # indicator[i,j] = 1 if position i has ticker j, else 0
  indicator <- matrix(0, nrow = n_positions, ncol = n_tickers)
  for (i in seq_len(n_positions)) {
    ticker_idx <- which(unique_tickers == tickers[i])
    indicator[i, ticker_idx] <- 1
  }

  # Ensure ticker correlation matrix has same ticker ordering
  # Reorder ticker_correlation_matrix to match unique_tickers ordering
  ticker_cor_ordered <- ticker_correlation_matrix[unique_tickers, unique_tickers, drop = FALSE]

  # Vectorized matrix multiplication: Indicator %*% TickerCor %*% t(Indicator)
  # This efficiently maps ticker-level correlations to position-level
  # Positions with same ticker automatically get correlation 1.0
  pos_cor_matrix <- indicator %*% ticker_cor_ordered %*% t(indicator)

  # Ensure diagonal is exactly 1 (numerical precision safety)
  diag(pos_cor_matrix) <- 1.0

  # POSITIVE DEFINITENESS CHECK (Required for Cholesky Decomposition)
  #
  # MATHEMATICAL BACKGROUND:
  # When multiple positions share the same ticker, they have perfect correlation (1.0).
  # This creates linear dependencies in the correlation matrix, leading to near-zero
  # or negative eigenvalues (near-singular matrix). This is NOT a bug - it's a
  # mathematical property of the portfolio structure.
  #
  # SOLUTION:
  # Apply Ledoit-Wolf style shrinkage to blend the empirical correlation with the
  # identity matrix. This is the academically accepted approach for handling
  # near-singular correlation matrices in portfolio risk analysis.
  eigenvalues <- eigen(pos_cor_matrix, only.values = TRUE, symmetric = TRUE)$values
  min_eigenvalue <- min(eigenvalues)

  if (any(eigenvalues <= 1e-10)) {
    log_info("Portfolio Risk: Correlation matrix has near-zero eigenvalue ({sprintf('%.2e', min_eigenvalue)}). This occurs when multiple positions share tickers (perfect correlation creates linear dependencies). Applying Ledoit-Wolf style shrinkage.")

    # Ledoit-Wolf shrinkage: blend with identity matrix to ensure positive definiteness
    shrinkage_factor <- 0.1
    pos_cor_matrix <- (1 - shrinkage_factor) * pos_cor_matrix + shrinkage_factor * diag(nrow(pos_cor_matrix))

    # Verify fix worked
    eigenvalues_after <- eigen(pos_cor_matrix, only.values = TRUE, symmetric = TRUE)$values
    min_eigenvalue_after <- min(eigenvalues_after)

    if (any(eigenvalues_after <= 1e-10)) {
      log_error("Portfolio Risk: Shrinkage failed to fix correlation matrix (min eigenvalue after: {sprintf('%.2e', min_eigenvalue_after)}), falling back to identity matrix")
      pos_cor_matrix <- diag(nrow(pos_cor_matrix))
    } else {
      log_debug("Portfolio Risk: Shrinkage successful, min eigenvalue improved from {sprintf('%.2e', min_eigenvalue)} to {sprintf('%.2e', min_eigenvalue_after)}")
    }
  } else {
    log_debug("Portfolio Risk: Correlation matrix is positive definite (min eigenvalue: {sprintf('%.2e', min_eigenvalue)}), no shrinkage needed")
  }

  return(pos_cor_matrix)
}

#' Run correlated Monte Carlo simulation for portfolio
#'
#' Uses Cholesky decomposition to generate correlated random shocks.
#'
#' @param positions Tibble of position details
#' @param correlation_matrix Correlation matrix for positions (not tickers)
#' @param simulation_paths Number of simulation paths
#' @return List with portfolio and position P&L (in dollars, not percentages)
#' @noRd
run_correlated_monte_carlo <- function(positions, correlation_matrix, simulation_paths = 10000) {

  n_positions <- nrow(positions)

  # Cholesky decomposition for correlated shocks
  # Note: correlation_matrix should already be positive definite from calculate_correlation_matrix()
  L <- tryCatch({
    chol(correlation_matrix)
  }, error = function(e) {
    # This should rarely happen after positive definiteness check, but just in case
    log_error("Portfolio Risk: Cholesky decomposition failed unexpectedly - {e$message}")
    log_warn("Portfolio Risk: Falling back to independent simulations (zero correlation assumed)")
    diag(ncol(correlation_matrix))
  })

  # Get parameters for each position
  log_info("Portfolio Risk: Preparing simulation parameters for {n_positions} positions")

  params <- lapply(seq_len(n_positions), function(i) {
    pos <- positions[i, ]

    # Calculate adaptive volatility based on time horizon
    sigma <- tryCatch({
      calculate_adaptive_volatility(
        ticker = pos$ticker,
        days_to_expiry = pos$days_to_expiry
      )
    }, error = function(e) {
      log_error("Portfolio Risk: Failed to calculate volatility for {pos$ticker} (days_to_expiry={pos$days_to_expiry}, class={class(pos$days_to_expiry)}): {e$message}")
      stop(e)
    })

    list(
      ticker = pos$ticker,
      current_price = pos$current_price,
      purchase_price = pos$purchase_price,
      strike = pos$strike,
      premium_received = pos$premium_received,
      shares = pos$shares,
      days_to_expiry = pos$days_to_expiry,
      sigma = sigma
    )
  })

  log_info("Portfolio Risk: Running {simulation_paths} correlated simulation paths")

  # Initialize results matrices (dollar P&L, not returns)
  position_pnl_matrix <- matrix(0, nrow = simulation_paths, ncol = n_positions)
  portfolio_pnl <- numeric(simulation_paths)

  # Run simulation
  for (path in seq_len(simulation_paths)) {
    # Generate correlated random shocks
    z <- rnorm(n_positions)
    correlated_shocks <- t(L) %*% z

    # Simulate each position
    for (i in seq_len(n_positions)) {
      param <- params[[i]]

      if (is.null(param$current_price) || is.na(param$current_price)) {
        position_pnl_matrix[path, i] <- 0
        next
      }

      # Simple price path: S_T = S_0 * exp((mu - 0.5*sigma^2)*T + sigma*sqrt(T)*Z)
      # Determine time horizon
      T <- if (!is.null(param$days_to_expiry) && !is.na(param$days_to_expiry)) {
        max(param$days_to_expiry, 1) / 365
      } else if (param$ticker %in% c("ZMMK.TO", "SGOV")) {
        0.001  # Cash equivalents - minimal time horizon (near-zero volatility impact)
      } else {
        # This shouldn't happen after filtering, but log if it does
        log_warn("Portfolio Risk: Position {param$ticker} has no expiration and is not cash equivalent - using 1 year default")
        1.0
      }

      r <- RISK_CONFIG$risk_free_rate
      shock <- correlated_shocks[i]

      final_price <- param$current_price * exp((r - 0.5 * param$sigma^2) * T + param$sigma * sqrt(T) * shock)

      # Calculate covered call payoff
      # P&L is calculated from purchase_price (cost basis), not current_price
      if (!is.null(param$strike) && !is.na(param$strike)) {
        if (final_price >= param$strike) {
          # Called away at strike price
          stock_pnl <- (param$strike - param$purchase_price) * param$shares
        } else {
          # Keep shares at final price
          stock_pnl <- (final_price - param$purchase_price) * param$shares
        }

        total_pnl <- stock_pnl + param$premium_received
      } else {
        # No option, just stock
        total_pnl <- (final_price - param$purchase_price) * param$shares
      }

      # Store dollar P&L (not returns)
      position_pnl_matrix[path, i] <- total_pnl
      portfolio_pnl[path] <- portfolio_pnl[path] + total_pnl
    }
  }

  list(
    portfolio_pnl = portfolio_pnl,
    position_pnl_matrix = position_pnl_matrix
  )
}

#' Calculate position contributions to portfolio risk and return
#'
#' Implements Component VaR for risk attribution and expected contribution for return attribution.
#' Component VaR formula: Risk_i = (Cov(Position_i, Portfolio) / Var(Portfolio)) × Portfolio_VaR
#'
#' @param positions Tibble of position details
#' @param portfolio_pnl Vector of portfolio P&L from MC simulations
#' @param position_pnl_matrix Matrix of position P&L (paths x positions)
#' @param portfolio_var Portfolio VaR (5th percentile P&L)
#' @return Tibble with position contributions
#' @noRd
calculate_position_contributions <- function(positions, portfolio_pnl, position_pnl_matrix, portfolio_var) {

  n_positions <- nrow(positions)

  # Calculate portfolio variance (for Component VaR denominator)
  portfolio_variance <- var(portfolio_pnl)

  # Pre-calculate expected contributions (mean P&L per position)
  expected_contributions <- colMeans(position_pnl_matrix)

  # Calculate Component VaR for each position
  # Component VaR_i = (Cov(Position_i, Portfolio) / Var(Portfolio)) × Portfolio_VaR
  risk_contributions <- sapply(seq_len(n_positions), function(i) {
    position_pnl <- position_pnl_matrix[, i]

    # Calculate covariance between position and portfolio
    covariance <- cov(position_pnl, portfolio_pnl)

    # Component VaR formula
    component_var <- (covariance / portfolio_variance) * abs(portfolio_var)

    component_var
  })

  # Calculate total absolute risk for percentage calculations
  total_abs_risk <- sum(abs(risk_contributions))

  # Build results tibble with both metrics
  tibble(
    group_id = positions$group_id,
    ticker = positions$ticker,
    expected_contribution = expected_contributions,
    risk_contribution = risk_contributions,
    pct_of_portfolio_risk = abs(risk_contributions) / total_abs_risk,
    risk_return_ratio = ifelse(expected_contributions != 0,
                               abs(risk_contributions) / abs(expected_contributions),
                               NA_real_)
  ) %>%
    arrange(desc(abs(risk_contribution)))
}

#' Run portfolio stress tests
#'
#' @param positions Tibble of position details
#' @param correlation_matrix Correlation matrix
#' @return Tibble with stress test results
#' @noRd
run_portfolio_stress_tests <- function(positions, correlation_matrix) {

  scenario_names <- c("financial_crisis_2008", "covid_crash_2020", "rising_rates", "stagflation", "volatility_spike")

  stress_results <- lapply(scenario_names, function(scenario_name) {
    scenario <- get_stress_scenario(scenario_name)

    # Calculate portfolio P&L under scenario
    total_pnl <- 0

    for (i in seq_len(nrow(positions))) {
      pos <- positions[i, ]

      if (is.null(pos$ticker) || is.na(pos$ticker)) next

      # Get sector for this ticker (pass symbol_id for Questrade API lookup)
      sector <- get_ticker_sector(pos$ticker, pos$symbol_id)

      # Get price change for this sector
      if (!is.null(scenario$sector_returns[[sector]])) {
        price_change_pct <- scenario$sector_returns[[sector]]
      } else {
        price_change_pct <- scenario$default_return
      }

      stressed_price <- pos$current_price * (1 + price_change_pct)

      # Calculate P&L
      if (!is.null(pos$strike) && !is.na(pos$strike)) {
        if (stressed_price >= pos$strike) {
          stock_pnl <- (pos$strike - pos$current_price) * pos$shares
        } else {
          stock_pnl <- (stressed_price - pos$current_price) * pos$shares
        }
        position_pnl <- stock_pnl + pos$premium_received
      } else {
        position_pnl <- (stressed_price - pos$current_price) * pos$shares
      }

      total_pnl <- total_pnl + position_pnl
    }

    tibble(
      scenario = scenario$name,
      portfolio_pnl = total_pnl,
      portfolio_return_pct = total_pnl / sum(positions$current_value, na.rm = TRUE)
    )
  })

  bind_rows(stress_results)
}

#' Calculate portfolio concentration metrics
#'
#' @param positions Tibble of position details
#' @return List with concentration metrics
#' @noRd
calculate_concentration <- function(positions) {

  total_value <- sum(positions$current_value, na.rm = TRUE)

  # By ticker
  ticker_concentration <- positions %>%
    group_by(ticker) %>%
    summarize(
      total_value = sum(current_value, na.rm = TRUE),
      n_positions = n()
    ) %>%
    mutate(pct_of_portfolio = total_value / !!total_value) %>%
    arrange(desc(pct_of_portfolio))

  # By sector
  sector_concentration <- positions %>%
    mutate(sector = purrr::map2_chr(ticker, symbol_id, ~get_ticker_sector(.x, .y) %||% "Unknown")) %>%
    group_by(sector) %>%
    summarize(
      total_value = sum(current_value, na.rm = TRUE),
      n_positions = n()
    ) %>%
    mutate(pct_of_portfolio = total_value / !!total_value) %>%
    arrange(desc(pct_of_portfolio))

  # Check concentration alerts
  max_ticker_pct <- if (nrow(ticker_concentration) > 0) max(ticker_concentration$pct_of_portfolio) else 0
  max_sector_pct <- if (nrow(sector_concentration) > 0) max(sector_concentration$pct_of_portfolio) else 0

  alerts <- c()
  if (max_ticker_pct > 0.25) {
    alerts <- c(alerts, sprintf("High concentration: %.1f%% in single ticker", max_ticker_pct * 100))
  }
  if (max_sector_pct > 0.40) {
    alerts <- c(alerts, sprintf("High concentration: %.1f%% in single sector", max_sector_pct * 100))
  }

  list(
    by_ticker = ticker_concentration,
    by_sector = sector_concentration,
    alerts = alerts
  )
}
