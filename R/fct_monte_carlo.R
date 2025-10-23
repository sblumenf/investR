#' Monte Carlo Simulation Functions
#'
#' Core Monte Carlo simulation engine for options pricing and risk analysis.
#' Implements multiple stochastic models: Jump Diffusion and Heston.
#'
#' @name monte-carlo
#' @importFrom stats rnorm rpois
#' @importFrom MASS mvrnorm
NULL

################################################################################
# STOCHASTIC PROCESS GENERATORS
################################################################################

#' Generate stock price paths using Geometric Brownian Motion with Jumps
#'
#' Implements jump diffusion model for realistic tail risk.
#' Based on Merton (1976) model.
#'
#' @param S0 Initial stock price
#' @param mu Drift (expected return)
#' @param sigma Volatility
#' @param T Time to maturity (years)
#' @param n_steps Number of time steps
#' @param n_paths Number of simulation paths
#' @param jump_freq Jump frequency (average jumps per year)
#' @param jump_mean Average jump size (log returns)
#' @param jump_vol Jump volatility
#' @return Matrix of price paths (n_steps x n_paths)
#' @noRd
simulate_jump_diffusion <- function(S0,
                                    mu,
                                    sigma,
                                    T,
                                    n_steps,
                                    n_paths,
                                    jump_freq = RISK_CONFIG$jump_frequency,
                                    jump_mean = RISK_CONFIG$jump_mean,
                                    jump_vol = RISK_CONFIG$jump_volatility) {

  dt <- T / n_steps

  # Pre-allocate price matrix
  prices <- matrix(0, nrow = n_steps + 1, ncol = n_paths)
  prices[1, ] <- S0

  # Simulate each path
  for (path in seq_len(n_paths)) {
    S <- S0

    for (step in seq_len(n_steps)) {
      # Continuous diffusion component
      dW <- rnorm(1, mean = 0, sd = sqrt(dt))
      drift_diffusion <- (mu - 0.5 * sigma^2) * dt + sigma * dW

      # Jump component
      n_jumps <- rpois(1, lambda = jump_freq * dt)
      jump_component <- 0

      if (n_jumps > 0) {
        jumps <- rnorm(n_jumps, mean = jump_mean, sd = jump_vol)
        jump_component <- sum(jumps)
      }

      # Update price
      S <- S * exp(drift_diffusion + jump_component)
      prices[step + 1, path] <- S
    }
  }

  prices
}

#' Generate stock price paths using Heston Stochastic Volatility model
#'
#' Implements Heston (1993) model for realistic volatility clustering.
#' Important for modeling vega risk on options.
#'
#' @param S0 Initial stock price
#' @param V0 Initial variance
#' @param mu Drift
#' @param kappa Mean reversion speed
#' @param theta Long-term variance
#' @param sigma Vol of vol
#' @param rho Correlation between price and vol
#' @param T Time to maturity (years)
#' @param n_steps Number of time steps
#' @param n_paths Number of simulation paths
#' @return List with price_paths (matrix) and variance_paths (matrix)
#' @noRd
simulate_heston <- function(S0,
                            V0,
                            mu,
                            kappa = RISK_CONFIG$heston_kappa,
                            theta = RISK_CONFIG$heston_theta,
                            sigma = RISK_CONFIG$heston_sigma,
                            rho = RISK_CONFIG$heston_rho,
                            T,
                            n_steps,
                            n_paths) {

  dt <- T / n_steps

  # Pre-allocate matrices
  S_paths <- matrix(0, nrow = n_steps + 1, ncol = n_paths)
  V_paths <- matrix(0, nrow = n_steps + 1, ncol = n_paths)

  S_paths[1, ] <- S0
  V_paths[1, ] <- V0

  # Correlation matrix for correlated Brownian motions
  cor_matrix <- matrix(c(1, rho, rho, 1), nrow = 2)

  for (path in seq_len(n_paths)) {
    S <- S0
    V <- V0

    for (step in seq_len(n_steps)) {
      # Generate correlated random numbers
      Z <- mvrnorm(1, mu = c(0, 0), Sigma = cor_matrix)
      dW1 <- Z[1] * sqrt(dt)
      dW2 <- Z[2] * sqrt(dt)

      # Ensure variance stays positive (reflection at zero)
      V <- max(V, 0)

      # Update variance (Cox-Ingersoll-Ross process)
      dV <- kappa * (theta - V) * dt + sigma * sqrt(V) * dW2
      V <- V + dV

      # Update stock price
      dS <- mu * S * dt + sqrt(V) * S * dW1
      S <- S + dS

      S_paths[step + 1, path] <- S
      V_paths[step + 1, path] <- V
    }
  }

  list(
    price_paths = S_paths,
    variance_paths = V_paths
  )
}

################################################################################
# DIVIDEND SCHEDULE BUILDER
################################################################################

#' Build complete dividend schedule for option life
#'
#' Reuses existing dividend projection logic but returns detailed schedule.
#'
#' @param ticker Stock ticker
#' @param days_to_expiry Days until option expiration
#' @param is_aristocrat Logical, is this a dividend aristocrat
#' @return Tibble with dividend_date, dividend_amount, days_until, confidence
#' @noRd
#' @importFrom tibble tibble
#' @importFrom lubridate days
build_dividend_schedule <- function(ticker, days_to_expiry, is_aristocrat = FALSE) {

  # Fetch historical dividends (2 years should be enough for pattern)
  end_date <- Sys.Date()
  start_date <- end_date - lubridate::years(2)

  dividends <- fetch_dividend_history(ticker, from = start_date, to = end_date)

  # If no dividend history, return empty schedule
  if (is.null(dividends) || length(dividends) == 0 || nrow(dividends) < 2) {
    return(tibble(
      dividend_date = as.Date(character(0)),
      dividend_amount = numeric(0),
      days_until = numeric(0),
      confidence = character(0)
    ))
  }

  # Calculate average payment frequency
  recent_divs <- tail(dividends, min(6, nrow(dividends)))
  div_dates <- index(recent_divs)
  days_between <- as.numeric(diff(div_dates))
  avg_days_between <- as.integer(round(mean(days_between)))

  # Get latest dividend amount
  latest_dividend <- as.numeric(tail(dividends, 1))

  # Project future dividends
  expiration_date <- Sys.Date() + lubridate::days(as.integer(days_to_expiry))
  projected_dates <- seq(
    from = Sys.Date() + lubridate::days(avg_days_between),
    to = expiration_date,
    by = paste(avg_days_between, "days")
  )

  # Build schedule
  schedule <- tibble(
    dividend_date = as.Date(projected_dates),
    dividend_amount = latest_dividend,  # Assume constant (conservative)
    days_until = as.numeric(difftime(dividend_date, Sys.Date(), units = "days")),
    confidence = if (is_aristocrat) "high" else if (nrow(dividends) >= 4) "medium" else "low"
  )

  schedule
}

################################################################################
# OPTION PAYOFF CALCULATORS
################################################################################

#' Calculate covered call payoff at given stock price
#'
#' @param stock_price Stock price at evaluation
#' @param strike Strike price
#' @param premium_received Premium received from selling call
#' @param entry_stock_price Original stock purchase price
#' @param shares Number of shares (default 100)
#' @return Profit/loss amount
#' @noRd
calculate_covered_call_payoff <- function(stock_price,
                                          strike,
                                          premium_received,
                                          entry_stock_price,
                                          shares = 100) {

  # If assigned (stock price >= strike)
  if (stock_price >= strike) {
    # Sell stock at strike, keep premium
    stock_pnl <- (strike - entry_stock_price) * shares
    total_pnl <- stock_pnl + premium_received
  } else {
    # Hold stock, keep premium
    stock_pnl <- (stock_price - entry_stock_price) * shares
    total_pnl <- stock_pnl + premium_received
  }

  total_pnl
}

#' Determine if early exercise is optimal at dividend date
#'
#' Simplified early exercise check based on Merton (1973):
#' Early exercise optimal if dividend > remaining time value
#'
#' @param stock_price Stock price at dividend date
#' @param strike Strike price
#' @param dividend Dividend amount
#' @param time_to_expiry Remaining time to expiration (years)
#' @param volatility Implied volatility
#' @param risk_free_rate Risk-free rate
#' @return Logical TRUE if early exercise optimal
#' @noRd
is_early_exercise_optimal <- function(stock_price,
                                      strike,
                                      dividend,
                                      time_to_expiry,
                                      volatility,
                                      risk_free_rate) {

  # If not in the money, never exercise early
  if (stock_price <= strike) {
    return(FALSE)
  }

  # Intrinsic value
  intrinsic <- stock_price - strike

  # Approximate time value using Black-Scholes for European call
  # (This is upper bound on American call time value)
  d1 <- (log(stock_price / strike) + (risk_free_rate + 0.5 * volatility^2) * time_to_expiry) /
        (volatility * sqrt(time_to_expiry))
  d2 <- d1 - volatility * sqrt(time_to_expiry)

  # Use normal CDF approximation
  N_d1 <- pnorm(d1)
  N_d2 <- pnorm(d2)

  call_value <- stock_price * N_d1 - strike * exp(-risk_free_rate * time_to_expiry) * N_d2
  time_value <- max(0, call_value - intrinsic)

  # Exercise if dividend exceeds time value
  dividend > time_value
}

################################################################################
# MAIN MONTE CARLO ENGINE
################################################################################

#' Run Monte Carlo simulation for American call option with dividends
#'
#' Main simulation engine. Generates price paths and evaluates early exercise
#' at each dividend date along each path.
#'
#' @param ticker Stock ticker
#' @param current_price Current stock price (for simulation)
#' @param entry_price Stock entry/cost basis price (for return calculations, defaults to current_price)
#' @param strike Option strike price
#' @param expiration_date Option expiration date
#' @param premium_received Premium received from selling call
#' @param n_paths Number of simulation paths
#' @param model Stochastic model ("jump_diffusion" or "heston")
#' @param is_aristocrat Is this a dividend aristocrat
#' @return List with simulation results
#' @export
run_monte_carlo_simulation <- function(ticker,
                                       current_price,
                                       entry_price = NULL,
                                       strike,
                                       expiration_date,
                                       premium_received,
                                       n_paths = RISK_CONFIG$default_simulation_paths,
                                       model = "jump_diffusion",
                                       is_aristocrat = FALSE) {

  # Use entry_price for return calculations if provided, otherwise use current_price
  if (is.null(entry_price)) {
    entry_price <- current_price
  }

  # Calculate days to expiration
  days_to_expiry <- as.numeric(difftime(expiration_date, Sys.Date(), units = "days"))

  if (days_to_expiry <= 0) {
    stop("Option has already expired")
  }

  T <- days_to_expiry / 365.25  # Convert to years

  # Build dividend schedule
  div_schedule <- build_dividend_schedule(ticker, days_to_expiry, is_aristocrat)

  # Get volatility (implied + historical blend, or fallback)
  # Uses get_volatility() which tries implied vol first, then historical adaptive
  use_implied <- RISK_CONFIG$features$use_implied_volatility %||% TRUE

  sigma <- get_volatility(
    ticker = ticker,
    days_to_expiry = days_to_expiry,
    use_implied = use_implied,
    fallback_to_historical = TRUE,
    blend_weight = RISK_CONFIG$advanced$implied_vol_blend_weight %||% 0.70
  )

  # Use risk-free rate from config (could fetch SOFR)
  r <- RISK_CONFIG$risk_free_rate
  mu <- r  # Risk-neutral measure

  # Simulation parameters
  n_steps <- max(days_to_expiry, 252)  # At least daily steps

  # Apply regime-based adjustments if enabled
  use_regime <- RISK_CONFIG$features$use_regime_adjustment %||% FALSE
  regime_info <- NULL

  if (use_regime) {
    regime_params <- get_regime_adjusted_parameters()
    regime_info <- list(
      name = regime_params$regime_name,
      description = regime_params$regime_description,
      risk_multiplier = regime_params$risk_multiplier,
      vix_current = regime_params$vix_current
    )

    # Adjust jump frequency based on regime
    adjusted_jump_freq <- regime_params$jump_frequency
    log_info("Regime adjustment: {regime_params$regime_name} - jump freq = {round(adjusted_jump_freq, 3)}")
  } else {
    adjusted_jump_freq <- RISK_CONFIG$jump_frequency
  }

  # Generate price paths
  if (model == "jump_diffusion") {
    price_paths <- simulate_jump_diffusion(
      S0 = current_price,
      mu = mu,
      sigma = sigma,
      T = T,
      n_steps = n_steps,
      n_paths = n_paths,
      jump_freq = adjusted_jump_freq  # Use regime-adjusted frequency
    )
  } else if (model == "heston") {
    V0 <- sigma^2  # Initial variance
    heston_result <- simulate_heston(
      S0 = current_price,
      V0 = V0,
      mu = mu,
      T = T,
      n_steps = n_steps,
      n_paths = n_paths
    )
    price_paths <- heston_result$price_paths
  } else {
    stop("Unknown model: ", model)
  }

  # Evaluate each path - use LSM or simple method based on config
  use_lsm <- RISK_CONFIG$features$use_lsm %||% FALSE

  if (use_lsm && nrow(div_schedule) > 0) {
    # Use Least Squares Monte Carlo for sophisticated early exercise
    log_info("Using LSM for early exercise analysis")

    lsm_result <- run_lsm_early_exercise(
      price_paths = price_paths,
      strike = strike,
      dividend_schedule = div_schedule,
      risk_free_rate = r,
      days_to_expiry = days_to_expiry,
      config = RISK_CONFIG
    )

    # Convert LSM exercise matrix to payoffs
    payoffs <- numeric(n_paths)
    early_exercise_dates <- rep(NA, n_paths)
    assigned_at_dividend <- integer(n_paths)

    for (path in seq_len(n_paths)) {
      exercised <- FALSE

      # Check if this path exercised at any dividend
      for (div_idx in seq_len(nrow(div_schedule))) {
        if (lsm_result$exercise_matrix[div_idx, path]) {
          # Exercised at this dividend
          exercised <- TRUE
          assigned_at_dividend[path] <- div_idx
          early_exercise_dates[path] <- as.character(div_schedule$dividend_date[div_idx])

          # Calculate payoff (called away at strike)
          payoffs[path] <- calculate_covered_call_payoff(
            stock_price = strike,
            strike = strike,
            premium_received = premium_received,
            entry_stock_price = entry_price,
            shares = 100
          )
          break
        }
      }

      # If not exercised early, evaluate at expiration
      if (!exercised) {
        final_price <- price_paths[n_steps + 1, path]

        payoffs[path] <- calculate_covered_call_payoff(
          stock_price = final_price,
          strike = strike,
          premium_received = premium_received,
          entry_stock_price = entry_price,
          shares = 100
        )
      }
    }

  } else {
    # Use simple approximation (original method)
    log_info("Using simple approximation for early exercise")

    payoffs <- numeric(n_paths)
    early_exercise_dates <- rep(NA, n_paths)
    assigned_at_dividend <- integer(n_paths)

    for (path in seq_len(n_paths)) {
      exercised <- FALSE
      exercise_step <- NA

      # Check early exercise at each dividend date
      if (nrow(div_schedule) > 0) {
        for (div_idx in seq_len(nrow(div_schedule))) {
          div_date <- div_schedule$dividend_date[div_idx]
          div_amount <- div_schedule$dividend_amount[div_idx]
          days_to_div <- as.numeric(difftime(div_date, Sys.Date(), units = "days"))

          # Find corresponding step in simulation
          div_step <- round((days_to_div / days_to_expiry) * n_steps)
          div_step <- max(1, min(div_step, n_steps))

          stock_price_at_div <- price_paths[div_step, path]
          time_remaining <- (days_to_expiry - days_to_div) / 365.25

          # Check if early exercise optimal (simple method)
          if (is_early_exercise_optimal(
            stock_price_at_div,
            strike,
            div_amount,
            time_remaining,
            sigma,
            r
          )) {
            # Early exercise
            exercised <- TRUE
            exercise_step <- div_step
            assigned_at_dividend[path] <- div_idx
            early_exercise_dates[path] <- as.character(div_date)

            # Calculate payoff (assigned at dividend date)
            payoffs[path] <- calculate_covered_call_payoff(
              stock_price = strike,  # Called away at strike
              strike = strike,
              premium_received = premium_received,
              entry_stock_price = entry_price,
              shares = 100
            )

            break  # Exit dividend loop
          }
        }
      }

      # If not exercised early, evaluate at expiration
      if (!exercised) {
        final_price <- price_paths[n_steps + 1, path]

        payoffs[path] <- calculate_covered_call_payoff(
          stock_price = final_price,
          strike = strike,
          premium_received = premium_received,
          entry_stock_price = entry_price,
          shares = 100
        )
      }
    }
  }

  # Calculate statistics
  # Return based on net outlay (actual cost basis - premium received)
  net_outlay <- (entry_price * 100) - premium_received
  returns <- payoffs / net_outlay

  list(
    # Simulation inputs
    n_paths = n_paths,
    model = model,
    ticker = ticker,
    current_price = current_price,
    strike = strike,
    days_to_expiry = days_to_expiry,

    # Results
    payoffs = payoffs,
    returns = returns,
    early_exercise_dates = early_exercise_dates,
    assigned_at_dividend = assigned_at_dividend,

    # Summary statistics
    expected_return = mean(returns),
    median_return = median(returns),
    sd_return = sd(returns),
    prob_profit = mean(payoffs > 0),
    percentile_5 = quantile(returns, 0.05),
    percentile_25 = quantile(returns, 0.25),
    percentile_75 = quantile(returns, 0.75),
    percentile_95 = quantile(returns, 0.95),

    # Early exercise statistics
    early_exercise_prob = mean(!is.na(early_exercise_dates)),
    avg_payoff_if_exercised = if (any(!is.na(early_exercise_dates))) {
      mean(payoffs[!is.na(early_exercise_dates)])
    } else {
      NA
    },
    avg_payoff_if_held = if (any(is.na(early_exercise_dates))) {
      mean(payoffs[is.na(early_exercise_dates)])
    } else {
      NA
    },

    # Dividend schedule for reference
    dividend_schedule = div_schedule,

    # Price paths (for visualization, but can be large - store sample)
    sample_paths = price_paths[, sample(1:n_paths, min(100, n_paths))],

    # Volatility used
    implied_volatility = sigma,

    # Regime information (if enabled)
    regime = regime_info
  )
}
