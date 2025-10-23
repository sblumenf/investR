#' Shared Risk Simulation Engine
#'
#' DRY PRINCIPLE: Single source of truth for all Monte Carlo simulations.
#' This module consolidates simulation logic to avoid duplication across
#' position-level and portfolio-level risk analysis.
#'
#' Supports:
#' - Multiple stochastic models (GBM, Jump-Diffusion, Heston)
#' - Single position or correlated multi-position simulations
#' - Configurable via RISK_CONFIG and presets
#'
#' @name shared-risk-engine
#' @importFrom stats rnorm rpois pnorm cor
#' @importFrom MASS mvrnorm
#' @importFrom logger log_info log_warn log_debug
NULL

################################################################################
# UNIFIED SIMULATION INTERFACE
################################################################################

#' Run correlated Monte Carlo simulation for multiple positions
#'
#' Master simulation function that handles all model types and correlation.
#' DRY: Single implementation used by both position and portfolio analysis.
#'
#' @param model_specs List of model specifications, one per position
#' @param correlation_matrix Correlation matrix for positions (optional for single position)
#' @param n_paths Number of Monte Carlo paths
#' @param n_steps Number of time steps per path (optional, computed from max T)
#' @return Matrix of simulated prices (n_steps + 1) × (n_paths × n_positions)
#' @export
simulate_correlated_paths <- function(model_specs,
                                     correlation_matrix = NULL,
                                     n_paths = 10000,
                                     n_steps = NULL) {

  n_positions <- length(model_specs)

  # Single position shortcut (no correlation needed)
  if (n_positions == 1) {
    return(simulate_single_position(model_specs[[1]], n_paths, n_steps))
  }

  # Multi-position: need correlation
  if (is.null(correlation_matrix)) {
    log_warn("No correlation matrix provided for multi-position simulation, using identity (no correlation)")
    correlation_matrix <- diag(n_positions)
  }

  # Determine time steps (use maximum T across all positions)
  if (is.null(n_steps)) {
    max_T <- max(sapply(model_specs, function(spec) spec$params$T))
    n_steps <- max(ceiling(max_T * 252), 100)  # At least daily steps
  }

  log_info("Shared Engine: Simulating {n_positions} correlated positions, {n_paths} paths, {n_steps} steps")

  # Generate correlated random shocks (DRY - shared across all models)
  correlated_shocks <- generate_correlated_shocks(
    correlation_matrix = correlation_matrix,
    n_paths = n_paths,
    n_steps = n_steps
  )

  # Simulate each position using its model spec
  all_paths <- list()

  for (i in seq_len(n_positions)) {
    spec <- model_specs[[i]]
    shocks <- correlated_shocks[[i]]  # Pre-correlated shocks for this position

    # Route to appropriate model
    paths <- simulate_with_model(
      model_type = spec$type,
      params = spec$params,
      shocks = shocks,
      n_paths = n_paths,
      n_steps = n_steps
    )

    all_paths[[i]] <- paths
  }

  # Return list of price path matrices
  return(all_paths)
}

#' Simulate single position (no correlation needed)
#'
#' @param model_spec Model specification
#' @param n_paths Number of paths
#' @param n_steps Number of steps
#' @return Matrix of price paths
#' @noRd
simulate_single_position <- function(model_spec, n_paths, n_steps = NULL) {

  params <- model_spec$params

  # Determine steps from time horizon
  if (is.null(n_steps)) {
    n_steps <- max(ceiling(params$T * 252), 100)
  }

  log_debug("Shared Engine: Single position simulation, model={model_spec$type}")

  # Route to model
  simulate_with_model(
    model_type = model_spec$type,
    params = params,
    shocks = NULL,  # Will generate internally
    n_paths = n_paths,
    n_steps = n_steps
  )
}

################################################################################
# MODEL ROUTING
################################################################################

#' Route simulation to appropriate model implementation
#'
#' DRY: Single dispatcher for all model types
#'
#' @param model_type Character: "gbm", "jump_diffusion", or "heston"
#' @param params List of model parameters
#' @param shocks Pre-generated shocks (optional, for correlation)
#' @param n_paths Number of paths
#' @param n_steps Number of steps
#' @return Matrix of simulated prices
#' @noRd
simulate_with_model <- function(model_type, params, shocks, n_paths, n_steps) {

  model_type <- tolower(model_type)

  if (model_type == "gbm") {
    simulate_gbm_internal(params, shocks, n_paths, n_steps)

  } else if (model_type == "jump_diffusion") {
    simulate_jump_diffusion_internal(params, shocks, n_paths, n_steps)

  } else if (model_type == "heston") {
    simulate_heston_internal(params, shocks, n_paths, n_steps)

  } else {
    stop("Unknown model type: ", model_type,
         ". Supported: gbm, jump_diffusion, heston")
  }
}

################################################################################
# MODEL IMPLEMENTATIONS (Internal)
################################################################################

#' Geometric Brownian Motion (Simple model)
#'
#' dS = mu * S * dt + sigma * S * dW
#'
#' @noRd
simulate_gbm_internal <- function(params, shocks, n_paths, n_steps) {

  S0 <- params$S0
  mu <- params$mu %||% params$r %||% 0.05
  sigma <- params$sigma
  T <- params$T

  dt <- T / n_steps

  # Pre-allocate
  prices <- matrix(0, nrow = n_steps + 1, ncol = n_paths)
  prices[1, ] <- S0

  # Generate or use provided shocks
  if (is.null(shocks)) {
    shocks <- matrix(rnorm(n_steps * n_paths), nrow = n_steps, ncol = n_paths)
  }

  # Simulate paths
  for (step in seq_len(n_steps)) {
    dW <- shocks[step, ] * sqrt(dt)
    prices[step + 1, ] <- prices[step, ] * exp((mu - 0.5 * sigma^2) * dt + sigma * dW)
  }

  prices
}

#' Jump-Diffusion Model (Merton 1976)
#'
#' dS = mu * S * dt + sigma * S * dW + S * dJ
#' where J is compound Poisson process
#'
#' @noRd
simulate_jump_diffusion_internal <- function(params, shocks, n_paths, n_steps) {

  S0 <- params$S0
  mu <- params$mu %||% params$r %||% 0.05
  sigma <- params$sigma
  T <- params$T

  # Jump parameters (use config defaults if not specified)
  jump_freq <- params$jump_frequency %||% RISK_CONFIG$jump_frequency
  jump_mean <- params$jump_mean %||% RISK_CONFIG$jump_mean
  jump_vol <- params$jump_volatility %||% RISK_CONFIG$jump_volatility

  dt <- T / n_steps

  # Pre-allocate
  prices <- matrix(0, nrow = n_steps + 1, ncol = n_paths)
  prices[1, ] <- S0

  # Generate or use provided shocks (for diffusion component)
  if (is.null(shocks)) {
    shocks <- matrix(rnorm(n_steps * n_paths), nrow = n_steps, ncol = n_paths)
  }

  # Simulate paths
  for (step in seq_len(n_steps)) {
    # Diffusion component
    dW <- shocks[step, ] * sqrt(dt)
    drift_diffusion <- (mu - 0.5 * sigma^2) * dt + sigma * dW

    # Jump component (independent of correlation)
    n_jumps <- rpois(n_paths, lambda = jump_freq * dt)
    jump_component <- numeric(n_paths)

    for (path in seq_len(n_paths)) {
      if (n_jumps[path] > 0) {
        jumps <- rnorm(n_jumps[path], mean = jump_mean, sd = jump_vol)
        jump_component[path] <- sum(jumps)
      }
    }

    # Update prices
    prices[step + 1, ] <- prices[step, ] * exp(drift_diffusion + jump_component)
  }

  prices
}

#' Heston Stochastic Volatility Model (1993)
#'
#' dS = mu * S * dt + sqrt(V) * S * dW1
#' dV = kappa * (theta - V) * dt + sigma * sqrt(V) * dW2
#' where Cor(dW1, dW2) = rho
#'
#' @noRd
simulate_heston_internal <- function(params, shocks, n_paths, n_steps) {

  S0 <- params$S0
  V0 <- params$V0 %||% params$sigma^2
  mu <- params$mu %||% params$r %||% 0.05
  T <- params$T

  # Heston parameters (use config defaults if not specified)
  kappa <- params$kappa %||% RISK_CONFIG$heston_kappa
  theta <- params$theta %||% RISK_CONFIG$heston_theta
  sigma_v <- params$sigma %||% RISK_CONFIG$heston_sigma
  rho <- params$rho %||% RISK_CONFIG$heston_rho

  dt <- T / n_steps

  # Pre-allocate
  S_paths <- matrix(0, nrow = n_steps + 1, ncol = n_paths)
  V_paths <- matrix(0, nrow = n_steps + 1, ncol = n_paths)

  S_paths[1, ] <- S0
  V_paths[1, ] <- V0

  # Correlation matrix for two Brownian motions
  cor_matrix <- matrix(c(1, rho, rho, 1), nrow = 2)

  # If shocks provided, use for price component (ignore correlation for now)
  # Full correlated Heston across positions is complex - simplified here
  if (is.null(shocks)) {
    shocks <- matrix(rnorm(n_steps * n_paths), nrow = n_steps, ncol = n_paths)
  }

  # Simulate paths
  for (step in seq_len(n_steps)) {
    for (path in seq_len(n_paths)) {
      # Generate correlated random numbers
      Z <- mvrnorm(1, mu = c(0, 0), Sigma = cor_matrix)
      dW1 <- Z[1] * sqrt(dt)
      dW2 <- Z[2] * sqrt(dt)

      # Current values
      S <- S_paths[step, path]
      V <- max(V_paths[step, path], 0)  # Reflect at zero

      # Update variance (CIR process)
      dV <- kappa * (theta - V) * dt + sigma_v * sqrt(V) * dW2
      V_new <- V + dV

      # Update stock price
      dS <- mu * S * dt + sqrt(V) * S * dW1
      S_new <- S + dS

      S_paths[step + 1, path] <- S_new
      V_paths[step + 1, path] <- V_new
    }
  }

  # Return price paths (variance paths could be returned too if needed)
  S_paths
}

################################################################################
# CORRELATION UTILITIES
################################################################################

#' Generate correlated random shocks for multiple positions
#'
#' Uses Cholesky decomposition to create correlated normal random variables
#'
#' @param correlation_matrix Correlation matrix (n_positions × n_positions)
#' @param n_paths Number of Monte Carlo paths
#' @param n_steps Number of time steps
#' @return List of shock matrices, one per position
#' @noRd
generate_correlated_shocks <- function(correlation_matrix, n_paths, n_steps) {

  n_positions <- nrow(correlation_matrix)

  # Cholesky decomposition
  L <- tryCatch({
    chol(correlation_matrix)
  }, error = function(e) {
    log_warn("Correlation matrix not positive definite, using identity")
    diag(n_positions)
  })

  # Generate independent shocks then correlate
  shocks_list <- list()

  for (step in seq_len(n_steps)) {
    # Independent standard normal
    Z <- matrix(rnorm(n_positions * n_paths), nrow = n_positions, ncol = n_paths)

    # Apply correlation
    correlated_Z <- t(L) %*% Z

    # Store for each position
    for (i in seq_len(n_positions)) {
      if (step == 1) {
        shocks_list[[i]] <- matrix(0, nrow = n_steps, ncol = n_paths)
      }
      shocks_list[[i]][step, ] <- correlated_Z[i, ]
    }
  }

  shocks_list
}

################################################################################
# HELPER UTILITIES
################################################################################

#' Null coalescing operator
#'
#' @noRd
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' Create model specification helper
#'
#' Convenience function for building model specs
#'
#' @param type Model type: "gbm", "jump_diffusion", or "heston"
#' @param S0 Initial stock price
#' @param T Time to expiration (years)
#' @param sigma Volatility (annualized)
#' @param r Risk-free rate (optional)
#' @param ... Additional model-specific parameters
#' @return Model specification list
#' @export
#' @examples
#' \dontrun{
#'   # Simple GBM
#'   spec <- create_model_spec("gbm", S0 = 100, T = 0.25, sigma = 0.25)
#'
#'   # Jump-diffusion
#'   spec <- create_model_spec("jump_diffusion", S0 = 100, T = 0.25, sigma = 0.30,
#'                            jump_frequency = 0.15)
#'
#'   # Heston
#'   spec <- create_model_spec("heston", S0 = 100, T = 0.25, sigma = 0.25,
#'                            V0 = 0.0625)
#' }
create_model_spec <- function(type, S0, T, sigma, r = NULL, ...) {

  list(
    type = type,
    params = c(
      list(
        S0 = S0,
        T = T,
        sigma = sigma,
        r = r %||% RISK_CONFIG$risk_free_rate,
        mu = r %||% RISK_CONFIG$risk_free_rate
      ),
      list(...)
    )
  )
}
