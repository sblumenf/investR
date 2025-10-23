#' Risk Analysis Preset Configurations
#'
#' Defines preset configurations for different user types, following KISS principle.
#' 90% of users will use these presets without customization.
#'
#' @name risk-presets
NULL

#' Get predefined risk analysis preset
#'
#' @param preset_name Character: "retail", "professional", or "institutional"
#' @return List with preset configuration
#' @export
get_risk_preset <- function(preset_name = "professional") {

  presets <- list(

    #' Retail Preset - Fast analysis for casual investors
    retail = list(
      name = "Retail (Fast)",
      description = "Quick analysis optimized for speed over precision",

      # Simulation parameters
      simulation_paths = 5000,        # Fewer paths = faster
      portfolio_paths = 10000,

      # Feature toggles
      use_lsm = FALSE,                # Use simple approximation (faster)
      use_implied_volatility = FALSE, # Historical only (simpler)
      use_regime_adjustment = FALSE,  # Static parameters
      use_correlated_jumps = FALSE,   # Simpler portfolio model

      # Model selection
      position_model = "jump_diffusion",  # Still sophisticated
      portfolio_model = "gbm",             # Simple for speed

      # Calibration
      jump_frequency = 0.10,
      jump_mean = -0.02,
      jump_volatility = 0.05
    ),

    #' Professional Preset - Balanced accuracy and speed
    professional = list(
      name = "Professional (Balanced)",
      description = "Balanced analysis suitable for active traders",

      # Simulation parameters
      simulation_paths = 10000,       # Standard accuracy
      portfolio_paths = 25000,

      # Feature toggles
      use_lsm = TRUE,                 # Real LSM for early exercise
      use_implied_volatility = TRUE,  # Blend implied + historical
      use_regime_adjustment = TRUE,   # Dynamic parameters
      use_correlated_jumps = TRUE,    # Realistic portfolio risk

      # Model selection
      position_model = "jump_diffusion",
      portfolio_model = "jump_diffusion",

      # Calibration (standard academic values)
      jump_frequency = 0.10,
      jump_mean = -0.02,
      jump_volatility = 0.05,
      jump_correlation_factor = 0.7   # How much jumps correlate
    ),

    #' Institutional Preset - Maximum accuracy
    institutional = list(
      name = "Institutional (Accurate)",
      description = "Institutional-grade analysis prioritizing accuracy",

      # Simulation parameters
      simulation_paths = 50000,       # Maximum accuracy
      portfolio_paths = 50000,

      # Feature toggles
      use_lsm = TRUE,                 # Full LSM implementation
      use_implied_volatility = TRUE,  # Market-based volatility
      use_regime_adjustment = TRUE,   # Dynamic calibration
      use_correlated_jumps = TRUE,    # Full correlation model

      # Model selection
      position_model = "heston",             # Stochastic volatility
      portfolio_model = "heston",            # Full Heston at portfolio

      # Calibration
      jump_frequency = 0.10,
      jump_mean = -0.02,
      jump_volatility = 0.05,
      jump_correlation_factor = 0.75,

      # LSM advanced settings
      lsm_polynomial_degree = 4,      # Higher order for accuracy
      lsm_min_itm_paths = 20,         # More conservative

      # Heston model parameters
      use_heston = TRUE,
      heston_kappa = 2.0,
      heston_theta = 0.04,
      heston_sigma = 0.3,
      heston_rho = -0.7
    )
  )

  if (!preset_name %in% names(presets)) {
    stop("Unknown preset: ", preset_name,
         ". Available presets: ", paste(names(presets), collapse = ", "))
  }

  presets[[preset_name]]
}

#' Apply risk analysis preset with optional overrides
#'
#' KISS principle: Use preset for 90% of parameters, override only what you need
#'
#' @param preset Character: preset name
#' @param overrides List: optional parameter overrides
#' @return Merged configuration list
#' @export
#' @examples
#' \dontrun{
#'   # Use institutional preset as-is
#'   config <- apply_risk_preset("institutional")
#'
#'   # Use professional preset but with more paths
#'   config <- apply_risk_preset("professional",
#'                              overrides = list(simulation_paths = 20000))
#'
#'   # Use retail preset but enable LSM
#'   config <- apply_risk_preset("retail",
#'                              overrides = list(use_lsm = TRUE))
#' }
#' }
apply_risk_preset <- function(preset = "professional", overrides = NULL) {

  # Get base preset
  config <- get_risk_preset(preset)

  # Apply overrides if provided (DRY - single merge logic)
  if (!is.null(overrides) && length(overrides) > 0) {
    for (param_name in names(overrides)) {
      config[[param_name]] <- overrides[[param_name]]
    }
  }

  # Add metadata
  config$preset_used <- preset
  config$custom_overrides <- if (is.null(overrides)) list() else overrides
  config$timestamp <- Sys.time()

  return(config)
}

#' Display preset comparison table
#'
#' Helper for users to understand differences between presets
#'
#' @return Data frame comparing presets
#' @export
compare_risk_presets <- function() {
  presets <- c("retail", "professional", "institutional")

  comparison <- data.frame(
    Feature = character(),
    Retail = character(),
    Professional = character(),
    Institutional = character(),
    stringsAsFactors = FALSE
  )

  # Key features to compare
  features <- list(
    "Simulation Paths" = "simulation_paths",
    "LSM Early Exercise" = "use_lsm",
    "Implied Volatility" = "use_implied_volatility",
    "Regime Adjustment" = "use_regime_adjustment",
    "Correlated Jumps" = "use_correlated_jumps",
    "Position Model" = "position_model",
    "Portfolio Model" = "portfolio_model",
    "Est. Analysis Time (Position)" = NA  # Computed
  )

  for (feature_name in names(features)) {
    param <- features[[feature_name]]

    row <- data.frame(
      Feature = feature_name,
      Retail = NA,
      Professional = NA,
      Institutional = NA,
      stringsAsFactors = FALSE
    )

    for (preset_name in presets) {
      preset <- get_risk_preset(preset_name)

      if (is.na(param)) {
        # Special handling for estimated time
        if (feature_name == "Est. Analysis Time (Position)") {
          time <- if (preset_name == "retail") "5-10s"
                  else if (preset_name == "professional") "10-20s"
                  else "20-40s"
          row[[tools::toTitleCase(preset_name)]] <- time
        }
      } else {
        value <- preset[[param]]
        if (is.logical(value)) {
          value <- if (value) "Yes" else "No"
        }
        row[[tools::toTitleCase(preset_name)]] <- as.character(value)
      }
    }

    comparison <- rbind(comparison, row)
  }

  comparison
}
