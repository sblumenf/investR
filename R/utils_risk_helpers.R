#' Risk Analysis Helpers
#'
#' Shared utility functions for risk analysis across the application.
#' Includes volatility calculations, UI components, and common risk computations.
#' Eliminates code duplication across portfolio risk, position risk, and strategy modules.
#'
#' @name risk-helpers
NULL

#' Create Risk Analysis Button
#'
#' Creates a standardized "Analyze Risk" button for strategy result cards.
#'
#' @param ns Namespace function from the calling module
#' @param idx Row index for the card (used in button ID)
#' @return tags$div with action button
#' @export
create_risk_analysis_button <- function(ns, idx) {
  tags$div(
    style = "margin-bottom: 15px;",
    actionButton(
      inputId = ns(paste0("analyze_risk_btn_", idx)),
      label = "Analyze Risk",
      icon = icon("chart-line"),
      class = "btn btn-primary btn-sm",
      style = "width: 100%;"
    )
  )
}

#' Setup Risk Analysis Modules
#'
#' Creates the observe() block and mod_position_risk_server() calls for all cards.
#' This pattern is used identically in aristocrats, zero_dividend, and collar modules.
#'
#' @param results_data Reactive containing results data frame
#' @param input Shiny input object
#' @param ns Namespace function
#' @param is_aristocrat Logical indicating if this is aristocrats strategy (default FALSE)
#' @return NULL (called for side effects - creates observers)
#' @export
#' @importFrom shiny observe reactive req
setup_risk_analysis_modules <- function(results_data, input, ns, is_aristocrat = FALSE) {

  observe({
    req(results_data())
    results <- results_data()

    lapply(seq_len(nrow(results)), function(i) {
      row <- results[i, ]
      risk_id <- paste0("risk_", i)

      # Create reactive trigger for this card's button
      trigger <- reactive({
        input[[paste0("analyze_risk_btn_", i)]]
      })

      # Call risk analysis module
      mod_position_risk_server(
        id = risk_id,
        trigger = trigger,
        ticker = reactive(row$ticker),
        strike = reactive(row$strike),
        expiration = reactive(row$expiration),
        premium_received = reactive(row$premium_received),
        current_price = reactive(row$current_price),
        is_aristocrat = reactive(is_aristocrat),
        simulation_paths = reactive(10000)
      )
    })
  })
}

################################################################################
# VOLATILITY CALCULATION
################################################################################

#' Calculate Adaptive Volatility Based on Time Horizon
#'
#' Estimates annualized volatility using time-to-expiry adaptive methodology:
#' - Short-term (≤90 days): EWMA with λ=0.94 on 60 days (reactive to recent volatility)
#' - Medium-term (90-365 days): EWMA with λ=0.97 on 180 days (balanced approach)
#' - Long-term (>365 days): Historical volatility on 500 days (full market cycles)
#'
#' This approach matches forecast horizon to lookback period, following academic
#' best practices from volatility forecasting literature.
#'
#' @param ticker Stock ticker symbol (e.g., "AAPL")
#' @param days_to_expiry Numeric days until position expires (can be NA for non-expiring)
#' @return Numeric annualized volatility (e.g., 0.35 for 35%)
#' @export
#' @importFrom logger log_info log_warn log_debug
#' @examples
#' \dontrun{
#'   # Short-term position
#'   vol_smci <- calculate_adaptive_volatility("SMCI", days_to_expiry = 33)
#'
#'   # Long-term position
#'   vol_tgt <- calculate_adaptive_volatility("TGT", days_to_expiry = 800)
#' }
calculate_adaptive_volatility <- function(ticker, days_to_expiry) {

  # Validate inputs
  if (missing(ticker) || is.null(ticker) || nchar(ticker) == 0) {
    stop("ticker is required")
  }

  # Get configuration
  cfg <- RISK_CONFIG

  # Validate configuration has required fields
  required_fields <- c(
    "volatility_short_term_threshold", "volatility_long_term_threshold",
    "volatility_short_term_lookback", "volatility_medium_term_lookback", "volatility_long_term_lookback",
    "volatility_lambda_fast", "volatility_lambda_slow",
    "volatility_min_observations", "volatility_default"
  )

  for (field in required_fields) {
    if (is.null(cfg[[field]]) || is.na(cfg[[field]])) {
      stop(paste("RISK_CONFIG is missing or has NA for:", field))
    }
  }

  # Determine method and lookback based on time horizon
  if (is.null(days_to_expiry) || is.na(days_to_expiry) || days_to_expiry < 0) {
    # Expired or no expiry: use default medium-term approach
    lookback_days <- cfg$volatility_medium_term_lookback
    method <- "historical"
    log_debug("Volatility ({ticker}): Using default (expired/NA), lookback={lookback_days}")
  } else if (days_to_expiry <= cfg$volatility_short_term_threshold) {
    # Short-term: EWMA fast
    lookback_days <- cfg$volatility_short_term_lookback
    method <- "ewma_fast"
    log_debug("Volatility ({ticker}): Short-term ({days_to_expiry} days), EWMA λ={cfg$volatility_lambda_fast}")
  } else if (days_to_expiry <= cfg$volatility_long_term_threshold) {
    # Medium-term: EWMA slow
    lookback_days <- cfg$volatility_medium_term_lookback
    method <- "ewma_slow"
    log_debug("Volatility ({ticker}): Medium-term ({days_to_expiry} days), EWMA λ={cfg$volatility_lambda_slow}")
  } else {
    # Long-term: Historical
    lookback_days <- cfg$volatility_long_term_lookback
    method <- "historical"
    log_debug("Volatility ({ticker}): Long-term ({days_to_expiry} days), historical lookback={lookback_days}")
  }

  # Fetch historical price data
  hist_data <- tryCatch({
    fetch_price_history(
      ticker,
      from = Sys.Date() - lubridate::days(lookback_days + 50),  # Buffer for holidays
      auto_adjust = TRUE
    )
  }, error = function(e) {
    log_warn("Volatility ({ticker}): Failed to fetch price history - {e$message}")
    NULL
  })

  # Check data quality - handle potential NA from nrow()
  n_rows <- if (!is.null(hist_data)) nrow(hist_data) else 0
  if (is.null(hist_data) || is.null(n_rows) || is.na(n_rows) || n_rows < cfg$volatility_min_observations) {
    log_warn("Volatility ({ticker}): Insufficient data ({n_rows} obs), using default {cfg$volatility_default}")
    return(cfg$volatility_default)
  }

  # Calculate daily log returns
  returns <- diff(log(Cl(hist_data)))
  returns <- returns[!is.na(returns)]

  # Convert to plain numeric vector (from xts/zoo object)
  returns <- as.numeric(returns)

  n_returns <- length(returns)
  if (is.null(n_returns) || is.na(n_returns) || n_returns < cfg$volatility_min_observations) {
    log_warn("Volatility ({ticker}): Insufficient valid returns ({n_returns}), using default {cfg$volatility_default}")
    return(cfg$volatility_default)
  }

  # Calculate volatility based on method
  if (method == "historical") {
    # Simple historical volatility (equal weights)
    sigma <- sd(returns, na.rm = TRUE) * sqrt(252)
    log_info("Volatility ({ticker}): Historical vol = {round(sigma * 100, 1)}%")

  } else if (method == "ewma_fast") {
    # Exponentially weighted moving average (fast decay)
    sigma <- calculate_ewma_volatility(returns, lambda = cfg$volatility_lambda_fast)
    log_info("Volatility ({ticker}): EWMA fast vol = {round(sigma * 100, 1)}%")

  } else if (method == "ewma_slow") {
    # Exponentially weighted moving average (slow decay)
    sigma <- calculate_ewma_volatility(returns, lambda = cfg$volatility_lambda_slow)
    log_info("Volatility ({ticker}): EWMA slow vol = {round(sigma * 100, 1)}%")
  }

  # Sanity check
  if (is.na(sigma) || sigma <= 0 || sigma > 5) {
    log_warn("Volatility ({ticker}): Unreasonable value ({sigma}), using default {cfg$volatility_default}")
    return(cfg$volatility_default)
  }

  return(sigma)
}

#' Calculate EWMA Volatility
#'
#' Implements exponentially weighted moving average volatility estimation.
#' Recent observations receive more weight via exponential decay.
#'
#' @param returns Numeric vector of daily log returns
#' @param lambda Decay factor (0 < lambda < 1), typically 0.94 or 0.97
#' @return Numeric annualized volatility
#' @noRd
calculate_ewma_volatility <- function(returns, lambda) {
  n <- length(returns)

  # Initialize with first observation's squared return
  sigma_sq <- returns[1]^2

  # Iterate through returns, updating EWMA
  for (i in 2:n) {
    sigma_sq <- lambda * sigma_sq + (1 - lambda) * returns[i]^2
  }

  # Convert to annualized volatility
  sigma <- sqrt(sigma_sq) * sqrt(252)

  return(sigma)
}
