#' Market Regime Detection
#'
#' KISS PRINCIPLE: Rule-based regime detection, no machine learning.
#' Uses simple threshold logic on market indicators (VIX, correlation, momentum).
#'
#' Dynamically adjusts risk parameters based on current market environment:
#' - Crisis: VIX > 25, high correlation → increase jump frequency, correlation
#' - Stressed: VIX rising or moderate spike → moderate adjustments
#' - Normal: Stable markets → baseline parameters
#'
#' Data sources: Yahoo Finance (VIX via quantmod), calculated from price data
#'
#' @name regime-detection
#' @importFrom quantmod getSymbols Cl
#' @importFrom stats cor
#' @importFrom logger log_info log_warn log_debug
NULL

################################################################################
# MAIN REGIME DETECTION INTERFACE
################################################################################

#' Detect current market regime
#'
#' Analyzes market indicators and classifies into regime types
#'
#' @param use_cache Logical: cache results for performance
#' @return List with regime classification and adjustment factors
#' @export
detect_market_regime <- function(use_cache = TRUE) {

  # Check cache (valid for 1 hour)
  if (use_cache) {
    cached <- get_cached_regime()
    if (!is.null(cached)) {
      log_debug("Regime Detection: Using cached result")
      return(cached)
    }
  }

  log_info("Regime Detection: Analyzing current market conditions")

  # Fetch market indicators
  indicators <- fetch_market_indicators()

  # Classify regime using rule-based logic
  regime <- classify_regime(indicators)

  # Cache result
  if (use_cache) {
    cache_regime(regime)
  }

  regime
}

#' Get regime-adjusted parameters
#'
#' Convenience function that combines regime detection with parameter adjustment
#'
#' @return List with adjusted risk parameters
#' @export
get_regime_adjusted_parameters <- function() {

  regime <- detect_market_regime()

  # Apply adjustments to base parameters
  list(
    # Adjusted parameters
    jump_frequency = RISK_CONFIG$jump_frequency * regime$jump_frequency_adj,
    jump_mean = RISK_CONFIG$jump_mean,  # Don't adjust mean
    jump_volatility = RISK_CONFIG$jump_volatility,
    correlation_multiplier = regime$correlation_adj,

    # Regime metadata
    regime_name = regime$name,
    regime_description = regime$description,
    risk_multiplier = regime$risk_multiplier,

    # Raw indicators for display
    vix_current = regime$indicators$vix_current,
    vix_20d_avg = regime$indicators$vix_20d_avg,
    market_correlation = regime$indicators$market_correlation
  )
}

################################################################################
# REGIME CLASSIFICATION LOGIC
################################################################################

#' Classify market regime based on indicators
#'
#' KISS: Clear threshold-based rules, no complex models
#'
#' @param indicators List of market indicators
#' @return Regime classification with adjustment factors
#' @noRd
classify_regime <- function(indicators) {

  vix_current <- indicators$vix_current
  vix_20d_avg <- indicators$vix_20d_avg
  vix_trend <- indicators$vix_trend
  correlation <- indicators$market_correlation

  # Get thresholds from config
  vix_low <- RISK_CONFIG$advanced$regime_vix_thresholds["low"] %||% 15
  vix_high <- RISK_CONFIG$advanced$regime_vix_thresholds["high"] %||% 25
  corr_threshold <- RISK_CONFIG$advanced$regime_correlation_threshold %||% 0.70

  # Initialize default (normal regime)
  regime <- list(
    name = "normal",
    description = "Stable market conditions",
    risk_multiplier = 1.0,
    jump_frequency_adj = 1.0,
    correlation_adj = 1.0,
    indicators = indicators
  )

  # RULE 1: Crisis regime (highest priority)
  # VIX > 25 AND rising correlation
  if (!is.na(vix_current) && vix_current > vix_high &&
      !is.na(correlation) && correlation > corr_threshold) {

    regime$name <- "crisis"
    regime$description <- "High volatility with elevated correlation - crisis conditions"
    regime$risk_multiplier <- 1.5
    regime$jump_frequency_adj <- 2.0    # Jumps much more likely
    regime$correlation_adj <- 1.3       # Assets move together

    log_warn("REGIME: Crisis detected (VIX={round(vix_current, 1)}, Corr={round(correlation, 2)})")

  # RULE 2: Stressed regime
  # VIX elevated OR rising significantly
  } else if (!is.na(vix_current) &&
             (vix_current > vix_high ||
              (!is.na(vix_20d_avg) && vix_current > vix_20d_avg * 1.3))) {

    regime$name <- "stressed"
    regime$description <- "Elevated or rising volatility"
    regime$risk_multiplier <- 1.2
    regime$jump_frequency_adj <- 1.4
    regime$correlation_adj <- 1.15

    log_info("REGIME: Stressed (VIX={round(vix_current, 1)})")

  # RULE 3: Correlation spike (even if VIX normal)
  # Sudden increase in asset correlation
  } else if (!is.na(correlation) && correlation > corr_threshold) {

    regime$name <- "correlation_spike"
    regime$description <- "High correlation despite normal volatility"
    regime$risk_multiplier <- 1.1
    regime$jump_frequency_adj <- 1.2
    regime$correlation_adj <- 1.25    # Main feature

    log_info("REGIME: Correlation spike (Corr={round(correlation, 2)})")

  # RULE 4: Calm markets
  # VIX < 15
  } else if (!is.na(vix_current) && vix_current < vix_low) {

    regime$name <- "calm"
    regime$description <- "Low volatility environment"
    regime$risk_multiplier <- 0.8
    regime$jump_frequency_adj <- 0.7   # Jumps less likely
    regime$correlation_adj <- 0.95

    log_info("REGIME: Calm markets (VIX={round(vix_current, 1)})")

  } else {
    # Normal regime (default)
    log_info("REGIME: Normal market conditions")
  }

  regime
}

################################################################################
# MARKET INDICATOR FETCHING
################################################################################

#' Fetch all market indicators
#'
#' Gathers VIX, correlation, and other market metrics
#'
#' @return List of indicators
#' @noRd
fetch_market_indicators <- function() {

  # VIX (volatility index)
  vix_data <- get_vix_data()

  # Market correlation (from representative basket)
  correlation <- get_market_correlation()

  list(
    vix_current = vix_data$current,
    vix_20d_avg = vix_data$avg_20d,
    vix_trend = vix_data$trend,
    market_correlation = correlation,
    timestamp = Sys.time()
  )
}

#' Fetch VIX data from Yahoo Finance
#'
#' @return List with current VIX, averages, and trend
#' @noRd
get_vix_data <- function() {

  vix_result <- tryCatch({

    # Fetch VIX from Yahoo
    suppressWarnings({
      vix <- getSymbols("^VIX", auto.assign = FALSE,
                       from = Sys.Date() - 60,
                       to = Sys.Date())
    })

    if (is.null(vix) || nrow(vix) < 5) {
      return(list(current = NA, avg_20d = NA, trend = NA))
    }

    vix_close <- as.numeric(Cl(vix))
    vix_close <- vix_close[!is.na(vix_close)]

    current <- tail(vix_close, 1)
    avg_20d <- mean(tail(vix_close, min(20, length(vix_close))), na.rm = TRUE)

    # Trend: is VIX rising?
    if (length(vix_close) >= 5) {
      recent_5d <- mean(tail(vix_close, 5), na.rm = TRUE)
      prior_5d <- mean(vix_close[(length(vix_close)-9):(length(vix_close)-5)], na.rm = TRUE)
      trend <- if (!is.na(recent_5d) && !is.na(prior_5d)) {
        if (recent_5d > prior_5d * 1.1) "rising" else if (recent_5d < prior_5d * 0.9) "falling" else "stable"
      } else {
        "unknown"
      }
    } else {
      trend <- "unknown"
    }

    list(
      current = current,
      avg_20d = avg_20d,
      trend = trend
    )

  }, error = function(e) {
    log_warn("Failed to fetch VIX data: {e$message}")
    list(current = NA, avg_20d = NA, trend = NA)
  })

  vix_result
}

#' Calculate market correlation
#'
#' Uses a representative basket of liquid ETFs as market proxy
#' (SPY, QQQ, IWM cover large cap, tech, small cap)
#'
#' @return Average pairwise correlation (0-1)
#' @noRd
get_market_correlation <- function() {

  lookback_days <- RISK_CONFIG$advanced$regime_lookback_days %||% 60

  correlation <- tryCatch({

    # Representative market basket
    symbols <- c("SPY", "QQQ", "IWM")

    # Fetch price data
    prices_list <- lapply(symbols, function(sym) {
      tryCatch({
        suppressWarnings({
          data <- getSymbols(sym, auto.assign = FALSE,
                           from = Sys.Date() - lookback_days - 10,
                           to = Sys.Date())
        })

        if (!is.null(data) && nrow(data) > 0) {
          data.frame(
            date = index(data),
            close = as.numeric(Cl(data))
          )
        } else {
          NULL
        }
      }, error = function(e) NULL)
    })

    # Remove failed fetches
    prices_list <- prices_list[!sapply(prices_list, is.null)]

    if (length(prices_list) < 2) {
      return(NA)
    }

    # Merge on date
    prices_df <- prices_list[[1]]
    names(prices_df) <- c("date", symbols[1])

    for (i in 2:length(prices_list)) {
      df <- prices_list[[i]]
      names(df) <- c("date", symbols[i])
      prices_df <- merge(prices_df, df, by = "date", all = FALSE)
    }

    # Calculate returns
    returns_matrix <- apply(prices_df[, -1], 2, function(x) diff(log(x)))

    # Correlation matrix
    cor_matrix <- cor(returns_matrix, use = "pairwise.complete.obs")

    # Average correlation (excluding diagonal)
    n <- nrow(cor_matrix)
    avg_corr <- (sum(cor_matrix) - n) / (n * (n - 1))

    avg_corr

  }, error = function(e) {
    log_warn("Failed to calculate market correlation: {e$message}")
    NA
  })

  correlation
}

################################################################################
# CACHING
################################################################################

# Simple in-memory cache - create immediately as proper package environment
.regime_cache <- new.env(parent = emptyenv())

get_regime_cache_env <- function() {
  .regime_cache
}

get_cached_regime <- function() {
  cache_env <- get_regime_cache_env()
  if (base::exists("regime", envir = cache_env, inherits = FALSE)) {
    cached <- base::get("regime", envir = cache_env, inherits = FALSE)

    # Check if cache is still valid (1 hour)
    if (difftime(Sys.time(), cached$cached_at, units = "hours") < 1) {
      return(cached$data)
    }
  }

  NULL
}

cache_regime <- function(regime) {
  cache_env <- get_regime_cache_env()
  base::assign("regime", list(data = regime, cached_at = Sys.time()),
               envir = cache_env, inherits = FALSE)
}

#' Clear regime cache
#'
#' Force re-detection on next call
#'
#' @export
clear_regime_cache <- function() {
  cache_env <- get_regime_cache_env()
  if (base::exists("regime", envir = cache_env, inherits = FALSE)) {
    base::rm("regime", envir = cache_env, inherits = FALSE)
  }
  invisible(NULL)
}

################################################################################
# UTILITIES
################################################################################


#' Display current regime information
#'
#' User-friendly summary of regime and adjustments
#'
#' @return Data frame with regime summary
#' @export
show_current_regime <- function() {

  regime <- detect_market_regime()

  data.frame(
    Regime = regime$name,
    Description = regime$description,
    Risk_Multiplier = sprintf("%.1fx", regime$risk_multiplier),
    Jump_Frequency_Adj = sprintf("%.1fx", regime$jump_frequency_adj),
    Correlation_Adj = sprintf("%.1fx", regime$correlation_adj),
    VIX_Current = ifelse(is.na(regime$indicators$vix_current), "N/A",
                        sprintf("%.1f", regime$indicators$vix_current)),
    VIX_20d_Avg = ifelse(is.na(regime$indicators$vix_20d_avg), "N/A",
                        sprintf("%.1f", regime$indicators$vix_20d_avg)),
    Market_Correlation = ifelse(is.na(regime$indicators$market_correlation), "N/A",
                               sprintf("%.2f", regime$indicators$market_correlation)),
    stringsAsFactors = FALSE
  )
}
