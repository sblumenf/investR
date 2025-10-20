#' Risk Management Configuration
#'
#' Configuration constants and settings for risk analysis and Monte Carlo simulation.
#' Follows same pattern as existing config files (aristocrats, collar, etc.)
#'
#' @name risk-config
NULL

################################################################################
# SIMULATION SETTINGS
################################################################################

#' Risk Analysis Configuration List
#'
#' @export
RISK_CONFIG <- list(
  # Monte Carlo simulation parameters
  default_simulation_paths = 10000,
  deep_analysis_paths = 50000,
  portfolio_analysis_paths = 50000,

  # Simulation models
  default_stochastic_model = "jump_diffusion",  # or "heston" for enhanced analysis

  # Jump diffusion parameters (calibrated from S&P 500 historical data)
  jump_frequency = 0.1,      # ~10% chance of jump per year
  jump_mean = -0.02,         # Average jump is -2% (crashes more common than spikes)
  jump_volatility = 0.05,    # Jump size standard deviation

  # Heston model parameters (for stochastic volatility)
  heston_kappa = 2.0,        # Mean reversion speed
  heston_theta = 0.04,       # Long-term variance
  heston_sigma = 0.3,        # Vol of vol
  heston_rho = -0.7,         # Correlation between price and vol (typically negative)

  # Risk-free rate source
  risk_free_rate = 0.045,    # Default 4.5%, will fetch SOFR if available

  # Early exercise thresholds
  min_exercise_probability = 0.01,  # Below 1%, consider negligible
  high_exercise_warning = 0.40,     # Flag positions above 40%
  critical_exercise_threshold = 0.70,  # Red alert above 70%

  # Dividend modeling
  aristocrat_stress_dividend_cut = 0.0,    # Aristocrats don't cut in stress scenarios
  regular_stress_dividend_cut = 0.30,      # Regular payers cut 30% in crisis
  severe_stress_dividend_cut = 0.50,       # Non-aristocrats cut 50% in severe crisis

  # VaR/CVaR settings
  var_confidence_levels = c(0.95, 0.99),  # 95% and 99% VaR
  cvar_confidence_level = 0.95,           # Expected shortfall at 95%

  # Performance settings
  max_workers = 4,           # Conservative for API rate limits

  # UI settings
  show_progress = TRUE,
  cache_results = FALSE,     # User explicitly doesn't want caching

  # Historical data for correlation
  correlation_lookback_years = 2,
  min_observations_for_correlation = 250  # ~1 year of trading days
)

################################################################################
# STRESS TEST SCENARIOS
################################################################################

#' Pre-built Stress Test Scenarios
#'
#' Based on historical crises and academic research
#'
#' @export
STRESS_SCENARIOS <- list(

  #' 2008 Financial Crisis
  financial_crisis_2008 = list(
    name = "2008 Financial Crisis",
    description = "Global financial crisis with credit freeze",

    # Stock price changes by sector
    sector_returns = list(
      "Financials" = -0.60,
      "Consumer Discretionary" = -0.45,
      "Industrials" = -0.40,
      "Technology" = -0.35,
      "Energy" = -0.35,
      "Materials" = -0.40,
      "Consumer Staples" = -0.20,  # Defensive
      "Healthcare" = -0.25,         # Defensive
      "Utilities" = -0.30,
      "Real Estate" = -0.50,
      "Communication Services" = -0.35
    ),

    # Overall market if sector unknown
    default_return = -0.40,

    # Correlation spike (everything moves together in crisis)
    correlation_override = 0.90,

    # Volatility multiplier
    volatility_multiplier = 3.0,

    # Dividend cuts
    aristocrat_dividend_cut = 0.0,     # Aristocrats maintained
    regular_dividend_cut = 0.35,       # Many cut dividends

    # Interest rate (Fed cut to near zero)
    interest_rate_change_bps = -300
  ),

  #' 2020 COVID Crash
  covid_crash_2020 = list(
    name = "2020 COVID Crash",
    description = "Pandemic-driven market crash with rapid recovery",

    sector_returns = list(
      "Technology" = -0.15,            # Stay-at-home beneficiaries
      "Communication Services" = -0.10,
      "Consumer Discretionary" = -0.30,
      "Energy" = -0.50,                # Hardest hit
      "Financials" = -0.35,
      "Industrials" = -0.30,
      "Materials" = -0.25,
      "Consumer Staples" = -0.10,     # Defensive held up
      "Healthcare" = -0.05,            # Defensive + pandemic relevance
      "Utilities" = -0.20,
      "Real Estate" = -0.30
    ),

    default_return = -0.30,

    # High initial correlation, then divergence
    correlation_override = 0.80,

    volatility_multiplier = 2.5,

    # Dividends mostly maintained (unprecedented Fed support)
    aristocrat_dividend_cut = 0.0,
    regular_dividend_cut = 0.15,

    interest_rate_change_bps = -150
  ),

  #' Rising Rate Regime
  rising_rates = list(
    name = "Rising Rate Regime",
    description = "Fed aggressively raising rates to combat inflation",

    sector_returns = list(
      "Technology" = -0.30,           # Growth stocks hit hard
      "Communication Services" = -0.25,
      "Consumer Discretionary" = -0.20,
      "Real Estate" = -0.25,          # Rate-sensitive
      "Utilities" = -0.15,            # Rate-sensitive
      "Financials" = -0.10,           # Banks benefit from rates
      "Energy" = 0.05,                # Commodities hedge
      "Materials" = 0.0,
      "Industrials" = -0.15,
      "Consumer Staples" = -0.08,    # Defensive, stable
      "Healthcare" = -0.10            # Defensive
    ),

    default_return = -0.15,

    # Sector divergence (low correlation)
    correlation_override = 0.50,

    volatility_multiplier = 1.5,

    # Dividends stable (strong economy, just high rates)
    aristocrat_dividend_cut = 0.0,
    regular_dividend_cut = 0.05,

    interest_rate_change_bps = 300
  ),

  #' Stagflation
  stagflation = list(
    name = "Stagflation Scenario",
    description = "High inflation + slow growth (1970s style)",

    sector_returns = list(
      "Energy" = 0.10,               # Commodities benefit
      "Materials" = 0.05,
      "Consumer Staples" = -0.10,    # Pricing power helps but margins compressed
      "Healthcare" = -0.12,
      "Utilities" = -0.15,
      "Financials" = -0.20,
      "Industrials" = -0.25,
      "Consumer Discretionary" = -0.30,
      "Technology" = -0.25,
      "Communication Services" = -0.20,
      "Real Estate" = -0.25
    ),

    default_return = -0.20,

    correlation_override = 0.65,

    volatility_multiplier = 2.0,

    # Dividend cuts due to margin pressure
    aristocrat_dividend_cut = 0.0,   # Track record of growth through 1970s
    regular_dividend_cut = 0.25,

    interest_rate_change_bps = 200
  ),

  #' Volatility Spike
  volatility_spike = list(
    name = "Volatility Spike",
    description = "VIX doubles but prices relatively unchanged",

    # Minimal price changes
    sector_returns = list(),  # Use defaults
    default_return = -0.05,

    # Correlation unchanged
    correlation_override = NULL,  # Use historical

    # Key feature: volatility spike
    volatility_multiplier = 2.0,

    # No dividend impact
    aristocrat_dividend_cut = 0.0,
    regular_dividend_cut = 0.0,

    interest_rate_change_bps = 0
  )
)

################################################################################
# SECTOR MAPPING
################################################################################

#' Map tickers to sectors (for stress testing)
#'
#' Common sector classifications. Expand as needed.
#'
#' @export
TICKER_SECTOR_MAP <- list(
  # Financials
  "JPM" = "Financials", "BAC" = "Financials", "WFC" = "Financials",
  "C" = "Financials", "GS" = "Financials", "MS" = "Financials",

  # Technology
  "AAPL" = "Technology", "MSFT" = "Technology", "NVDA" = "Technology",
  "GOOGL" = "Technology", "META" = "Technology", "TSLA" = "Technology",
  "AMD" = "Technology", "INTC" = "Technology", "CRM" = "Technology",

  # Consumer Staples (many aristocrats here)
  "PG" = "Consumer Staples", "KO" = "Consumer Staples", "PEP" = "Consumer Staples",
  "WMT" = "Consumer Staples", "COST" = "Consumer Staples", "MCD" = "Consumer Staples",
  "CL" = "Consumer Staples", "KMB" = "Consumer Staples", "GIS" = "Consumer Staples",

  # Healthcare
  "JNJ" = "Healthcare", "UNH" = "Healthcare", "PFE" = "Healthcare",
  "ABBV" = "Healthcare", "MRK" = "Healthcare", "TMO" = "Healthcare",
  "ABT" = "Healthcare", "LLY" = "Healthcare",

  # Industrials
  "CAT" = "Industrials", "MMM" = "Industrials", "HON" = "Industrials",
  "UPS" = "Industrials", "BA" = "Industrials", "GE" = "Industrials",

  # Energy
  "XOM" = "Energy", "CVX" = "Energy", "COP" = "Energy",
  "SLB" = "Energy", "EOG" = "Energy",

  # Consumer Discretionary
  "AMZN" = "Consumer Discretionary", "HD" = "Consumer Discretionary",
  "NKE" = "Consumer Discretionary", "SBUX" = "Consumer Discretionary",
  "TGT" = "Consumer Discretionary",

  # Utilities
  "NEE" = "Utilities", "DUK" = "Utilities", "SO" = "Utilities",

  # Real Estate
  "AMT" = "Real Estate", "PLD" = "Real Estate", "SPG" = "Real Estate",

  # Materials
  "LIN" = "Materials", "APD" = "Materials", "ECL" = "Materials",

  # Communication Services
  "GOOGL" = "Communication Services", "META" = "Communication Services",
  "DIS" = "Communication Services", "NFLX" = "Communication Services"
)

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Get risk configuration value
#'
#' @param key Configuration key (optional, returns full config if NULL)
#' @return Configuration value or full list
#' @export
get_risk_config <- function(key = NULL) {
  if (is.null(key)) {
    return(RISK_CONFIG)
  }

  RISK_CONFIG[[key]]
}

#' Get stress scenario definition
#'
#' @param scenario_name Scenario key (e.g., "financial_crisis_2008")
#' @return Scenario definition list
#' @export
get_stress_scenario <- function(scenario_name) {
  if (!scenario_name %in% names(STRESS_SCENARIOS)) {
    stop("Unknown scenario: ", scenario_name)
  }

  STRESS_SCENARIOS[[scenario_name]]
}

#' Get ticker sector
#'
#' Looks up sector classification using a hybrid approach:
#' 1. Check hardcoded TICKER_SECTOR_MAP (instant)
#' 2. Fetch from Questrade API using symbol_id (cached)
#' 3. Return "Unknown" if not found
#'
#' @param ticker Stock ticker symbol
#' @param symbol_id Optional Questrade symbol ID (speeds up lookup)
#' @return Sector name or "Unknown" if not mapped
#' @export
get_ticker_sector <- function(ticker, symbol_id = NULL) {
  # Try hardcoded map first (instant)
  sector <- TICKER_SECTOR_MAP[[ticker]]
  if (!is.null(sector)) {
    return(sector)
  }

  # Try Questrade API (lazy-loaded from utils_sector_cache.R)
  sector <- lookup_sector(ticker, symbol_id)
  if (!is.null(sector) && sector != "" && !is.na(sector)) {
    return(sector)
  }

  # Default to Unknown
  return("Unknown")
}

#' Check if ticker is dividend aristocrat
#'
#' Reuses existing aristocrat detection logic
#'
#' @param ticker Stock ticker symbol
#' @return Logical TRUE if aristocrat
#' @noRd
is_aristocrat <- function(ticker) {
  # Will implement by checking against aristocrats list
  # For now, return FALSE (will enhance when integrating)
  FALSE
}
