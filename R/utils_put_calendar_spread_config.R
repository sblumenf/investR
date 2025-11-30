#' Put Calendar Spread Strategy Configuration
#'
#' Configuration constants and validation functions for put calendar spread strategy.
#' A calendar spread sells a near-term put and buys a longer-dated put at the same strike.
#'
#' @name put-calendar-spread-config
NULL

#' Put Calendar Spread Configuration Constants
#'
#' Configuration object containing all strategy-specific parameters for
#' put calendar spread analysis on dividend aristocrats.
#'
#' @format List with configuration parameters:
#' \describe{
#'   \item{strike_pct}{Target strike as % of current price (default 0.95 = ATM)}
#'   \item{short_expiry_min_days}{Minimum days for short leg (default 20)}
#'   \item{short_expiry_max_days}{Maximum days for short leg (default 45)}
#'   \item{short_expiry_target_days}{Target days for short leg (default 30)}
#'   \item{long_expiry_min_days}{Minimum days for long leg (default 50)}
#'   \item{long_expiry_max_days}{Maximum days for long leg (default 120)}
#'   \item{long_expiry_target_days}{Target days for long leg (default 60)}
#'   \item{min_expiry_ratio}{Minimum ratio of long/short DTE (default 1.5)}
#'   \item{max_workers}{Number of parallel workers (default 10)}
#'   \item{min_option_bid}{Minimum option bid price filter (default 0.05)}
#'   \item{min_open_interest}{Minimum open interest for liquidity (default 100)}
#'   \item{max_stock_price}{Maximum stock price filter (default 250)}
#'   \item{shares_per_contract}{Shares per option contract (default 100)}
#'   \item{days_per_year}{Days per year for annualization (default 365)}
#'   \item{profit_target_conservative}{Conservative profit target % (default 0.15)}
#'   \item{profit_target_standard}{Standard profit target % (default 0.20)}
#'   \item{profit_target_aggressive}{Aggressive profit target % (default 0.25)}
#'   \item{iv_rank_excellent}{IV Rank threshold for excellent (default 0.20)}
#'   \item{iv_rank_good}{IV Rank threshold for good (default 0.30)}
#'   \item{iv_rank_acceptable}{IV Rank threshold for acceptable (default 0.40)}
#'   \item{default_top_n}{Default number of results to display (default 10)}
#'   \item{output_dir}{Output directory for results (default "strategies")}
#' }
#' @export
PUT_CALENDAR_SPREAD_CONFIG <- list(

# Strike Selection (ATM preferred for dividend aristocrats per research)
strike_pct = 0.95,                      # 95% = ATM/slightly OTM puts

# Short Leg (Front Month) Expiration - sell this put
short_expiry_min_days = 20,             # Min days for short leg
short_expiry_max_days = 45,             # Max days for short leg
short_expiry_target_days = 30,          # Target 30 DTE (optimal theta decay)

# Long Leg (Back Month) Expiration - buy this put
long_expiry_min_days = 50,              # Min days for long leg
long_expiry_max_days = 120,             # Max days for long leg
long_expiry_target_days = 60,           # Target 60 DTE (2:1 ratio)

# Expiration Relationship
min_expiry_ratio = 1.5,                 # Long DTE must be >= 1.5x short DTE

# Parallel Processing
max_workers = 10,                       # Parallel processing workers

# Liquidity Requirements (higher for spreads per research)
min_option_bid = 0.05,                  # Higher min bid for calendar spreads
min_open_interest = 100,                # Higher OI requirement (100 per research)
max_stock_price = 250,                  # Skip very expensive stocks

# Financial Constants
shares_per_contract = 100,              # Standard option contract size
days_per_year = 365,                    # Days per year for annualization

# Profit Targets (adjusted for dividend aristocrats - lower but more consistent)
profit_target_conservative = 0.15,      # 15% of net debit
profit_target_standard = 0.20,          # 20% of net debit
profit_target_aggressive = 0.25,        # 25% of net debit

# IV Rank Thresholds (from research scoring framework)
# Lower IV favors calendar spreads - buy cheap back-month premium
iv_rank_excellent = 0.20,               # <20% IV Rank = excellent
iv_rank_good = 0.30,                    # 20-30% = good
iv_rank_acceptable = 0.40,              # 30-40% = acceptable, >40% avoid

# IV Ratio Thresholds (front IV / back IV)
iv_ratio_optimal_min = 1.05,            # Optimal when front > back
iv_ratio_optimal_max = 1.20,            # But not too much higher

# Scoring Weights (100-point framework from research)
score_weights = list(
  iv_rank = 25,                         # IV Rank weight (most important)
  iv_ratio = 15,                        # Front/Back IV ratio weight
  net_theta = 15,                       # Net theta weight
  vega = 10,                            # Net vega weight
  debit_profit_ratio = 15,              # Debit to profit ratio weight
  liquidity = 10,                       # Open interest weight
  technical = 5,                        # Range-bound indicator
  dividend_safety = 5                   # Ex-div timing weight
),

# Risk Parameters
max_loss_pct_of_account = 0.02,         # 2% max risk per position

# Output
default_top_n = 10,                     # Default results display
output_dir = "strategies"               # Output directory
)

#' Validate Put Calendar Spread Configuration
#'
#' Validates that configuration parameters are within acceptable ranges.
#' Ensures short expiry < long expiry and ratio requirements are met.
#'
#' @param config List of configuration parameters (defaults to PUT_CALENDAR_SPREAD_CONFIG)
#' @return TRUE if valid, throws error otherwise
#' @export
#' @examples
#' \dontrun{
#'   validate_calendar_spread_config(PUT_CALENDAR_SPREAD_CONFIG)
#' }
validate_calendar_spread_config <- function(config = PUT_CALENDAR_SPREAD_CONFIG) {
# Strike percentage validation
if (!is.numeric(config$strike_pct) ||
    config$strike_pct < 0.80 ||
    config$strike_pct > 1.05) {
  stop("strike_pct must be between 0.80 and 1.05")
}

# Short expiry validation
if (!is.numeric(config$short_expiry_min_days) || config$short_expiry_min_days < 7) {
  stop("short_expiry_min_days must be >= 7")
}

if (!is.numeric(config$short_expiry_max_days) ||
    config$short_expiry_max_days <= config$short_expiry_min_days) {
  stop("short_expiry_max_days must be > short_expiry_min_days")
}

# Long expiry validation
if (!is.numeric(config$long_expiry_min_days) || config$long_expiry_min_days < 30) {
  stop("long_expiry_min_days must be >= 30")
}

if (!is.numeric(config$long_expiry_max_days) ||
    config$long_expiry_max_days <= config$long_expiry_min_days) {
  stop("long_expiry_max_days must be > long_expiry_min_days")
}

# Critical: Long expiry must be greater than short expiry
if (config$long_expiry_min_days <= config$short_expiry_max_days) {
  stop("long_expiry_min_days must be > short_expiry_max_days to ensure valid calendar spread")
}

# Expiry ratio validation
if (!is.numeric(config$min_expiry_ratio) ||
    config$min_expiry_ratio < 1.0 ||
    config$min_expiry_ratio > 5.0) {
  stop("min_expiry_ratio must be between 1.0 and 5.0")
}

# Worker validation
if (!is.numeric(config$max_workers) ||
    config$max_workers < 1 ||
    config$max_workers > 50) {
  stop("max_workers must be between 1 and 50")
}

# Liquidity thresholds
if (!is.numeric(config$min_option_bid) || config$min_option_bid < 0) {
  stop("min_option_bid must be non-negative")
}

if (!is.numeric(config$min_open_interest) || config$min_open_interest < 0) {
  stop("min_open_interest must be non-negative")
}

if (!is.numeric(config$max_stock_price) || config$max_stock_price <= 0) {
  stop("max_stock_price must be positive")
}

# Financial constants validation
if (!is.numeric(config$shares_per_contract) || config$shares_per_contract != 100) {
  stop("shares_per_contract must be 100")
}

if (!is.numeric(config$days_per_year) || config$days_per_year <= 0) {
  stop("days_per_year must be positive")
}

# Profit target validation
if (!is.numeric(config$profit_target_conservative) ||
    config$profit_target_conservative <= 0 ||
    config$profit_target_conservative > 1) {
  stop("profit_target_conservative must be between 0 and 1")
}

# IV thresholds validation
if (!is.numeric(config$iv_rank_excellent) ||
    config$iv_rank_excellent < 0 ||
    config$iv_rank_excellent > 1) {
  stop("iv_rank_excellent must be between 0 and 1")
}

TRUE
}

#' Get Put Calendar Spread Configuration
#'
#' Accessor function for configuration with optional overrides.
#' Validates the resulting configuration after applying overrides.
#'
#' @param ... Named arguments to override default config values
#' @return List with configuration parameters
#' @export
#' @examples
#' \dontrun{
#'   # Get default config
#'   config <- get_calendar_spread_config()
#'
#'   # Get config with overrides
#'   config <- get_calendar_spread_config(
#'     strike_pct = 0.90,
#'     short_expiry_target_days = 21,
#'     max_workers = 4
#'   )
#' }
get_calendar_spread_config <- function(...) {
config <- PUT_CALENDAR_SPREAD_CONFIG
overrides <- list(...)

if (length(overrides) > 0) {
  for (name in names(overrides)) {
    if (name %in% names(config)) {
      config[[name]] <- overrides[[name]]
    } else if (name %in% names(config$score_weights)) {
      # Allow overriding nested score weights
      config$score_weights[[name]] <- overrides[[name]]
    } else {
      warning(sprintf("Unknown config parameter: %s", name))
    }
  }

  # Validate modified config
  validate_calendar_spread_config(config)
}

config
}

#' Calculate Opportunity Score
#'
#' Calculates a composite score (0-100) for a calendar spread opportunity
#' based on the research framework scoring system.
#'
#' @param iv_rank IV Rank (0-1)
#' @param iv_ratio Front IV / Back IV ratio
#' @param net_theta Net theta (should be positive)
#' @param net_vega Net vega (should be positive)
#' @param debit_profit_ratio Debit to estimated max profit ratio
#' @param open_interest Minimum OI of both legs
#' @param is_range_bound Boolean indicating if stock is range-bound
#' @param dividend_safe Boolean indicating if ex-div timing is safe
#' @param config Configuration object
#' @return Numeric score from 0-100
#' @export
calculate_calendar_score <- function(
iv_rank = NA,
iv_ratio = NA,
net_theta = NA,
net_vega = NA,
debit_profit_ratio = NA,
open_interest = NA,
is_range_bound = TRUE,
dividend_safe = TRUE,
config = PUT_CALENDAR_SPREAD_CONFIG
) {
weights <- config$score_weights
score <- 0

# IV Rank scoring (25 points max)
if (!is.na(iv_rank)) {
  if (iv_rank < config$iv_rank_excellent) {
    score <- score + weights$iv_rank
  } else if (iv_rank < config$iv_rank_good) {
    score <- score + weights$iv_rank * 0.8
  } else if (iv_rank < config$iv_rank_acceptable) {
    score <- score + weights$iv_rank * 0.4
  }
  # >40% IV rank = 0 points
}

# IV Ratio scoring (15 points max)
if (!is.na(iv_ratio)) {
  if (iv_ratio >= config$iv_ratio_optimal_min && iv_ratio <= config$iv_ratio_optimal_max) {
    score <- score + weights$iv_ratio
  } else if (iv_ratio >= 0.95 && iv_ratio < config$iv_ratio_optimal_min) {
    score <- score + weights$iv_ratio * 0.67
  }
  # Unfavorable ratio = 0 points
}
# Net Theta scoring (15 points max)
if (!is.na(net_theta)) {
  if (net_theta > 0.05) {
    score <- score + weights$net_theta
  } else if (net_theta > 0.02) {
    score <- score + weights$net_theta * 0.67
  } else if (net_theta > 0.01) {
    score <- score + weights$net_theta * 0.33
  }
}

# Vega scoring (10 points max)
if (!is.na(net_vega)) {
  if (net_vega > 0.15) {
    score <- score + weights$vega
  } else if (net_vega > 0.10) {
    score <- score + weights$vega * 0.7
  } else if (net_vega > 0.05) {
    score <- score + weights$vega * 0.5
  }
}

# Debit/Profit ratio scoring (15 points max)
# Lower ratio is better (1:2.5 means pay $1, profit $2.50)
if (!is.na(debit_profit_ratio)) {
  if (debit_profit_ratio <= 0.4) {        # 1:2.5 or better
    score <- score + weights$debit_profit_ratio
  } else if (debit_profit_ratio <= 0.5) { # 1:2
    score <- score + weights$debit_profit_ratio * 0.8
  } else if (debit_profit_ratio <= 0.67) { # 1:1.5
    score <- score + weights$debit_profit_ratio * 0.53
  }
}

# Liquidity scoring (10 points max)
if (!is.na(open_interest)) {
  if (open_interest > 500) {
    score <- score + weights$liquidity
  } else if (open_interest > 200) {
    score <- score + weights$liquidity * 0.7
  } else if (open_interest > 100) {
    score <- score + weights$liquidity * 0.5
  }
}

# Technical scoring (5 points max)
if (is_range_bound) {
  score <- score + weights$technical
}

# Dividend safety scoring (5 points max)
if (dividend_safe) {
  score <- score + weights$dividend_safety
}

round(score, 1)
}
