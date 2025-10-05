#' Financial Calculations
#'
#' Shared financial metric calculations used across multiple strategies.
#' Provides standardized implementations of returns, risk metrics, and
#' performance measures.
#'
#' @name calculations
#' @importFrom quantmod Cl dailyReturn
#' @importFrom stats sd
NULL

#' Calculate annualized return
#'
#' Converts a total return over a given period to an annualized rate.
#' Uses compound growth formula: (1 + total_return)^(1/years) - 1
#'
#' @param total_return Total return as decimal (0.15 = 15%)
#' @param days Number of days for the investment
#' @param days_per_year Days per year for annualization (default 365)
#' @return Annualized return as decimal (0.10 = 10%)
#' @export
#' @examples
#' \dontrun{
#'   # 15% return over 90 days
#'   calculate_annualized_return(0.15, 90)  # Returns ~0.73 (73% annualized)
#' }
calculate_annualized_return <- function(total_return,
                                       days,
                                       days_per_year = 365) {
  if (days <= 0) {
    return(0)
  }

  years <- days / days_per_year

  # Annualized return = (1 + total_return) ^ (1/years) - 1
  annualized <- if (years > 0) {
    (1 + total_return)^(1/years) - 1
  } else {
    0
  }

  return(annualized)
}

#' Calculate compound annual return from periodic return
#'
#' Projects annual return assuming periodic returns are compounded.
#' Formula: (1 + periodic_return)^periods_per_year - 1
#'
#' @param periodic_return Return per period as decimal (0.002 = 0.2%)
#' @param periods_per_year Number of periods per year (e.g., 52 for weekly)
#' @return Compound annual return as decimal
#' @export
#' @examples
#' \dontrun{
#'   # 0.2% weekly return compounded 52 times
#'   calculate_compound_annual_return(0.002, 52)  # Returns ~0.109 (10.9%)
#' }
calculate_compound_annual_return <- function(periodic_return,
                                            periods_per_year) {
  compound_return <- (1 + periodic_return)^periods_per_year - 1
  return(compound_return)
}

#' Calculate maximum drawdown from price series
#'
#' Measures the maximum peak-to-trough decline in a price series.
#' Useful for understanding downside risk.
#'
#' @param price_series xts object with price data (typically closing prices)
#' @return Maximum drawdown as negative decimal (-0.25 = -25% drawdown)
#' @export
calculate_max_drawdown <- function(price_series) {
  if (is.null(price_series) || length(price_series) == 0) {
    return(NA)
  }

  tryCatch({
    # Calculate daily returns
    daily_returns <- dailyReturn(price_series, type = "arithmetic")

    # Calculate cumulative returns
    cumulative_returns <- cumprod(1 + daily_returns)

    # Running maximum
    running_max <- cummax(cumulative_returns)

    # Drawdown at each point
    drawdown <- (cumulative_returns - running_max) / running_max

    # Maximum drawdown (most negative value)
    max_dd <- min(drawdown, na.rm = TRUE)

    return(max_dd)

  }, error = function(e) {
    return(NA)
  })
}

#' Calculate Sharpe ratio
#'
#' Measures risk-adjusted return. Higher values indicate better
#' risk-adjusted performance.
#'
#' Formula: (mean_return - risk_free_rate) / std_dev_returns
#'
#' @param returns Numeric vector of returns (as decimals)
#' @param risk_free_rate Risk-free rate per period (as decimal)
#' @return Sharpe ratio (dimensionless), or 0 if std dev is zero
#' @export
#' @examples
#' \dontrun{
#'   returns <- c(0.01, 0.02, -0.005, 0.015, 0.008)
#'   risk_free <- 0.0001  # per period
#'   calculate_sharpe_ratio(returns, risk_free)
#' }
calculate_sharpe_ratio <- function(returns, risk_free_rate = 0) {
  if (length(returns) == 0) {
    return(0)
  }

  mean_return <- mean(returns, na.rm = TRUE)
  std_dev <- sd(returns, na.rm = TRUE)

  # Handle zero standard deviation (no volatility)
  if (is.na(std_dev) || std_dev == 0) {
    return(0)
  }

  excess_return <- mean_return - risk_free_rate
  sharpe <- excess_return / std_dev

  return(sharpe)
}

#' Calculate annualized Sharpe ratio
#'
#' Converts per-period Sharpe ratio to annualized version.
#' Formula: sharpe_ratio * sqrt(periods_per_year)
#'
#' @param sharpe_ratio Per-period Sharpe ratio
#' @param periods_per_year Number of periods per year (e.g., 252 for daily, 52 for weekly)
#' @return Annualized Sharpe ratio
#' @export
calculate_annual_sharpe <- function(sharpe_ratio, periods_per_year) {
  annual_sharpe <- sharpe_ratio * sqrt(periods_per_year)
  return(annual_sharpe)
}

#' Calculate annualized Sortino ratio
#'
#' Converts per-period Sortino ratio to annualized version.
#' Formula: sortino_ratio * sqrt(periods_per_year)
#'
#' @param sortino_ratio Per-period Sortino ratio
#' @param periods_per_year Number of periods per year (e.g., 252 for daily, 52 for weekly, 12 for monthly)
#' @return Annualized Sortino ratio
#' @export
calculate_annual_sortino <- function(sortino_ratio, periods_per_year) {
  annual_sortino <- sortino_ratio * sqrt(periods_per_year)
  return(annual_sortino)
}

#' Calculate Sortino ratio
#'
#' Similar to Sharpe ratio but uses downside deviation instead of
#' total standard deviation. Penalizes downside volatility only.
#'
#' Formula: (mean_return - risk_free_rate) / downside_std_dev
#'
#' @param returns Numeric vector of returns (as decimals)
#' @param risk_free_rate Risk-free rate per period (as decimal)
#' @return Sortino ratio, Inf if no losing returns, 0 if downside std dev is zero
#' @export
calculate_sortino_ratio <- function(returns, risk_free_rate = 0) {
  if (length(returns) == 0) {
    return(0)
  }

  mean_return <- mean(returns, na.rm = TRUE)
  excess_return <- mean_return - risk_free_rate

  # Only consider returns below the risk-free rate (downside)
  downside_returns <- returns[returns < risk_free_rate]

  # If no downside returns, Sortino ratio is infinite (perfect)
  if (length(downside_returns) == 0) {
    return(Inf)
  }

  downside_std <- sd(downside_returns, na.rm = TRUE)

  # Handle zero downside deviation
  if (is.na(downside_std) || downside_std == 0) {
    return(0)
  }

  sortino <- excess_return / downside_std

  return(sortino)
}

#' Calculate success rate
#'
#' Percentage of profitable trades/periods
#'
#' @param returns Numeric vector of returns (as decimals)
#' @param threshold Threshold for "success" (default 0 for positive returns)
#' @return Success rate as decimal (0.75 = 75%)
#' @export
calculate_success_rate <- function(returns, threshold = 0) {
  if (length(returns) == 0) {
    return(0)
  }

  successful <- sum(returns > threshold, na.rm = TRUE)
  total <- sum(!is.na(returns))

  if (total == 0) {
    return(0)
  }

  success_rate <- successful / total

  return(success_rate)
}
