#' ETF Universe for Collar Strategy
#'
#' Functions for fetching and filtering liquid ETFs suitable for collar strategies.
#' Uses a curated list of major liquid ETFs that meet minimum liquidity requirements.
#'
#' @name collar-etf-universe
#' @importFrom dplyr filter %>% arrange desc
#' @importFrom logger log_info log_warn log_debug
NULL

# Curated list of liquid ETFs suitable for collar strategy
# These are major ETFs known to have:
# - High liquidity (>$1B market cap, >1M avg volume)
# - Active options markets
# - Reliable price discovery
LIQUID_ETFS_LIST <- c(
  # Major Index ETFs
  "SPY",   # S&P 500
  "QQQ",   # NASDAQ-100
  "IWM",   # Russell 2000
  "DIA",   # Dow Jones
  "VTI",   # Total Stock Market
  "VOO",   # S&P 500 (Vanguard)

  # Sector ETFs
  "XLF",   # Financials
  "XLE",   # Energy
  "XLK",   # Technology
  "XLV",   # Healthcare
  "XLI",   # Industrials
  "XLP",   # Consumer Staples
  "XLY",   # Consumer Discretionary
  "XLU",   # Utilities
  "XLB",   # Materials
  "XLRE",  # Real Estate
  "XLC",   # Communication Services

  # Bond ETFs
  "TLT",   # 20+ Year Treasury
  "IEF",   # 7-10 Year Treasury
  "LQD",   # Investment Grade Corporate
  "HYG",   # High Yield Corporate
  "AGG",   # Aggregate Bond
  "TIP",   # TIPS

  # International
  "EFA",   # EAFE (Developed ex-US)
  "EEM",   # Emerging Markets
  "VEA",   # FTSE Developed Markets
  "VWO",   # FTSE Emerging Markets

  # Commodity/Alternative
  "GLD",   # Gold
  "SLV",   # Silver
  "USO",   # Oil
  "UNG",   # Natural Gas

  # Growth/Tech
  "XLG",   # Top 50 Large Caps
  "VUG",   # Growth
  "MGK",   # Mega Cap Growth

  # Dividend
  "VYM",   # High Dividend Yield
  "SCHD",  # Dividend Appreciation
  "DVY",   # Dividend Aristocrats

  # Volatility
  "VXX",   # Short-term VIX futures

  # Leveraged (use with caution)
  "TQQQ",  # 3x NASDAQ
  "SQQQ",  # 3x Inverse NASDAQ
  "UPRO",  # 3x S&P 500
  "SPXU"   # 3x Inverse S&P 500
)

#' Get liquid ETFs for collar strategy
#'
#' Returns a filtered list of ETF tickers suitable for collar strategy based on
#' liquidity criteria. Currently uses a curated list of major liquid ETFs.
#'
#' Future enhancement: Could fetch from external data source and dynamically
#' filter by market cap and volume.
#'
#' @param min_market_cap Minimum market capitalization (default from config)
#' @param min_avg_volume Minimum average daily volume (default from config)
#' @return Character vector of ETF tickers
#' @export
#' @examples
#' \dontrun{
#'   # Get default liquid ETFs
#'   etfs <- get_liquid_etfs()
#'
#'   # Get ETFs with custom filters (currently uses curated list)
#'   etfs <- get_liquid_etfs(min_market_cap = 5e9, min_avg_volume = 5e6)
#' }
get_liquid_etfs <- function(min_market_cap = COLLAR_CONFIG$min_market_cap,
                            min_avg_volume = COLLAR_CONFIG$min_avg_volume) {

  log_info("Fetching liquid ETFs for collar strategy...")
  log_info("Filters: Market cap >= ${format(min_market_cap / 1e9, nsmall = 1)}B, Volume >= {format(min_avg_volume / 1e6, nsmall = 1)}M")

  # For now, use curated list which is known to meet criteria
  # Future enhancement: Fetch from external source and filter dynamically
  etfs <- LIQUID_ETFS_LIST

  log_info("Found {length(etfs)} liquid ETFs")

  return(etfs)
}

#' Get specific ETF subsets
#'
#' Helper functions to get specific types of ETFs
#'
#' @name etf-subsets
#' @return Character vector of ETF tickers
#' @export
NULL

#' @describeIn etf-subsets Get major index ETFs only
#' @export
get_index_etfs <- function() {
  c("SPY", "QQQ", "IWM", "DIA", "VTI", "VOO")
}

#' @describeIn etf-subsets Get sector ETFs only
#' @export
get_sector_etfs <- function() {
  c("XLF", "XLE", "XLK", "XLV", "XLI", "XLP", "XLY", "XLU", "XLB", "XLRE", "XLC")
}

#' @describeIn etf-subsets Get bond ETFs only
#' @export
get_bond_etfs <- function() {
  c("TLT", "IEF", "LQD", "HYG", "AGG", "TIP")
}

#' @describeIn etf-subsets Get commodity ETFs only
#' @export
get_commodity_etfs <- function() {
  c("GLD", "SLV", "USO", "UNG")
}
