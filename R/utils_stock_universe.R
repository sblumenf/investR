#' Stock Universe Fetching Functions
#'
#' Centralized functions for fetching and filtering stock universes.
#' Supports caching to minimize expensive API calls.
#'
#' @name stock-universe
#' @importFrom rvest read_html html_table
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr filter %>%
#' @importFrom purrr map_lgl compact
#' @importFrom logger log_info log_warn log_debug log_success
#' @importFrom httr GET user_agent write_disk timeout
NULL

# Russell 1000 fallback list (iShares IWB holdings as of 2026-04-23)
# Used when the live IWB download fails (network outage, URL change, etc.)
RUSSELL1000_FALLBACK_TICKERS <- c(
  "NVDA", "AAPL", "MSFT", "AMZN", "GOOGL", "AVGO", "GOOG", "META", "TSLA", "BRK.B",
  "JPM", "LLY", "XOM", "WMT", "JNJ", "MU", "V", "AMD", "COST", "MA",
  "NFLX", "CAT", "ABBV", "CSCO", "CVX", "BAC", "PG", "HD", "LRCX", "UNH",
  "AMAT", "GEV", "PLTR", "ORCL", "KO", "INTC", "GE", "MRK", "GS", "PM",
  "TXN", "WFC", "RTX", "KLAC", "LIN", "C", "MCD", "IBM", "MS", "PEP",
  "NEE", "VZ", "ADI", "AMGN", "DIS", "T", "APH", "ANET", "TJX", "BA",
  "TMO", "AXP", "ISRG", "GILD", "ETN", "UNP", "BLK", "ABT", "CRM", "COP",
  "PFE", "UBER", "SCHW", "DE", "BKNG", "QCOM", "WELL", "MRVL", "PANW", "LOW",
  "WDC", "HON", "SNDK", "SPGI", "GLW", "PLD", "PH", "VRT", "NEM", "CB",
  "COF", "BMY", "PGR", "CMCSA", "SYK", "MO", "SBUX", "DHR", "VRTX", "CRWD",
  "ACN", "LMT", "EQIX", "APP", "TT", "MDT", "INTU", "SO", "MCK", "CME",
  "DUK", "HWM", "CVS", "ADBE", "BSX", "TMUS", "PWR", "BK", "CEG", "BX",
  "CMI", "ICE", "NOW", "USB", "FCX", "PNC", "SNPS", "WMB", "JCI", "CSX",
  "WM", "CDNS", "MRSH", "FDX", "AMT", "SLB", "MAR", "GD", "EMR", "ADP",
  "SPOT", "ITW", "UPS", "NOC", "ORLY", "CRH", "SHW", "REGN", "MMM", "HCA",
  "ELV", "HLT", "MDLZ", "MSI", "MPWR", "CI", "EOG", "CIEN", "NSC", "AEP",
  "ROST", "VLO", "GM", "MCO", "DLR", "KKR", "ECL", "AON", "APD", "CL",
  "TRV", "MPC", "DELL", "RCL", "SPG", "PCAR", "TFC", "DASH", "PSX", "TDG",
  "NET", "BKR", "HOOD", "WBD", "TER", "URI", "FIX", "LHX", "KMI", "SRE",
  "CTAS", "AZO", "KEYS", "LITE", "ABNB", "TGT", "COHR", "O", "COR", "ALL",
  "AJG", "LNG", "OKE", "CVNA", "AME", "MNST", "AFL", "CTVA", "MSTR", "D",
  "VST", "APO", "NKE", "FAST", "FTNT", "ZTS", "EA", "ETR", "TRGP", "GWW",
  "FERG", "ADSK", "NUE", "PSA", "CAH", "AU", "F", "SNOW", "CARR", "NU",
  "MCHP", "EXC", "XEL", "PYPL", "EBAY", "ROK", "EW", "WAB", "FITB", "YUM",
  "DAL", "IDXX", "CMG", "CBRE", "BDX", "COIN", "RSG", "AMP", "DHI", "MSCI",
  "GRMN", "MET", "STT", "RKLB", "ODFL", "AIG", "OXY", "KR", "PEG", "ON",
  "DDOG", "ED", "ALNY", "NDAQ", "HIG", "ROP", "EME", "VTR", "VMC", "FANG",
  "CCI", "TTWO", "XYZ", "WEC", "MLM", "HPE", "KDP", "PCG", "SYY", "JBL",
  "EQT", "ACGL", "RBLX", "IRM", "HBAN", "IR", "MTB", "ADM", "KVUE", "PRU",
  "FISV", "HAL", "A", "IBKR", "KMB", "FLEX", "GEHC", "RMD", "NRG", "NTRS",
  "OTIS", "CBOE", "DOV", "WAT", "STLD", "VICI", "CCL", "DTE", "TDY", "EXR",
  "TPR", "AEE", "INSM", "HUBB", "ATO", "UAL", "CPRT", "DVN", "AXON", "XYL",
  "FTI", "EXPE", "Q", "PAYX", "CASY", "PPL", "CNP", "CFG", "HSY", "WTW",
  "QSR", "BIIB", "FE", "DOW", "RJF", "IQV", "CPNG", "DG", "EIX", "CW",
  "SYF", "CTSH", "NTRA", "MTD", "AWK", "CINF", "ES", "HUM", "LPLA", "PHM",
  "ALAB", "XPO", "ULTA", "TPL", "CTRA", "PPG", "WDAY", "RF", "VRSK", "OMC",
  "AVB", "DXCM", "LYV", "UTHR", "FIS", "FTAI", "ZM", "MKL", "SBAC", "EQR",
  "MTZ", "DRI", "VEEV", "VRSN", "CMS", "RVMD", "NI", "FICO", "TROW", "CHD",
  "NVT", "WSM", "STZ", "ALB", "DGX", "ENTG", "SOFI", "LH", "VLTO", "WST",
  "WWD", "EXE", "STE", "P", "CHRW", "NTAP", "ARES", "CPAY", "EFX", "RGLD",
  "PFG", "SW", "BURL", "USFD", "KEY", "ATI", "CRS", "EXPD", "BWXT", "MDB",
  "TSCO", "SNA", "CHTR", "CNC", "TWLO", "FSLR", "RDDT", "JBHT", "DLTR", "RBA",
  "CF", "BRO", "FTV", "DD", "RPRX", "LEN", "ILMN", "L", "MKSI", "GIS",
  "PKG", "HPQ", "MTSI", "EVRG", "LDOS", "EL", "KHC", "MRNA", "LNT", "FWONK",
  "RBC", "ITT", "ZBH", "BAM", "BR", "IFF", "WY", "TSN", "RS", "APG",
  "LYB", "AMCR", "NVR", "CDW", "FCNCA", "IP", "LUV", "ASTS", "FFIV", "VTRS",
  "BALL", "AFRM", "ROL", "SNX", "AA", "EWBC", "BG", "FLUT", "INVH", "THC",
  "GPN", "PTC", "ESS", "OVV", "LVS", "LSCC", "WPC", "WCC", "SGI", "TRMB",
  "NLY", "DECK", "SUI", "TXT", "JLL", "MAS", "IEX", "KIM", "LII", "TLN",
  "CSGP", "CLH", "HEIA", "WRB", "SCCO", "NDSN", "INCY", "WSO", "GPC", "J",
  "CSL", "MDLN", "LULU", "PR", "EG", "MAA", "SSNC", "PNR", "MLI", "ROKU",
  "REG", "PNFP", "TYL", "HII", "HST", "RKT", "TOL", "ONTO", "QXO", "LECO",
  "FNF", "DKS", "TRU", "RRX", "RNR", "VIK", "RL", "PFGC", "RGA", "AKAM",
  "OHI", "SMCI", "GGG", "FOXA", "APA", "HAS", "DTM", "PODD", "RPM", "ZS",
  "TW", "BLD", "MKC", "UNM", "RIVN", "APTV", "ALGN", "AVY", "FIVE", "NYT",
  "EQH", "SOLS", "COO", "BJ", "TOST", "CG", "TKO", "GLPI", "GNRC", "ALLY",
  "NBIX", "EVR", "ALLE", "PEN", "OKTA", "FHN", "EXEL", "PNW", "GL", "BWA",
  "ARMK", "CLX", "SCI", "ELS", "AGNC", "CCK", "ROIV", "WBS", "BBY", "DPZ",
  "SWK", "JAZZ", "LAMR", "PINS", "GDDY", "ZBRA", "SAIA", "CACI", "UDR", "AIZ",
  "WTRG", "IONS", "SF", "CRCL", "DOC", "HEI", "ALSN", "TEAM", "AIT", "JKHY",
  "GWRE", "CPT", "AR", "ELAN", "COKE", "HUBS", "IT", "WMS", "TXRH", "ACM",
  "DKNG", "GMED", "OC", "AES", "EGP", "FLS", "AMH", "GEN", "RVTY", "DCI",
  "GME", "KNX", "SOLV", "DT", "TTD", "BMRN", "NWSA", "NTNX", "ARW", "RRC",
  "EHC", "UHS", "WTFC", "OSK", "SJM", "SSB", "ZION", "BLDR", "R", "OGE",
  "VMI", "BPOP", "LFUS", "DAR", "BAH", "FRT", "DINO", "TTC", "SN", "BRX",
  "CNM", "SWKS", "BAX", "IVZ", "CGNX", "SEIC", "PRI", "CAVA", "CNH", "IOT",
  "ORI", "MUSA", "AMKR", "MP", "DOCU", "AYI", "VNOM", "BXP", "MOH", "CUBE",
  "CRUS", "MTCH", "ADC", "COLB", "CR", "AFG", "MEDP", "FDS", "TIGO", "FOX",
  "ESI", "REXR", "HLI", "WAL", "CFR", "NFG", "KEX", "ATR", "TECH", "TTEK",
  "MANH", "EMN", "NNN", "LKQ", "HSIC", "NCLH", "CRL", "UGI", "MASI", "AXS",
  "WYNN", "CHRD", "XP", "MGM", "ONON", "FR", "AMG", "ARE", "IDA", "SSD",
  "AAL", "QGEN", "MOS", "HALO", "U", "AWI", "AGCO", "KNSL", "VOYA", "GFS",
  "POOL", "NOV", "AOS", "WFRD", "W", "CART", "RBRK", "TAP", "STAG", "DOX",
  "HXL", "AM", "Z", "BEN", "INGR", "MTDR", "QRVO", "CE", "SFM", "VFC",
  "ECG", "JEF", "MIDD", "CAG", "EXP", "OMF", "LEA", "CBSH", "BROS", "H",
  "AAON", "TKR", "PCOR", "EPAM", "FAF", "JHG", "LAD", "GXO", "SITE", "WH",
  "MSA", "THG", "LNC", "PAYC", "FNB", "GTES", "LSTR", "AXTA", "RYN", "MKTX",
  "HRL", "PB", "CHDN", "MTG", "AVT", "LBRDK", "STWD", "ETSY", "TFX", "HR",
  "RGEN", "LW", "ACI", "CHWY", "ST", "KMX", "VSNT", "ESAB", "GAP", "FCN",
  "NXST", "PLNT", "OWL", "CZR", "MHK", "AUR", "TPG", "LYFT", "SARO", "AN",
  "WEX", "CELH", "G", "DVA", "BEPC", "CHE", "M", "WTM", "OZK", "VNT",
  "BIO", "FND", "ALGM", "GNTX", "AS", "MSGS", "SIRI", "OLLI", "CROX", "RITM",
  "WING", "PRMB", "BYD", "RAL", "VNO", "BSY", "BC", "AVTR", "FBIN", "POST",
  "CLF", "SLM", "UHALB", "TEM", "SON", "EXLS", "LPX", "ENPH", "MAT", "ALK",
  "MRP", "BFAM", "RLI", "TREX", "SAIC", "KBR", "LOPE", "MDU", "PVH", "NVST",
  "DBX", "BBWI", "MTN", "PCTY", "UI", "LAZ", "OLED", "LLYVK", "MSM", "APLS",
  "CORT", "VVV", "BFB", "MORN", "S", "HLNE", "AMTM", "EPR", "HRB", "THO",
  "CPB", "CAR", "CUZ", "VIRT", "TNL", "PATH", "NEU", "ESTC", "WSC", "KRMN",
  "DLB", "NWS", "IRDM", "AGO", "DUOL", "SHC", "BRKR", "ADT", "WLK", "BHF",
  "RYAN", "KRC", "COLD", "ELF", "VKTX", "YETI", "FHB", "SLGN", "APPF", "RNG",
  "JHX", "CWEN", "IAC", "DRS", "CHH", "KD", "FOUR", "IPGP", "QS", "FRPT",
  "BOKF", "HAYW", "PEGA", "DOCS", "OLN", "NSA", "BILL", "EEFT", "WHR", "CCC",
  "PAG", "SMG", "RHI", "GPK", "PSN", "HOG", "GTLB", "PENN", "TDC", "WU",
  "MPT", "SMMT", "HIW", "FRHC", "ASH", "LINE", "LOAR", "VGNT", "ACHC", "XRAY",
  "CACC", "PK", "HUN", "OGN", "CXT", "BIRK", "BRBR", "DXC", "HHH", "RARE",
  "ZG", "KMPR", "FWONA", "RH", "LBTYA", "SAM", "SRPT", "DDS", "GLOB", "NCNO",
  "CAI", "FMC", "NWL", "LLYVA", "SEB", "INSP", "LBTYK", "GTM", "FLO", "COLM",
  "PRGO", "DJT", "DV", "BFA", "SNDR", "BLSH", "MAN", "CWENA", "SFD", "PPC",
  "UAA", "WEN", "UA", "CNXC", "CNA", "GLIBK", "FIGR", "REYN", "CLVT", "LBRDA",
  "COTY", "TFSL", "LENB", "LCID", "SAIL", "CERT", "UWMC", "INGM", "NIQ", "CBC",
  "UHAL", "FRMI", "GLIBA"
)

#' Cache directory path
#'
#' @return Path to cache directory
#' @noRd
get_cache_dir <- function() {
  cache_dir <- system.file("cache", package = "investR")

  # If package cache doesn't exist, use inst/cache for development
  if (cache_dir == "") {
    cache_dir <- here::here("inst", "cache")
  }

  # Create directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  cache_dir
}

#' Check if cached data is fresh
#'
#' @param cache_file Cache file name
#' @param max_age_days Maximum age in days (default 30)
#' @return TRUE if cache exists and is fresh, FALSE otherwise
#' @noRd
is_cache_fresh <- function(cache_file, max_age_days = 30) {
  cache_path <- file.path(get_cache_dir(), cache_file)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  tryCatch({
    cached_data <- readRDS(cache_path)

    if (is.null(cached_data$timestamp)) {
      return(FALSE)
    }

    days_old <- as.numeric(difftime(Sys.time(), cached_data$timestamp, units = "days"))

    if (days_old <= max_age_days) {
      log_debug("Cache is fresh ({round(days_old, 1)} days old)")
      return(TRUE)
    } else {
      log_debug("Cache is stale ({round(days_old, 1)} days old)")
      return(FALSE)
    }

  }, error = function(e) {
    log_warn("Error reading cache: {e$message}")
    return(FALSE)
  })
}

#' Save data to cache
#'
#' @param cache_file Cache file name
#' @param data Data to cache
#' @noRd
save_to_cache <- function(cache_file, data) {
  tryCatch({
    cache_path <- file.path(get_cache_dir(), cache_file)

    cached_data <- list(
      data = data,
      timestamp = Sys.time()
    )

    saveRDS(cached_data, cache_path)
    log_debug("Saved to cache: {cache_file}")

  }, error = function(e) {
    log_warn("Failed to save cache: {e$message}")
  })
}

#' Load data from cache
#'
#' @param cache_file Cache file name
#' @return Cached data or NULL
#' @noRd
load_from_cache <- function(cache_file) {
  tryCatch({
    cache_path <- file.path(get_cache_dir(), cache_file)
    cached_data <- readRDS(cache_path)
    return(cached_data$data)
  }, error = function(e) {
    log_warn("Failed to load cache: {e$message}")
    return(NULL)
  })
}

#' Fetch S&P 500 list from Wikipedia
#'
#' Primary source for S&P 500 constituents. Uses rvest to scrape the
#' Wikipedia page maintained by S&P Dow Jones Indices.
#'
#' @return Character vector of tickers, or NULL on failure
#' @noRd
fetch_sp500_from_wikipedia <- function() {
  safely_fetch(
    expr = {
      url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
      log_debug("Fetching S&P 500 list from Wikipedia...")

      tables <- read_html(url) %>%
        html_table(fill = TRUE)

      if (length(tables) == 0) {
        stop("No tables found on Wikipedia page")
      }

      # First table contains the constituents
      sp500_df <- tables[[1]]

      if (!"Symbol" %in% names(sp500_df)) {
        stop("Symbol column not found in Wikipedia table")
      }

      tickers <- sp500_df$Symbol %>%
        na.omit() %>%
        unique() %>%
        as.character()

      # Sanity check: S&P 500 should have ~500 stocks
      if (length(tickers) < 400) {
        stop("Too few tickers found ({length(tickers)}), expected ~500")
      }

      log_success("Found {length(tickers)} S&P 500 stocks from Wikipedia")
      return(tickers)
    },
    error_msg = "Wikipedia fetch failed",
    default = NULL,
    log_level = "warn"
  )
}

#' Fetch S&P 500 list from DataHub.io
#'
#' Secondary source for S&P 500 constituents. Uses direct CSV download.
#'
#' @return Character vector of tickers, or NULL on failure
#' @noRd
fetch_sp500_from_datahub <- function() {
  safely_fetch(
    expr = {
      url <- "https://datahub.io/core/s-and-p-500-companies/r/constituents.csv"
      log_debug("Fetching S&P 500 list from DataHub.io...")

      sp500_df <- read_csv(url, show_col_types = FALSE, col_types = cols())

      if (!"Symbol" %in% names(sp500_df)) {
        stop("Symbol column not found in DataHub CSV")
      }

      tickers <- sp500_df$Symbol %>%
        na.omit() %>%
        unique() %>%
        as.character()

      if (length(tickers) < 400) {
        stop("Too few tickers found ({length(tickers)}), expected ~500")
      }

      log_success("Found {length(tickers)} S&P 500 stocks from DataHub")
      return(tickers)
    },
    error_msg = "DataHub fetch failed",
    default = NULL,
    log_level = "warn"
  )
}

#' Fetch Russell 1000 constituents from iShares IWB ETF holdings CSV
#'
#' @return Character vector of tickers, or NULL on failure
#' @noRd
fetch_russell1000_from_iwb <- function() {
  safely_fetch(
    expr = {
      url <- "https://www.ishares.com/us/products/239707/ishares-russell-1000-etf/1467271812596.ajax?fileType=csv&fileName=IWB_holdings&dataType=fund"
      log_debug("Fetching Russell 1000 constituent list from iShares IWB...")

      tmp <- tempfile(fileext = ".csv")
      on.exit(unlink(tmp), add = TRUE)

      resp <- httr::GET(
        url,
        httr::user_agent("Mozilla/5.0"),
        httr::write_disk(tmp, overwrite = TRUE),
        httr::timeout(30)
      )
      httr::stop_for_status(resp)

      df <- suppressWarnings(readr::read_csv(
        tmp,
        skip = 9,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE,
        name_repair = "minimal"
      ))

      equity_rows <- df[!is.na(df[["Asset Class"]]) & df[["Asset Class"]] == "Equity", ]
      tickers <- equity_rows[["Ticker"]]
      tickers <- tickers[!is.na(tickers) & nzchar(trimws(tickers)) & tickers != "-"]
      tickers <- trimws(tickers)

      if (length(tickers) < 800) {
        stop("Too few tickers from IWB ({length(tickers)}), expected ~1000")
      }

      log_success("Found {length(tickers)} Russell 1000 stocks from iShares IWB")
      return(tickers)
    },
    error_msg = "IWB holdings fetch failed",
    default = NULL,
    log_level = "warn"
  )
}

#' Get S&P 500 stock list with caching and fallback
#'
#' Fetches the current list of S&P 500 constituent stocks. Uses Wikipedia as
#' primary source, DataHub as secondary, and hardcoded fallback as last resort.
#' Results are cached for 30 days to minimize API calls.
#'
#' @return Character vector of S&P 500 ticker symbols
#' @export
#' @examples
#' \dontrun{
#'   sp500_stocks <- get_sp500_stocks()
#'   length(sp500_stocks)  # Should be ~500
#' }
get_sp500_stocks <- function() {
  cache_file <- "sp500_stocks.rds"

  # Check cache first
  if (is_cache_fresh(cache_file, max_age_days = 30)) {
    log_info("Using cached Russell 1000 stock list")
    return(load_from_cache(cache_file))
  }

  # Load current cached list for diff comparison (may be NULL if cache absent)
  cached_tickers <- tryCatch(load_from_cache(cache_file), error = function(e) NULL)

  # Try IWB (Russell 1000) — primary source
  tickers <- fetch_russell1000_from_iwb()

  if (!is.null(tickers)) {
    # Log diff vs cached list when updating
    if (!is.null(cached_tickers)) {
      added   <- setdiff(tickers, cached_tickers)
      removed <- setdiff(cached_tickers, tickers)
      if (length(added) > 0 || length(removed) > 0) {
        log_info("Stock universe updated: +{length(added)} added, -{length(removed)} removed")
      } else {
        log_info("Stock universe unchanged ({length(tickers)} stocks)")
      }
    }
    save_to_cache(cache_file, tickers)
    return(tickers)
  }

  # IWB unavailable — use hardcoded Russell 1000 fallback (not S&P 500 fallbacks)
  log_warn("IWB download failed — using hardcoded Russell 1000 fallback list")
  log_info("Found {length(RUSSELL1000_FALLBACK_TICKERS)} stocks from fallback")
  return(RUSSELL1000_FALLBACK_TICKERS)
}

#' Check if a stock pays dividends
#'
#' Uses dividend history from Yahoo Finance to determine if stock pays dividends.
#'
#' @param ticker Stock ticker symbol
#' @param lookback_years Years of history to check (default 2)
#' @return TRUE if pays dividends, FALSE if zero dividend
#' @noRd
stock_pays_dividend <- function(ticker, lookback_years = 2) {
  safely_fetch(
    expr = {
      start_date <- Sys.Date() - lubridate::years(lookback_years)

      dividends <- fetch_dividend_history(ticker, from = start_date)

      # NULL means fetch_dividend_history() failed (network error, rate limit, etc.)
      # Stop here so safely_fetch returns NA (unknown) instead of FALSE (no dividends)
      if (is.null(dividends)) {
        stop("Dividend data unavailable for lookup")
      }
      # Empty xts object means no dividends were paid
      # Note: nrow() on empty xts returns NULL, so use length() instead
      if (length(dividends) == 0) {
        return(FALSE)
      }

      # Has dividend history
      return(TRUE)
    },
    error_msg = "{ticker}: Error checking dividend status",
    default = NA,  # Return NA for errors
    log_level = "debug"
  )
}

#' Get zero-dividend S&P 500 stocks with caching
#'
#' Filters S&P 500 for stocks that don't pay dividends. Results are cached
#' for 30 days to avoid expensive repeated scans.
#'
#' @param limit Optional limit on number of stocks to check (for testing)
#' @param max_workers Number of parallel workers to use (default 4)
#' @return Character vector of zero-dividend ticker symbols
#' @export
#' @examples
#' \dontrun{
#'   zero_div_stocks <- get_zero_dividend_stocks()
#'   length(zero_div_stocks)  # Should be ~80-100
#'
#'   # Test with limit
#'   zero_div_stocks <- get_zero_dividend_stocks(limit = 50)
#' }
get_zero_dividend_stocks <- function(limit = NULL, max_workers = 10) {
  cache_file <- "zero_dividend_stocks.rds"

  # Check cache first (unless limit specified, which is for testing)
  if (is.null(limit) && is_cache_fresh(cache_file, max_age_days = 30)) {
    log_info("Using cached zero-dividend stocks list")
    return(load_from_cache(cache_file))
  }

  # Get S&P 500 universe
  sp500_stocks <- get_sp500_stocks()

  # Apply limit if specified (for testing)
  if (!is.null(limit)) {
    sp500_stocks <- head(sp500_stocks, limit)
    log_info("Limiting scan to first {limit} S&P 500 stocks (testing mode)")
  } else {
    log_info("Scanning {length(sp500_stocks)} S&P 500 stocks for dividend status...")
    log_info("This may take 5-10 minutes. Results will be cached for 30 days.")
  }

  # Use parallel processing for speed
  log_info("Setting up {max_workers} parallel workers for dividend status check")
  oplan <- future::plan(future::multisession, workers = max_workers)
  on.exit(future::plan(oplan), add = TRUE)

  # Check dividend status for each stock
  log_info("Checking dividend status in parallel...")
  dividend_status <- furrr::future_map_lgl(
    sp500_stocks,
    stock_pays_dividend,
    .options = furrr::furrr_options(seed = TRUE),
    .progress = TRUE
  )

  # Filter for zero-dividend stocks (FALSE means no dividends)
  # Exclude NAs (errors) and TRUE (pays dividends)
  zero_div_stocks <- sp500_stocks[!is.na(dividend_status) & !dividend_status]

  log_success("Found {length(zero_div_stocks)} zero-dividend stocks")

  # Cache results (unless limit was specified)
  if (is.null(limit)) {
    save_to_cache(cache_file, zero_div_stocks)
  }

  return(zero_div_stocks)
}

#' Get dividend-paying S&P 500 stocks with caching
#'
#' Filters S&P 500 for stocks that pay dividends. Results are cached
#' for 30 days to avoid expensive repeated scans.
#'
#' @param limit Optional limit on number of stocks to check (for testing)
#' @param max_workers Number of parallel workers to use (default 4)
#' @return Character vector of dividend-paying ticker symbols
#' @export
#' @examples
#' \dontrun{
#'   div_stocks <- get_dividend_paying_sp500()
#'   length(div_stocks)  # Should be ~400
#'
#'   # Test with limit
#'   div_stocks <- get_dividend_paying_sp500(limit = 50)
#' }
get_dividend_paying_sp500 <- function(limit = NULL, max_workers = 10) {
  cache_file <- "dividend_paying_stocks.rds"

  # Check cache first (unless limit specified, which is for testing)
  if (is.null(limit) && is_cache_fresh(cache_file, max_age_days = 30)) {
    log_info("Using cached dividend-paying stocks list")
    return(load_from_cache(cache_file))
  }

  # Get S&P 500 universe
  sp500_stocks <- get_sp500_stocks()

  # Apply limit if specified (for testing)
  if (!is.null(limit)) {
    sp500_stocks <- head(sp500_stocks, limit)
    log_info("Limiting scan to first {limit} S&P 500 stocks (testing mode)")
  } else {
    log_info("Scanning {length(sp500_stocks)} S&P 500 stocks for dividend status...")
    log_info("This may take 5-10 minutes. Results will be cached for 30 days.")
  }

  # Use parallel processing for speed
  log_info("Setting up {max_workers} parallel workers for dividend status check")
  oplan <- future::plan(future::multisession, workers = max_workers)
  on.exit(future::plan(oplan), add = TRUE)

  # Check dividend status for each stock
  log_info("Checking dividend status in parallel...")
  dividend_status <- furrr::future_map_lgl(
    sp500_stocks,
    stock_pays_dividend,
    .options = furrr::furrr_options(seed = TRUE),
    .progress = TRUE
  )

  # Filter for dividend-paying stocks (TRUE means pays dividends)
  # Exclude NAs (errors) and FALSE (zero dividends)
  dividend_paying_stocks <- sp500_stocks[!is.na(dividend_status) & dividend_status]

  log_success("Found {length(dividend_paying_stocks)} dividend-paying stocks")

  # Cache results (unless limit was specified)
  if (is.null(limit)) {
    save_to_cache(cache_file, dividend_paying_stocks)
  }

  return(dividend_paying_stocks)
}

#' Get Russell 2000 constituents
#'
#' Fetches Russell 2000 stock tickers from iShares IWM ETF holdings CSV.
#' Results are cached for 7 days since Russell 2000 constituents change regularly.
#'
#' @return Character vector of ticker symbols
#' @export
#' @examples
#' \dontrun{
#'   # Get current Russell 2000 constituents
#'   r2000_stocks <- get_russell_2000_stocks()
#' }
get_russell_2000_stocks <- function() {
  cache_file <- "russell_2000_stocks.rds"

  # Check cache first (7 day expiry for index constituents)
  if (is_cache_fresh(cache_file, max_age_days = 7)) {
    log_info("Using cached Russell 2000 constituents")
    return(load_from_cache(cache_file))
  }

  tryCatch({
    log_info("Fetching Russell 2000 constituents from iShares IWM ETF...")

    # iShares IWM (Russell 2000 ETF) holdings CSV
    csv_url <- "https://www.ishares.com/us/products/239710/ishares-russell-2000-etf/1467271812596.ajax?fileType=csv&fileName=IWM_holdings&dataType=fund"

    # Read CSV, skipping first 9 header rows (line 10 has column names)
    russell_data <- read_csv(
      csv_url,
      skip = 9,
      col_types = cols(
        Ticker = col_character(),
        .default = col_character()
      ),
      show_col_types = FALSE
    )

    # Extract tickers and clean
    tickers <- russell_data$Ticker %>%
      as.character() %>%
      trimws() %>%
      toupper() %>%
      unique() %>%
      .[. != "" & !is.na(.) & . != "-"]  # Remove blanks, NAs, and "-" placeholders

    log_success("Fetched {length(tickers)} Russell 2000 tickers from iShares IWM")

    # Cache results
    save_to_cache(cache_file, tickers)

    return(tickers)

  }, error = function(e) {
    log_warn("Failed to fetch Russell 2000 from iShares: {e$message}")
    log_warn("Returning empty vector. Check internet connection.")
    return(character(0))
  })
}

#' Clear stock universe caches
#'
#' Utility function to force refresh of cached data. Useful when S&P 500
#' constituents change or for troubleshooting.
#'
#' @param cache_type Type of cache to clear: "sp500", "zero_dividend", "dividend_paying", or "all"
#' @return TRUE if successful
#' @export
#' @examples
#' \dontrun{
#'   # Clear specific cache
#'   clear_stock_cache("zero_dividend")
#'
#'   # Clear all caches
#'   clear_stock_cache("all")
#' }
clear_stock_cache <- function(cache_type = "all") {
  cache_dir <- get_cache_dir()

  files_to_remove <- switch(cache_type,
    "sp500" = "sp500_stocks.rds",
    "zero_dividend" = "zero_dividend_stocks.rds",
    "dividend_paying" = "dividend_paying_stocks.rds",
    "all" = c("sp500_stocks.rds", "zero_dividend_stocks.rds", "dividend_paying_stocks.rds"),
    stop("Invalid cache_type. Use 'sp500', 'zero_dividend', 'dividend_paying', or 'all'")
  )

  for (file in files_to_remove) {
    cache_path <- file.path(cache_dir, file)
    if (file.exists(cache_path)) {
      file.remove(cache_path)
      log_info("Cleared cache: {file}")
    }
  }

  invisible(TRUE)
}
