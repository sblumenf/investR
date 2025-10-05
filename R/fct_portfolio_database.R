#' Portfolio Database Operations
#'
#' Functions for managing portfolio position data in DuckDB
#'
#' @name portfolio-database
#' @import dplyr
#' @importFrom duckdb duckdb dbConnect dbDisconnect
#' @importFrom DBI dbExecute dbWriteTable dbGetQuery
#' @importFrom logger log_info log_warn log_error log_debug
NULL

################################################################################
# DATABASE CONNECTION MANAGEMENT
################################################################################

#' Get or create DuckDB connection
#'
#' Returns a connection to the portfolio DuckDB database. Creates the database
#' file if it doesn't exist. Connection should be closed when done.
#'
#' @return DBI connection object
#' @noRd
get_portfolio_db_connection <- function() {
  db_path <- get_portfolio_db_path()

  tryCatch({
    conn <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
    log_debug("Portfolio DB: Connected to {db_path}")
    return(conn)
  }, error = function(e) {
    log_error("Portfolio DB: Failed to connect - {e$message}")
    stop("Could not connect to portfolio database: ", e$message)
  })
}

################################################################################
# SCHEMA INITIALIZATION
################################################################################

#' Initialize portfolio database schema
#'
#' Creates the positions_history table if it doesn't exist
#'
#' @param conn DBI connection object
#' @return Logical TRUE if successful
#' @noRd
initialize_portfolio_database <- function(conn) {
  tryCatch({
    # Create positions_history table
    dbExecute(conn, "
      CREATE TABLE IF NOT EXISTS positions_history (
        snapshot_timestamp TIMESTAMP NOT NULL,
        account_number VARCHAR NOT NULL,
        account_type VARCHAR NOT NULL,
        symbol VARCHAR NOT NULL,
        symbol_id INTEGER NOT NULL,
        open_quantity DOUBLE,
        closed_quantity DOUBLE,
        current_market_value DOUBLE,
        current_price DOUBLE,
        average_entry_price DOUBLE,
        day_pnl DOUBLE,
        closed_pnl DOUBLE,
        open_pnl DOUBLE,
        total_cost DOUBLE,
        is_real_time BOOLEAN,
        is_under_reorg BOOLEAN
      )
    ")

    # Create indexes for performance
    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_snapshot_timestamp
      ON positions_history(snapshot_timestamp)
    ")

    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_account_symbol
      ON positions_history(account_number, symbol)
    ")

    log_info("Portfolio DB: Schema initialized successfully")
    return(TRUE)
  }, error = function(e) {
    log_error("Portfolio DB: Schema initialization failed - {e$message}")
    return(FALSE)
  })
}

################################################################################
# DATA OPERATIONS
################################################################################

#' Save positions snapshot to database
#'
#' Appends a new timestamped snapshot of positions to the database
#'
#' @param positions_df Tibble with position data
#' @param snapshot_timestamp POSIXct timestamp for this snapshot
#' @return Logical TRUE if successful
#' @noRd
save_positions_snapshot <- function(positions_df, snapshot_timestamp) {
  if (is.null(positions_df) || nrow(positions_df) == 0) {
    log_warn("Portfolio DB: No positions to save")
    return(FALSE)
  }

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_portfolio_database(conn)

    # Add snapshot timestamp to all rows
    positions_df <- positions_df %>%
      mutate(snapshot_timestamp = snapshot_timestamp)

    # Append to table
    dbWriteTable(conn, "positions_history", positions_df, append = TRUE)

    log_info("Portfolio DB: Saved {nrow(positions_df)} positions at {snapshot_timestamp}")
    return(TRUE)
  }, error = function(e) {
    log_error("Portfolio DB: Failed to save positions - {e$message}")
    return(FALSE)
  })
}

#' Get latest snapshot timestamp
#'
#' Returns the most recent snapshot timestamp in the database
#'
#' @return POSIXct timestamp or NULL if no data
#' @noRd
get_latest_snapshot_timestamp <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_portfolio_database(conn)

    result <- dbGetQuery(conn, "
      SELECT MAX(snapshot_timestamp) as latest_timestamp
      FROM positions_history
    ")

    if (is.null(result$latest_timestamp) || is.na(result$latest_timestamp)) {
      log_debug("Portfolio DB: No snapshots found")
      return(NULL)
    }

    timestamp <- as.POSIXct(result$latest_timestamp)
    log_debug("Portfolio DB: Latest snapshot at {timestamp}")
    return(timestamp)
  }, error = function(e) {
    log_error("Portfolio DB: Failed to get latest timestamp - {e$message}")
    return(NULL)
  })
}

#' Get positions by timestamp
#'
#' Retrieves positions for a specific snapshot timestamp, optionally filtered
#' by account
#'
#' @param timestamp POSIXct timestamp to retrieve
#' @param account_filter Character account number to filter by, or NULL for all
#' @return Tibble with position data
#' @noRd
get_positions_by_timestamp <- function(timestamp, account_filter = NULL) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_portfolio_database(conn)

    # Build query
    query <- "
      SELECT *
      FROM positions_history
      WHERE snapshot_timestamp = ?
    "

    if (!is.null(account_filter)) {
      query <- paste(query, "AND account_number = ?")
      result <- dbGetQuery(conn, query, params = list(timestamp, account_filter))
    } else {
      result <- dbGetQuery(conn, query, params = list(timestamp))
    }

    if (nrow(result) == 0) {
      log_debug("Portfolio DB: No positions found for timestamp {timestamp}")
      return(tibble::tibble())
    }

    log_debug("Portfolio DB: Retrieved {nrow(result)} positions for {timestamp}")
    return(tibble::as_tibble(result))
  }, error = function(e) {
    log_error("Portfolio DB: Failed to get positions - {e$message}")
    return(tibble::tibble())
  })
}

#' Get latest positions
#'
#' Retrieves the most recent snapshot of positions, optionally filtered by account
#'
#' @param account_filter Character account number to filter by, or NULL for all
#' @return Tibble with position data
#' @noRd
get_latest_positions <- function(account_filter = NULL) {
  timestamp <- get_latest_snapshot_timestamp()

  if (is.null(timestamp)) {
    log_debug("Portfolio DB: No positions available")
    return(tibble::tibble())
  }

  get_positions_by_timestamp(timestamp, account_filter)
}
