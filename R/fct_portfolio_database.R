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
#' @param read_only Logical, if TRUE opens connection in read-only mode (default FALSE)
#' @return DBI connection object
#' @noRd
get_portfolio_db_connection <- function(read_only = FALSE) {
  db_path <- get_portfolio_db_path()

  tryCatch({
    conn <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
    log_debug("Portfolio DB: Connected to {db_path} (read_only={read_only})")
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

    # Initialize position groups schema
    initialize_groups_schema(conn)

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

#' Compare position sets to detect changes
#'
#' Compares new positions against old positions to detect actual position changes
#' (trades executed). Ignores market-driven changes like prices and P/L.
#'
#' @param new_positions Tibble with new position data
#' @param old_positions Tibble with previous position data
#' @return Logical TRUE if positions have changed (trades executed)
#' @noRd
positions_have_changed <- function(new_positions, old_positions) {
  # If one is empty and the other isn't, positions changed
  if (nrow(new_positions) == 0 && nrow(old_positions) == 0) {
    return(FALSE)
  }
  if (nrow(new_positions) == 0 || nrow(old_positions) == 0) {
    return(TRUE)
  }

  # Select only position-defining fields (ignore prices/values/P&L)
  position_key_fields <- c(
    "account_number",
    "symbol",
    "symbol_id",
    "open_quantity",
    "closed_quantity"
  )

  new_keys <- new_positions %>%
    select(all_of(position_key_fields))

  old_keys <- old_positions %>%
    select(all_of(position_key_fields))

  # Check for removed positions (in old but not in new)
  removed <- anti_join(old_keys, new_keys,
                       by = c("account_number", "symbol", "symbol_id"))

  # Check for added positions (in new but not in old)
  added <- anti_join(new_keys, old_keys,
                     by = c("account_number", "symbol", "symbol_id"))

  # Check for quantity changes (same account/symbol but different quantities)
  # First get positions that exist in both
  common <- inner_join(
    new_keys,
    old_keys,
    by = c("account_number", "symbol", "symbol_id"),
    suffix = c("_new", "_old")
  )

  # Then check if any quantities changed
  quantity_changed <- common %>%
    filter(
      open_quantity_new != open_quantity_old |
      closed_quantity_new != closed_quantity_old
    )

  # Log what changed
  if (nrow(removed) > 0 || nrow(added) > 0 || nrow(quantity_changed) > 0) {
    log_debug(
      "Portfolio DB: Position changes detected - ",
      "Added: {nrow(added)}, Removed: {nrow(removed)}, ",
      "Quantity changed: {nrow(quantity_changed)}"
    )
    return(TRUE)
  }

  log_debug("Portfolio DB: No position changes detected")
  return(FALSE)
}

#' Save positions snapshot to database
#'
#' Appends a new timestamped snapshot of positions to the database, but only if
#' positions have actually changed since the last snapshot. Market-driven changes
#' (prices, P/L) are ignored - only position changes (trades) trigger a save.
#'
#' @param positions_df Tibble with position data
#' @param snapshot_timestamp POSIXct timestamp for this snapshot
#' @return Character status: "saved", "unchanged", or "error"
#' @noRd
save_positions_snapshot <- function(positions_df, snapshot_timestamp) {
  if (is.null(positions_df) || nrow(positions_df) == 0) {
    log_warn("Portfolio DB: No positions to save")
    return("error")
  }

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_portfolio_database(conn)

    # Get latest snapshot for comparison
    latest_timestamp_result <- dbGetQuery(conn, "
      SELECT MAX(snapshot_timestamp) as latest_timestamp
      FROM positions_history
    ")

    # If there's existing data, check for changes
    if (!is.null(latest_timestamp_result$latest_timestamp) &&
        !is.na(latest_timestamp_result$latest_timestamp)) {

      latest_timestamp <- as.POSIXct(latest_timestamp_result$latest_timestamp)

      # Retrieve latest positions
      latest_positions <- dbGetQuery(conn, "
        SELECT *
        FROM positions_history
        WHERE snapshot_timestamp = ?
      ", params = list(latest_timestamp)) %>%
        tibble::as_tibble()

      # Compare positions
      if (!positions_have_changed(positions_df, latest_positions)) {
        log_info("Portfolio DB: Skipped snapshot - no position changes detected")
        return("unchanged")
      }
    }

    # Positions have changed (or this is first snapshot), save to database
    positions_df <- positions_df %>%
      mutate(snapshot_timestamp = snapshot_timestamp)

    # Append to table
    dbWriteTable(conn, "positions_history", positions_df, append = TRUE)

    log_info("Portfolio DB: Saved {nrow(positions_df)} positions at {snapshot_timestamp}")
    return("saved")
  }, error = function(e) {
    log_error("Portfolio DB: Failed to save positions - {e$message}")
    return("error")
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

#' Get latest positions
#'
#' Retrieves the most recent snapshot of positions from the database
#'
#' @return Tibble with position data
#' @noRd
get_latest_positions <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_portfolio_database(conn)

    # Get latest timestamp and positions in single query
    result <- dbGetQuery(conn, "
      SELECT *
      FROM positions_history
      WHERE snapshot_timestamp = (
        SELECT MAX(snapshot_timestamp)
        FROM positions_history
      )
    ")

    if (nrow(result) == 0) {
      log_debug("Portfolio DB: No positions available")
      return(tibble::tibble())
    }

    log_debug("Portfolio DB: Retrieved {nrow(result)} positions (latest snapshot)")
    return(tibble::as_tibble(result))
  }, error = function(e) {
    log_error("Portfolio DB: Failed to get latest positions - {e$message}")
    return(tibble::tibble())
  })
}
