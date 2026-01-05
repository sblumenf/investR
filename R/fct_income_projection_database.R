#' Income Projection Database Operations
#'
#' Functions for managing projected and actual cash flow events for position
#' groups. Tracks dividends and option gains for position groups.
#'
#' @name income-projection-database
#' @import dplyr
#' @importFrom duckdb duckdb dbConnect dbDisconnect
#' @importFrom DBI dbExecute dbWriteTable dbGetQuery
#' @importFrom logger log_info log_warn log_error log_debug
#' @importFrom lubridate year month
NULL

################################################################################
# SCHEMA INITIALIZATION
################################################################################

#' Initialize income projection database schema
#'
#' Creates the position_group_cash_flows and projection_recalculations tables
#' if they don't exist. Should be called during database initialization.
#'
#' @param conn DBI connection object
#' @return Logical TRUE if successful
#' @noRd
initialize_income_projection_schema <- function(conn) {
  tryCatch({
    # Create cash flows table
    dbExecute(conn, "
      CREATE TABLE IF NOT EXISTS position_group_cash_flows (
        event_id VARCHAR PRIMARY KEY,
        group_id VARCHAR NOT NULL,
        event_date DATE NOT NULL,
        event_type VARCHAR NOT NULL,
        amount DOUBLE NOT NULL,
        status VARCHAR NOT NULL,
        confidence VARCHAR NOT NULL,
        created_at TIMESTAMP NOT NULL,
        updated_at TIMESTAMP NOT NULL
      )
    ")

    # Create projection recalculations log table
    dbExecute(conn, "
      CREATE TABLE IF NOT EXISTS projection_recalculations (
        recalc_id VARCHAR PRIMARY KEY,
        group_id VARCHAR NOT NULL,
        recalc_date TIMESTAMP NOT NULL,
        reason VARCHAR NOT NULL,
        old_projection_count INTEGER,
        new_projection_count INTEGER
      )
    ")

    # Create indexes for performance
    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_cash_flows_group
      ON position_group_cash_flows(group_id)
    ")

    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_cash_flows_date
      ON position_group_cash_flows(event_date)
    ")

    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_cash_flows_status
      ON position_group_cash_flows(status)
    ")

    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_recalc_group
      ON projection_recalculations(group_id)
    ")

    log_debug("Income Projection DB: Schema initialized successfully")
    return(TRUE)
  }, error = function(e) {
    log_error("Income Projection DB: Schema initialization failed - {e$message}")
    return(FALSE)
  })
}

################################################################################
# CASH FLOW EVENT CRUD OPERATIONS
################################################################################

#' Save a cash flow event
#'
#' Creates a new cash flow event (dividend or option gain) for a position group.
#'
#' @param group_id Group identifier
#' @param event_date Date of the event (Date object)
#' @param event_type "dividend" or "option_gain"
#' @param amount Dollar amount of the event
#' @param status "projected" or "actual"
#' @param confidence "high", "medium", or "low"
#' @param conn Optional DBI connection (for transaction support)
#' @return Character event_id if successful, NULL if failed
#' @noRd
save_cash_flow_event <- function(group_id, event_date, event_type, amount,
                                 status = "projected", confidence = "high", conn = NULL) {

  # Connection management - use provided conn or create new one
  should_close <- FALSE
  if (is.null(conn)) {
    conn <- get_portfolio_db_connection()
    should_close <- TRUE
  }

  if (should_close) {
    on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  }

  tryCatch({
    # Ensure schema exists
    initialize_income_projection_schema(conn)

    # Generate unique event ID
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    random_suffix <- sample(1000:9999, 1)
    event_id <- sprintf("EVENT_%s_%s_%s_%s", group_id,
                       format(event_date, "%Y%m%d"),
                       timestamp, random_suffix)

    timestamp <- Sys.time()
    event_data <- tibble::tibble(
      event_id = event_id,
      group_id = group_id,
      event_date = as.Date(event_date),
      event_type = event_type,
      amount = amount,
      status = status,
      confidence = confidence,
      created_at = timestamp,
      updated_at = timestamp
    )

    dbWriteTable(conn, "position_group_cash_flows", event_data, append = TRUE)

    log_debug("Income Projection DB: Saved {event_type} event for group {group_id} on {event_date}")
    return(event_id)
  }, error = function(e) {
    log_error("Income Projection DB: Failed to save cash flow event - {e$message}")
    return(NULL)
  })
}

#' Delete all cash flow events for a group
#'
#' Removes all events associated with a group (used when group is closed).
#'
#' @param group_id Group identifier
#' @param status_filter Optional status filter ("projected" to delete only projections)
#' @return Logical TRUE if successful
#' @noRd
delete_group_cash_flows <- function(group_id, status_filter = NULL) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    if (!is.null(status_filter)) {
      sql <- "DELETE FROM position_group_cash_flows
              WHERE group_id = ? AND status = ?"
      params <- list(group_id, status_filter)
    } else {
      sql <- "DELETE FROM position_group_cash_flows WHERE group_id = ?"
      params <- list(group_id)
    }

    rows_affected <- dbExecute(conn, sql, params = params)

    log_info("Income Projection DB: Deleted {rows_affected} cash flow events for group {group_id}")
    return(TRUE)
  }, error = function(e) {
    log_error("Income Projection DB: Failed to delete cash flows for {group_id} - {e$message}")
    return(FALSE)
  })
}

#' Delete projected cash flow events for a specific month
#'
#' Removes projected events matching a specific group, event type, and calendar month.
#' Used for dividend reconciliation - when an actual dividend arrives, this deletes
#' the projected dividend(s) for that month.
#'
#' @param group_id Group identifier
#' @param event_type Event type ("dividend" or "option_gain")
#' @param event_date Date object - year and month will be extracted
#' @param conn Optional DBI connection (for transaction support)
#' @return Integer count of rows deleted
#' @noRd
delete_projected_cash_flows_by_month <- function(group_id, event_type, event_date, conn = NULL) {

  # Connection management - use provided conn or create new one
  should_close <- FALSE
  if (is.null(conn)) {
    conn <- get_portfolio_db_connection()
    should_close <- TRUE
  }

  if (should_close) {
    on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  }

  tryCatch({
    # Extract year and month from event date
    event_year <- lubridate::year(event_date)
    event_month <- lubridate::month(event_date)

    # Delete all projected events in the same month
    sql <- "
      DELETE FROM position_group_cash_flows
      WHERE group_id = ?
        AND event_type = ?
        AND status = 'projected'
        AND EXTRACT(YEAR FROM event_date) = ?
        AND EXTRACT(MONTH FROM event_date) = ?
    "

    rows_affected <- dbExecute(conn, sql, params = list(
      group_id,
      event_type,
      event_year,
      event_month
    ))

    if (rows_affected > 0) {
      log_debug("Income Projection DB: Reconciled {rows_affected} projected {event_type} event(s) for group {group_id} ({event_year}-{sprintf('%02d', event_month)})")
    }

    return(rows_affected)

  }, error = function(e) {
    log_error("Income Projection DB: Failed to delete projected events by month - {e$message}")
    return(0L)
  })
}

#' Delete all projected option gain events for a group
#'
#' Removes all projected option_gain events for a group. Used when recalculating
#' projections after an option roll - all old projected gains are deleted and
#' new ones are calculated based on the accumulated premiums and new strike/expiry.
#'
#' @param group_id Group identifier
#' @param conn Optional DBI connection (for transaction support)
#' @return Integer count of rows deleted
#' @noRd
delete_projected_option_gains <- function(group_id, conn = NULL) {

  # Connection management - use provided conn or create new one
  should_close <- FALSE
  if (is.null(conn)) {
    conn <- get_portfolio_db_connection()
    should_close <- TRUE
  }

  if (should_close) {
    on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  }

  tryCatch({
    # Delete all projected option_gain events for this group
    sql <- "
      DELETE FROM position_group_cash_flows
      WHERE group_id = ?
        AND event_type = 'option_gain'
        AND status = 'projected'
    "

    rows_affected <- dbExecute(conn, sql, params = list(group_id))

    if (rows_affected > 0) {
      log_debug("Income Projection DB: Deleted {rows_affected} projected option_gain event(s) for group {group_id}")
    }

    return(rows_affected)

  }, error = function(e) {
    log_error("Income Projection DB: Failed to delete projected option gains - {e$message}")
    return(0L)
  })
}

################################################################################
# CASH FLOW RETRIEVAL OPERATIONS
################################################################################

#' Get all cash flow events for a group
#'
#' Retrieves all projected and actual events, ordered by date.
#'
#' @param group_id Group identifier
#' @return Tibble with event data
#' @noRd
get_group_cash_flows <- function(group_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_income_projection_schema(conn)

    result <- dbGetQuery(conn, "
      SELECT
        event_id,
        group_id,
        event_date,
        event_type,
        amount,
        status,
        confidence,
        created_at,
        updated_at
      FROM position_group_cash_flows
      WHERE group_id = ?
      ORDER BY event_date ASC
    ", params = list(group_id))

    if (nrow(result) == 0) {
      log_debug("Income Projection DB: No cash flows found for group {group_id}")
      # Return empty tibble with correct schema (tidyverse best practice)
      return(tibble::tibble(
        event_id = character(),
        group_id = character(),
        event_date = as.Date(character()),
        event_type = character(),
        amount = numeric(),
        status = character(),
        confidence = character(),
        created_at = as.POSIXct(character()),
        updated_at = as.POSIXct(character())
      ))
    }

    log_debug("Income Projection DB: Retrieved {nrow(result)} cash flows for group {group_id}")
    return(tibble::as_tibble(result))
  }, error = function(e) {
    log_error("Income Projection DB: Failed to get cash flows for {group_id} - {e$message}")
    # Return empty tibble with correct schema even on error
    return(tibble::tibble(
      event_id = character(),
      group_id = character(),
      event_date = as.Date(character()),
      event_type = character(),
      amount = numeric(),
      status = character(),
      confidence = character(),
      created_at = as.POSIXct(character()),
      updated_at = as.POSIXct(character())
    ))
  })
}

#' Get cash flows for multiple groups (batch operation)
#'
#' Retrieves all cash flow events for multiple position groups in a single query.
#' More efficient than calling get_group_cash_flows() multiple times.
#'
#' @param group_ids Vector of group identifiers
#' @return Tibble with cash flow data including group_id column
#' @noRd
get_cash_flows_for_groups <- function(group_ids) {
  if (length(group_ids) == 0) {
    return(tibble::tibble(
      event_id = character(),
      group_id = character(),
      event_date = as.Date(character()),
      event_type = character(),
      amount = numeric(),
      status = character(),
      confidence = character(),
      created_at = as.POSIXct(character()),
      updated_at = as.POSIXct(character())
    ))
  }

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Build IN clause for SQL
    placeholders <- paste(rep("?", length(group_ids)), collapse = ", ")
    sql <- sprintf("
      SELECT
        event_id,
        group_id,
        event_date,
        event_type,
        amount,
        status,
        confidence,
        created_at,
        updated_at
      FROM position_group_cash_flows
      WHERE group_id IN (%s)
      ORDER BY group_id, event_date ASC
    ", placeholders)

    result <- dbGetQuery(conn, sql, params = as.list(group_ids))

    if (nrow(result) == 0) {
      log_debug("Income Projection DB: No cash flows found for {length(group_ids)} groups")
      return(tibble::tibble(
        event_id = character(),
        group_id = character(),
        event_date = as.Date(character()),
        event_type = character(),
        amount = numeric(),
        status = character(),
        confidence = character(),
        created_at = as.POSIXct(character()),
        updated_at = as.POSIXct(character())
      ))
    }

    log_debug("Income Projection DB: Retrieved {nrow(result)} cash flows for {length(group_ids)} groups")
    return(tibble::as_tibble(result))

  }, error = function(e) {
    log_error("Income Projection DB: Failed to get cash flows for groups - {e$message}")
    return(tibble::tibble(
      event_id = character(),
      group_id = character(),
      event_date = as.Date(character()),
      event_type = character(),
      amount = numeric(),
      status = character(),
      confidence = character(),
      created_at = as.POSIXct(character()),
      updated_at = as.POSIXct(character())
    ))
  })
}

#' Get projected income summary for a group
#'
#' Calculates total projected income by type.
#'
#' @param group_id Group identifier
#' @return Tibble with event_type and total_amount columns
#' @noRd
get_projected_income_summary <- function(group_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    result <- dbGetQuery(conn, "
      SELECT
        event_type,
        SUM(amount) AS total_amount,
        COUNT(*) AS event_count
      FROM position_group_cash_flows
      WHERE group_id = ? AND status = 'projected'
      GROUP BY event_type
    ", params = list(group_id))

    if (nrow(result) == 0) {
      return(tibble::tibble(
        event_type = character(0),
        total_amount = numeric(0),
        event_count = integer(0)
      ))
    }

    return(tibble::as_tibble(result))
  }, error = function(e) {
    log_error("Income Projection DB: Failed to get income summary for {group_id} - {e$message}")
    return(tibble::tibble())
  })
}

################################################################################
# RECALCULATION LOG OPERATIONS
################################################################################

#' Log a projection recalculation
#'
#' Records when and why projections were recalculated.
#'
#' @param group_id Group identifier
#' @param reason Reason for recalculation ("initial_creation", "variance_detected", etc.)
#' @param old_count Number of projections before recalc
#' @param new_count Number of projections after recalc
#' @return Character recalc_id if successful, NULL if failed
#' @noRd
log_projection_recalculation <- function(group_id, reason, old_count = 0, new_count = 0) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_income_projection_schema(conn)

    recalc_id <- paste0("RECALC_", group_id, "_", format(Sys.time(), "%Y%m%d%H%M%S"))

    recalc_data <- tibble::tibble(
      recalc_id = recalc_id,
      group_id = group_id,
      recalc_date = Sys.time(),
      reason = reason,
      old_projection_count = as.integer(old_count),
      new_projection_count = as.integer(new_count)
    )

    dbWriteTable(conn, "projection_recalculations", recalc_data, append = TRUE)

    log_info("Income Projection DB: Logged recalculation for group {group_id} - {reason}")
    return(recalc_id)
  }, error = function(e) {
    log_error("Income Projection DB: Failed to log recalculation - {e$message}")
    return(NULL)
  })
}
