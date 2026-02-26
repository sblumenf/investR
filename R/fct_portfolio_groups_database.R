#' Portfolio Groups Database Operations
#'
#' Functions for managing position groups in SQLite - allows combining
#' multiple positions (stock + options) into logical trading positions
#' tied to investment strategies.
#'
#' @name portfolio-groups-database
#' @import dplyr
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbExecute dbWriteTable dbGetQuery
#' @importFrom logger log_info log_warn log_error log_debug
NULL

# Helper: check if a column exists in a SQLite table
.column_exists <- function(conn, table_name, column_name) {
  cols <- DBI::dbGetQuery(conn, paste0("PRAGMA table_info(", table_name, ")"))
  column_name %in% cols$name
}

################################################################################
# SCHEMA INITIALIZATION
################################################################################

#' Initialize portfolio groups database schema
#'
#' Creates the position_groups and position_group_members tables if they
#' don't exist. Should be called during database initialization.
#'
#' @param conn DBI connection object
#' @return Logical TRUE if successful
#' @noRd
initialize_groups_schema <- function(conn) {
  tryCatch({
    # Create position_groups table
    dbExecute(conn, "
      CREATE TABLE IF NOT EXISTS position_groups (
        group_id VARCHAR PRIMARY KEY,
        group_name VARCHAR NOT NULL,
        strategy_type VARCHAR NOT NULL,
        account_number VARCHAR NOT NULL,
        created_at TIMESTAMP NOT NULL,
        updated_at TIMESTAMP NOT NULL
      )
    ")

    # Create position_group_members table
    dbExecute(conn, "
      CREATE TABLE IF NOT EXISTS position_group_members (
        group_id VARCHAR NOT NULL,
        account_number VARCHAR NOT NULL,
        symbol VARCHAR NOT NULL,
        role VARCHAR NOT NULL,
        added_at TIMESTAMP NOT NULL,
        PRIMARY KEY (group_id, symbol)
      )
    ")

    # Create indexes for performance
    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_groups_account
      ON position_groups(account_number)
    ")

    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_groups_strategy
      ON position_groups(strategy_type)
    ")

    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_members_group
      ON position_group_members(group_id)
    ")

    # Migrate schema to add P&L and status columns if they don't exist
    migrate_groups_schema_for_pnl(conn)

    # Migrate schema to add allocated_quantity column if it doesn't exist
    migrate_members_schema_for_allocation(conn)

    log_debug("Portfolio Groups DB: Schema initialized successfully")
    return(TRUE)
  }, error = function(e) {
    log_error("Portfolio Groups DB: Schema initialization failed - {e$message}")
    return(FALSE)
  })
}

#' Migrate position_groups schema to add P&L and status columns
#'
#' Adds status, total_return_pct, total_return_amount, and annualized_return_pct
#' columns if they don't exist. Safe to call multiple times.
#'
#' @param conn DBI connection object
#' @return Logical TRUE if successful
#' @noRd
migrate_groups_schema_for_pnl <- function(conn) {
  tryCatch({
    # Check if status column exists
    if (!.column_exists(conn, "position_groups", "status")) {
      # Add status column (open, closed)
      dbExecute(conn, "
        ALTER TABLE position_groups
        ADD COLUMN status VARCHAR DEFAULT 'open'
      ")
      log_info("Portfolio Groups DB: Added status column to position_groups")
    }

    # Check if total_return_pct column exists
    if (!.column_exists(conn, "position_groups", "total_return_pct")) {
      # Add total_return_pct column
      dbExecute(conn, "
        ALTER TABLE position_groups
        ADD COLUMN total_return_pct DOUBLE
      ")
      log_info("Portfolio Groups DB: Added total_return_pct column to position_groups")
    }

    # Check if total_return_amount column exists
    if (!.column_exists(conn, "position_groups", "total_return_amount")) {
      # Add total_return_amount column
      dbExecute(conn, "
        ALTER TABLE position_groups
        ADD COLUMN total_return_amount DOUBLE
      ")
      log_info("Portfolio Groups DB: Added total_return_amount column to position_groups")
    }

    # Check if annualized_return_pct column exists
    if (!.column_exists(conn, "position_groups", "annualized_return_pct")) {
      # Add annualized_return_pct column
      dbExecute(conn, "
        ALTER TABLE position_groups
        ADD COLUMN annualized_return_pct DOUBLE
      ")
      log_info("Portfolio Groups DB: Added annualized_return_pct column to position_groups")
    }

    return(TRUE)
  }, error = function(e) {
    log_warn("Portfolio Groups DB: Schema migration warning - {e$message}")
    # Don't fail if migration has issues - schema might already be updated
    return(TRUE)
  })
}

#' Migrate position_group_members schema to add allocated_quantity column
#'
#' Adds allocated_quantity column for stock position splitting across multiple groups.
#' For stock members: stores allocated shares. For options: NULL (uses full position).
#' Safe to call multiple times.
#'
#' @param conn DBI connection object
#' @return Logical TRUE if successful
#' @noRd
migrate_members_schema_for_allocation <- function(conn) {
  tryCatch({
    # Check if allocated_quantity column exists
    if (!.column_exists(conn, "position_group_members", "allocated_quantity")) {
      # Add allocated_quantity column (NULL for options, quantity for stocks)
      dbExecute(conn, "
        ALTER TABLE position_group_members
        ADD COLUMN allocated_quantity DOUBLE
      ")
      log_info("Portfolio Groups DB: Added allocated_quantity column to position_group_members")
    }

    return(TRUE)
  }, error = function(e) {
    log_warn("Portfolio Groups DB: Members schema migration warning - {e$message}")
    # Don't fail if migration has issues - schema might already be updated
    return(TRUE)
  })
}

################################################################################
# GROUP CRUD OPERATIONS
################################################################################

#' Create a new position group
#'
#' Creates a group with metadata and associated member positions.
#' Transactional - all or nothing.
#' If auto_name = TRUE and group_name is NULL/empty, generates standard name
#' from members and strategy type.
#'
#' @param group_id Unique group identifier
#' @param group_name Human-readable group name (NULL = auto-generate if auto_name = TRUE)
#' @param strategy_type Strategy name (e.g., "Dividend Aristocrats")
#' @param account_number Questrade account number
#' @param members Tibble with columns: symbol, role
#' @param auto_name Logical, auto-generate name from members and strategy (default: TRUE)
#' @return Logical TRUE if successful, FALSE otherwise
#' @noRd
create_position_group <- function(group_id, group_name = NULL, strategy_type,
                                  account_number, members = NULL,
                                  auto_name = TRUE) {

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_groups_schema(conn)

    # Auto-generate group name if not provided
    if (auto_name && (is.null(group_name) || nchar(trimws(group_name)) == 0)) {
      if (!is.null(members) && nrow(members) > 0) {
        generated_name <- generate_standard_group_name(members, strategy_type)
        if (!is.null(generated_name)) {
          group_name <- generated_name
          log_info("Portfolio Groups DB: Auto-generated group name: {group_name}")
        }
      }
    }

    # Validate that group_name is not empty
    if (is.null(group_name) || nchar(trimws(group_name)) == 0) {
      log_error("Portfolio Groups DB: Group name is required")
      return(FALSE)
    }

    # Begin transaction
    dbExecute(conn, "BEGIN TRANSACTION")

    # Insert group metadata
    timestamp <- Sys.time()
    group_data <- tibble::tibble(
      group_id = group_id,
      group_name = group_name,
      strategy_type = strategy_type,
      account_number = account_number,
      status = "open",
      created_at = timestamp,
      updated_at = timestamp
    )

    dbWriteTable(conn, "position_groups", group_data, append = TRUE)

    # Insert members if provided
    if (!is.null(members) && nrow(members) > 0) {
      members_data <- members %>%
        mutate(
          group_id = group_id,
          account_number = account_number,
          added_at = timestamp
        )

      # Select columns based on whether allocated_quantity is present
      if ("allocated_quantity" %in% names(members_data)) {
        members_data <- members_data %>%
          select(group_id, account_number, symbol, role, allocated_quantity, added_at)
      } else {
        members_data <- members_data %>%
          select(group_id, account_number, symbol, role, added_at)
      }

      # POINT D: Log members RIGHT BEFORE database write
      log_info(sprintf("[POINT D] About to write %d members to database for group %s:", nrow(members_data), group_id))
      for (i in seq_len(nrow(members_data))) {
        log_info(sprintf("[POINT D]   %s → role: %s", members_data$symbol[i], members_data$role[i]))
      }
      log_info(sprintf("[POINT D] Members data structure: %s", paste(capture.output(str(members_data)), collapse = " | ")))

      dbWriteTable(conn, "position_group_members", members_data, append = TRUE)
      log_info("Portfolio Groups DB: Created group '{group_name}' with {nrow(members)} members")
    } else {
      log_info("Portfolio Groups DB: Created empty group '{group_name}'")
    }

    # Commit transaction
    dbExecute(conn, "COMMIT")

    return(TRUE)
  }, error = function(e) {
    # Rollback on error
    tryCatch(dbExecute(conn, "ROLLBACK"), error = function(e2) NULL)
    log_error("Portfolio Groups DB: Failed to create group '{group_name}' - {e$message}")
    return(FALSE)
  })
}

#' Update position group metadata
#'
#' Updates group name or strategy type. Does not modify members.
#'
#' @param group_id Group identifier
#' @param group_name New group name (NULL = no change)
#' @param strategy_type New strategy type (NULL = no change)
#' @return Logical TRUE if successful
#' @noRd
update_position_group <- function(group_id, group_name = NULL,
                                  strategy_type = NULL) {

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    # Build dynamic UPDATE statement
    updates <- c()
    params <- list()

    if (!is.null(group_name)) {
      updates <- c(updates, "group_name = ?")
      params <- c(params, list(group_name))
    }

    if (!is.null(strategy_type)) {
      updates <- c(updates, "strategy_type = ?")
      params <- c(params, list(strategy_type))
    }

    if (length(updates) == 0) {
      log_debug("Portfolio Groups DB: No updates provided for group {group_id}")
      return(TRUE)
    }

    # Always update timestamp
    updates <- c(updates, "updated_at = ?")
    params <- c(params, list(Sys.time()))

    # Add WHERE clause parameter
    params <- c(params, list(group_id))

    sql <- sprintf(
      "UPDATE position_groups SET %s WHERE group_id = ?",
      paste(updates, collapse = ", ")
    )

    rows_affected <- dbExecute(conn, sql, params = params)

    if (rows_affected > 0) {
      log_info("Portfolio Groups DB: Updated group {group_id}")
      return(TRUE)
    } else {
      log_warn("Portfolio Groups DB: Group {group_id} not found")
      return(FALSE)
    }
  }, error = function(e) {
    log_error("Portfolio Groups DB: Failed to update group {group_id} - {e$message}")
    return(FALSE)
  })
}

# NOTE: close_position_group() has been removed from this file.
# The comprehensive version with P&L calculation is in fct_group_pnl.R.
# Use that function instead - it handles status update + P&L calculation + cash flow cleanup.

#' Update option member when a roll occurs
#'
#' When a covered call option is rolled (buy-to-close old option, sell-to-open new option),
#' this function updates the position_group_members table to reflect the new option symbol.
#' This ensures portfolio risk analysis sees the current active option, not the expired one.
#'
#' @param group_id Group identifier
#' @param old_symbol Old option symbol being closed (e.g., "BAC24Oct25C51.00")
#' @param new_symbol New option symbol being opened (e.g., "BAC21Nov25C52.00")
#' @param conn Optional DBI connection (for transaction support)
#' @return Logical TRUE if successful, FALSE otherwise
#' @noRd
update_group_option_member <- function(group_id, old_symbol, new_symbol, conn = NULL) {
  # Connection management - use provided conn or create new one
  should_close <- FALSE
  if (is.null(conn)) {
    conn <- get_portfolio_db_connection()
    should_close <- TRUE
  }

  if (should_close) {
    on.exit(dbDisconnect(conn), add = TRUE)
  }

  tryCatch({
    # Begin transaction for safety
    dbExecute(conn, "BEGIN TRANSACTION")

    # Verify old option exists in members table
    old_member <- dbGetQuery(conn, "
      SELECT * FROM position_group_members
      WHERE group_id = ? AND symbol = ? AND role = 'short_call'
    ", params = list(group_id, old_symbol))

    if (nrow(old_member) == 0) {
      log_warn("Portfolio Groups DB: Cannot roll - old option {old_symbol} not found in group {group_id} members")
      dbExecute(conn, "ROLLBACK")
      return(FALSE)
    }

    # Update the member symbol (keeps same role, account_number, just changes symbol and timestamp)
    rows_affected <- dbExecute(conn, "
      UPDATE position_group_members
      SET symbol = ?, added_at = ?
      WHERE group_id = ? AND symbol = ? AND role = 'short_call'
    ", params = list(new_symbol, Sys.time(), group_id, old_symbol))

    if (rows_affected > 0) {
      dbExecute(conn, "COMMIT")
      log_info("Portfolio Groups DB: Rolled option in group {group_id}: {old_symbol} → {new_symbol}")
      return(TRUE)
    } else {
      dbExecute(conn, "ROLLBACK")
      log_warn("Portfolio Groups DB: Failed to update member for group {group_id}")
      return(FALSE)
    }

  }, error = function(e) {
    tryCatch(dbExecute(conn, "ROLLBACK"), error = function(e2) NULL)
    log_error("Portfolio Groups DB: Failed to roll option for group {group_id} - {e$message}")
    return(FALSE)
  })
}

#' Reopen a closed position group
#'
#' Changes a closed group back to open status.
#'
#' @param group_id Group identifier
#' @return Logical TRUE if successful
#' @noRd
reopen_position_group <- function(group_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    # Update status to open
    rows_affected <- dbExecute(conn,
      "UPDATE position_groups SET status = 'open', updated_at = ? WHERE group_id = ?",
      params = list(Sys.time(), group_id)
    )

    if (rows_affected > 0) {
      log_info("Portfolio Groups DB: Reopened group {group_id}")
      return(TRUE)
    } else {
      log_warn("Portfolio Groups DB: Group {group_id} not found")
      return(FALSE)
    }
  }, error = function(e) {
    log_error("Portfolio Groups DB: Failed to reopen group {group_id} - {e$message}")
    return(FALSE)
  })
}

#' DEPRECATED: Delete a position group - DO NOT USE
#'
#' This function is deprecated and should not be used. Groups should be closed
#' using close_position_group() to preserve historical data.
#' This function only exists for backwards compatibility with old tests.
#'
#' @param group_id Group identifier
#' @return Logical FALSE with error message
#' @noRd
delete_position_group <- function(group_id) {
  log_error("Portfolio Groups DB: delete_position_group() is DEPRECATED - use close_position_group() instead")
  stop("delete_position_group() is deprecated. Use close_position_group() to soft-delete groups and preserve historical data.")
}

#' Unlink position group (hard delete for setup mistakes)
#'
#' Completely removes a position group and unlinks all activities, restoring
#' them to their pre-linked state. Use this to undo mistakes during position
#' setup, NOT for closing completed positions (use close_position_group instead).
#'
#' This function:
#' - Deletes all cash flows (projected and actual)
#' - Deletes all group members
#' - Deletes the position group record
#' - Unlinks all activities (sets group_id = NULL)
#'
#' Activities remain in the database and can be re-linked to new groups.
#'
#' @param group_id Group identifier
#' @return Logical TRUE if successful, FALSE otherwise
#' @export
unlink_position_group <- function(group_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    log_info("Unlink Group: Starting unlink operation for group {group_id}")

    # Step 1: Delete all cash flows (projected and actual)
    flows_deleted <- dbExecute(conn, "
      DELETE FROM position_group_cash_flows
      WHERE group_id = ?
    ", params = list(group_id))
    log_info("Unlink Group: Deleted {flows_deleted} cash flow projection(s)")

    # Step 2: Delete group members
    members_deleted <- dbExecute(conn, "
      DELETE FROM position_group_members
      WHERE group_id = ?
    ", params = list(group_id))
    log_info("Unlink Group: Deleted {members_deleted} group member(s)")

    # Step 3: Delete position group
    group_deleted <- dbExecute(conn, "
      DELETE FROM position_groups
      WHERE group_id = ?
    ", params = list(group_id))
    log_info("Unlink Group: Deleted {group_deleted} position group(s)")

    # Step 4: Unlink activities
    activities_unlinked <- dbExecute(conn, "
      UPDATE account_activities
      SET group_id = NULL
      WHERE group_id = ?
    ", params = list(group_id))
    log_info("Unlink Group: Unlinked {activities_unlinked} activity/activities")

    # Verification
    remaining_groups <- dbGetQuery(conn, "
      SELECT COUNT(*) as count
      FROM position_groups
      WHERE group_id = ?
    ", params = list(group_id))

    if (remaining_groups$count == 0) {
      log_info("Unlink Group: Successfully unlinked group {group_id}")
      return(TRUE)
    } else {
      log_warn("Unlink Group: Group {group_id} may not have been fully deleted")
      return(FALSE)
    }

  }, error = function(e) {
    log_error("Unlink Group: Failed to unlink group {group_id} - {e$message}")
    return(FALSE)
  })
}

################################################################################
# GROUP RETRIEVAL OPERATIONS
################################################################################

#' Get all position groups
#'
#' Retrieves groups. By default, only returns open groups.
#' Set include_closed = TRUE to retrieve all groups including closed ones.
#'
#' @param include_closed Logical, if TRUE includes closed groups (default: FALSE)
#' @return Tibble with group data
#' @noRd
get_all_groups <- function(include_closed = FALSE) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_groups_schema(conn)

    # Build query with optional status filter
    if (include_closed) {
      sql <- "
        SELECT
          g.group_id,
          g.group_name,
          g.strategy_type,
          g.account_number,
          g.status,
          g.total_return_pct,
          g.total_return_amount,
          g.annualized_return_pct,
          g.created_at,
          g.updated_at
        FROM position_groups g
        ORDER BY g.updated_at DESC
      "
    } else {
      sql <- "
        SELECT
          g.group_id,
          g.group_name,
          g.strategy_type,
          g.account_number,
          g.status,
          g.total_return_pct,
          g.total_return_amount,
          g.annualized_return_pct,
          g.created_at,
          g.updated_at
        FROM position_groups g
        WHERE g.status = 'open'
        ORDER BY g.updated_at DESC
      "
    }

    result <- dbGetQuery(conn, sql)

    if (nrow(result) == 0) {
      log_debug("Portfolio Groups DB: No groups found")
      return(tibble::tibble())
    }

    log_debug("Portfolio Groups DB: Retrieved {nrow(result)} groups")
    return(tibble::as_tibble(result))
  }, error = function(e) {
    log_error("Portfolio Groups DB: Failed to get groups - {e$message}")
    return(tibble::tibble())
  })
}

#' Get single position group by ID
#'
#' @param group_id Group identifier
#' @return Tibble with single row or empty tibble
#' @noRd
get_group_by_id <- function(group_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    result <- dbGetQuery(conn, "
      SELECT * FROM position_groups WHERE group_id = ?
    ", params = list(group_id))

    if (nrow(result) == 0) {
      log_debug("Portfolio Groups DB: Group {group_id} not found")
      return(tibble::tibble())
    }

    return(tibble::as_tibble(result))
  }, error = function(e) {
    log_error("Portfolio Groups DB: Failed to get group {group_id} - {e$message}")
    return(tibble::tibble())
  })
}

#' Get members of a position group
#'
#' @param group_id Group identifier
#' @return Tibble with member data
#' @noRd
get_group_members <- function(group_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    result <- dbGetQuery(conn, "
      SELECT * FROM position_group_members WHERE group_id = ?
      ORDER BY added_at
    ", params = list(group_id))

    if (nrow(result) == 0) {
      log_debug("Portfolio Groups DB: No members for group {group_id}")
      return(tibble::tibble())
    }

    log_debug("Portfolio Groups DB: Retrieved {nrow(result)} members for group {group_id}")
    return(tibble::as_tibble(result))
  }, error = function(e) {
    log_error("Portfolio Groups DB: Failed to get members for {group_id} - {e$message}")
    return(tibble::tibble())
  })
}

#' Get members for multiple groups (batch operation)
#'
#' Retrieves all members for multiple position groups in a single query.
#' More efficient than calling get_group_members() multiple times.
#'
#' @param group_ids Vector of group identifiers
#' @return Tibble with member data including group_id column
#' @noRd
get_members_for_groups <- function(group_ids) {
  if (length(group_ids) == 0) {
    return(tibble::tibble())
  }

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    # Build IN clause for SQL
    placeholders <- paste(rep("?", length(group_ids)), collapse = ", ")
    sql <- sprintf("
      SELECT * FROM position_group_members
      WHERE group_id IN (%s)
      ORDER BY group_id, added_at
    ", placeholders)

    result <- dbGetQuery(conn, sql, params = as.list(group_ids))

    if (nrow(result) == 0) {
      log_debug("Portfolio Groups DB: No members found for {length(group_ids)} groups")
      return(tibble::tibble())
    }

    log_debug("Portfolio Groups DB: Retrieved {nrow(result)} members for {length(group_ids)} groups")
    return(tibble::as_tibble(result))

  }, error = function(e) {
    log_error("Portfolio Groups DB: Failed to get members for groups - {e$message}")
    return(tibble::tibble())
  })
}

################################################################################
# MEMBER OPERATIONS
################################################################################

#' Add a member to a position group
#'
#' @param group_id Group identifier
#' @param account_number Account number
#' @param symbol Position symbol
#' @param role Position role (e.g., "underlying_stock", "short_call")
#' @return Logical TRUE if successful
#' @noRd
add_group_member <- function(group_id, account_number, symbol, role) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    member_data <- tibble::tibble(
      group_id = group_id,
      account_number = account_number,
      symbol = symbol,
      role = role,
      added_at = Sys.time()
    )

    dbWriteTable(conn, "position_group_members", member_data, append = TRUE)

    # Update group timestamp
    dbExecute(conn, "UPDATE position_groups SET updated_at = ? WHERE group_id = ?",
              params = list(Sys.time(), group_id))

    log_info("Portfolio Groups DB: Added member {symbol} to group {group_id}")
    return(TRUE)
  }, error = function(e) {
    log_error("Portfolio Groups DB: Failed to add member {symbol} to {group_id} - {e$message}")
    return(FALSE)
  })
}

#' Remove a member from a position group
#'
#' @param group_id Group identifier
#' @param symbol Position symbol to remove
#' @return Logical TRUE if successful
#' @noRd
remove_group_member <- function(group_id, symbol) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    rows_affected <- dbExecute(conn,
      "DELETE FROM position_group_members WHERE group_id = ? AND symbol = ?",
      params = list(group_id, symbol)
    )

    # Update group timestamp
    dbExecute(conn, "UPDATE position_groups SET updated_at = ? WHERE group_id = ?",
              params = list(Sys.time(), group_id))

    if (rows_affected > 0) {
      log_info("Portfolio Groups DB: Removed member {symbol} from group {group_id}")
      return(TRUE)
    } else {
      log_warn("Portfolio Groups DB: Member {symbol} not found in group {group_id}")
      return(FALSE)
    }
  }, error = function(e) {
    log_error("Portfolio Groups DB: Failed to remove member {symbol} from {group_id} - {e$message}")
    return(FALSE)
  })
}

################################################################################
# CONVERSION TO LEGACY COVERED CALL
################################################################################

#' Check if a group can be converted to Legacy Covered Call
#'
#' Validates that a group meets criteria for conversion:
#' - Group exists
#' - Strategy type is "Dynamic Covered Calls"
#' - Status is "open"
#' - Has projected cash flows to remove
#'
#' @param conn DBI connection object
#' @param group_id Group identifier
#' @return List with valid (logical) and either reason (if invalid) or
#'   projected_amount (if valid)
#' @noRd
can_convert_to_legacy <- function(conn, group_id) {
  tryCatch({
    # Check if group exists
    group <- dbGetQuery(conn, "
      SELECT group_id, strategy_type, status
      FROM position_groups
      WHERE group_id = ?
    ", params = list(group_id))

    if (nrow(group) == 0) {
      return(list(valid = FALSE, reason = "Group not found"))
    }

    # Check status
    if (group$status != "open") {
      return(list(valid = FALSE, reason = "Group is not open"))
    }

    # Check for projected cash flows
    projected <- dbGetQuery(conn, "
      SELECT COUNT(*) as count, SUM(amount) as total_amount
      FROM position_group_cash_flows
      WHERE group_id = ? AND status = 'projected'
    ", params = list(group_id))

    if (projected$count == 0) {
      return(list(valid = FALSE, reason = "No projected cash flows to remove"))
    }

    # Valid for conversion
    return(list(
      valid = TRUE,
      projected_count = projected$count,
      projected_amount = projected$total_amount
    ))

  }, error = function(e) {
    log_error("Convert to Legacy: Validation error for group {group_id} - {e$message}")
    return(list(valid = FALSE, reason = paste("Validation error:", e$message)))
  })
}

#' Preview what will be deleted during legacy conversion
#'
#' Returns detailed breakdown of projected events that will be removed.
#' Used to populate confirmation modal.
#'
#' @param conn DBI connection object
#' @param group_id Group identifier
#' @return List with event details grouped by type, or NULL on error
#' @noRd
preview_legacy_conversion <- function(conn, group_id) {
  tryCatch({
    # Get all projected events grouped by type
    events <- dbGetQuery(conn, "
      SELECT
        event_type,
        COUNT(*) as event_count,
        SUM(amount) as total_amount
      FROM position_group_cash_flows
      WHERE group_id = ? AND status = 'projected'
      GROUP BY event_type
    ", params = list(group_id))

    if (nrow(events) == 0) {
      return(list(
        has_events = FALSE,
        message = "No projected events found"
      ))
    }

    # Transform to named list for easy access
    preview <- list(
      has_events = TRUE,
      total_count = sum(events$event_count),
      total_amount = sum(events$total_amount),
      by_type = list()
    )

    # Add each event type
    for (i in seq_len(nrow(events))) {
      event_type <- events$event_type[i]
      preview$by_type[[event_type]] <- list(
        count = events$event_count[i],
        amount = events$total_amount[i]
      )
    }

    log_debug("Convert to Legacy: Preview generated for group {group_id} - {preview$total_count} events, ${round(preview$total_amount, 2)}")

    return(preview)

  }, error = function(e) {
    log_error("Convert to Legacy: Preview error for group {group_id} - {e$message}")
    return(NULL)
  })
}

#' Convert Dynamic Covered Call to Legacy Covered Call
#'
#' Executes the conversion in a transactional manner:
#' 1. Validates conversion eligibility
#' 2. Deletes all projected cash flows
#' 3. Updates strategy_type to "Legacy Covered Call"
#' 4. Updates timestamp
#' 5. Logs conversion in projection_recalculations table
#'
#' All operations are atomic - either all succeed or all rollback.
#'
#' @param conn DBI connection object (optional, will create if not provided)
#' @param group_id Group identifier
#' @return TRUE on success, FALSE on failure
#' @noRd
convert_to_legacy_covered_call <- function(conn = NULL, group_id) {
  # Connection management - use provided conn or create new one
  should_close <- FALSE
  if (is.null(conn)) {
    conn <- get_portfolio_db_connection()
    should_close <- TRUE
  }

  if (should_close) {
    on.exit(dbDisconnect(conn), add = TRUE)
  }

  tryCatch({
    log_info("Convert to Legacy: Starting conversion for group {group_id}")

    # Begin transaction for atomic operation
    dbExecute(conn, "BEGIN TRANSACTION")

    # Step 1: Validate conversion eligibility
    validation <- can_convert_to_legacy(conn, group_id)

    if (!validation$valid) {
      dbExecute(conn, "ROLLBACK")
      log_warn("Convert to Legacy: Validation failed for group {group_id} - {validation$reason}")
      return(FALSE)
    }

    # Store count for logging
    old_projection_count <- validation$projected_count

    # Step 2: Delete all projected cash flows
    deleted_count <- dbExecute(conn, "
      DELETE FROM position_group_cash_flows
      WHERE group_id = ? AND status = 'projected'
    ", params = list(group_id))

    log_info("Convert to Legacy: Deleted {deleted_count} projected cash flow(s) for group {group_id}")

    # Step 3: Update strategy_type to "Legacy Covered Call"
    update_count <- dbExecute(conn, "
      UPDATE position_groups
      SET strategy_type = 'Legacy Covered Call',
          updated_at = ?
      WHERE group_id = ?
    ", params = list(Sys.time(), group_id))

    if (update_count == 0) {
      dbExecute(conn, "ROLLBACK")
      log_error("Convert to Legacy: Failed to update strategy_type for group {group_id}")
      return(FALSE)
    }

    log_info("Convert to Legacy: Updated strategy_type to 'Legacy Covered Call' for group {group_id}")

    # Step 4: Log conversion in projection_recalculations table
    # Ensure schema exists
    initialize_income_projection_schema(conn)

    recalc_id <- paste0("RECALC_", group_id, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
    recalc_data <- tibble::tibble(
      recalc_id = recalc_id,
      group_id = group_id,
      recalc_date = Sys.time(),
      reason = "converted_to_legacy",
      old_projection_count = as.integer(old_projection_count),
      new_projection_count = 0L
    )

    dbWriteTable(conn, "projection_recalculations", recalc_data, append = TRUE)

    log_info("Convert to Legacy: Logged conversion in projection_recalculations table")

    # Commit transaction - all operations succeeded
    dbExecute(conn, "COMMIT")

    log_info("Convert to Legacy: Successfully converted group {group_id} to Legacy Covered Call")
    return(TRUE)

  }, error = function(e) {
    # Rollback on any error
    tryCatch(dbExecute(conn, "ROLLBACK"), error = function(e2) NULL)
    log_error("Convert to Legacy: Conversion failed for group {group_id} - {e$message}")
    return(FALSE)
  })
}

################################################################################
# INTEGRITY CHECKING
################################################################################

#' Check integrity of all position groups
#'
#' Verifies that all group members still exist in the latest positions snapshot.
#' Returns groups with status information.
#'
#' @param current_positions Tibble with latest positions (from positions_history)
#' @return Tibble with groups and status column ("active", "incomplete", "broken")
#' @noRd
check_all_groups_integrity <- function(current_positions) {
  groups <- get_all_groups()

  if (nrow(groups) == 0) {
    return(tibble::tibble())
  }

  # Add status column to each group
  groups_with_status <- groups %>%
    mutate(
      status = purrr::map_chr(group_id, function(gid) {
        check_group_integrity(gid, current_positions)
      })
    )

  log_info("Portfolio Groups DB: Checked {nrow(groups)} groups - {sum(groups_with_status$status == 'active')} active, {sum(groups_with_status$status == 'incomplete')} incomplete, {sum(groups_with_status$status == 'broken')} broken")

  return(groups_with_status)
}

#' Check integrity of a single position group
#'
#' @param group_id Group identifier
#' @param current_positions Tibble with latest positions
#' @return Character status: "active", "incomplete", or "broken"
#' @noRd
check_group_integrity <- function(group_id, current_positions) {
  members <- get_group_members(group_id)

  if (nrow(members) == 0) {
    return("broken")
  }

  # Check which members exist in current positions
  # Use anti_join to find members NOT in current positions (tidyverse best practice)
  missing_members <- members %>%
    anti_join(
      current_positions %>% select(account_number, symbol),
      by = c("account_number", "symbol")
    )

  missing_count <- nrow(missing_members)

  if (missing_count == 0) {
    return("active")
  } else if (missing_count < nrow(members)) {
    return("incomplete")
  } else {
    return("broken")
  }
}

#' Get broken groups summary
#'
#' Returns count and details of groups needing attention.
#'
#' @param current_positions Tibble with latest positions
#' @return List with broken_count, incomplete_count, and details tibble
#' @noRd
get_broken_groups_summary <- function(current_positions) {
  groups_status <- check_all_groups_integrity(current_positions)

  if (nrow(groups_status) == 0) {
    return(list(
      broken_count = 0,
      incomplete_count = 0,
      details = tibble::tibble()
    ))
  }

  broken <- groups_status %>% filter(status == "broken")
  incomplete <- groups_status %>% filter(status == "incomplete")

  list(
    broken_count = nrow(broken),
    incomplete_count = nrow(incomplete),
    details = groups_status %>% filter(status %in% c("broken", "incomplete"))
  )
}
