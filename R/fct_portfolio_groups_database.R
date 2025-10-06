#' Portfolio Groups Database Operations
#'
#' Functions for managing position groups in DuckDB - allows combining
#' multiple positions (stock + options) into logical trading positions
#' tied to investment strategies.
#'
#' @name portfolio-groups-database
#' @import dplyr
#' @importFrom duckdb duckdb dbConnect dbDisconnect
#' @importFrom DBI dbExecute dbWriteTable dbGetQuery
#' @importFrom logger log_info log_warn log_error log_debug
NULL

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

    log_info("Portfolio Groups DB: Schema initialized successfully")
    return(TRUE)
  }, error = function(e) {
    log_error("Portfolio Groups DB: Schema initialization failed - {e$message}")
    return(FALSE)
  })
}

################################################################################
# GROUP CRUD OPERATIONS
################################################################################

#' Create a new position group
#'
#' Creates a group with metadata and associated member positions.
#' Transactional - all or nothing.
#'
#' @param group_id Unique group identifier
#' @param group_name Human-readable group name
#' @param strategy_type Strategy name (e.g., "Dividend Aristocrats")
#' @param account_number Questrade account number
#' @param members Tibble with columns: symbol, role
#' @return Logical TRUE if successful, FALSE otherwise
#' @noRd
create_position_group <- function(group_id, group_name, strategy_type,
                                  account_number, members = NULL) {

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_groups_schema(conn)

    # Begin transaction
    dbExecute(conn, "BEGIN TRANSACTION")

    # Insert group metadata
    timestamp <- Sys.time()
    group_data <- tibble::tibble(
      group_id = group_id,
      group_name = group_name,
      strategy_type = strategy_type,
      account_number = account_number,
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
        ) %>%
        select(group_id, account_number, symbol, role, added_at)

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
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

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

#' Delete a position group
#'
#' Deletes a group and all its members (cascade).
#'
#' @param group_id Group identifier
#' @return Logical TRUE if successful
#' @noRd
delete_position_group <- function(group_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Begin transaction
    dbExecute(conn, "BEGIN TRANSACTION")

    # Delete members first (manual cascade)
    dbExecute(conn, "DELETE FROM position_group_members WHERE group_id = ?",
              params = list(group_id))

    # Delete group
    rows_affected <- dbExecute(conn, "DELETE FROM position_groups WHERE group_id = ?",
                               params = list(group_id))

    # Commit transaction
    dbExecute(conn, "COMMIT")

    if (rows_affected > 0) {
      log_info("Portfolio Groups DB: Deleted group {group_id}")
      return(TRUE)
    } else {
      log_warn("Portfolio Groups DB: Group {group_id} not found")
      return(FALSE)
    }
  }, error = function(e) {
    # Rollback on error
    tryCatch(dbExecute(conn, "ROLLBACK"), error = function(e2) NULL)
    log_error("Portfolio Groups DB: Failed to delete group {group_id} - {e$message}")
    return(FALSE)
  })
}

################################################################################
# GROUP RETRIEVAL OPERATIONS
################################################################################

#' Get all position groups
#'
#' Retrieves all groups.
#'
#' @return Tibble with group data
#' @noRd
get_all_groups <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_groups_schema(conn)

    result <- dbGetQuery(conn, "
      SELECT
        g.group_id,
        g.group_name,
        g.strategy_type,
        g.account_number,
        g.created_at,
        g.updated_at
      FROM position_groups g
      ORDER BY g.updated_at DESC
    ")

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
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

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
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

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
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

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
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

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
  # Match on account_number AND symbol
  members_check <- members %>%
    left_join(
      current_positions %>% select(account_number, symbol),
      by = c("account_number", "symbol")
    ) %>%
    mutate(exists = !is.na(symbol))

  missing_count <- sum(!members_check$exists)

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
