#' Grouping Suggestions Database Operations
#'
#' Functions for managing position grouping suggestions in DuckDB.
#' Stores pattern-matched suggestions awaiting user approval.
#'
#' @name suggestions-database
#' @import dplyr
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbExecute dbWriteTable dbGetQuery
#' @importFrom logger log_info log_warn log_error log_debug
#' @importFrom jsonlite toJSON fromJSON
NULL

################################################################################
# SCHEMA INITIALIZATION
################################################################################

#' Initialize grouping suggestions database schema
#'
#' Creates the grouping_suggestions table if it doesn't exist
#'
#' @param conn DBI connection object
#' @return Logical TRUE if successful
#' @noRd
initialize_suggestions_schema <- function(conn) {
  tryCatch({
    # Create grouping_suggestions table
    dbExecute(conn, "
      CREATE TABLE IF NOT EXISTS grouping_suggestions (
        suggestion_id VARCHAR PRIMARY KEY,
        created_at TIMESTAMP NOT NULL,
        pattern_type VARCHAR NOT NULL,
        involved_activity_ids VARCHAR NOT NULL,
        suggested_group_name VARCHAR NOT NULL,
        suggested_strategy_type VARCHAR NOT NULL,
        reasoning VARCHAR NOT NULL,
        status VARCHAR DEFAULT 'pending',
        user_action_at TIMESTAMP,
        created_group_id VARCHAR
      )
    ")

    # Create indexes for performance
    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_suggestions_status
      ON grouping_suggestions(status)
    ")

    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_suggestions_pattern
      ON grouping_suggestions(pattern_type)
    ")

    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_suggestions_created
      ON grouping_suggestions(created_at)
    ")

    log_debug("Suggestions DB: Schema initialized successfully")
    return(TRUE)
  }, error = function(e) {
    log_error("Suggestions DB: Schema initialization failed - {e$message}")
    return(FALSE)
  })
}

################################################################################
# CRUD OPERATIONS
################################################################################

#' Save a grouping suggestion
#'
#' Creates a new suggestion record for user review
#'
#' @param pattern_type Type of pattern detected
#' @param activity_ids Character vector of activity IDs involved in the pattern
#' @param suggested_group_name Auto-generated group name
#' @param suggested_strategy_type Suggested strategy type
#' @param reasoning Human-readable explanation
#' @return Character suggestion_id if successful, NULL if failed
#' @noRd
save_suggestion <- function(pattern_type, activity_ids, suggested_group_name,
                            suggested_strategy_type, reasoning) {

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_suggestions_schema(conn)

    # Generate unique suggestion ID
    suggestion_id <- generate_suggestion_id(pattern_type)

    # Convert activity_ids to JSON string
    activity_ids_json <- jsonlite::toJSON(activity_ids, auto_unbox = FALSE)

    suggestion_data <- tibble::tibble(
      suggestion_id = suggestion_id,
      created_at = Sys.time(),
      pattern_type = pattern_type,
      involved_activity_ids = as.character(activity_ids_json),
      suggested_group_name = suggested_group_name,
      suggested_strategy_type = suggested_strategy_type,
      reasoning = reasoning,
      status = "pending",
      user_action_at = NA_character_,
      created_group_id = NA_character_
    )

    dbWriteTable(conn, "grouping_suggestions", suggestion_data, append = TRUE)

    log_info("Suggestions DB: Created suggestion {suggestion_id} ({pattern_type})")
    return(suggestion_id)

  }, error = function(e) {
    log_error("Suggestions DB: Failed to save suggestion - {e$message}")
    return(NULL)
  })
}

#' Get pending suggestions
#'
#' Retrieves all suggestions awaiting user action, ordered by creation time
#'
#' @return Tibble with suggestion data
#' @noRd
get_pending_suggestions <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_suggestions_schema(conn)

    result <- dbGetQuery(conn, "
      SELECT *
      FROM grouping_suggestions
      WHERE status = 'pending'
      ORDER BY created_at DESC
    ")

    if (nrow(result) == 0) {
      log_debug("Suggestions DB: No pending suggestions found")
      return(tibble::tibble())
    }

    # Convert JSON activity IDs back to character vectors
    result_tibble <- tibble::as_tibble(result) %>%
      mutate(
        involved_activity_ids = purrr::map(involved_activity_ids, function(ids_json) {
          jsonlite::fromJSON(ids_json)
        })
      )

    log_debug("Suggestions DB: Retrieved {nrow(result)} pending suggestions")
    return(result_tibble)

  }, error = function(e) {
    log_error("Suggestions DB: Failed to get pending suggestions - {e$message}")
    return(tibble::tibble())
  })
}

#' Get suggestion by ID
#'
#' Retrieves a single suggestion with full details
#'
#' @param suggestion_id Suggestion identifier
#' @return Tibble with single row or empty tibble
#' @noRd
get_suggestion_by_id <- function(suggestion_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    result <- dbGetQuery(conn, "
      SELECT *
      FROM grouping_suggestions
      WHERE suggestion_id = ?
    ", params = list(suggestion_id))

    if (nrow(result) == 0) {
      log_debug("Suggestions DB: Suggestion {suggestion_id} not found")
      return(tibble::tibble())
    }

    # Convert JSON activity IDs
    result_tibble <- tibble::as_tibble(result) %>%
      mutate(
        involved_activity_ids = purrr::map(involved_activity_ids, function(ids_json) {
          jsonlite::fromJSON(ids_json)
        })
      )

    return(result_tibble)

  }, error = function(e) {
    log_error("Suggestions DB: Failed to get suggestion {suggestion_id} - {e$message}")
    return(tibble::tibble())
  })
}

#' Approve a suggestion
#'
#' Updates suggestion status to approved and records the created group ID
#'
#' @param suggestion_id Suggestion identifier
#' @param created_group_id ID of the group that was created
#' @return Logical TRUE if successful
#' @noRd
approve_suggestion <- function(suggestion_id, created_group_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    rows_affected <- dbExecute(conn, "
      UPDATE grouping_suggestions
      SET status = 'approved',
          user_action_at = ?,
          created_group_id = ?
      WHERE suggestion_id = ?
    ", params = list(Sys.time(), created_group_id, suggestion_id))

    if (rows_affected > 0) {
      log_info("Suggestions DB: Approved suggestion {suggestion_id}")
      return(TRUE)
    } else {
      log_warn("Suggestions DB: Suggestion {suggestion_id} not found")
      return(FALSE)
    }

  }, error = function(e) {
    log_error("Suggestions DB: Failed to approve suggestion {suggestion_id} - {e$message}")
    return(FALSE)
  })
}

#' Reject a suggestion
#'
#' Updates suggestion status to rejected
#'
#' @param suggestion_id Suggestion identifier
#' @return Logical TRUE if successful
#' @noRd
reject_suggestion <- function(suggestion_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    rows_affected <- dbExecute(conn, "
      UPDATE grouping_suggestions
      SET status = 'rejected',
          user_action_at = ?
      WHERE suggestion_id = ?
    ", params = list(Sys.time(), suggestion_id))

    if (rows_affected > 0) {
      log_info("Suggestions DB: Rejected suggestion {suggestion_id}")
      return(TRUE)
    } else {
      log_warn("Suggestions DB: Suggestion {suggestion_id} not found")
      return(FALSE)
    }

  }, error = function(e) {
    log_error("Suggestions DB: Failed to reject suggestion {suggestion_id} - {e$message}")
    return(FALSE)
  })
}

#' Get suggestion count by status
#'
#' Returns count of suggestions grouped by status
#'
#' @return Tibble with status and count columns
#' @noRd
get_suggestion_counts <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_suggestions_schema(conn)

    result <- dbGetQuery(conn, "
      SELECT
        status,
        COUNT(*) AS count
      FROM grouping_suggestions
      GROUP BY status
    ")

    if (nrow(result) == 0) {
      return(tibble::tibble(
        status = character(),
        count = integer()
      ))
    }

    return(tibble::as_tibble(result))

  }, error = function(e) {
    log_error("Suggestions DB: Failed to get suggestion counts - {e$message}")
    return(tibble::tibble())
  })
}

#' Get suggestion history
#'
#' Retrieves all suggestions (pending, approved, rejected) for audit trail
#'
#' @param limit Optional limit on number of results
#' @return Tibble with suggestion data
#' @noRd
get_suggestion_history <- function(limit = NULL) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_suggestions_schema(conn)

    sql <- "
      SELECT *
      FROM grouping_suggestions
      ORDER BY created_at DESC
    "

    if (!is.null(limit)) {
      sql <- paste(sql, "LIMIT ?")
      result <- dbGetQuery(conn, sql, params = list(limit))
    } else {
      result <- dbGetQuery(conn, sql)
    }

    if (nrow(result) == 0) {
      return(tibble::tibble())
    }

    # Convert JSON activity IDs
    result_tibble <- tibble::as_tibble(result) %>%
      mutate(
        involved_activity_ids = purrr::map(involved_activity_ids, function(ids_json) {
          jsonlite::fromJSON(ids_json)
        })
      )

    return(result_tibble)

  }, error = function(e) {
    log_error("Suggestions DB: Failed to get suggestion history - {e$message}")
    return(tibble::tibble())
  })
}

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Generate unique suggestion identifier
#'
#' Creates a unique ID based on pattern type and timestamp
#'
#' @param pattern_type Pattern type string
#' @return Character unique suggestion ID
#' @noRd
generate_suggestion_id <- function(pattern_type) {
  # Sanitize pattern type for ID
  pattern_clean <- pattern_type %>%
    gsub(" ", "_", .) %>%
    gsub("[^A-Za-z0-9_]", "", .) %>%
    toupper()

  # Create timestamp component
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

  # Random suffix to ensure uniqueness
  random_suffix <- sample(1000:9999, 1)

  sprintf("SUG_%s_%s_%s", pattern_clean, timestamp, random_suffix)
}
