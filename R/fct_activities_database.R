#' Account Activities Database Operations
#'
#' Functions for managing account activity transactions (trades, dividends,
#' option events) in DuckDB. Stores all transactions fetched from Questrade API
#' for pattern matching and P&L calculations.
#'
#' @name activities-database
#' @import dplyr
#' @importFrom duckdb duckdb dbConnect dbDisconnect
#' @importFrom DBI dbExecute dbWriteTable dbGetQuery
#' @importFrom logger log_info log_warn log_error log_debug
#' @importFrom purrr map_chr
NULL

################################################################################
# SCHEMA INITIALIZATION
################################################################################

#' Initialize account activities database schema
#'
#' Creates the account_activities table if it doesn't exist
#'
#' @param conn DBI connection object
#' @return Logical TRUE if successful
#' @noRd
initialize_activities_schema <- function(conn) {
  tryCatch({
    # Create account_activities table
    dbExecute(conn, "
      CREATE TABLE IF NOT EXISTS account_activities (
        activity_id VARCHAR PRIMARY KEY,
        account_number VARCHAR NOT NULL,
        account_type VARCHAR NOT NULL,
        trade_date TIMESTAMP,
        transaction_date TIMESTAMP,
        settlement_date TIMESTAMP,
        action VARCHAR,
        symbol VARCHAR,
        symbol_id INTEGER,
        description VARCHAR,
        currency VARCHAR,
        quantity DOUBLE,
        price DOUBLE,
        gross_amount DOUBLE,
        commission DOUBLE,
        net_amount DOUBLE,
        type VARCHAR,
        group_id VARCHAR,
        is_processed BOOLEAN DEFAULT FALSE,
        fetched_at TIMESTAMP NOT NULL
      )
    ")

    # Create unique constraint to prevent duplicates
    dbExecute(conn, "
      CREATE UNIQUE INDEX IF NOT EXISTS idx_activities_unique
      ON account_activities(account_number, symbol, transaction_date, action, quantity, net_amount)
    ")

    # Create indexes for performance
    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_activities_processed
      ON account_activities(is_processed)
    ")

    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_activities_group
      ON account_activities(group_id)
    ")

    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_activities_symbol
      ON account_activities(symbol)
    ")

    dbExecute(conn, "
      CREATE INDEX IF NOT EXISTS idx_activities_type
      ON account_activities(type)
    ")

    log_info("Activities DB: Schema initialized successfully")
    return(TRUE)
  }, error = function(e) {
    log_error("Activities DB: Schema initialization failed - {e$message}")
    return(FALSE)
  })
}

################################################################################
# CRUD OPERATIONS
################################################################################

#' Save batch of activities to database
#'
#' Inserts new activities after checking for duplicates using unique constraint.
#' Skips activities that already exist.
#'
#' @param activities_df Tibble with activity data from fetch_all_activities()
#' @return List with inserted_count and skipped_count
#' @noRd
save_activities_batch <- function(activities_df) {
  if (is.null(activities_df) || nrow(activities_df) == 0) {
    log_warn("Activities DB: No activities to save")
    return(list(inserted_count = 0, skipped_count = 0))
  }

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_activities_schema(conn)

    # Add required fields
    fetched_at <- Sys.time()
    activities_to_save <- activities_df %>%
      mutate(
        activity_id = map_chr(seq_len(nrow(activities_df)), function(i) {
          generate_activity_id(
            account_number[i],
            symbol[i],
            transaction_date[i]
          )
        }),
        # Add account_type if missing (for backward compatibility with tests)
        account_type = if ("account_type" %in% names(.)) account_type else "UNKNOWN",
        group_id = NA_character_,
        is_processed = FALSE,
        fetched_at = fetched_at
      )

    # Query existing activity keys (for duplicate detection)
    existing_keys <- dbGetQuery(conn, "
      SELECT account_number, symbol, transaction_date, action, quantity, net_amount
      FROM account_activities
    ") %>%
      as_tibble() %>%
      mutate(transaction_date = as.character(transaction_date))

    # Find new activities using anti_join (tidyverse way to filter duplicates)
    # Normalize transaction_date to character for type-safe comparison
    new_activities <- activities_to_save %>%
      mutate(transaction_date_char = as.character(transaction_date)) %>%
      anti_join(
        existing_keys,
        by = c("account_number", "symbol", "transaction_date_char" = "transaction_date", "action", "quantity", "net_amount")
      ) %>%
      select(-transaction_date_char)

    # Calculate counts
    inserted_count <- nrow(new_activities)
    skipped_count <- nrow(activities_to_save) - inserted_count

    # Bulk insert all new activities (one database operation)
    if (inserted_count > 0) {
      dbWriteTable(conn, "account_activities", new_activities, append = TRUE)
    }

    log_info("Activities DB: Saved {inserted_count} activities, skipped {skipped_count} duplicates")
    return(list(inserted_count = inserted_count, skipped_count = skipped_count))

  }, error = function(e) {
    log_error("Activities DB: Failed to save activities - {e$message}")
    return(list(inserted_count = 0, skipped_count = 0))
  })
}

#' Get unprocessed activities
#'
#' Retrieves all activities that haven't been analyzed for pattern matching
#'
#' @return Tibble with unprocessed activity data
#' @noRd
get_unprocessed_activities <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_activities_schema(conn)

    result <- dbGetQuery(conn, "
      SELECT *
      FROM account_activities
      WHERE is_processed = FALSE
      ORDER BY trade_date DESC
    ")

    if (nrow(result) == 0) {
      log_debug("Activities DB: No unprocessed activities found")
      return(tibble::tibble())
    }

    log_debug("Activities DB: Retrieved {nrow(result)} unprocessed activities")
    return(tibble::as_tibble(result))

  }, error = function(e) {
    log_error("Activities DB: Failed to get unprocessed activities - {e$message}")
    return(tibble::tibble())
  })
}

#' Get last activities update timestamp
#'
#' Returns the most recent fetched_at timestamp from activities database
#'
#' @return POSIXct timestamp of last fetch, or NULL if no activities exist
#' @export
get_last_activities_update <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_activities_schema(conn)

    result <- dbGetQuery(conn, "
      SELECT MAX(fetched_at) as latest_timestamp
      FROM account_activities
    ")

    if (is.null(result$latest_timestamp) || is.na(result$latest_timestamp)) {
      log_debug("Activities DB: No activities found")
      return(NULL)
    }

    timestamp <- as.POSIXct(result$latest_timestamp)
    log_debug("Activities DB: Latest fetch at {timestamp}")
    return(timestamp)
  }, error = function(e) {
    log_error("Activities DB: Failed to get latest timestamp - {e$message}")
    return(NULL)
  })
}

#' Get activities by group ID
#'
#' Retrieves all activities linked to a specific position group
#'
#' @param group_id Group identifier
#' @return Tibble with activity data
#' @noRd
get_activities_by_group <- function(group_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    result <- dbGetQuery(conn, "
      SELECT *
      FROM account_activities
      WHERE group_id = ?
      ORDER BY trade_date ASC
    ", params = list(group_id))

    if (nrow(result) == 0) {
      log_debug("Activities DB: No activities found for group {group_id}")
      return(tibble::tibble())
    }

    log_debug("Activities DB: Retrieved {nrow(result)} activities for group {group_id}")
    return(tibble::as_tibble(result))

  }, error = function(e) {
    log_error("Activities DB: Failed to get activities for group {group_id} - {e$message}")
    return(tibble::tibble())
  })
}

#' Get activities for multiple groups (batch operation)
#'
#' Retrieves all activities for multiple position groups in a single query.
#' More efficient than calling get_activities_by_group() multiple times.
#'
#' @param group_ids Vector of group identifiers
#' @return Tibble with activity data including group_id column
#' @noRd
get_activities_for_groups <- function(group_ids) {
  if (length(group_ids) == 0) {
    return(tibble::tibble())
  }

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Build IN clause for SQL
    placeholders <- paste(rep("?", length(group_ids)), collapse = ", ")
    sql <- sprintf("
      SELECT *
      FROM account_activities
      WHERE group_id IN (%s)
      ORDER BY group_id, trade_date ASC
    ", placeholders)

    result <- dbGetQuery(conn, sql, params = as.list(group_ids))

    if (nrow(result) == 0) {
      log_debug("Activities DB: No activities found for {length(group_ids)} groups")
      return(tibble::tibble())
    }

    log_debug("Activities DB: Retrieved {nrow(result)} activities for {length(group_ids)} groups")
    return(tibble::as_tibble(result))

  }, error = function(e) {
    log_error("Activities DB: Failed to get activities for groups - {e$message}")
    return(tibble::tibble())
  })
}

#' Get unlinked trade activities for a ticker and account
#'
#' Finds buy/sell trade activities that haven't been linked to any group yet
#'
#' @param ticker Stock ticker symbol
#' @param account_number Account number
#' @return Tibble with unlinked activity data
#' @noRd
get_unlinked_activities_for_ticker <- function(ticker, account_number) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    result <- dbGetQuery(conn, "
      SELECT *
      FROM account_activities
      WHERE symbol = ?
        AND account_number = ?
        AND type = 'Trades'
        AND (group_id IS NULL OR group_id = '')
      ORDER BY trade_date ASC
    ", params = list(ticker, account_number))

    if (nrow(result) == 0) {
      log_debug("Activities DB: No unlinked activities found for {ticker} in account {account_number}")
      return(tibble::tibble())
    }

    log_debug("Activities DB: Found {nrow(result)} unlinked activities for {ticker}")
    return(tibble::as_tibble(result))

  }, error = function(e) {
    log_error("Activities DB: Failed to get unlinked activities for {ticker} - {e$message}")
    return(tibble::tibble())
  })
}

#' Mark activities as processed
#'
#' Updates is_processed flag to TRUE for specified activities
#'
#' @param activity_ids Character vector of activity IDs
#' @return Logical TRUE if successful
#' @noRd
mark_activities_processed <- function(activity_ids) {
  if (length(activity_ids) == 0) {
    return(TRUE)
  }

  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Create placeholders for IN clause
    placeholders <- paste(rep("?", length(activity_ids)), collapse = ", ")

    sql <- sprintf("
      UPDATE account_activities
      SET is_processed = TRUE
      WHERE activity_id IN (%s)
    ", placeholders)

    rows_affected <- dbExecute(conn, sql, params = as.list(activity_ids))

    log_info("Activities DB: Marked {rows_affected} activities as processed")
    return(TRUE)

  }, error = function(e) {
    log_error("Activities DB: Failed to mark activities as processed - {e$message}")
    return(FALSE)
  })
}

#' Link activity to position group
#'
#' Updates group_id field to associate activity with a group
#'
#' @param activity_id Activity identifier
#' @param group_id Group identifier
#' @return Logical TRUE if successful
#' @noRd
link_activity_to_group <- function(activity_id, group_id) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    rows_affected <- dbExecute(conn, "
      UPDATE account_activities
      SET group_id = ?
      WHERE activity_id = ?
    ", params = list(group_id, activity_id))

    if (rows_affected > 0) {
      log_debug("Activities DB: Linked activity {activity_id} to group {group_id}")
      return(TRUE)
    } else {
      log_warn("Activities DB: Activity {activity_id} not found")
      return(FALSE)
    }

  }, error = function(e) {
    log_error("Activities DB: Failed to link activity {activity_id} to group {group_id} - {e$message}")
    return(FALSE)
  })
}

#' Get all activities (optional filters)
#'
#' Retrieves activities with optional filtering by type, symbol, or date range
#'
#' @param type_filter Optional type filter ("Trades", "Dividends", "Other")
#' @param symbol_filter Optional symbol filter
#' @param start_date Optional start date
#' @param end_date Optional end date
#' @return Tibble with activity data
#' @noRd
get_activities <- function(type_filter = NULL, symbol_filter = NULL,
                           start_date = NULL, end_date = NULL) {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Ensure schema exists
    initialize_activities_schema(conn)

    # Build dynamic SQL query
    sql <- "SELECT * FROM account_activities WHERE 1=1"
    params <- list()

    if (!is.null(type_filter)) {
      sql <- paste(sql, "AND type = ?")
      params <- c(params, list(type_filter))
    }

    if (!is.null(symbol_filter)) {
      sql <- paste(sql, "AND symbol = ?")
      params <- c(params, list(symbol_filter))
    }

    if (!is.null(start_date)) {
      sql <- paste(sql, "AND trade_date >= ?")
      params <- c(params, list(start_date))
    }

    if (!is.null(end_date)) {
      sql <- paste(sql, "AND trade_date <= ?")
      params <- c(params, list(end_date))
    }

    sql <- paste(sql, "ORDER BY trade_date DESC")

    result <- if (length(params) > 0) {
      dbGetQuery(conn, sql, params = params)
    } else {
      dbGetQuery(conn, sql)
    }

    if (nrow(result) == 0) {
      log_debug("Activities DB: No activities found with specified filters")
      return(tibble::tibble())
    }

    log_debug("Activities DB: Retrieved {nrow(result)} activities")
    return(tibble::as_tibble(result))

  }, error = function(e) {
    log_error("Activities DB: Failed to get activities - {e$message}")
    return(tibble::tibble())
  })
}

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Generate unique activity identifier
#'
#' Creates a unique ID based on account, symbol, and timestamp
#'
#' @param account_number Account number
#' @param symbol Transaction symbol
#' @param transaction_date Transaction date
#' @return Character unique activity ID
#' @noRd
generate_activity_id <- function(account_number, symbol, transaction_date) {
  # Sanitize symbol for ID (handle empty symbols for FX conversions, etc.)
  symbol_clean <- if (is.na(symbol) || symbol == "") {
    "NOSYMBOL"
  } else {
    gsub("[^A-Za-z0-9]", "", symbol)
  }

  # Create timestamp component
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

  # Random suffix to ensure uniqueness
  random_suffix <- sample(1000:9999, 1)

  sprintf("ACT_%s_%s_%s_%s", account_number, symbol_clean, timestamp, random_suffix)
}
