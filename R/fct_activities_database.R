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
#' @importFrom lubridate as_datetime
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
    # Uses description instead of symbol because Questrade API inconsistently returns
    # symbol field (sometimes blank, sometimes populated) for the same transaction.
    # Description field is always consistent across API calls.
    # Uses trade_date (when trade occurred) instead of transaction_date (settlement timestamp)
    # This prevents API timestamp inconsistencies from creating duplicate records

    # Drop old index if it exists (using symbol)
    tryCatch({
      dbExecute(conn, "DROP INDEX IF EXISTS idx_activities_unique")
    }, error = function(e) {
      log_warn("Activities DB: Could not drop old index - {e$message}")
    })

    # Create new index using description instead of symbol
    dbExecute(conn, "
      CREATE UNIQUE INDEX IF NOT EXISTS idx_activities_unique
      ON account_activities(account_number, description, trade_date, action, quantity, net_amount)
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

    # Migration: Add ignore_for_grouping column if it doesn't exist
    tryCatch({
      dbExecute(conn, "
        ALTER TABLE account_activities
        ADD COLUMN IF NOT EXISTS ignore_for_grouping BOOLEAN DEFAULT FALSE
      ")
      log_info("Activities DB: Added ignore_for_grouping column (migration)")
    }, error = function(e) {
      # Column might already exist in older databases, that's okay
      log_debug("Activities DB: ignore_for_grouping column already exists")
    })

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
    initialize_activities_schema(conn)

    # 1. Standardize incoming data: Parse character dates to POSIXct.
    #    The API provides ISO 8601 format; lubridate::as_datetime is robust.
    activities_to_save <- activities_df %>% 
      mutate(
        transaction_date = lubridate::as_datetime(transaction_date),
        trade_date = lubridate::as_datetime(trade_date),
        settlement_date = lubridate::as_datetime(settlement_date)
      )

    # 2. Query existing keys using trade_date (business key, not settlement timestamp).
    # Uses description instead of symbol for deduplication because Questrade API
    # inconsistently returns symbol field for the same transaction.
    existing_keys <- dbGetQuery(conn, "
      SELECT account_number, description, trade_date, action, quantity, net_amount
      FROM account_activities
    ") %>%
      as_tibble()

    # 3. Perform the anti_join on native data types for robust duplicate detection.
    # Uses description instead of symbol to match the unique constraint.
    # Description is consistent across Questrade API calls, symbol is not.
    new_activities <- activities_to_save %>%
      anti_join(
        existing_keys,
        by = c("account_number", "description", "trade_date", "action", "quantity", "net_amount")
      )

    if (nrow(new_activities) == 0) {
      log_info("Activities DB: No new activities to save. Skipped {nrow(activities_to_save)} duplicates.")
      return(list(inserted_count = 0, skipped_count = nrow(activities_to_save)))
    }

    # Add remaining metadata to new activities only
    activities_to_insert <- new_activities %>%
      mutate(
        activity_id = map_chr(seq_len(nrow(.)), ~generate_activity_id(
          account_number[.], symbol[.], transaction_date[.]
        )),
        account_type = if ("account_type" %in% names(.)) account_type else "UNKNOWN",
        group_id = NA_character_,
        is_processed = FALSE,
        fetched_at = Sys.time()
      )

    # Bulk insert all new activities
    dbWriteTable(conn, "account_activities", activities_to_insert, append = TRUE)

    inserted_count <- nrow(activities_to_insert)
    skipped_count <- nrow(activities_to_save) - inserted_count

    log_info("Activities DB: Saved {inserted_count} activities, skipped {skipped_count} duplicates")
    return(list(inserted_count = inserted_count, skipped_count = skipped_count))

  }, error = function(e) {
    log_error("Activities DB: Failed to save activities - {conditionMessage(e)}")
    return(list(inserted_count = 0, skipped_count = 0))
  })
}

#' Enrich blank symbols for unlinked transactions
#'
#' Finds unlinked transactions with blank/missing symbol fields and enriches them
#' by copying symbol data from matching transactions that have populated symbols.
#' Only updates transactions that are NOT linked to groups (preserves manual work).
#'
#' Matching criteria: same account_number, description, trade_date, quantity, net_amount
#'
#' @return Integer count of transactions enriched
#' @noRd
enrich_blank_symbols_unlinked <- function() {
  conn <- get_portfolio_db_connection()
  on.exit(dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  tryCatch({
    # Find unlinked transactions with blank symbols
    blank_symbols <- dbGetQuery(conn, "
      SELECT
        activity_id,
        account_number,
        description,
        trade_date,
        quantity,
        net_amount,
        symbol,
        symbol_id
      FROM account_activities
      WHERE (symbol IS NULL OR symbol = '' OR symbol = 'NOSYMBOL')
        AND (group_id IS NULL OR group_id = '')
    ") %>% as_tibble()

    if (nrow(blank_symbols) == 0) {
      log_debug("Symbol Enrichment: No unlinked blank symbols to enrich")
      return(0)
    }

    log_info("Symbol Enrichment: Found {nrow(blank_symbols)} unlinked transactions with blank symbols")

    # For each blank symbol transaction, look for matching transaction with populated symbol
    enriched_count <- 0

    for (i in seq_len(nrow(blank_symbols))) {
      blank_tx <- blank_symbols[i, ]

      # Find matching transactions with populated symbols
      # Match on: account, description, date, quantity, amount (same criteria as deduplication)
      matching_tx <- dbGetQuery(conn, "
        SELECT symbol, symbol_id
        FROM account_activities
        WHERE account_number = ?
          AND description = ?
          AND trade_date = ?
          AND quantity = ?
          AND net_amount = ?
          AND symbol IS NOT NULL
          AND symbol != ''
          AND symbol != 'NOSYMBOL'
        LIMIT 1
      ", params = list(
        blank_tx$account_number,
        blank_tx$description,
        blank_tx$trade_date,
        blank_tx$quantity,
        blank_tx$net_amount
      ))

      if (nrow(matching_tx) > 0) {
        # Update the blank symbol transaction with enriched data
        rows_updated <- dbExecute(conn, "
          UPDATE account_activities
          SET symbol = ?,
              symbol_id = ?
          WHERE activity_id = ?
        ", params = list(
          matching_tx$symbol[1],
          matching_tx$symbol_id[1],
          blank_tx$activity_id
        ))

        if (rows_updated > 0) {
          enriched_count <- enriched_count + 1
          log_debug("Symbol Enrichment: Updated {blank_tx$activity_id} with symbol '{matching_tx$symbol[1]}'")
        }
      }
    }

    if (enriched_count > 0) {
      log_info("Symbol Enrichment: Enriched {enriched_count} unlinked transactions with symbol data")
    }

    return(enriched_count)

  }, error = function(e) {
    log_error("Symbol Enrichment: Failed - {e$message}")
    return(0)
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
    # First, get activity details to check if reconciliation is needed
    activity <- dbGetQuery(conn, "
      SELECT activity_id, type, transaction_date
      FROM account_activities
      WHERE activity_id = ?
    ", params = list(activity_id))

    if (nrow(activity) == 0) {
      log_warn("Activities DB: Activity {activity_id} not found")
      return(FALSE)
    }

    # Link activity to group
    rows_affected <- dbExecute(conn, "
      UPDATE account_activities
      SET group_id = ?
      WHERE activity_id = ?
    ", params = list(group_id, activity_id))

    if (rows_affected > 0) {
      log_debug("Activities DB: Linked activity {activity_id} to group {group_id}")

      # Reconcile projected dividends if this is a dividend activity
      if (activity$type[1] == "Dividends") {
        delete_projected_cash_flows_by_month(
          group_id = group_id,
          event_type = "dividend",
          event_date = as.Date(activity$transaction_date[1]),
          conn = conn
        )
      }

      return(TRUE)
    } else {
      log_warn("Activities DB: Failed to update activity {activity_id}")
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
