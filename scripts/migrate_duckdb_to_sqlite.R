#!/usr/bin/env Rscript
#
# DuckDB to SQLite Migration
#
# Transfers all tables from the DuckDB portfolio database to a new SQLite file.
# Verifies row counts match after transfer.
#
# Usage:
#   Rscript scripts/migrate_duckdb_to_sqlite.R

library(DBI)
library(duckdb)
library(RSQLite)

TABLES <- c(
  "account_activities",
  "grouping_suggestions",
  "position_group_cash_flows",
  "position_group_members",
  "position_groups",
  "positions_history",
  "projection_recalculations"
)

SOURCE_PATH <- file.path(getwd(), "inst", "database", "portfolio.duckdb")
TARGET_PATH <- file.path(getwd(), "inst", "database", "portfolio.sqlite")

cat("=== DuckDB to SQLite Migration ===\n\n")

if (!file.exists(SOURCE_PATH)) {
  stop("Source database not found: ", SOURCE_PATH)
}

if (file.exists(TARGET_PATH)) {
  stop("Target file already exists: ", TARGET_PATH, "\nRemove it manually before running this script.")
}

cat("Source: ", SOURCE_PATH, "\n")
cat("Target: ", TARGET_PATH, "\n\n")

duck_con <- dbConnect(duckdb::duckdb(), dbdir = SOURCE_PATH, read_only = TRUE)
on.exit(dbDisconnect(duck_con, shutdown = TRUE), add = TRUE)

lite_con <- dbConnect(RSQLite::SQLite(), TARGET_PATH)
on.exit(dbDisconnect(lite_con), add = TRUE)

results <- data.frame(
  table          = character(length(TABLES)),
  source_rows    = integer(length(TABLES)),
  target_rows    = integer(length(TABLES)),
  status         = character(length(TABLES)),
  stringsAsFactors = FALSE
)

for (i in seq_along(TABLES)) {
  tbl <- TABLES[i]
  cat("Migrating: ", tbl, "... ", sep = "")

  tryCatch({
    data <- dbReadTable(duck_con, tbl)
    dbWriteTable(lite_con, tbl, data, overwrite = FALSE)

    source_rows <- nrow(data)
    target_rows <- dbGetQuery(lite_con, paste0("SELECT COUNT(*) AS n FROM ", tbl))$n
    ok <- source_rows == target_rows

    results$table[i]       <- tbl
    results$source_rows[i] <- source_rows
    results$target_rows[i] <- target_rows
    results$status[i]      <- if (ok) "PASS" else "FAIL"

    cat(if (ok) "PASS" else "FAIL", " (", source_rows, " rows)\n", sep = "")

  }, error = function(e) {
    results$table[i]  <<- tbl
    results$status[i] <<- "ERROR"
    cat("ERROR: ", conditionMessage(e), "\n", sep = "")
  })
}

cat("\n=== Migration Summary ===\n")
cat(sprintf("%-35s %12s %12s %s\n", "Table", "Source Rows", "Target Rows", "Status"))
cat(strrep("-", 65), "\n")
for (i in seq_len(nrow(results))) {
  cat(sprintf("%-35s %12d %12d %s\n",
    results$table[i],
    results$source_rows[i],
    results$target_rows[i],
    results$status[i]
  ))
}
cat(strrep("-", 65), "\n")

n_fail <- sum(results$status != "PASS")
if (n_fail == 0) {
  cat("All tables migrated successfully.\n")
} else {
  cat(n_fail, "table(s) failed. Review output above.\n")
  quit(status = 1)
}
