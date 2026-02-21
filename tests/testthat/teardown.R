# Test Teardown
#
# Runs once after all tests. Releases DuckDB locks and removes any temp files
# created by helper-test-db.R that were not cleaned up individually.

# Release DuckDB locks before attempting file removal
gc()

# Remove any leftover temp databases matching the pattern used by helper-test-db.R
leftover <- list.files(
  path      = tempdir(),
  pattern   = "^investR_test_.*\\.sqlite",
  full.names = TRUE
)

for (f in leftover) {
  tryCatch(
    unlink(f, recursive = TRUE),
    error = function(e) warning(sprintf("Could not remove temp file %s: %s", f, e$message))
  )
}
