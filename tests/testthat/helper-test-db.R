# Test Database Helper
#
# Provides local_test_db() for per-test database isolation.
#
# WHY namespace-level mocking matters:
#
# When package functions call get_portfolio_db_path() internally, R resolves
# that name in the PACKAGE namespace (investR:::get_portfolio_db_path), not in
# .GlobalEnv. Assigning a replacement in .GlobalEnv has no effect on internal
# calls — the package never looks there.
#
# local_mocked_bindings(.package = "investR") patches the binding directly in
# the investR namespace for the duration of the test, so every internal call
# to get_portfolio_db_path() — including those inside get_portfolio_db_connection()
# — sees the temp path. The binding is automatically restored when the test exits.

local_test_db <- function(env = parent.frame()) {
  # 1. Create a unique temp path for this test's isolated DuckDB file.
  test_db_path <- tempfile(pattern = "investR_test_", fileext = ".sqlite")

  # 2. Mock get_portfolio_db_path in the investR package namespace.
  # This ensures all internal package calls (e.g. get_portfolio_db_connection)
  # route to the temp DB rather than the production DB at inst/database/portfolio.duckdb.
  local_mocked_bindings(
    get_portfolio_db_path = function() test_db_path,
    .package = "investR",
    .env = env
  )

  # 3. Initialize the full schema in the temp DB.
  # We open a direct connection here (bypassing get_portfolio_db_connection so
  # we control shutdown), run both schema initializers, then disconnect cleanly
  # before any test code runs.
  conn <- DBI::dbConnect(RSQLite::SQLite(), test_db_path)
  tryCatch({
    initialize_portfolio_database(conn)
    initialize_income_projection_schema(conn)
    initialize_activities_schema(conn)
  }, finally = {
    DBI::dbDisconnect(conn)
  })

  # 4. Register cleanup: remove the temp file when the calling test finishes.
  # withr::defer() ties the cleanup lifetime to `env` (the test frame), so it
  # runs automatically whether the test passes, fails, or errors.
  withr::defer(unlink(test_db_path, force = TRUE), envir = env)

  # 5. Return the path so tests can reference it directly if needed.
  invisible(test_db_path)
}
