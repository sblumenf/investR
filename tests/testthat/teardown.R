# Test Teardown: Cleanup
#
# This file runs after all tests and cleans up the test database.

# Clean up temporary test database if it exists
if (exists("test_db_path") && file.exists(test_db_path)) {
  tryCatch({
    # Ensure all connections are closed first
    gc()  # Force garbage collection to close any lingering connections

    # Remove the test database file
    unlink(test_db_path)
    message(sprintf("Test database cleaned up: %s", test_db_path))
  }, error = function(e) {
    warning(sprintf("Could not remove test database: %s", e$message))
  })
}

# Restore original function if it existed
if (exists(".original_get_portfolio_db_path", mode = "function")) {
  get_portfolio_db_path <- .original_get_portfolio_db_path
  rm(.original_get_portfolio_db_path)
}
