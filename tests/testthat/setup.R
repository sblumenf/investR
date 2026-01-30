# Test Setup: Database Isolation
#
# This file runs before all tests and sets up a separate test database
# to prevent test data from polluting the production database.
#
# The test database is created in a temporary location and is automatically
# cleaned up after all tests complete.

library(duckdb)
library(DBI)
library(logger)

# Create temporary test database path
test_db_path <- tempfile(pattern = "investR_test_", fileext = ".duckdb")

# Store original database path getter (if it exists)
if (exists("get_portfolio_db_path", mode = "function")) {
  .original_get_portfolio_db_path <- get_portfolio_db_path
}

# Override get_portfolio_db_path to return test database
# Assign to global environment so it's available to all sourced functions
assign("get_portfolio_db_path", function() {
  return(test_db_path)
}, envir = .GlobalEnv)

# Override get_portfolio_db_connection to use test database
# Assign to global environment so it's available to all sourced functions
assign("get_portfolio_db_connection", function() {
  conn <- dbConnect(duckdb::duckdb(), dbdir = test_db_path, read_only = FALSE)
  return(conn)
}, envir = .GlobalEnv)

# Initialize test database schema
# The schema will be initialized automatically when tests call the database functions
# We just need to make sure the test database path is set

# Message for debugging (only shows in verbose test mode)
message(sprintf("Test database configured at: %s", test_db_path))
message("Database schema will be initialized on first use")
