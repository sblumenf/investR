# Test Setup
#
# Runs once before all tests. Loads required libraries and reduces log noise.
# DB isolation is handled per-test via helper-test-db.R — nothing is mocked here.

library(RSQLite)
library(DBI)
library(logger)

log_threshold(WARN)
