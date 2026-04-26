# tests/testthat/test-fct_questrade_api.R
# Tests for get_questrade_auth() file-lock behavior

library(testthat)
library(withr)

# ---------------------------------------------------------------------------
# Helper: build a valid-looking token list with a future expiry
# ---------------------------------------------------------------------------
make_fresh_token <- function(expires_in_secs = 1800) {
  list(
    access_token  = "fresh_access_token_abc",
    refresh_token = "fresh_refresh_token_xyz",
    api_server    = "https://api01.iq.questrade.com/",
    expires_at    = Sys.time() + expires_in_secs
  )
}

# ---------------------------------------------------------------------------
# Test 1: Override bypass path
#   When override_refresh_token is supplied, get_questrade_auth() must skip
#   the file-lock entirely and call .do_token_exchange() directly.
# ---------------------------------------------------------------------------
test_that("override_refresh_token bypasses file lock and calls .do_token_exchange", {
  expected <- list(
    access_token = "override_access",
    api_server   = "https://api99.iq.questrade.com/"
  )

  exchange_called_with <- NULL

  with_mocked_bindings(
    # No cached token — ensures we don't return early from the cache fast-path
    read_token_file = function() NULL,
    # Record what .do_token_exchange was called with and return a fake result
    .do_token_exchange = function(refresh_token) {
      exchange_called_with <<- refresh_token
      expected
    },
    # filelock::lock must NOT be called; if it is, throw so the test fails
    {
      with_mocked_bindings(
        lock = function(...) stop("filelock::lock must not be called when override token is supplied"),
        .package = "filelock",
        {
          result <- get_questrade_auth(override_refresh_token = "my_override_token")
        }
      )
    }
  )

  expect_equal(exchange_called_with, "my_override_token")
  expect_equal(result$access_token, "override_access")
  expect_equal(result$api_server, "https://api99.iq.questrade.com/")
})

# ---------------------------------------------------------------------------
# Test 2: Lock timeout path
#   When filelock::lock() returns NULL (timeout), get_questrade_auth()
#   must return NULL without attempting a token exchange.
# ---------------------------------------------------------------------------
test_that("lock timeout returns NULL without attempting token exchange", {
  exchange_called <- FALSE

  with_mocked_bindings(
    read_token_file      = function() NULL,
    get_token_file_path  = function() withr::local_tempdir(),
    .do_token_exchange   = function(...) { exchange_called <<- TRUE; list() },
    {
      with_mocked_bindings(
        lock   = function(...) NULL,   # simulate 30-second timeout
        unlock = function(...) invisible(NULL),
        .package = "filelock",
        {
          result <- get_questrade_auth()
        }
      )
    }
  )

  expect_null(result)
  expect_false(exchange_called)
})

# ---------------------------------------------------------------------------
# Test 3: Re-read inside lock path
#   After acquiring the lock, if read_token_file() now returns a fresh token
#   (another worker refreshed while we waited), get_questrade_auth() must
#   return that token without calling .do_token_exchange().
# ---------------------------------------------------------------------------
test_that("re-read inside lock returns fresh token without calling .do_token_exchange", {
  fresh <- make_fresh_token(expires_in_secs = 600)

  # Simulate two reads: first returns NULL (expired fast-path), second (inside
  # the lock) returns the fresh token written by a concurrent worker.
  read_call_count  <- 0L
  exchange_called  <- FALSE

  fake_lock_obj <- structure(list(), class = "filelock_lock")

  with_mocked_bindings(
    read_token_file = function() {
      read_call_count <<- read_call_count + 1L
      if (read_call_count == 1L) NULL else fresh
    },
    get_token_file_path = function() withr::local_tempdir(),
    .do_token_exchange  = function(...) { exchange_called <<- TRUE; list() },
    {
      with_mocked_bindings(
        lock   = function(...) fake_lock_obj,
        unlock = function(...) invisible(NULL),
        .package = "filelock",
        {
          result <- get_questrade_auth()
        }
      )
    }
  )

  expect_false(exchange_called)
  expect_equal(result$access_token, "fresh_access_token_abc")
  expect_equal(result$api_server,   "https://api01.iq.questrade.com/")
  expect_equal(read_call_count, 2L)
})
