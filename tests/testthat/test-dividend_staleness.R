test_that("is_dividend_stale returns FALSE for NULL input", {
  result <- is_dividend_stale(NULL)
  expect_false(result$is_stale)
  expect_true(is.na(result$last_date))
  expect_true(is.na(result$days_since))
  expect_true(is.na(result$threshold))
})

test_that("is_dividend_stale returns FALSE for single-record input", {
  single <- xts::xts(0.50, order.by = Sys.Date() - 90)
  result <- is_dividend_stale(single)
  expect_false(result$is_stale)
  expect_true(is.na(result$last_date))
})

test_that("is_dividend_stale returns FALSE for active quarterly payer", {
  # 8 quarterly payments ending ~today (not stale)
  dates <- seq(Sys.Date() - 7 * 91, Sys.Date() - 30, by = 91)
  divs <- xts::xts(rep(0.50, length(dates)), order.by = dates)
  result <- is_dividend_stale(divs)
  expect_false(result$is_stale)
  expect_false(is.na(result$last_date))
  expect_false(is.na(result$threshold))
})

test_that("is_dividend_stale returns TRUE for suspended quarterly payer", {
  # 8 quarterly payments, last one was ~500 days ago (well beyond 1.5x 91 = 136 days)
  dates <- seq(Sys.Date() - 7 * 91 - 500, Sys.Date() - 500, by = 91)
  divs <- xts::xts(rep(0.50, length(dates)), order.by = dates)
  result <- is_dividend_stale(divs)
  expect_true(result$is_stale)
  expect_true(result$days_since > result$threshold)
})

test_that("is_dividend_stale returns FALSE for annual payer mid-cycle", {
  # 4 annual payments, last one was ~180 days ago (not stale: threshold = 365 * 1.5 = 548)
  dates <- seq(Sys.Date() - 3 * 365 - 180, Sys.Date() - 180, by = 365)
  divs <- xts::xts(rep(1.00, length(dates)), order.by = dates)
  result <- is_dividend_stale(divs)
  expect_false(result$is_stale)
  expect_equal(result$last_date, max(dates))
})

test_that("is_dividend_stale threshold is 1.5x average interval", {
  # Two payments exactly 100 days apart, last payment 140 days ago
  # threshold = 100 * 1.5 = 150; days_since = 140 => not stale
  dates <- c(Sys.Date() - 240, Sys.Date() - 140)
  divs <- xts::xts(c(0.50, 0.50), order.by = dates)
  result <- is_dividend_stale(divs)
  expect_equal(result$threshold, round(100 * 1.5))
  expect_false(result$is_stale)

  # Shift last payment to 160 days ago => stale
  dates2 <- c(Sys.Date() - 260, Sys.Date() - 160)
  divs2 <- xts::xts(c(0.50, 0.50), order.by = dates2)
  result2 <- is_dividend_stale(divs2)
  expect_true(result2$is_stale)
})
