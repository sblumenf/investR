library(quantmod)
library(dplyr)
library(lubridate)

# Configuration
investment <- 60000
start_date <- as.Date("2025-03-06")
end_date <- as.Date("2025-12-02")
output_dir <- "/home/sergeblumenfeld/investR/money_market_analysis"

# Fetch all data
fetch_data <- function(ticker) {
  data <- getSymbols(ticker, src = "yahoo", from = start_date - 10, to = end_date + 10, auto.assign = FALSE)

  df <- data.frame(
    date = index(data),
    open = as.numeric(Op(data)),
    close = as.numeric(Cl(data))
  )

  dividends <- tryCatch({
    getDividends(ticker, from = start_date - 10, to = end_date + 10, auto.assign = FALSE)
  }, error = function(e) NULL)

  if (!is.null(dividends) && length(dividends) > 0) {
    div_df <- data.frame(date = index(dividends), dividend = as.numeric(dividends))
    df <- df %>% left_join(div_df, by = "date") %>% mutate(dividend = ifelse(is.na(dividend), 0, dividend))
  } else {
    df$dividend <- 0
  }

  df %>% arrange(date)
}

message("Fetching data for WEEK, MMKT, SGOV, TBLL, and USFR...")
week_data <- fetch_data("WEEK")
mmkt_data <- fetch_data("MMKT")
sgov_data <- fetch_data("SGOV")
tbll_data <- fetch_data("TBLL")
usfr_data <- fetch_data("USFR")

# Helper functions
get_price <- function(data, date, type = "close") {
  row <- data[data$date == date, ]
  if (nrow(row) == 0) return(NA)
  if (type == "open") row$open else row$close
}

get_prev_trading_day <- function(data, date) {
  dates <- data$date[data$date < date]
  if (length(dates) == 0) return(NA)
  max(dates)
}

get_next_trading_day <- function(data, date) {
  dates <- data$date[data$date > date]
  if (length(dates) == 0) return(NA)
  min(dates)
}

# Identify ex-dividend dates for weekly ETFs (day before dividend payment)
get_weekly_exdiv_dates <- function(data, ticker_name) {
  div_dates <- data$date[data$dividend > 0]
  exdiv <- sapply(div_dates, function(d) {
    idx <- which(data$date == d)
    if (idx > 1) data$date[idx - 1] else NA
  })
  exdiv <- as.Date(exdiv[!is.na(exdiv)], origin = "1970-01-01")
  exdiv <- exdiv[exdiv >= start_date & exdiv <= end_date]
  sort(exdiv)
}

week_exdiv <- get_weekly_exdiv_dates(week_data, "WEEK")
mmkt_exdiv <- get_weekly_exdiv_dates(mmkt_data, "MMKT")

# Monthly ETFs: ex-div is typically last business day of month for SGOV,
# but TBLL and USFR have different schedules
# We'll detect from actual dividend payment dates

# SGOV: last business day of month
is_last_bday <- function(d, trading_days) {
  month_start <- floor_date(d, "month")
  next_month <- month_start %m+% months(1)
  month_days <- trading_days[trading_days >= month_start & trading_days < next_month]
  d == max(month_days)
}

sgov_exdiv <- sgov_data$date[sapply(sgov_data$date, function(d) is_last_bday(d, sgov_data$date))]
sgov_exdiv <- sgov_exdiv[sgov_exdiv >= start_date & sgov_exdiv <= end_date]
sgov_exdiv <- sort(sgov_exdiv)

# TBLL: has a different ex-div schedule - detect from dividend payments
# Ex-div is typically the day before the dividend payment date
get_monthly_exdiv_from_payments <- function(data, ticker_name) {
  div_dates <- data$date[data$dividend > 0]
  exdiv <- sapply(div_dates, function(d) {
    idx <- which(data$date == d)
    if (idx > 1) data$date[idx - 1] else NA
  })
  exdiv <- as.Date(exdiv[!is.na(exdiv)], origin = "1970-01-01")
  exdiv <- exdiv[exdiv >= start_date & exdiv <= end_date]
  sort(exdiv)
}

tbll_exdiv <- get_monthly_exdiv_from_payments(tbll_data, "TBLL")
usfr_exdiv <- get_monthly_exdiv_from_payments(usfr_data, "USFR")

message("WEEK ex-dividend dates: ", paste(head(week_exdiv, 5), collapse = ", "), "...")
message("MMKT ex-dividend dates: ", paste(head(mmkt_exdiv, 5), collapse = ", "), "...")
message("SGOV ex-dividend dates: ", paste(sgov_exdiv, collapse = ", "))
message("TBLL ex-dividend dates: ", paste(tbll_exdiv, collapse = ", "))
message("USFR ex-dividend dates: ", paste(usfr_exdiv, collapse = ", "))

# Combine all monthly ex-div dates
all_monthly_exdiv <- sort(unique(c(sgov_exdiv, tbll_exdiv, usfr_exdiv)))

# Find overlapping dates (weekly with any monthly)
overlap_week_monthly <- week_exdiv[week_exdiv %in% all_monthly_exdiv]
overlap_mmkt_monthly <- mmkt_exdiv[mmkt_exdiv %in% all_monthly_exdiv]

message("All monthly ex-div dates: ", paste(all_monthly_exdiv, collapse = ", "))
message("Overlap dates (WEEK & any monthly): ", ifelse(length(overlap_week_monthly) > 0, paste(overlap_week_monthly, collapse = ", "), "None"))
message("Overlap dates (MMKT & any monthly): ", ifelse(length(overlap_mmkt_monthly) > 0, paste(overlap_mmkt_monthly, collapse = ", "), "None"))

# Check for monthly-monthly overlaps
overlap_tbll_usfr <- tbll_exdiv[tbll_exdiv %in% usfr_exdiv]
overlap_sgov_tbll <- sgov_exdiv[sgov_exdiv %in% tbll_exdiv]
overlap_sgov_usfr <- sgov_exdiv[sgov_exdiv %in% usfr_exdiv]
message("Overlap dates (TBLL & USFR): ", ifelse(length(overlap_tbll_usfr) > 0, paste(overlap_tbll_usfr, collapse = ", "), "None"))
message("Overlap dates (SGOV & TBLL): ", ifelse(length(overlap_sgov_tbll) > 0, paste(overlap_sgov_tbll, collapse = ", "), "None"))
message("Overlap dates (SGOV & USFR): ", ifelse(length(overlap_sgov_usfr) > 0, paste(overlap_sgov_usfr, collapse = ", "), "None"))

# Run five-ticker rotation strategy
run_five_ticker_strategy <- function() {

  transactions <- data.frame(
    date = character(),
    action = character(),
    ticker = character(),
    price = numeric(),
    shares = numeric(),
    value = numeric(),
    stringsAsFactors = FALSE
  )

  dividends_log <- data.frame(
    date = character(),
    ticker = character(),
    div_per_share = numeric(),
    shares = numeric(),
    amount = numeric(),
    exdiv_date = character(),  # Track which ex-div date this dividend is for
    stringsAsFactors = FALSE
  )

  # Track which ex-div dates we've already recorded dividends for (to prevent double-counting)
  recorded_exdiv_dates <- c()

  # Helper to get data for a ticker
  get_ticker_data <- function(ticker) {
    switch(ticker,
           "WEEK" = week_data,
           "MMKT" = mmkt_data,
           "SGOV" = sgov_data,
           "TBLL" = tbll_data,
           "USFR" = usfr_data)
  }

  # Helper to get exdiv dates for a ticker
  get_ticker_exdiv <- function(ticker) {
    switch(ticker,
           "WEEK" = week_exdiv,
           "MMKT" = mmkt_exdiv,
           "SGOV" = sgov_exdiv,
           "TBLL" = tbll_exdiv,
           "USFR" = usfr_exdiv)
  }

  # Check if ticker is monthly
  is_monthly_ticker <- function(ticker) {
    ticker %in% c("SGOV", "TBLL", "USFR")
  }

  # Find which monthly ticker has ex-div on a given date
  # Priority: SGOV > TBLL > USFR (for overlapping dates like 2025-11-21 where TBLL+USFR overlap)
  get_monthly_ticker_for_date <- function(date) {
    monthly_on_date <- c()
    if (date %in% sgov_exdiv) monthly_on_date <- c(monthly_on_date, "SGOV")
    if (date %in% tbll_exdiv) monthly_on_date <- c(monthly_on_date, "TBLL")
    if (date %in% usfr_exdiv) monthly_on_date <- c(monthly_on_date, "USFR")

    if (length(monthly_on_date) == 0) return(NA)

    # Priority: SGOV > TBLL > USFR
    if ("SGOV" %in% monthly_on_date) return("SGOV")
    if ("TBLL" %in% monthly_on_date) return("TBLL")
    return("USFR")
  }

  # Check which monthly ETFs have ex-div on a given date (for overlap detection)
  get_all_monthly_tickers_for_date <- function(date) {
    tickers <- c()
    if (date %in% sgov_exdiv) tickers <- c(tickers, "SGOV")
    if (date %in% tbll_exdiv) tickers <- c(tickers, "TBLL")
    if (date %in% usfr_exdiv) tickers <- c(tickers, "USFR")
    return(tickers)
  }

  # Find next ex-div date for weekly ETFs (WEEK or MMKT)
  find_next_weekly_exdiv <- function(after_date) {
    week_next <- week_exdiv[week_exdiv > after_date]
    mmkt_next <- mmkt_exdiv[mmkt_exdiv > after_date]

    week_next <- if (length(week_next) > 0) min(week_next) else as.Date(NA)
    mmkt_next <- if (length(mmkt_next) > 0) min(mmkt_next) else as.Date(NA)

    if (is.na(week_next) && is.na(mmkt_next)) {
      return(list(date = NA, ticker = NA))
    } else if (is.na(week_next)) {
      return(list(date = mmkt_next, ticker = "MMKT"))
    } else if (is.na(mmkt_next)) {
      return(list(date = week_next, ticker = "WEEK"))
    } else if (week_next <= mmkt_next) {
      return(list(date = week_next, ticker = "WEEK"))
    } else {
      return(list(date = mmkt_next, ticker = "MMKT"))
    }
  }

  # Find next monthly ex-div date
  find_next_monthly_exdiv <- function(after_date) {
    next_monthly <- all_monthly_exdiv[all_monthly_exdiv > after_date]
    if (length(next_monthly) == 0) {
      return(list(date = NA, ticker = NA))
    }
    next_date <- min(next_monthly)
    next_ticker <- get_monthly_ticker_for_date(next_date)
    return(list(date = next_date, ticker = next_ticker))
  }

  # Determine starting weekly ETF (whichever has first ex-div)
  first_weekly <- find_next_weekly_exdiv(start_date - 1)
  current_ticker <- first_weekly$ticker

  # Initial purchase
  initial_data <- get_ticker_data(current_ticker)
  open_price <- get_price(initial_data, start_date, "open")
  shares <- investment / open_price
  current_shares <- shares
  total_dividends <- 0

  transactions <- rbind(transactions, data.frame(
    date = as.character(start_date),
    action = "BUY",
    ticker = current_ticker,
    price = open_price,
    shares = shares,
    value = investment
  ))

  # Process each day
  all_dates <- sort(unique(c(week_data$date, mmkt_data$date, sgov_data$date, tbll_data$date, usfr_data$date)))
  all_dates <- all_dates[all_dates >= start_date & all_dates <= end_date]

  for (current_date in all_dates) {
    current_date <- as.Date(current_date, origin = "1970-01-01")
    current_data <- get_ticker_data(current_ticker)
    current_exdiv <- get_ticker_exdiv(current_ticker)

    # Check if we're holding on an EX-DIV date (entitled to dividend)
    # CRITICAL: Only record dividend if:
    # 1. We haven't already recorded a dividend for this ex-div date
    # 2. For monthly ETFs, we're holding the priority ticker if there's an overlap (SGOV > TBLL > USFR)
    if (current_date %in% current_exdiv) {
      exdiv_key <- paste0(current_date, "_", current_ticker)

      # Check for monthly-monthly overlaps (e.g., 2025-11-21 has both TBLL and USFR)
      # Only record dividend if we're holding the priority ticker
      should_record <- TRUE
      if (is_monthly_ticker(current_ticker)) {
        all_monthly_on_date <- get_all_monthly_tickers_for_date(current_date)
        if (length(all_monthly_on_date) > 1) {
          # There's an overlap - only record if we're holding the priority ticker
          priority_ticker <- get_monthly_ticker_for_date(current_date)
          if (current_ticker != priority_ticker) {
            should_record <- FALSE
            message(sprintf("  Skipping %s dividend on %s - overlap with %s (priority)",
                          current_ticker, current_date, priority_ticker))
          }
        }
      }

      # Also check we haven't already recorded for this ex-div date
      if (as.character(current_date) %in% recorded_exdiv_dates) {
        should_record <- FALSE
      }

      if (should_record) {
        if (current_ticker %in% c("WEEK", "MMKT")) {
          # Weekly: dividend pays day after ex-div
          next_day <- get_next_trading_day(current_data, current_date)
          if (!is.na(next_day)) {
            div <- current_data$dividend[current_data$date == next_day]
            if (length(div) > 0 && div > 0) {
              div_amount <- current_shares * div
              total_dividends <- total_dividends + div_amount
              dividends_log <- rbind(dividends_log, data.frame(
                date = as.character(next_day),
                ticker = current_ticker,
                div_per_share = div,
                shares = current_shares,
                amount = div_amount,
                exdiv_date = as.character(current_date)
              ))
              recorded_exdiv_dates <- c(recorded_exdiv_dates, as.character(current_date))
            }
          }
        } else if (is_monthly_ticker(current_ticker)) {
          # Monthly: dividend pays around 1st of next month
          # Find the specific dividend payment for THIS ex-div date
          next_day <- get_next_trading_day(current_data, current_date)
          if (!is.na(next_day)) {
            # Check if dividend pays the day after (like weekly ETFs sometimes do)
            div <- current_data$dividend[current_data$date == next_day]
            if (length(div) > 0 && div > 0) {
              div_amount <- current_shares * div
              total_dividends <- total_dividends + div_amount
              dividends_log <- rbind(dividends_log, data.frame(
                date = as.character(next_day),
                ticker = current_ticker,
                div_per_share = div,
                shares = current_shares,
                amount = div_amount,
                exdiv_date = as.character(current_date)
              ))
              recorded_exdiv_dates <- c(recorded_exdiv_dates, as.character(current_date))
            } else {
              # Look for dividend in the next month (typical for SGOV)
              next_month <- floor_date(current_date, "month") %m+% months(1)
              payment_dates <- current_data$date[current_data$dividend > 0 & current_data$date >= next_month]
              if (length(payment_dates) > 0) {
                payment_date <- min(payment_dates)
                div <- current_data$dividend[current_data$date == payment_date]
                if (length(div) > 0 && div > 0) {
                  div_amount <- current_shares * div
                  total_dividends <- total_dividends + div_amount
                  dividends_log <- rbind(dividends_log, data.frame(
                    date = as.character(payment_date),
                    ticker = current_ticker,
                    div_per_share = div,
                    shares = current_shares,
                    amount = div_amount,
                    exdiv_date = as.character(current_date)
                  ))
                  recorded_exdiv_dates <- c(recorded_exdiv_dates, as.character(current_date))
                }
              }
            }
          }
        }
      }
    }

    # SWITCH FROM WEEKLY TO MONTHLY: day before any monthly ex-div date
    if (current_ticker %in% c("WEEK", "MMKT")) {
      for (mexdiv in all_monthly_exdiv) {
        monthly_ticker <- get_monthly_ticker_for_date(mexdiv)
        monthly_data <- get_ticker_data(monthly_ticker)
        prev_day <- get_prev_trading_day(monthly_data, mexdiv)
        if (!is.na(prev_day) && current_date == prev_day) {
          # Sell current weekly at close
          weekly_close <- get_price(current_data, current_date, "close")
          sale_value <- current_shares * weekly_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "SELL",
            ticker = current_ticker,
            price = weekly_close,
            shares = current_shares,
            value = sale_value
          ))

          # Buy monthly ETF at close
          monthly_close <- get_price(monthly_data, current_date, "close")
          new_shares <- sale_value / monthly_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "BUY",
            ticker = monthly_ticker,
            price = monthly_close,
            shares = new_shares,
            value = sale_value
          ))

          current_ticker <- monthly_ticker
          current_shares <- new_shares
          break
        }
      }
    }

    # SWITCH FROM MONTHLY TO WEEKLY: day before next weekly ex-div (that is NOT also a monthly ex-div)
    if (is_monthly_ticker(current_ticker)) {
      # Find next weekly ex-div (WEEK or MMKT) that is NOT overlapping with any monthly
      week_next <- week_exdiv[week_exdiv > current_date & !(week_exdiv %in% all_monthly_exdiv)]
      mmkt_next <- mmkt_exdiv[mmkt_exdiv > current_date & !(mmkt_exdiv %in% all_monthly_exdiv)]

      week_next <- if (length(week_next) > 0) min(week_next) else as.Date(NA)
      mmkt_next <- if (length(mmkt_next) > 0) min(mmkt_next) else as.Date(NA)

      next_weekly_date <- NA
      next_weekly_ticker <- NA

      if (!is.na(week_next) && !is.na(mmkt_next)) {
        if (week_next <= mmkt_next) {
          next_weekly_date <- week_next
          next_weekly_ticker <- "WEEK"
        } else {
          next_weekly_date <- mmkt_next
          next_weekly_ticker <- "MMKT"
        }
      } else if (!is.na(week_next)) {
        next_weekly_date <- week_next
        next_weekly_ticker <- "WEEK"
      } else if (!is.na(mmkt_next)) {
        next_weekly_date <- mmkt_next
        next_weekly_ticker <- "MMKT"
      }

      # But also check if there's another monthly ex-div coming before the next weekly
      next_monthly <- find_next_monthly_exdiv(current_date)

      if (!is.na(next_weekly_date)) {
        target_data <- get_ticker_data(next_weekly_ticker)
        prev_day <- get_prev_trading_day(target_data, next_weekly_date)

        # Only switch to weekly if:
        # 1. We're on the day before the weekly ex-div
        # 2. There's no monthly ex-div between now and the weekly ex-div (or monthly is after weekly)
        should_switch_to_weekly <- !is.na(prev_day) && current_date == prev_day

        if (should_switch_to_weekly && !is.na(next_monthly$date)) {
          # Check if there's a monthly ex-div coming before the next weekly
          if (next_monthly$date < next_weekly_date) {
            # There's a monthly coming first - switch to that monthly instead
            monthly_data <- get_ticker_data(next_monthly$ticker)
            monthly_prev_day <- get_prev_trading_day(monthly_data, next_monthly$date)
            if (!is.na(monthly_prev_day) && current_date == monthly_prev_day) {
              # Sell current monthly at close
              current_close <- get_price(current_data, current_date, "close")
              sale_value <- current_shares * current_close

              transactions <- rbind(transactions, data.frame(
                date = as.character(current_date),
                action = "SELL",
                ticker = current_ticker,
                price = current_close,
                shares = current_shares,
                value = sale_value
              ))

              # Buy next monthly ETF at close
              next_monthly_close <- get_price(monthly_data, current_date, "close")
              new_shares <- sale_value / next_monthly_close

              transactions <- rbind(transactions, data.frame(
                date = as.character(current_date),
                action = "BUY",
                ticker = next_monthly$ticker,
                price = next_monthly_close,
                shares = new_shares,
                value = sale_value
              ))

              current_ticker <- next_monthly$ticker
              current_shares <- new_shares
              should_switch_to_weekly <- FALSE
            }
          }
        }

        if (should_switch_to_weekly) {
          # Sell current monthly at close
          monthly_close <- get_price(current_data, current_date, "close")
          sale_value <- current_shares * monthly_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "SELL",
            ticker = current_ticker,
            price = monthly_close,
            shares = current_shares,
            value = sale_value
          ))

          # Buy next weekly ETF at close
          weekly_close <- get_price(target_data, current_date, "close")
          new_shares <- sale_value / weekly_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "BUY",
            ticker = next_weekly_ticker,
            price = weekly_close,
            shares = new_shares,
            value = sale_value
          ))

          current_ticker <- next_weekly_ticker
          current_shares <- new_shares
        }
      }
    }

    # SWITCH BETWEEN WEEKLY ETFs: when holding one and other has next ex-div
    if (current_ticker %in% c("WEEK", "MMKT")) {
      other_ticker <- if (current_ticker == "WEEK") "MMKT" else "WEEK"
      other_exdiv <- get_ticker_exdiv(other_ticker)
      other_data <- get_ticker_data(other_ticker)

      # Find next ex-div for both
      current_next <- current_exdiv[current_exdiv > current_date]
      other_next <- other_exdiv[other_exdiv > current_date]

      current_next <- if (length(current_next) > 0) min(current_next) else as.Date(NA)
      other_next <- if (length(other_next) > 0) min(other_next) else as.Date(NA)

      # Check if we need to switch to the other weekly ETF
      # But also check we're not about to switch to a monthly
      if (!is.na(other_next)) {
        prev_day <- get_prev_trading_day(other_data, other_next)

        # Only switch if:
        # 1. Other weekly ex-div is before current weekly ex-div (or current is NA)
        # 2. Other weekly ex-div is not a monthly overlap
        # 3. We're on the day before the other weekly ex-div
        # 4. We're not about to switch to a monthly (no monthly ex-div between now and other_next)
        should_switch <- !is.na(prev_day) && current_date == prev_day &&
                         !(other_next %in% all_monthly_exdiv) &&
                         (is.na(current_next) || other_next < current_next)

        # Check if there's a monthly coming before other_next
        if (should_switch) {
          next_monthly <- find_next_monthly_exdiv(current_date)
          if (!is.na(next_monthly$date) && next_monthly$date < other_next) {
            should_switch <- FALSE  # Monthly is coming first, don't switch to other weekly
          }
        }

        if (should_switch) {
          # Sell current weekly at close
          current_close <- get_price(current_data, current_date, "close")
          sale_value <- current_shares * current_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "SELL",
            ticker = current_ticker,
            price = current_close,
            shares = current_shares,
            value = sale_value
          ))

          # Buy other weekly at close
          other_close <- get_price(other_data, current_date, "close")
          new_shares <- sale_value / other_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "BUY",
            ticker = other_ticker,
            price = other_close,
            shares = new_shares,
            value = sale_value
          ))

          current_ticker <- other_ticker
          current_shares <- new_shares
        }
      }
    }

    # SWITCH BETWEEN MONTHLY ETFs: if one monthly is followed by another
    if (is_monthly_ticker(current_ticker)) {
      next_monthly <- find_next_monthly_exdiv(current_date)
      if (!is.na(next_monthly$date) && next_monthly$ticker != current_ticker) {
        monthly_data <- get_ticker_data(next_monthly$ticker)
        prev_day <- get_prev_trading_day(monthly_data, next_monthly$date)

        # Check if there's a weekly ex-div between now and the next monthly
        next_weekly <- find_next_weekly_exdiv(current_date)

        should_switch_monthly <- !is.na(prev_day) && current_date == prev_day

        # If there's a weekly ex-div coming before this monthly, we should switch to weekly first
        if (should_switch_monthly && !is.na(next_weekly$date)) {
          if (next_weekly$date < next_monthly$date && !(next_weekly$date %in% all_monthly_exdiv)) {
            should_switch_monthly <- FALSE  # Weekly comes first and isn't an overlap
          }
        }

        if (should_switch_monthly) {
          # Sell current monthly at close
          current_close <- get_price(current_data, current_date, "close")
          sale_value <- current_shares * current_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "SELL",
            ticker = current_ticker,
            price = current_close,
            shares = current_shares,
            value = sale_value
          ))

          # Buy next monthly ETF at close
          next_monthly_close <- get_price(monthly_data, current_date, "close")
          new_shares <- sale_value / next_monthly_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "BUY",
            ticker = next_monthly$ticker,
            price = next_monthly_close,
            shares = new_shares,
            value = sale_value
          ))

          current_ticker <- next_monthly$ticker
          current_shares <- new_shares
        }
      }
    }
  }

  # Sort dividends by date
  if (nrow(dividends_log) > 0) {
    dividends_log <- dividends_log %>% arrange(as.Date(date))
  }

  # Final valuation
  current_data <- get_ticker_data(current_ticker)
  final_price <- get_price(current_data, end_date, "close")
  final_stock_value <- current_shares * final_price
  final_total <- final_stock_value + total_dividends

  list(
    transactions = transactions,
    dividends = dividends_log,
    final_ticker = current_ticker,
    final_shares = current_shares,
    final_price = final_price,
    final_stock_value = final_stock_value,
    total_dividends = total_dividends,
    final_total = final_total
  )
}

message("\nRunning WEEK/MMKT/SGOV/TBLL/USFR five-ticker strategy...")
result <- run_five_ticker_strategy()

# Generate markdown file
generate_markdown <- function(result, filename) {

  md <- "# WEEK/MMKT/SGOV/TBLL/USFR Five-Ticker Dividend Capture Rotation Strategy\n\n"
  md <- paste0(md, "**Period:** March 6, 2025 to December 2, 2025\n")
  md <- paste0(md, "**Initial Investment:** $60,000\n\n")

  md <- paste0(md, "## Strategy Overview\n\n")
  md <- paste0(md, "This strategy rotates between five money market ETFs to maximize dividend capture:\n\n")
  md <- paste0(md, "- **Weekly ETFs:** WEEK and MMKT (both pay weekly dividends on different ex-div dates)\n")
  md <- paste0(md, "- **Monthly ETFs:** SGOV, TBLL, and USFR (all pay monthly dividends on different ex-div dates)\n\n")
  md <- paste0(md, "### Rotation Logic\n\n")
  md <- paste0(md, "1. **Base Position:** Alternates between WEEK and MMKT to capture BOTH weekly dividend streams\n")
  md <- paste0(md, "2. **Monthly Switch:** Before ANY monthly ex-div date (SGOV, TBLL, or USFR), swap to that monthly ETF\n")
  md <- paste0(md, "3. **Return to Weekly:** After capturing the monthly dividend, swap back to whichever weekly ETF (WEEK or MMKT) has the next ex-div date\n")
  md <- paste0(md, "4. **Tiebreaker (CRITICAL):** If a monthly ex-div date coincides with a weekly ex-div date, hold the monthly ETF. The weekly dividend is FORFEITED - no double counting.\n\n")
  md <- paste0(md, "**Key Fact:** The three monthly ETFs (SGOV, TBLL, USFR) do NOT overlap with each other. The only possible overlaps are between weekly (WEEK/MMKT) and monthly (SGOV/TBLL/USFR).\n\n")

  md <- paste0(md, "### Ex-Dividend Dates\n\n")
  md <- paste0(md, "**Weekly ETFs:**\n")
  md <- paste0(md, "- **WEEK ex-div dates:** ", paste(week_exdiv, collapse = ", "), "\n")
  md <- paste0(md, "- **MMKT ex-div dates:** ", paste(mmkt_exdiv, collapse = ", "), "\n\n")
  md <- paste0(md, "**Monthly ETFs:**\n")
  md <- paste0(md, "- **SGOV ex-div dates:** ", paste(sgov_exdiv, collapse = ", "), "\n")
  md <- paste0(md, "- **TBLL ex-div dates:** ", paste(tbll_exdiv, collapse = ", "), "\n")
  md <- paste0(md, "- **USFR ex-div dates:** ", paste(usfr_exdiv, collapse = ", "), "\n\n")
  md <- paste0(md, "**Overlap Dates (Weekly vs Monthly - weekly dividend forfeited):**\n")
  md <- paste0(md, "- **WEEK & any monthly:** ", ifelse(length(overlap_week_monthly) > 0, paste(overlap_week_monthly, collapse = ", "), "None"), "\n")
  md <- paste0(md, "- **MMKT & any monthly:** ", ifelse(length(overlap_mmkt_monthly) > 0, paste(overlap_mmkt_monthly, collapse = ", "), "None"), "\n\n")
  md <- paste0(md, "**Overlap Dates (Monthly vs Monthly - priority: SGOV > TBLL > USFR):**\n")
  md <- paste0(md, "- **TBLL & USFR:** ", ifelse(length(overlap_tbll_usfr) > 0, paste(overlap_tbll_usfr, collapse = ", "), "None"), "\n")
  md <- paste0(md, "- **SGOV & TBLL:** ", ifelse(length(overlap_sgov_tbll) > 0, paste(overlap_sgov_tbll, collapse = ", "), "None"), "\n")
  md <- paste0(md, "- **SGOV & USFR:** ", ifelse(length(overlap_sgov_usfr) > 0, paste(overlap_sgov_usfr, collapse = ", "), "None"), "\n\n")

  md <- paste0(md, "## Transaction Log\n\n")
  md <- paste0(md, "| Date | Action | Ticker | Price | Shares | Value |\n")
  md <- paste0(md, "|------|--------|--------|-------|--------|-------|\n")

  for (i in 1:nrow(result$transactions)) {
    row <- result$transactions[i, ]
    md <- paste0(md, sprintf("| %s | %s | %s | $%.4f | %.4f | $%.2f |\n",
                             row$date, row$action, row$ticker, row$price, row$shares, row$value))
  }

  md <- paste0(md, "\n## Dividends Received\n\n")
  md <- paste0(md, "| Date | Ticker | Div/Share | Shares | Amount | Ex-Div Date |\n")
  md <- paste0(md, "|------|--------|-----------|--------|--------|-------------|\n")

  for (i in 1:nrow(result$dividends)) {
    row <- result$dividends[i, ]
    md <- paste0(md, sprintf("| %s | %s | $%.6f | %.4f | $%.2f | %s |\n",
                             row$date, row$ticker, row$div_per_share, row$shares, row$amount,
                             ifelse("exdiv_date" %in% names(row), row$exdiv_date, "N/A")))
  }

  # Dividend summary by ticker
  week_divs <- result$dividends[result$dividends$ticker == "WEEK", ]
  mmkt_divs <- result$dividends[result$dividends$ticker == "MMKT", ]
  sgov_divs <- result$dividends[result$dividends$ticker == "SGOV", ]
  tbll_divs <- result$dividends[result$dividends$ticker == "TBLL", ]
  usfr_divs <- result$dividends[result$dividends$ticker == "USFR", ]

  md <- paste0(md, "\n## Dividend Summary\n\n")
  md <- paste0(md, "| Source | Count | Total |\n")
  md <- paste0(md, "|--------|-------|-------|\n")
  md <- paste0(md, sprintf("| WEEK | %d | $%.2f |\n", nrow(week_divs), sum(week_divs$amount)))
  md <- paste0(md, sprintf("| MMKT | %d | $%.2f |\n", nrow(mmkt_divs), sum(mmkt_divs$amount)))
  md <- paste0(md, sprintf("| SGOV | %d | $%.2f |\n", nrow(sgov_divs), sum(sgov_divs$amount)))
  md <- paste0(md, sprintf("| TBLL | %d | $%.2f |\n", nrow(tbll_divs), sum(tbll_divs$amount)))
  md <- paste0(md, sprintf("| USFR | %d | $%.2f |\n", nrow(usfr_divs), sum(usfr_divs$amount)))
  md <- paste0(md, sprintf("| **Total** | **%d** | **$%.2f** |\n", nrow(result$dividends), result$total_dividends))

  md <- paste0(md, "\n## Final Valuation (December 2, 2025 Close)\n\n")
  md <- paste0(md, "| Metric | Value |\n")
  md <- paste0(md, "|--------|-------|\n")
  md <- paste0(md, sprintf("| Final Position | %s |\n", result$final_ticker))
  md <- paste0(md, sprintf("| Shares Held | %.4f |\n", result$final_shares))
  md <- paste0(md, sprintf("| Share Price | $%.4f |\n", result$final_price))
  md <- paste0(md, sprintf("| Stock Value | $%.2f |\n", result$final_stock_value))
  md <- paste0(md, sprintf("| Total Dividends | $%.2f |\n", result$total_dividends))
  md <- paste0(md, sprintf("| **Total Value** | **$%.2f** |\n", result$final_total))
  md <- paste0(md, sprintf("| **Total Return** | **$%.2f** |\n", result$final_total - investment))
  md <- paste0(md, sprintf("| **Percentage Return** | **%.2f%%** |\n", (result$final_total - investment) / investment * 100))

  writeLines(md, file.path(output_dir, filename))
  message(sprintf("Created %s", filename))
}

generate_markdown(result, "WEEK_MMKT_SGOV_TBLL_USFR_rotation_strategy.md")

# Print summary
cat("\n=== FIVE-TICKER STRATEGY SUMMARY ===\n\n")
cat(sprintf("WEEK/MMKT/SGOV/TBLL/USFR Strategy:\n"))

week_divs <- result$dividends[result$dividends$ticker == "WEEK", ]
mmkt_divs <- result$dividends[result$dividends$ticker == "MMKT", ]
sgov_divs <- result$dividends[result$dividends$ticker == "SGOV", ]
tbll_divs <- result$dividends[result$dividends$ticker == "TBLL", ]
usfr_divs <- result$dividends[result$dividends$ticker == "USFR", ]

cat(sprintf("  WEEK dividends: %d payments, $%.2f\n", nrow(week_divs), sum(week_divs$amount)))
cat(sprintf("  MMKT dividends: %d payments, $%.2f\n", nrow(mmkt_divs), sum(mmkt_divs$amount)))
cat(sprintf("  SGOV dividends: %d payments, $%.2f\n", nrow(sgov_divs), sum(sgov_divs$amount)))
cat(sprintf("  TBLL dividends: %d payments, $%.2f\n", nrow(tbll_divs), sum(tbll_divs$amount)))
cat(sprintf("  USFR dividends: %d payments, $%.2f\n", nrow(usfr_divs), sum(usfr_divs$amount)))
cat(sprintf("  Total Dividends: $%.2f\n", result$total_dividends))
cat(sprintf("  Final Stock Value: $%.2f\n", result$final_stock_value))
cat(sprintf("  Total Value: $%.2f\n", result$final_total))
cat(sprintf("  Return: $%.2f (%.2f%%)\n", result$final_total - investment, (result$final_total - investment) / investment * 100))
