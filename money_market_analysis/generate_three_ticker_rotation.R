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

message("Fetching data for WEEK, MMKT, and SGOV...")
week_data <- fetch_data("WEEK")
mmkt_data <- fetch_data("MMKT")
sgov_data <- fetch_data("SGOV")

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

message("WEEK ex-dividend dates: ", paste(head(week_exdiv, 5), collapse = ", "), "...")
message("MMKT ex-dividend dates: ", paste(head(mmkt_exdiv, 5), collapse = ", "), "...")
message("SGOV ex-dividend dates: ", paste(sgov_exdiv, collapse = ", "))

# Find overlapping dates (WEEK/MMKT with SGOV)
overlap_week_sgov <- week_exdiv[week_exdiv %in% sgov_exdiv]
overlap_mmkt_sgov <- mmkt_exdiv[mmkt_exdiv %in% sgov_exdiv]

message("Overlap dates (WEEK & SGOV): ", paste(overlap_week_sgov, collapse = ", "))
message("Overlap dates (MMKT & SGOV): ", paste(overlap_mmkt_sgov, collapse = ", "))

# Run three-ticker rotation strategy
# Strategy:
# - Base position alternates between WEEK and MMKT to capture BOTH weekly dividends
# - Before SGOV ex-div, switch to SGOV
# - After SGOV dividend captured, switch to whichever weekly ETF has next ex-div
# - On overlap days (SGOV ex-div same as WEEK or MMKT ex-div), hold SGOV ONLY - weekly dividend is forfeited

run_three_ticker_strategy <- function() {

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
    stringsAsFactors = FALSE
  )

  # Helper to get data for a ticker
  get_ticker_data <- function(ticker) {
    switch(ticker,
           "WEEK" = week_data,
           "MMKT" = mmkt_data,
           "SGOV" = sgov_data)
  }

  # Helper to get exdiv dates for a ticker
  get_ticker_exdiv <- function(ticker) {
    switch(ticker,
           "WEEK" = week_exdiv,
           "MMKT" = mmkt_exdiv,
           "SGOV" = sgov_exdiv)
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
  all_dates <- sort(unique(c(week_data$date, mmkt_data$date, sgov_data$date)))
  all_dates <- all_dates[all_dates >= start_date & all_dates <= end_date]

  for (current_date in all_dates) {
    current_date <- as.Date(current_date, origin = "1970-01-01")
    current_data <- get_ticker_data(current_ticker)
    current_exdiv <- get_ticker_exdiv(current_ticker)

    # Check if we're holding on an EX-DIV date (entitled to dividend)
    if (current_date %in% current_exdiv) {
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
              amount = div_amount
            ))
          }
        }
      } else if (current_ticker == "SGOV") {
        # Monthly: dividend pays around 1st of next month
        next_month <- floor_date(current_date, "month") %m+% months(1)
        payment_dates <- sgov_data$date[sgov_data$dividend > 0 & sgov_data$date >= next_month]
        if (length(payment_dates) > 0) {
          payment_date <- min(payment_dates)
          div <- sgov_data$dividend[sgov_data$date == payment_date]
          if (length(div) > 0 && div > 0) {
            div_amount <- current_shares * div
            total_dividends <- total_dividends + div_amount
            dividends_log <- rbind(dividends_log, data.frame(
              date = as.character(payment_date),
              ticker = "SGOV",
              div_per_share = div,
              shares = current_shares,
              amount = div_amount
            ))
          }
        }
      }
    }

    # SWITCH FROM WEEKLY TO SGOV: day before any SGOV ex-div date
    if (current_ticker %in% c("WEEK", "MMKT")) {
      for (sexdiv in sgov_exdiv) {
        prev_day <- get_prev_trading_day(sgov_data, sexdiv)
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

          # Buy SGOV at close
          sgov_close <- get_price(sgov_data, current_date, "close")
          new_shares <- sale_value / sgov_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "BUY",
            ticker = "SGOV",
            price = sgov_close,
            shares = new_shares,
            value = sale_value
          ))

          current_ticker <- "SGOV"
          current_shares <- new_shares
          break
        }
      }
    }

    # SWITCH FROM SGOV TO WEEKLY: day before next weekly ex-div (that is NOT also an SGOV ex-div)
    if (current_ticker == "SGOV") {
      # Find next weekly ex-div (WEEK or MMKT) that is NOT overlapping with SGOV
      week_next <- week_exdiv[week_exdiv > current_date & !(week_exdiv %in% sgov_exdiv)]
      mmkt_next <- mmkt_exdiv[mmkt_exdiv > current_date & !(mmkt_exdiv %in% sgov_exdiv)]

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

      if (!is.na(next_weekly_date)) {
        target_data <- get_ticker_data(next_weekly_ticker)
        prev_day <- get_prev_trading_day(target_data, next_weekly_date)

        if (!is.na(prev_day) && current_date == prev_day) {
          # Sell SGOV at close
          sgov_close <- get_price(sgov_data, current_date, "close")
          sale_value <- current_shares * sgov_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "SELL",
            ticker = "SGOV",
            price = sgov_close,
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
      # But also check we're not about to switch to SGOV
      if (!is.na(other_next)) {
        prev_day <- get_prev_trading_day(other_data, other_next)

        # Only switch if:
        # 1. Other weekly ex-div is before current weekly ex-div (or current is NA)
        # 2. Other weekly ex-div is not an SGOV overlap
        # 3. We're on the day before the other weekly ex-div
        # 4. We're not about to switch to SGOV (next day is not day before SGOV ex-div)
        should_switch <- !is.na(prev_day) && current_date == prev_day &&
                         !(other_next %in% sgov_exdiv) &&
                         (is.na(current_next) || other_next < current_next)

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

message("\nRunning WEEK/MMKT/SGOV three-ticker strategy...")
result <- run_three_ticker_strategy()

# Generate markdown file
generate_markdown <- function(result, filename) {

  md <- "# WEEK/MMKT/SGOV Three-Ticker Dividend Capture Rotation Strategy\n\n"
  md <- paste0(md, "**Period:** March 6, 2025 to December 2, 2025\n")
  md <- paste0(md, "**Initial Investment:** $60,000\n\n")

  md <- paste0(md, "## Strategy Overview\n\n")
  md <- paste0(md, "This strategy rotates between WEEK, MMKT (both weekly dividends), and SGOV (monthly dividends) to capture maximum dividends:\n\n")
  md <- paste0(md, "- **Base Position:** Alternates between WEEK and MMKT to capture BOTH weekly dividends (they pay on different days)\n")
  md <- paste0(md, "- **Monthly Switch:** Before each SGOV ex-dividend date, switch to SGOV to capture the monthly dividend\n")
  md <- paste0(md, "- **Return to Weekly:** After capturing SGOV dividend, switch to whichever weekly ETF (WEEK or MMKT) has the next ex-div\n")
  md <- paste0(md, "- **Overlap Rule (CRITICAL):** When SGOV ex-div overlaps with WEEK or MMKT ex-div, hold SGOV ONLY - the weekly dividend is FORFEITED (no double counting)\n\n")

  md <- paste0(md, "### Ex-Dividend Dates\n\n")
  md <- paste0(md, "- **WEEK ex-div dates:** ", paste(week_exdiv, collapse = ", "), "\n")
  md <- paste0(md, "- **MMKT ex-div dates:** ", paste(mmkt_exdiv, collapse = ", "), "\n")
  md <- paste0(md, "- **SGOV ex-div dates:** ", paste(sgov_exdiv, collapse = ", "), "\n")
  md <- paste0(md, "- **Overlap dates (WEEK & SGOV):** ", ifelse(length(overlap_week_sgov) > 0, paste(overlap_week_sgov, collapse = ", "), "None"), "\n")
  md <- paste0(md, "- **Overlap dates (MMKT & SGOV):** ", ifelse(length(overlap_mmkt_sgov) > 0, paste(overlap_mmkt_sgov, collapse = ", "), "None"), "\n\n")

  md <- paste0(md, "## Transaction Log\n\n")
  md <- paste0(md, "| Date | Action | Ticker | Price | Shares | Value |\n")
  md <- paste0(md, "|------|--------|--------|-------|--------|-------|\n")

  for (i in 1:nrow(result$transactions)) {
    row <- result$transactions[i, ]
    md <- paste0(md, sprintf("| %s | %s | %s | $%.4f | %.4f | $%.2f |\n",
                             row$date, row$action, row$ticker, row$price, row$shares, row$value))
  }

  md <- paste0(md, "\n## Dividends Received\n\n")
  md <- paste0(md, "| Date | Ticker | Div/Share | Shares | Amount |\n")
  md <- paste0(md, "|------|--------|-----------|--------|--------|\n")

  for (i in 1:nrow(result$dividends)) {
    row <- result$dividends[i, ]
    md <- paste0(md, sprintf("| %s | %s | $%.6f | %.4f | $%.2f |\n",
                             row$date, row$ticker, row$div_per_share, row$shares, row$amount))
  }

  # Dividend summary by ticker
  week_divs <- result$dividends[result$dividends$ticker == "WEEK", ]
  mmkt_divs <- result$dividends[result$dividends$ticker == "MMKT", ]
  sgov_divs <- result$dividends[result$dividends$ticker == "SGOV", ]

  md <- paste0(md, "\n## Dividend Summary\n\n")
  md <- paste0(md, "| Source | Count | Total |\n")
  md <- paste0(md, "|--------|-------|-------|\n")
  md <- paste0(md, sprintf("| WEEK | %d | $%.2f |\n", nrow(week_divs), sum(week_divs$amount)))
  md <- paste0(md, sprintf("| MMKT | %d | $%.2f |\n", nrow(mmkt_divs), sum(mmkt_divs$amount)))
  md <- paste0(md, sprintf("| SGOV | %d | $%.2f |\n", nrow(sgov_divs), sum(sgov_divs$amount)))
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

generate_markdown(result, "WEEK_MMKT_SGOV_rotation_strategy.md")

# Print summary
cat("\n=== THREE-TICKER STRATEGY SUMMARY ===\n\n")
cat(sprintf("WEEK/MMKT/SGOV Strategy:\n"))

week_divs <- result$dividends[result$dividends$ticker == "WEEK", ]
mmkt_divs <- result$dividends[result$dividends$ticker == "MMKT", ]
sgov_divs <- result$dividends[result$dividends$ticker == "SGOV", ]

cat(sprintf("  WEEK dividends: %d payments, $%.2f\n", nrow(week_divs), sum(week_divs$amount)))
cat(sprintf("  MMKT dividends: %d payments, $%.2f\n", nrow(mmkt_divs), sum(mmkt_divs$amount)))
cat(sprintf("  SGOV dividends: %d payments, $%.2f\n", nrow(sgov_divs), sum(sgov_divs$amount)))
cat(sprintf("  Total Dividends: $%.2f\n", result$total_dividends))
cat(sprintf("  Final Stock Value: $%.2f\n", result$final_stock_value))
cat(sprintf("  Total Value: $%.2f\n", result$final_total))
cat(sprintf("  Return: $%.2f (%.2f%%)\n", result$final_total - investment, (result$final_total - investment) / investment * 100))
