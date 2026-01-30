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

message("Fetching data...")
week_data <- fetch_data("WEEK")
sgov_data <- fetch_data("SGOV")
bil_data <- fetch_data("BIL")

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

# Get dividend amount for a given ex-div date
# For SGOV/BIL, dividend pays on 1st of next month
# For WEEK, dividend pays day after ex-div
get_dividend_for_exdiv <- function(data, exdiv_date, is_weekly = FALSE) {
  if (is_weekly) {
    # WEEK: dividend pays day after ex-div
    payment_date <- get_next_trading_day(data, exdiv_date)
  } else {
    # Monthly: dividend pays around 1st of next month
    # Find first dividend payment date after the ex-div
    next_month <- floor_date(exdiv_date, "month") %m+% months(1)
    payment_dates <- data$date[data$dividend > 0 & data$date >= next_month]
    payment_date <- if (length(payment_dates) > 0) min(payment_dates) else NA
  }

  if (is.na(payment_date)) return(0)

  div <- data$dividend[data$date == payment_date]
  if (length(div) > 0) div else 0
}

# Identify ex-dividend dates
# WEEK: day before dividend payment
week_div_dates <- week_data$date[week_data$dividend > 0]
week_exdiv <- sapply(week_div_dates, function(d) {
  idx <- which(week_data$date == d)
  if (idx > 1) week_data$date[idx - 1] else NA
})
week_exdiv <- as.Date(week_exdiv[!is.na(week_exdiv)], origin = "1970-01-01")
week_exdiv <- week_exdiv[week_exdiv >= start_date & week_exdiv <= end_date]
week_exdiv <- sort(week_exdiv)

# SGOV/BIL: last business day of month
is_last_bday <- function(d, trading_days) {
  month_start <- floor_date(d, "month")
  next_month <- month_start %m+% months(1)
  month_days <- trading_days[trading_days >= month_start & trading_days < next_month]
  d == max(month_days)
}

sgov_exdiv <- sgov_data$date[sapply(sgov_data$date, function(d) is_last_bday(d, sgov_data$date))]
sgov_exdiv <- sgov_exdiv[sgov_exdiv >= start_date & sgov_exdiv <= end_date]
sgov_exdiv <- sort(sgov_exdiv)

bil_exdiv <- bil_data$date[sapply(bil_data$date, function(d) is_last_bday(d, bil_data$date))]
bil_exdiv <- bil_exdiv[bil_exdiv >= start_date & bil_exdiv <= end_date]
bil_exdiv <- sort(bil_exdiv)

message("WEEK ex-dividend dates: ", paste(head(week_exdiv, 5), collapse = ", "), "...")
message("SGOV ex-dividend dates: ", paste(sgov_exdiv, collapse = ", "))
message("BIL ex-dividend dates: ", paste(bil_exdiv, collapse = ", "))

# Find overlapping dates (WEEK and monthly have same ex-div)
overlap_sgov <- week_exdiv[week_exdiv %in% sgov_exdiv]
overlap_bil <- week_exdiv[week_exdiv %in% bil_exdiv]

message("Overlap dates (WEEK & SGOV): ", paste(overlap_sgov, collapse = ", "))
message("Overlap dates (WEEK & BIL): ", paste(overlap_bil, collapse = ", "))

# Run strategy simulation
# Strategy:
# - Hold WEEK for weekly dividends
# - Before each monthly ex-div, switch to monthly ETF
# - After monthly ex-div, switch back to WEEK before next WEEK ex-div
# - On overlap days, stay in monthly (tiebreaker)
# - Dividends are credited based on holding on EX-DIV date (not payment date)
run_strategy <- function(monthly_ticker, monthly_data, monthly_exdiv, monthly_name) {

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

  # Track pending dividends (owed based on ex-div but not yet paid)
  pending_dividends <- list()

  # Initial purchase
  week_open_price <- get_price(week_data, start_date, "open")
  shares <- investment / week_open_price
  current_ticker <- "WEEK"
  current_shares <- shares
  total_dividends <- 0

  transactions <- rbind(transactions, data.frame(
    date = as.character(start_date),
    action = "BUY",
    ticker = "WEEK",
    price = week_open_price,
    shares = shares,
    value = investment
  ))

  # Process each day
  all_dates <- sort(unique(c(week_data$date, monthly_data$date)))
  all_dates <- all_dates[all_dates >= start_date & all_dates <= end_date]

  for (current_date in all_dates) {
    current_date <- as.Date(current_date, origin = "1970-01-01")

    # Check if we're holding on an EX-DIV date (this is when we become entitled to dividend)
    if (current_ticker == "WEEK") {
      # Check if today is a WEEK ex-div date
      if (current_date %in% week_exdiv) {
        # We're entitled to the dividend that will pay tomorrow
        next_day <- get_next_trading_day(week_data, current_date)
        if (!is.na(next_day)) {
          div <- week_data$dividend[week_data$date == next_day]
          if (length(div) > 0 && div > 0) {
            div_amount <- current_shares * div
            total_dividends <- total_dividends + div_amount
            dividends_log <- rbind(dividends_log, data.frame(
              date = as.character(next_day),  # Log on payment date
              ticker = "WEEK",
              div_per_share = div,
              shares = current_shares,
              amount = div_amount
            ))
          }
        }
      }
    } else {
      # Check if today is a monthly ex-div date
      if (current_date %in% monthly_exdiv) {
        # Find dividend that will pay around 1st of next month
        next_month <- floor_date(current_date, "month") %m+% months(1)
        payment_dates <- monthly_data$date[monthly_data$dividend > 0 & monthly_data$date >= next_month]
        if (length(payment_dates) > 0) {
          payment_date <- min(payment_dates)
          div <- monthly_data$dividend[monthly_data$date == payment_date]
          if (length(div) > 0 && div > 0) {
            div_amount <- current_shares * div
            total_dividends <- total_dividends + div_amount
            dividends_log <- rbind(dividends_log, data.frame(
              date = as.character(payment_date),  # Log on payment date
              ticker = monthly_name,
              div_per_share = div,
              shares = current_shares,
              amount = div_amount
            ))
          }
        }
      }
    }

    # SWITCH FROM WEEK TO MONTHLY: day before ANY monthly ex-div date
    if (current_ticker == "WEEK") {
      for (mexdiv in monthly_exdiv) {
        prev_day <- get_prev_trading_day(monthly_data, mexdiv)
        if (!is.na(prev_day) && current_date == prev_day) {
          # Sell WEEK at close
          week_close <- get_price(week_data, current_date, "close")
          sale_value <- current_shares * week_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "SELL",
            ticker = "WEEK",
            price = week_close,
            shares = current_shares,
            value = sale_value
          ))

          # Buy monthly at close
          monthly_close <- get_price(monthly_data, current_date, "close")
          new_shares <- sale_value / monthly_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "BUY",
            ticker = monthly_name,
            price = monthly_close,
            shares = new_shares,
            value = sale_value
          ))

          current_ticker <- monthly_name
          current_shares <- new_shares
          break
        }
      }
    }

    # SWITCH FROM MONTHLY TO WEEK: day before next WEEK ex-div (that is NOT also a monthly ex-div)
    if (current_ticker == monthly_name) {
      # Find next WEEK ex-div that is NOT a monthly ex-div (overlap)
      next_week_exdiv <- week_exdiv[week_exdiv > current_date & !(week_exdiv %in% monthly_exdiv)]

      if (length(next_week_exdiv) > 0) {
        next_exdiv <- min(next_week_exdiv)
        prev_day <- get_prev_trading_day(week_data, next_exdiv)

        if (!is.na(prev_day) && current_date == prev_day) {
          # Sell monthly at close
          monthly_close <- get_price(monthly_data, current_date, "close")
          sale_value <- current_shares * monthly_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "SELL",
            ticker = monthly_name,
            price = monthly_close,
            shares = current_shares,
            value = sale_value
          ))

          # Buy WEEK at close
          week_close <- get_price(week_data, current_date, "close")
          new_shares <- sale_value / week_close

          transactions <- rbind(transactions, data.frame(
            date = as.character(current_date),
            action = "BUY",
            ticker = "WEEK",
            price = week_close,
            shares = new_shares,
            value = sale_value
          ))

          current_ticker <- "WEEK"
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
  if (current_ticker == "WEEK") {
    final_price <- get_price(week_data, end_date, "close")
  } else {
    final_price <- get_price(monthly_data, end_date, "close")
  }
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

message("\nRunning WEEK/SGOV strategy...")
sgov_result <- run_strategy("SGOV", sgov_data, sgov_exdiv, "SGOV")

message("Running WEEK/BIL strategy...")
bil_result <- run_strategy("BIL", bil_data, bil_exdiv, "BIL")

# Generate markdown files
generate_markdown <- function(result, monthly_name, monthly_exdiv, overlap_dates, filename) {

  md <- sprintf("# WEEK/%s Dividend Capture Rotation Strategy\n\n", monthly_name)
  md <- paste0(md, "**Period:** March 6, 2025 to December 2, 2025\n")
  md <- paste0(md, "**Initial Investment:** $60,000\n\n")

  md <- paste0(md, "## Strategy Overview\n\n")
  md <- paste0(md, "This strategy rotates between WEEK (weekly dividends) and ", monthly_name, " (monthly dividends) to capture both:\n\n")
  md <- paste0(md, "- **Base Position:** WEEK for weekly dividend collection\n")
  md <- paste0(md, "- **Monthly Switch:** Before each ", monthly_name, " ex-dividend date, switch to ", monthly_name, " to capture the monthly dividend\n")
  md <- paste0(md, "- **Return to WEEK:** After capturing monthly dividend, switch back to WEEK before next WEEK-only ex-dividend\n")
  md <- paste0(md, "- **Overlap Rule:** When both have ex-div same day, stay in ", monthly_name, " (tiebreaker)\n\n")

  md <- paste0(md, "### Ex-Dividend Dates\n\n")
  md <- paste0(md, "- **", monthly_name, " ex-div dates:** ", paste(monthly_exdiv, collapse = ", "), "\n")
  md <- paste0(md, "- **Overlap dates:** ", paste(overlap_dates, collapse = ", "), "\n\n")

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
  monthly_divs <- result$dividends[result$dividends$ticker == monthly_name, ]

  md <- paste0(md, "\n## Dividend Summary\n\n")
  md <- paste0(md, "| Source | Count | Total |\n")
  md <- paste0(md, "|--------|-------|-------|\n")
  md <- paste0(md, sprintf("| WEEK | %d | $%.2f |\n", nrow(week_divs), sum(week_divs$amount)))
  md <- paste0(md, sprintf("| %s | %d | $%.2f |\n", monthly_name, nrow(monthly_divs), sum(monthly_divs$amount)))
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

generate_markdown(sgov_result, "SGOV", sgov_exdiv, overlap_sgov, "WEEK_SGOV_rotation_strategy.md")
generate_markdown(bil_result, "BIL", bil_exdiv, overlap_bil, "WEEK_BIL_rotation_strategy.md")

# Print summary comparison
cat("\n=== STRATEGY COMPARISON ===\n\n")
cat(sprintf("WEEK/SGOV Strategy:\n"))
cat(sprintf("  WEEK dividends: %d payments, $%.2f\n",
            nrow(sgov_result$dividends[sgov_result$dividends$ticker == "WEEK", ]),
            sum(sgov_result$dividends[sgov_result$dividends$ticker == "WEEK", "amount"])))
cat(sprintf("  SGOV dividends: %d payments, $%.2f\n",
            nrow(sgov_result$dividends[sgov_result$dividends$ticker == "SGOV", ]),
            sum(sgov_result$dividends[sgov_result$dividends$ticker == "SGOV", "amount"])))
cat(sprintf("  Total Dividends: $%.2f\n", sgov_result$total_dividends))
cat(sprintf("  Final Stock Value: $%.2f\n", sgov_result$final_stock_value))
cat(sprintf("  Total Value: $%.2f\n", sgov_result$final_total))
cat(sprintf("  Return: $%.2f (%.2f%%)\n\n", sgov_result$final_total - investment, (sgov_result$final_total - investment) / investment * 100))

cat(sprintf("WEEK/BIL Strategy:\n"))
cat(sprintf("  WEEK dividends: %d payments, $%.2f\n",
            nrow(bil_result$dividends[bil_result$dividends$ticker == "WEEK", ]),
            sum(bil_result$dividends[bil_result$dividends$ticker == "WEEK", "amount"])))
cat(sprintf("  BIL dividends: %d payments, $%.2f\n",
            nrow(bil_result$dividends[bil_result$dividends$ticker == "BIL", ]),
            sum(bil_result$dividends[bil_result$dividends$ticker == "BIL", "amount"])))
cat(sprintf("  Total Dividends: $%.2f\n", bil_result$total_dividends))
cat(sprintf("  Final Stock Value: $%.2f\n", bil_result$final_stock_value))
cat(sprintf("  Total Value: $%.2f\n", bil_result$final_total))
cat(sprintf("  Return: $%.2f (%.2f%%)\n", bil_result$final_total - investment, (bil_result$final_total - investment) / investment * 100))
