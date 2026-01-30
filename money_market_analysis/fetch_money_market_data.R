# Fetch historical data for money market ETFs from Yahoo Finance
# and generate markdown files with date, open, close, dividend

library(quantmod)
library(dplyr)
library(lubridate)
library(purrr)

# Configuration
tickers <- c("SGOV", "BIL", "WEEK")
start_date <- as.Date("2025-03-06")
end_date <- as.Date("2026-01-09")
output_dir <- "/home/sergeblumenfeld/investR/money_market_analysis"

# Function to fetch data and create markdown for a single ticker
create_ticker_markdown <- function(ticker, start_date, end_date, output_dir) {
  message(sprintf("Fetching data for %s...", ticker))

  # Fetch OHLC data from Yahoo Finance
  tryCatch({
    data <- getSymbols(
      ticker,
      src = "yahoo",
      from = start_date,
      to = end_date + 1,  # Add 1 day to include end_date
      auto.assign = FALSE
    )

    # Convert to data frame
    df <- data.frame(
      date = index(data),
      open = as.numeric(Op(data)),
      close = as.numeric(Cl(data))  # This is unadjusted close
    )

    # Fetch dividend data
    dividends <- tryCatch({
      getDividends(ticker, from = start_date, to = end_date + 1, auto.assign = FALSE)
    }, error = function(e) {
      message(sprintf("No dividend data for %s: %s", ticker, e$message))
      NULL
    })

    # Add dividend column
    if (!is.null(dividends) && length(dividends) > 0) {
      div_df <- data.frame(
        date = index(dividends),
        dividend = as.numeric(dividends)
      )
      df <- df %>%
        left_join(div_df, by = "date") %>%
        mutate(dividend = ifelse(is.na(dividend), 0, dividend))
    } else {
      df$dividend <- 0
    }

    # Filter to exact date range
    df <- df %>%
      filter(date >= start_date & date <= end_date) %>%
      arrange(date)

    # Format for markdown
    df <- df %>%
      mutate(
        date = format(date, "%Y-%m-%d"),
        open = sprintf("%.4f", open),
        close = sprintf("%.4f", close),
        dividend = sprintf("%.6f", dividend)
      )

    # Create markdown content
    md_content <- sprintf("# %s Historical Data\n\n", ticker)
    md_content <- paste0(md_content, sprintf("**Period:** %s to %s\n\n", start_date, end_date))
    md_content <- paste0(md_content, "| Date | Open | Close | Dividend |\n")
    md_content <- paste0(md_content, "|------|------|-------|----------|\n")

    for (i in 1:nrow(df)) {
      md_content <- paste0(
        md_content,
        sprintf("| %s | %s | %s | %s |\n",
                df$date[i], df$open[i], df$close[i], df$dividend[i])
      )
    }

    # Add summary statistics
    numeric_df <- df %>%
      mutate(
        open = as.numeric(open),
        close = as.numeric(close),
        dividend = as.numeric(dividend)
      )

    total_dividends <- sum(numeric_df$dividend)
    trading_days <- nrow(df)

    md_content <- paste0(md_content, sprintf("\n## Summary\n\n"))
    md_content <- paste0(md_content, sprintf("- **Trading Days:** %d\n", trading_days))
    md_content <- paste0(md_content, sprintf("- **Total Dividends:** $%.6f\n", total_dividends))
    md_content <- paste0(md_content, sprintf("- **First Open:** $%s\n", df$open[1]))
    md_content <- paste0(md_content, sprintf("- **Last Close:** $%s\n", df$close[nrow(df)]))

    # Write markdown file
    output_file <- file.path(output_dir, sprintf("%s_historical.md", ticker))
    writeLines(md_content, output_file)
    message(sprintf("Created %s", output_file))

    return(list(success = TRUE, ticker = ticker, rows = nrow(df)))

  }, error = function(e) {
    message(sprintf("Error fetching %s: %s", ticker, e$message))
    return(list(success = FALSE, ticker = ticker, error = e$message))
  })
}

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Process all tickers
results <- map(tickers, ~create_ticker_markdown(.x, start_date, end_date, output_dir))

# Print summary
message("\n=== Summary ===")
for (res in results) {
  if (res$success) {
    message(sprintf("%s: Success - %d rows", res$ticker, res$rows))
  } else {
    message(sprintf("%s: Failed - %s", res$ticker, res$error))
  }
}
