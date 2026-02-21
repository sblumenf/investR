# Extracted from test-fct_group_pnl.R:422

# test -------------------------------------------------------------------------
local_test_db()
group_id <- paste0("TEST_BUY_TO_CLOSE_", format(Sys.time(), "%Y%m%d%H%M%S"))
create_position_group(
    group_id = group_id,
    group_name = "Test Buy-to-Close",
    strategy_type = "DIVIDEND_ARISTOCRATS",
    account_number = "TEST222",
    members = tibble::tibble(
      symbol = c("ABC", "ABC250117C00050000"),
      role = c("underlying_stock", "short_call")
    )
  )
activities <- tibble::tibble(
    activity_id = c(
      paste0("BTC_BUY_STOCK_", group_id),
      paste0("BTC_SELL_CALL_", group_id),
      paste0("BTC_BUY_CALL_", group_id),
      paste0("BTC_SELL_STOCK_", group_id)
    ),
    account_number = rep("TEST222", 4),
    account_type = rep("TFSA", 4),
    trade_date = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 10:01:00",
      "2024-03-31 14:00:00",  # Buy to close
      "2024-03-31 14:01:00"
    )),
    transaction_date = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 10:01:00",
      "2024-03-31 14:00:00",
      "2024-03-31 14:01:00"
    )),
    settlement_date = as.POSIXct(c(
      "2024-01-03 10:00:00",
      "2024-01-03 10:01:00",
      "2024-04-02 14:00:00",
      "2024-04-02 14:01:00"
    )),
    action = c("Buy", "Sell", "Buy", "Sell"),
    symbol = c("ABC", "ABC250117C00050000", "ABC250117C00050000", "ABC"),
    symbol_id = c(12345, 67890, 67890, 12345),
    description = c(
      "Buy 100 ABC @ $236.45",
      "Sell 1 ABC Jan17'25 $50 Call",
      "Buy 1 ABC Jan17'25 $50 Call",
      "Sell 100 ABC @ $309.20"
    ),
    currency = rep("USD", 4),
    quantity = c(100, 1, -1, -100),
    price = c(236.45, 71.5301, 138.8299, 309.20),
    gross_amount = c(-23645, 7153.01, -13882.99, 30920),
    commission = c(0, 0, 0, 0),
    net_amount = c(-23645, 7153.01, -13882.99, 30920),
    type = rep("Trades", 4),
    group_id = rep(NA_character_, 4),
    is_processed = rep(FALSE, 4),
    fetched_at = rep(Sys.time(), 4)
  )
save_result <- save_activities_batch(activities)
saved_activities <- get_activities() %>%
    filter(account_number == "TEST222")
