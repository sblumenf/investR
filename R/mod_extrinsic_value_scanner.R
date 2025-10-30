#' extrinsic_value_scanner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_extrinsic_value_scanner_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Extrinsic Value Scanner"),
      sidebarLayout(
        # Sidebar: Control module
        mod_extrinsic_value_scanner_controls_ui(ns("controls")),

        # Main panel
        mainPanel(
          uiOutput(ns("scanner_results"))
        )
      )
    )
  )
}

#' extrinsic_value_scanner Server Functions
#'
#' @noRd
mod_extrinsic_value_scanner_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Call controls module and get reactive inputs
    controls <- mod_extrinsic_value_scanner_controls_server("controls")

    # Reactive expression for eligible stock universe
    eligible_stocks <- eventReactive(controls$scan_button(), { # Triggered by controls module's button # nolint
      # Show a loading indicator
      withProgress(message = "Generating eligible stock universe...", { # nolint
        # 1. Get non-dividend S&P 500 stocks
        log_info("Fetching zero-dividend S&P 500 stocks...") # nolint
        zero_div_stocks <- tryCatch({
          # get_zero_dividend_stocks() returns a character vector, convert to tibble
          tickers <- get_zero_dividend_stocks() # nolint
          tibble(symbol = tickers) # nolint
        }, error = function(e) {
          log_error("Failed to fetch zero-dividend stocks: {e$message}") # nolint
          showNotification("Error fetching zero-dividend stocks.", type = "error") # nolint
          return(tibble(symbol = character(0))) # nolint
        })
        incProgress(0.3) # nolint

        # 2. Get most shorted tickers
        log_info("Fetching most shorted tickers...") # nolint
        most_shorted_tickers <- tryCatch({
          fetch_most_shorted_tickers() # Assuming this returns a character vector of symbols # nolint
        }, error = function(e) {
          log_error("Failed to fetch most shorted tickers: {e$message}") # nolint
          showNotification("Error fetching most shorted tickers.", type = "error") # nolint
          return(character(0)) # nolint
        })
        incProgress(0.3) # nolint

        # 3. Filter eligible stocks
        if (nrow(zero_div_stocks) == 0) {
          log_warn("No zero-dividend stocks found. Cannot proceed with filtering.") # nolint
          return(tibble(symbol = character(0))) # nolint
        }

        if (length(most_shorted_tickers) > 0) {
          eligible <- zero_div_stocks %>%
            dplyr::filter(!symbol %in% most_shorted_tickers)
          log_info("Filtered out {nrow(zero_div_stocks) - nrow(eligible)} most shorted stocks.") # nolint
        } else {
          eligible <- zero_div_stocks
          log_info("No most shorted tickers found to filter.") # nolint
        }
        incProgress(0.4) # nolint

        if (nrow(eligible) == 0) {
          showNotification("No eligible stocks found after filtering.", type = "warning") # nolint
        } else {
          showNotification(sprintf("Found %d eligible stocks.", nrow(eligible)), type = "message") # nolint
        }

        return(eligible)
      })
    })

    # Reactive expression for scanning results
    scan_results <- eventReactive(controls$scan_button(), {
      req(controls$scan_button() > 0) # Only run if button has been clicked
      req(nrow(eligible_stocks()) > 0) # Ensure there are eligible stocks # nolint
      log_info("scan_results: eligible_stocks() has {nrow(eligible_stocks())} rows. Starting scan.") # ADDED LOGGING

      withProgress(message = "Scanning for opportunities...", value = 0, { # nolint
        opportunities <- list()
        eligible_count <- nrow(eligible_stocks())

        for (i in seq_len(eligible_count)) {
          stock_symbol <- eligible_stocks()$symbol[i]
          incProgress(1/eligible_count, detail = paste("Processing", stock_symbol)) # nolint
          log_info("Scanning options for {stock_symbol}...") # nolint

          # Fetch current stock price
          current_quote <- tryCatch({
            fetch_current_quote(stock_symbol) # Returns data.frame with 'Last' and 'Name' columns # nolint
          }, error = function(e) {
            log_warn("Could not fetch quote for {stock_symbol}: {e$message}") # nolint
            return(NULL) # nolint
          })

          if (is.null(current_quote) || is.null(current_quote$Last)) {
            log_debug("{stock_symbol}: Skipped - no quote data available") # nolint
            next # Skip if quote not available # nolint
          }
          stock_price <- current_quote$Last
          if (length(stock_price) == 0 || is.na(stock_price)) {
            log_warn("Current stock price is empty or NA for {stock_symbol}.")
            next # Skip if stock price is empty
          }
          # Extract company name if available
          company_name <- if (!is.null(current_quote$Name) && length(current_quote$Name) > 0) {
            current_quote$Name
          } else {
            NULL
          }
          log_debug("{stock_symbol}: Stock price = ${stock_price}") # nolint

          # Fetch option chain (returns Yahoo format - nested list)
          option_chain_raw <- tryCatch({
            fetch_questrade_options_chain(stock_symbol) # nolint
          }, error = function(e) {
            log_warn("Could not fetch option chain for {stock_symbol}: {e$message}") # nolint
            return(NULL) # nolint
          })

          if (is.null(option_chain_raw) || length(option_chain_raw) == 0) {
            log_debug("{stock_symbol}: Skipped - no option chain data from API") # nolint
            next # Skip if no option chain # nolint
          }

          # Convert Yahoo format to flat tibble
          option_chain <- convert_options_chain_to_tibble(option_chain_raw)

          if (is.null(option_chain) || nrow(option_chain) == 0) {
            log_debug("{stock_symbol}: Skipped - option chain conversion returned no data") # nolint
            next # Skip if conversion failed or no options # nolint
          }
          log_debug("{stock_symbol}: Found {nrow(option_chain)} total option contracts") # nolint

          # Filter by expiry
          max_expiry_days <- controls$max_expiry_days() # Get from controls module
          option_chain <- option_chain %>%            dplyr::mutate(days_to_expiry = as.numeric(difftime(expirationDate, Sys.Date(), units = "days"))) %>%
            dplyr::filter(days_to_expiry <= max_expiry_days, days_to_expiry > 0) # Only future expirations # nolint

          if (nrow(option_chain) == 0) {
            log_debug("{stock_symbol}: Skipped - no options within {max_expiry_days} day expiry window") # nolint
            next # Skip if no options within expiry range # nolint
          }
          log_debug("{stock_symbol}: After expiry filter: {nrow(option_chain)} contracts remain") # nolint

          # Identify ATM reverse collar opportunities
          # Strategy: Short stock + Sell ATM put + Buy ATM call
          # For each expiration, find the ATM strike and get both put and call

          # Group by expiration and find ATM strike (closest to stock price)
          atm_strikes_by_expiry <- option_chain %>%
            dplyr::group_by(expirationDate) %>% # nolint
            dplyr::summarise(
              atm_strike = strikePrice[which.min(abs(strikePrice - stock_price))],
              .groups = "drop"
            ) # nolint

          if (nrow(atm_strikes_by_expiry) == 0) {
            log_debug("{stock_symbol}: Skipped - no strikes found") # nolint
            next # nolint
          }

          # For each expiration, get both the put and call at ATM strike
          reverse_collar_opportunities <- list()

          for (i in seq_len(nrow(atm_strikes_by_expiry))) {
            expiry <- atm_strikes_by_expiry$expirationDate[i]
            strike <- atm_strikes_by_expiry$atm_strike[i]

            # Get the put at this strike
            put_option <- option_chain %>%
              dplyr::filter(
                expirationDate == expiry,
                strikePrice == strike,
                optionType == "Put"
              ) %>% # nolint
              dplyr::slice(1) # nolint

            # Get the call at this strike
            call_option <- option_chain %>%
              dplyr::filter(
                expirationDate == expiry,
                strikePrice == strike,
                optionType == "Call"
              ) %>% # nolint
              dplyr::slice(1) # nolint

            # Skip if either option is missing
            if (nrow(put_option) == 0 || nrow(call_option) == 0) {
              log_debug("{stock_symbol}: Skipped expiry {expiry} - missing put or call at strike {strike}") # nolint
              next
            }

            # Calculate reverse collar cash flows
            reverse_collar <- calculate_reverse_collar_metrics(
              stock_price = stock_price,
              strike = strike,
              put_option = put_option,
              call_option = call_option
            )

            # Only include if net credit is positive and annualized return is positive
            # Check for NA and ensure values are valid
            if (!is.null(reverse_collar) && nrow(reverse_collar) > 0) {
              net_credit_val <- reverse_collar$net_credit[1]
              ann_return_val <- reverse_collar$annualized_return[1]

              if (!is.na(net_credit_val) && !is.na(ann_return_val) &&
                  net_credit_val > 0 && ann_return_val > 0) {
                reverse_collar_opportunities[[length(reverse_collar_opportunities) + 1]] <- reverse_collar
              }
            }
          }

          if (length(reverse_collar_opportunities) == 0) {
            log_debug("{stock_symbol}: Skipped - no positive net credit reverse collars found") # nolint
            next # nolint
          }

          log_debug("{stock_symbol}: Found {length(reverse_collar_opportunities)} reverse collar opportunities") # nolint

          # Select the best opportunity (highest annualized return or net credit)
          if (length(reverse_collar_opportunities) > 0) {
            # Convert to tibble and sort
            opportunities_df <- dplyr::bind_rows(reverse_collar_opportunities)

            best_opportunity <- opportunities_df %>%
              dplyr::arrange(desc(annualized_return)) %>% # nolint
              dplyr::slice(1) %>% # nolint
              dplyr::mutate(
                symbol = stock_symbol,
                current_stock_price = stock_price,
                company_name = company_name
              ) # nolint

            opportunities <- append(opportunities, list(best_opportunity))
          }
        } # End for loop

        if (length(opportunities) == 0) {
          showNotification("No opportunities found matching criteria.", type = "warning") # nolint
          return(tibble()) # nolint
        } else {
          final_results <- dplyr::bind_rows(opportunities) %>%
            dplyr::arrange(desc(annualized_return_pct)) # nolint
          showNotification(sprintf("Found %d opportunities.", nrow(final_results)), type = "message") # nolint
          return(final_results) # nolint
        }
      }) # End withProgress
    }) # End scan_results eventReactive

    output$scanner_results <- renderUI({
      # Don't render anything until scan button is clicked
      req(controls$scan_button() > 0)

      current_scan_results <- scan_results() # Evaluate once

      # First, ensure current_scan_results is not NULL
      req(!is.null(current_scan_results))

      # Then, ensure it has rows
      if (NROW(current_scan_results) == 0) {
        return(tags$div(
          class = "alert alert-info",
          "No opportunities found. Try adjusting your filters."
        ))
      }

      log_info("Rendering scanner results in card format...") # nolint

      # Create a list of cards
      card_list <- purrr::map(seq_len(nrow(current_scan_results)), function(i) {
        opportunity_data <- current_scan_results[i, ]
        create_scanner_opportunity_card(opportunity_data)
      })

      # Arrange cards in a fluid row (e.g., 3 cards per row)
      fluidRow(card_list)
    })

  })
}