#' Review Transactions Modal UI Function
#'
#' @description A shiny Module for the transaction review workflow.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList modalDialog fluidRow column actionButton uiOutput showModal removeModal showNotification reactiveVal reactive req moduleServer selectInput icon modalButton tags textInput
#' @importFrom dplyr select mutate group_by summarize arrange %>%
#' @importFrom purrr map_chr map2_chr
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom logger log_info log_warn
#' @importFrom tibble tibble
mod_review_transactions_ui <- function(id){
  ns <- NS(id)
  # The UI is rendered entirely on the server side within a modalDialog
  tagList()
}

#' Review Transactions Modal Server Functions
#'
#' @param id Module ID
#' @param trigger A reactive trigger to launch the modal.
#' @param virgin_by_ticker A reactive list of virgin transactions, grouped by ticker.
#'
#' @noRd
mod_review_transactions_server <- function(id, trigger, virgin_by_ticker){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # --- State Management ---
    # Index of the current ticker being reviewed
    current_index <- reactiveVal(1)
    # Mutable copy of virgin_by_ticker for refreshing after actions
    ticker_data <- reactiveVal(NULL)
    # Total number of tickers to review
    total_tickers <- reactive(length(ticker_data()))
    # Track actions to trigger banner refresh in parent module
    action_count <- reactiveVal(0)
    # Track which transaction rows are selected via checkboxes
    selected_rows <- reactiveVal(integer())

    # Reactive: Transactions sorted exactly as displayed in table
    # This ensures row indices from DT selection map correctly to activity_ids
    displayed_transactions <- reactive({
      req(ticker_data())
      req(current_index() <= total_tickers())
      ticker_data()[[current_index()]] %>%
        arrange(trade_date)
    })

    # Helper functions for ticker-account key management
    extract_ticker_from_key <- function(key) {
      if (is.null(key) || is.na(key) || key == "") return(NA_character_)
      parts <- strsplit(key, "_")[[1]]
      if (length(parts) < 1) return(NA_character_)
      parts[1]
    }

    extract_account_from_key <- function(key) {
      if (is.null(key) || is.na(key) || key == "") return(NA_character_)
      parts <- strsplit(key, "_")[[1]]
      if (length(parts) < 2) return(NA_character_)
      parts[2]
    }

    # Normalize ticker by stripping exchange suffixes for grouping purposes
    # Examples: LB.TO -> LB, SRU.UN.TO -> SRU, AAPL.MX -> AAPL, BRK.B -> BRK.B
    normalize_ticker_for_grouping <- function(ticker) {
      if (is.na(ticker) || ticker == "") return(ticker)

      # Strip ALL exchange suffixes (handles multiple like .UN.TO)
      # Keeps stripping dot + 1-3 uppercase letters from end until no more matches
      # Won't match: BRK.B (has lowercase after), ticker internal dots
      while (grepl("\\.[A-Z]{1,3}$", ticker)) {
        ticker <- gsub("\\.[A-Z]{1,3}$", "", ticker)
      }
      return(ticker)
    }

    # Parse option symbol from description text when symbol is empty
    # Example: "CALL PESI   12/19/25    10  ..." -> "PESI19Dec25C10.00"
    parse_option_symbol_from_description <- function(description, underlying_ticker) {
      if (is.na(description) || is.na(underlying_ticker)) return(NA_character_)

      # Extract call/put indicator
      call_put <- if (grepl("^CALL", description)) "C" else if (grepl("^PUT", description)) "P" else return(NA_character_)

      # Extract date: MM/DD/YY
      date_match <- stringr::str_extract(description, "\\d{2}/\\d{2}/\\d{2}")
      if (is.na(date_match)) return(NA_character_)

      # Parse date components
      date_parts <- strsplit(date_match, "/")[[1]]
      month_num <- as.integer(date_parts[1])
      day <- date_parts[2]
      year <- date_parts[3]
      month_name <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")[month_num]

      # Extract strike (4th number in description)
      numbers <- stringr::str_extract_all(description, "\\d+")[[1]]
      if (length(numbers) < 4) return(NA_character_)
      strike <- as.numeric(numbers[4])

      # Construct symbol: PESI19Dec25C10.00
      sprintf("%s%s%s%s%s%.2f", underlying_ticker, day, month_name, year, call_put, strike)
    }

    # Helper function to fetch and group unlinked activities
    refresh_data <- function() {
      # Auto-link cash equivalents before fetching
      auto_link_cash_equivalents()

      unlinked <- get_unlinked_activities()

      if (nrow(unlinked) == 0) {
        return(list())
      }

      # Add ticker column (extract underlying ticker from option symbols)
      unlinked <- unlinked %>%
        mutate(ticker = purrr::map2_chr(symbol, description, function(sym, desc) {
          # FALLBACK: If symbol is empty/NULL, try parsing description
          if (is.null(sym) || is.na(sym) || sym == "") {
            ticker <- extract_ticker_from_description(desc)
            if (!is.na(ticker)) {
              normalized <- normalize_ticker_for_grouping(ticker)
              log_debug("Ticker extraction (fallback): {ticker} -> {normalized}")
              return(normalized)
            }
            return("UNKNOWN")  # Last resort if description parsing fails
          }

          # Normal logic: symbol exists
          if (grepl("\\d{1,2}[A-Z][a-z]{2}\\d{2}[CP]", sym)) {
            result <- parse_option_symbol(sym)
            if (is.na(result)) {
              normalized <- normalize_ticker_for_grouping(sym)
              log_debug("Ticker extraction (option, no parse): {sym} -> {normalized}")
              return(normalized)
            } else {
              normalized <- normalize_ticker_for_grouping(result)
              log_debug("Ticker extraction (option): {sym} -> {result} -> {normalized}")
              return(normalized)
            }
          } else {
            normalized <- normalize_ticker_for_grouping(sym)
            log_debug("Ticker extraction (stock): {sym} -> {normalized}")
            return(normalized)
          }
        }))

      # Get all grouping suggestions to enable suggestion-based sorting
      suggestions <- get_pending_suggestions()

      # Add suggestion grouping for sorting
      unlinked <- unlinked %>%
        mutate(
          suggestion_id = purrr::map_chr(activity_id, function(aid) {
            if (nrow(suggestions) == 0) return(NA_character_)

            for (i in seq_len(nrow(suggestions))) {
              involved_ids <- suggestions$involved_activity_ids[[i]]
              if (aid %in% involved_ids) {
                return(suggestions$suggestion_id[i])
              }
            }
            return(NA_character_)
          })
        ) %>%
        # Sort by trade date, then by suggestion grouping, then by symbol
        arrange(
          trade_date,
          suggestion_id,
          symbol
        )

      # Create composite key: ticker + account
      unlinked <- unlinked %>%
        mutate(ticker_account_key = paste(ticker, account_number, sep = "_"))

      # Group by ticker AND account (not just ticker)
      split(unlinked, unlinked$ticker_account_key)
    }

    # --- Modal Launch ---
    observeEvent(trigger(), {
      req(trigger() > 0)

      # Initialize ticker_data with data from parent
      ticker_data(virgin_by_ticker())

      req(total_tickers() > 0)

      # Reset index to 1 every time modal is launched
      current_index(1)

      showModal(review_modal())
    })

    # Re-render modal when index changes (enables Next/Prev navigation)
    observeEvent(current_index(), {
      req(ticker_data())
      req(current_index() > 0)
      req(current_index() <= total_tickers())
      showModal(review_modal())
    }, ignoreInit = TRUE)

    # --- Dynamic Modal UI ---
    review_modal <- reactive({
      req(ticker_data())
      req(current_index() <= total_tickers())

      current_ticker_account_key <- names(ticker_data())[current_index()]
      current_activities <- ticker_data()[[current_index()]]

      # Extract ticker and account from composite key
      current_ticker <- extract_ticker_from_key(current_ticker_account_key)
      current_account <- extract_account_from_key(current_ticker_account_key)

      modalDialog(
        title = tagList(
          tags$h3(sprintf("Reviewing %d transactions for: %s in account %s",
                         nrow(current_activities), current_ticker, current_account)),
          tags$p(sprintf("Item %d of %d", current_index(), total_tickers()))
        ),
        size = "l",
        easyClose = TRUE,

        # Transaction table
        tags$h4("Transactions"),
        DT::dataTableOutput(ns("transaction_table")),

        tags$hr(),

        # Groups table for linking
        tags$h4("Link to Existing Group"),
        DT::dataTableOutput(ns("groups_table")),

        footer = tagList(
          modalButton("Close"),
          actionButton(ns("link_to_group"), "Link to Group", icon = icon("link"), class = "btn-primary"),
          actionButton(ns("create_new_group"), "Create New Group", icon = icon("plus"), class = "btn-success"),
          actionButton(ns("ignore_transactions"), "Ignore These Transactions", icon = icon("ban"), class = "btn-warning"),
          tags$span(style = "flex: 1;"),  # Spacer
          actionButton(ns("prev_ticker"), "Previous"),
          actionButton(ns("next_ticker"), "Next")
        )
      )
    })
    
    # Render the transaction table for the current ticker
    output$transaction_table <- DT::renderDataTable({
      displayed_transactions() %>%
        mutate(trade_date = format(as.Date(trade_date), "%b %d, %Y")) %>%
        select(activity_id, trade_date, action, description, quantity, net_amount)
    }, options = list(
      dom = 't',
      paging = FALSE,
      ordering = FALSE,
      columnDefs = list(list(visible = FALSE, targets = 0))  # Hide activity_id column
    ), selection = list(mode = 'multiple', target = 'row'))

    # Render the groups table for linking - filtered by current account and ticker
    output$groups_table <- DT::renderDataTable({
      req(ticker_data())
      req(current_index() <= total_tickers())

      # Get current ticker and account
      current_key <- names(ticker_data())[current_index()]
      current_ticker <- extract_ticker_from_key(current_key)
      current_account <- extract_account_from_key(current_key)

      # Get all open groups
      open_groups <- get_all_groups(include_closed = FALSE)

      if (nrow(open_groups) == 0) {
        return(tibble::tibble(
          group_name = character(),
          strategy_type = character(),
          status = character()
        ))
      }

      # Filter groups by account
      account_groups <- open_groups %>%
        filter(account_number == current_account)

      if (nrow(account_groups) == 0) {
        return(tibble::tibble(
          group_name = character(),
          strategy_type = character(),
          status = character()
        ))
      }

      # Get members for these groups to filter by ticker
      group_ids <- account_groups$group_id
      members <- get_members_for_groups(group_ids)

      if (nrow(members) == 0) {
        return(tibble::tibble(
          group_name = character(),
          strategy_type = character(),
          status = character()
        ))
      }

      # Find groups that contain the current ticker (or options on that ticker)
      # Normalize current ticker for comparison
      current_ticker_norm <- normalize_ticker_for_grouping(current_ticker)

      ticker_groups <- members %>%
        mutate(
          # Extract and normalize ticker from each group member symbol
          member_ticker = purrr::map_chr(symbol, function(sym) {
            if (is_option_symbol(sym)) {
              underlying <- parse_option_symbol(sym)
              if (!is.na(underlying)) {
                return(normalize_ticker_for_grouping(underlying))
              }
            }
            return(normalize_ticker_for_grouping(sym))
          })
        ) %>%
        filter(member_ticker == current_ticker_norm) %>%
        pull(group_id) %>%
        unique()

      # Filter to matching groups
      filtered_groups <- account_groups %>%
        filter(group_id %in% ticker_groups) %>%
        select(group_name, strategy_type, status)

      return(filtered_groups)
    }, options = list(dom = 't', paging = FALSE), selection = list(mode = 'single', target = 'row'))

    # Update selected_rows when user clicks checkboxes in transaction table
    observeEvent(input$transaction_table_rows_selected, {
      selected_rows(input$transaction_table_rows_selected)
    })

    # --- Server-Side Action Logic ---

    # Helper function to advance to the next ticker or close modal
    advance_or_close <- function() {
      # Refresh data from database to get current unlinked activities
      fresh_data <- refresh_data()
      ticker_data(fresh_data)

      # If no more unlinked activities, close modal
      if (length(fresh_data) == 0) {
        removeModal()
        showNotification("All transactions have been processed!", type = "message")
        return()
      }

      # If current index is now beyond the end, reset to last ticker
      if (current_index() > length(fresh_data)) {
        current_index(length(fresh_data))
      }

      # Current index is still valid, modal will auto-refresh via observer
    }
    
    # --- Button Handlers ---
    observeEvent(input$next_ticker, {
      req(current_index() < total_tickers())
      selected_rows(integer())  # Clear selection when changing tickers
      current_index(current_index() + 1)
    })

    observeEvent(input$prev_ticker, {
      req(current_index() > 1)
      selected_rows(integer())  # Clear selection when changing tickers
      current_index(current_index() - 1)
    })

    observeEvent(input$ignore_transactions, {
      req(length(selected_rows()) > 0)
      current_key <- names(ticker_data())[current_index()]
      current_ticker <- extract_ticker_from_key(current_key)
      current_account <- extract_account_from_key(current_key)
      activity_ids <- displayed_transactions()$activity_id[selected_rows()]
      log_info(sprintf("Ignoring %d activities for ticker %s in account %s", length(activity_ids), current_ticker, current_account))
      batch_ignore_activities(activity_ids)
      action_count(action_count() + 1)
      advance_or_close()
    })

    observeEvent(input$link_to_group, {
      # Get selected group from groups_table
      req(length(input$groups_table_rows_selected) > 0)
      req(length(selected_rows()) > 0)

      # Get current ticker and account to filter groups (same logic as renderer)
      current_key <- names(ticker_data())[current_index()]
      current_ticker <- extract_ticker_from_key(current_key)
      current_account <- extract_account_from_key(current_key)

      # Get filtered groups (matching the displayed table)
      open_groups <- get_all_groups(include_closed = FALSE)
      account_groups <- open_groups %>%
        filter(account_number == current_account)

      if (nrow(account_groups) > 0) {
        group_ids <- account_groups$group_id
        members <- get_members_for_groups(group_ids)

        if (nrow(members) > 0) {
          ticker_groups <- members %>%
            filter(
              symbol == current_ticker |
              grepl(paste0("^", current_ticker, "\\d{1,2}[A-Z][a-z]{2}\\d{2}[CP]"), symbol)
            ) %>%
            pull(group_id) %>%
            unique()

          filtered_groups <- account_groups %>%
            filter(group_id %in% ticker_groups)

          # Now map row index to group_id from FILTERED list
          selected_group_row <- input$groups_table_rows_selected
          group_id <- filtered_groups$group_id[selected_group_row]

          activity_ids <- displayed_transactions()$activity_id[selected_rows()]
          log_info(sprintf("Linking %d activities to group %s", length(activity_ids), group_id))
          link_activities_to_group(activity_ids, group_id)
          action_count(action_count() + 1)
          advance_or_close()
        }
      }
    })

    observeEvent(input$create_new_group, {
      # Get current ticker-account key and activities
      current_key <- names(ticker_data())[current_index()]
      current_activities <- ticker_data()[[current_index()]]

      # Extract ticker from composite key
      current_ticker <- extract_ticker_from_key(current_key)

      # Get strategy types
      strategy_types <- get_strategy_types()

      # Show confirmation modal
      showModal(modalDialog(
        title = "Create New Position Group",
        size = "m",

        textInput(
          ns("new_group_name"),
          "Group Name (leave blank for auto-naming):",
          value = "",
          placeholder = "e.g., Dynamic Covered Calls - ALB - Dec 2027 @ $55"
        ),

        selectInput(
          ns("new_group_strategy"),
          "Strategy Type:",
          choices = strategy_types,
          selected = "Other"
        ),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_create_group"), "Create", class = "btn-primary")
        )
      ))
    })

    # Handle confirmation of group creation
    observeEvent(input$confirm_create_group, {
      req(length(selected_rows()) > 0)

      # Get values from form
      group_name <- input$new_group_name
      strategy_type <- input$new_group_strategy

      # Get current ticker-account key and SELECTED activities only
      current_key <- names(ticker_data())[current_index()]
      current_ticker <- extract_ticker_from_key(current_key)
      current_account <- extract_account_from_key(current_key)
      selected_activities <- displayed_transactions()[selected_rows(), ]

      # Resolve empty, NOSYMBOL, or malformed option symbols using description fallback
      # First, try to find a pure stock ticker in the selection (excluding NOSYMBOL and malformed symbols)
      stock_ticker <- unique(selected_activities$symbol[!is.na(selected_activities$symbol) &
                                                         selected_activities$symbol != "" &
                                                         selected_activities$symbol != "NOSYMBOL" &
                                                         !grepl("^\\d", selected_activities$symbol) &
                                                         !purrr::map_lgl(selected_activities$symbol, is_option_symbol)])[1]

      # If no stock ticker found in symbols, try extracting from description
      if (is.na(stock_ticker)) {
        for (i in seq_len(nrow(selected_activities))) {
          potential_ticker <- extract_ticker_from_description(selected_activities$description[i])
          if (!is.na(potential_ticker)) {
            stock_ticker <- potential_ticker
            log_info(sprintf("Extracted ticker from description (as fallback): '%s'", stock_ticker))
            break
          }
        }
      }

      resolved_symbols <- list()  # Track which activities we resolved

      if (!is.na(stock_ticker)) {
        # Identify rows that need resolution:
        # - symbol is NA, empty string, literal "NOSYMBOL", or starts with digit (malformed)
        needs_resolution <- which(
          is.na(selected_activities$symbol) |
          selected_activities$symbol == "" |
          selected_activities$symbol == "NOSYMBOL" |
          grepl("^\\d", selected_activities$symbol)
        )

        for (i in needs_resolution) {
          resolved <- parse_option_symbol_from_description(selected_activities$description[i], stock_ticker)
          if (!is.na(resolved)) {
            log_info(sprintf("Resolved symbol from description: '%s' -> '%s' (ticker: %s)",
                             selected_activities$description[i], resolved, stock_ticker))
            selected_activities$symbol[i] <- resolved
            # Track resolution for database update
            resolved_symbols[[selected_activities$activity_id[i]]] <- resolved
          } else {
            log_warn(sprintf("Failed to resolve symbol from description for activity %s: '%s'",
                             selected_activities$activity_id[i], selected_activities$description[i]))
          }
        }

        # Update database activity symbols to match resolved symbols
        if (length(resolved_symbols) > 0) {
          db_conn <- get_portfolio_db_connection()
          tryCatch({
            for (act_id in names(resolved_symbols)) {
              resolved_sym <- resolved_symbols[[act_id]]

              # Validate symbol before updating
              if (is_option_symbol(resolved_sym)) {
                updated <- dbExecute(db_conn,
                  "UPDATE account_activities SET symbol = ? WHERE activity_id = ? AND group_id IS NULL",
                  params = list(resolved_sym, act_id))

                if (updated > 0) {
                  log_info(sprintf("Updated database: activity %s symbol set to '%s'", act_id, resolved_sym))
                } else {
                  log_warn(sprintf("Failed to update activity %s - may already be linked", act_id))
                }
              } else {
                log_warn(sprintf("Skipped database update for activity %s - invalid option symbol '%s'", act_id, resolved_sym))
              }
            }
          }, error = function(e) {
            log_warn(sprintf("Error updating activity symbols in database: %s", e$message))
          }, finally = {
            dbDisconnect(db_conn, shutdown = TRUE)
          })
        }
      } else {
        log_warn(sprintf("Could not determine underlying ticker for symbol resolution. Selected %d activities but no stock ticker or extractable ticker found.",
                         nrow(selected_activities)))
      }

      # POINT A: Log selected activities in detail
      log_info(sprintf("Creating new group '%s' for ticker: %s in account %s with %d selected activities",
                       group_name, current_ticker, current_account, nrow(selected_activities)))
      log_info(sprintf("[POINT A] Selected row indices: %s", paste(selected_rows(), collapse = ", ")))
      for (i in seq_len(nrow(selected_activities))) {
        act <- selected_activities[i, ]
        log_info(sprintf("[POINT A] Activity %d: symbol=%s, action=%s, quantity=%s, activity_id=%s",
                         i, act$symbol, act$action, act$quantity, act$activity_id))
      }

      # Get account number from first selected activity
      account_number <- selected_activities$account_number[1]

      # Generate group ID
      group_id <- generate_group_id(strategy_type, account_number)

      # Build complete members list from selected activities with proper role assignment
      # Group by symbol and determine predominant action (net quantity direction)
      # Calculate net quantities and assign roles
      # SAFETY: Filter out activities with empty/NA symbols that couldn't be recovered
      valid_activities <- selected_activities %>%
        filter(!is.na(symbol), symbol != "", symbol != "NOSYMBOL")

      if (nrow(valid_activities) < nrow(selected_activities)) {
        skipped_count <- nrow(selected_activities) - nrow(valid_activities)
        log_warn(sprintf("Group Creation: Skipped %d activities with unrecoverable empty symbols", skipped_count))
      }

      net_quantities <- valid_activities %>%
        group_by(symbol) %>%
        summarize(
          # Calculate net position - quantity is already signed correctly in database
          # Positive = bought (long), Negative = sold (short)
          net_quantity = sum(quantity, na.rm = TRUE),
          # Preserve description for any final fallback symbol extraction
          description = first(description),
          .groups = "drop"
        )

      # POINT B: Log net quantities calculated
      log_info(sprintf("[POINT B] Calculated net quantities for %d symbols:", nrow(net_quantities)))
      for (i in seq_len(nrow(net_quantities))) {
        log_info(sprintf("[POINT B]   %s: net_qty = %s", net_quantities$symbol[i], net_quantities$net_quantity[i]))
      }

      members <- net_quantities %>%
        mutate(
          role = purrr::map2_chr(symbol, net_quantity, function(sym, net_qty) {
            # Check for cash equivalent tickers first
            if (toupper(sym) %in% c("SGOV", "ZMMK.TO")) {
              return("cash_equivalent")
            }

            if (is_option_symbol(sym)) {
              # Extract call/put indicator from option symbol
              is_call <- grepl("\\d{1,2}[A-Z][a-z]{2}\\d{2}C", sym)
              is_put <- grepl("\\d{1,2}[A-Z][a-z]{2}\\d{2}P", sym)

              # Determine if position is long (bought) or short (sold) based on net quantity
              is_long <- net_qty > 0

              if (is_call && !is_long) return("short_call")
              if (is_call && is_long) return("long_call")
              if (is_put && !is_long) return("short_put")
              if (is_put && is_long) return("long_put")

              # Edge case fallback - log warning and default to short_call
              log_warn(sprintf(
                "Group Creation: Unable to determine option type for symbol '%s' (net_qty: %s). Defaulting to 'short_call'.",
                sym, net_qty
              ))
              return("short_call")
            } else {
              return("underlying_stock")
            }
          })
        ) %>%
        select(symbol, role)

      # POINT C: Log assigned roles and complete members dataframe
      log_info(sprintf("[POINT C] Assigned roles for %d members:", nrow(members)))
      for (i in seq_len(nrow(members))) {
        log_info(sprintf("[POINT C]   %s → role: %s", members$symbol[i], members$role[i]))
      }
      log_info(sprintf("[POINT C] Complete members dataframe structure:"))
      log_info(sprintf("[POINT C]   %s", paste(capture.output(str(members)), collapse = " | ")))

      # Validation: Check for data quality issues where option symbols might have wrong role
      option_validation <- purrr::map_lgl(members$symbol, is_option_symbol)
      mismatched <- members$symbol[option_validation & members$role == "underlying_stock"]
      if (length(mismatched) > 0) {
        log_warn(sprintf(
          "Group Creation Validation: %d option symbols incorrectly assigned 'underlying_stock' role: %s. This indicates a data quality issue.",
          length(mismatched),
          paste(mismatched, collapse = ", ")
        ))
      }

      # Also check reverse: non-option symbols with option roles
      non_option_mismatched <- members$symbol[!option_validation & members$role == "short_call"]
      if (length(non_option_mismatched) > 0) {
        log_warn(sprintf(
          "Group Creation Validation: %d non-option symbols incorrectly assigned 'short_call' role: %s. This indicates a data quality issue.",
          length(non_option_mismatched),
          paste(non_option_mismatched, collapse = ", ")
        ))
      }

      # Create the group
      success <- create_position_group(
        group_id = group_id,
        group_name = group_name,
        strategy_type = strategy_type,
        account_number = account_number,
        members = members,
        auto_name = TRUE
      )

      if (!success) {
        showNotification(
          "Failed to create group. Please try again.",
          type = "error",
          duration = 5
        )
        return()
      }

      # POINT E: Query back what actually got saved to database
      conn <- get_portfolio_db_connection()
      saved_members <- tryCatch({
        dbGetQuery(conn, "SELECT symbol, role FROM position_group_members WHERE group_id = ?", params = list(group_id))
      }, finally = {
        dbDisconnect(conn, shutdown = TRUE)
      })

      log_info(sprintf("[POINT E] Queried back %d members from database for group %s:", nrow(saved_members), group_id))
      for (i in seq_len(nrow(saved_members))) {
        log_info(sprintf("[POINT E]   %s → role: %s", saved_members$symbol[i], saved_members$role[i]))
      }

      # Compare what we intended vs what was saved
      if (!identical(members$role, saved_members$role) || !identical(members$symbol, saved_members$symbol)) {
        log_warn("[POINT E] WARNING: Members in database DO NOT MATCH what we calculated!")
        log_warn(sprintf("[POINT E] Expected: %s", paste(members$symbol, members$role, sep=":", collapse=", ")))
        log_warn(sprintf("[POINT E] Got: %s", paste(saved_members$symbol, saved_members$role, sep=":", collapse=", ")))
      } else {
        log_info("[POINT E] Database members match calculated members - OK")
      }

      # Link only selected activities to the group
      activity_ids <- selected_activities$activity_id
      link_success <- link_activities_to_group(activity_ids, group_id)

      if (!link_success) {
        showNotification(
          "Group created but failed to link activities.",
          type = "warning",
          duration = 5
        )
        return()
      }

      # Generate initial income projections for the new group
      projection_success <- generate_initial_projections(
        group_id = group_id,
        members = members,
        account_number = account_number
      )

      if (projection_success) {
        log_info(sprintf("Generated initial projections for group %s", group_id))
      } else {
        log_warn(sprintf("Failed to generate projections for group %s", group_id))
      }

      # Success
      showNotification(
        sprintf("Created group: %s", group_name),
        type = "message",
        duration = 3
      )

      log_info(sprintf("Successfully created group %s and linked %d activities",
                      group_id, length(activity_ids)))

      # Increment action count to trigger banner refresh
      action_count(action_count() + 1)

      # Remove the confirmation modal
      removeModal()

      # Move to next ticker or close review modal
      advance_or_close()
    })

    # Return reactive to allow parent module to track actions
    return(reactive(action_count()))
  })
}
