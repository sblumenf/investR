#' Portfolio Groups Business Logic
#'
#' Pure functions for position matching, validation, and group analysis.
#' No database operations - these are testable business logic functions.
#'
#' @name portfolio-groups-logic
#' @import dplyr
#' @importFrom purrr map_chr map_dbl map
#' @importFrom stringr str_extract str_detect str_replace_all
#' @importFrom tibble tibble
NULL

################################################################################
# SYMBOL PARSING AND MATCHING
################################################################################

#' Parse option symbol to extract underlying ticker
#'
#' Extracts the underlying stock symbol from an option symbol.
#' Handles various formats like "ALB17Dec27C55.00", "AAPL240119C150"
#'
#' @param option_symbol Option symbol string
#' @return Character underlying symbol or NA if not parseable
#' @noRd
#'
#' @examples
#' parse_option_symbol("ALB17Dec27C55.00")  # Returns "ALB"
#' parse_option_symbol("AAPL240119C150")    # Returns "AAPL"
#' parse_option_symbol("IBM")                # Returns NA (not an option)
parse_option_symbol <- function(option_symbol) {
  # Option symbols typically have digits in the middle (date)
  # Pattern: TICKER + DIGITS + (C|P) + STRIKE
  # Extract letters before first digit

  if (is.na(option_symbol) || option_symbol == "") {
    return(NA_character_)
  }

  # Try to extract ticker (letters before first digit)
  ticker <- str_extract(option_symbol, "^[A-Z\\.]+(?=\\d)")

  # If no match, it's probably not an option symbol
  if (is.na(ticker)) {
    return(NA_character_)
  }

  return(ticker)
}

#' Extract ticker from transaction description (fallback)
#'
#' When symbol field is empty/NULL, parse ticker from description text.
#' Handles Questrade descriptions like "CALL PYPL 12/19/25 55 PAYPAL..."
#'
#' @param description Transaction description string
#' @return Character ticker or NA if not parseable
#' @noRd
#'
#' @examples
#' extract_ticker_from_description("CALL PYPL 12/19/25 55 PAYPAL HOLDINGS INC...")  # Returns "PYPL"
#' extract_ticker_from_description("PUT AAPL 1/15/26 150 APPLE INC...")              # Returns "AAPL"
extract_ticker_from_description <- function(description) {
  if (is.na(description) || description == "") {
    return(NA_character_)
  }

  # Pattern: (CALL|PUT) followed by whitespace, then ticker (letters/dots), then whitespace and date
  # Examples:
  # "CALL PYPL 12/19/25 55..."    -> PYPL
  # "PUT AAPL 1/15/26 150..."     -> AAPL
  # "CALL BRK.B 3/21/25 400..."   -> BRK.B
  match <- str_extract(description, "(?<=CALL\\s)([A-Z\\.]+)(?=\\s+\\d{1,2}/)")
  if (!is.na(match)) {
    return(match)
  }

  match <- str_extract(description, "(?<=PUT\\s)([A-Z\\.]+)(?=\\s+\\d{1,2}/)")
  if (!is.na(match)) {
    return(match)
  }

  return(NA_character_)
}

#' Detect if symbol is likely an option
#'
#' @param symbol Position symbol (vectorized)
#' @return Logical TRUE if symbol looks like an option
#' @noRd
is_option_symbol <- function(symbol) {
  # Options have mix of letters and numbers, plus C or P for call/put
  has_digits <- str_detect(symbol, "\\d")
  has_call_or_put <- str_detect(symbol, "[CP]\\d")

  # Use & for vectorized operation
  has_digits & has_call_or_put
}

#' Classify position type from symbol
#'
#' Determines if position is stock, option, or other based on symbol
#'
#' @param symbol Position symbol
#' @return Character: "stock", "option", or "other"
#' @noRd
classify_position_type <- function(symbol) {
  if (is.na(symbol)) {
    return("other")
  }

  if (is_option_symbol(symbol)) {
    return("option")
  }

  # Simple stock symbols are all uppercase letters or letters with dots
  if (str_detect(symbol, "^[A-Z\\.]+$")) {
    return("stock")
  }

  return("other")
}

################################################################################
# SELECTION VALIDATION
################################################################################

#' Validate selected positions for grouping
#'
#' Ensures all selected positions belong to same ticker
#'
#' @param selected_positions Tibble with position data (must have: symbol)
#' @return List with valid (logical), error (character), ticker (character)
#' @noRd
validate_selection_for_grouping <- function(selected_positions) {
  if (is.null(selected_positions) || nrow(selected_positions) == 0) {
    return(list(
      valid = FALSE,
      error = "No positions selected",
      ticker = NA_character_
    ))
  }

  # Extract ticker from each symbol
  tickers <- purrr::map_chr(selected_positions$symbol, function(sym) {
    if (is_option_symbol(sym)) {
      parse_option_symbol(sym)
    } else {
      stringr::str_trim(sym)
    }
  })

  # Check if all same ticker
  unique_tickers <- unique(tickers)

  if (length(unique_tickers) > 1) {
    return(list(
      valid = FALSE,
      error = sprintf("Mixed tickers: %s. All positions must have the same ticker.",
                     paste(unique_tickers, collapse = ", ")),
      ticker = NA_character_
    ))
  }

  return(list(
    valid = TRUE,
    error = NULL,
    ticker = unique_tickers[1]
  ))
}

#' Assign roles to positions based on quantity and type
#'
#' @param selected_positions Tibble with position data (must have: symbol, open_quantity)
#' @return Tibble with columns: symbol, role
#' @noRd
assign_roles_from_positions <- function(selected_positions) {
  selected_positions %>%
    mutate(
      role = case_when(
        open_quantity > 0 & !is_option_symbol(symbol) ~ "underlying_stock",
        open_quantity < 0 & is_option_symbol(symbol) ~ "short_call",
        TRUE ~ NA_character_
      )
    ) %>%
    select(symbol, role)
}

#' Get group name for a position
#'
#' Looks up if a position belongs to any group
#'
#' NOTE: This function is inefficient when called repeatedly.
#' Use build_group_name_lookup() for batch operations instead.
#'
#' @param symbol Position symbol
#' @param account_number Account number
#' @return Character group name or NULL if not in any group
#' @noRd
get_position_group_name <- function(symbol, account_number) {
  # Get all groups for this account
  all_groups <- get_all_groups() %>%
    filter(account_number == !!account_number)

  if (nrow(all_groups) == 0) {
    return(NULL)
  }

  # Check each group's members
  for (i in seq_len(nrow(all_groups))) {
    group_id <- all_groups$group_id[i]
    members <- get_group_members(group_id)

    if (symbol %in% members$symbol) {
      return(all_groups$group_name[i])
    }
  }

  return(NULL)
}

#' Build group name lookup table (batch operation)
#'
#' Fetches all groups and members in one go, returns a flat mapping
#' for efficient joining with positions data. Use this instead of
#' calling get_position_group_name() repeatedly.
#'
#' @return Tibble with columns: account_number, symbol, group_name
#' @noRd
build_group_name_lookup <- function() {
  # Fetch all groups once
  all_groups <- get_all_groups()

  if (nrow(all_groups) == 0) {
    return(tibble(
      account_number = character(),
      symbol = character(),
      group_name = character(),
      group_id = character()
    ))
  }

  # Fetch members for each group and combine
  all_members <- purrr::map_dfr(all_groups$group_id, function(group_id) {
    members <- get_group_members(group_id)
    if (nrow(members) == 0) {
      return(tibble())
    }
    members
  })

  if (nrow(all_members) == 0) {
    return(tibble(
      account_number = character(),
      symbol = character(),
      group_name = character(),
      group_id = character()
    ))
  }

  # Join to get group names
  lookup <- all_members %>%
    left_join(
      all_groups %>% select(group_id, group_name, account_number),
      by = "group_id"
    ) %>%
    select(account_number = account_number.y, symbol, group_name, group_id)

  return(lookup)
}

################################################################################
# GROUP VALIDATION
################################################################################

#' Validate group definition before creation
#'
#' Checks if group metadata and members are valid
#'
#' @param group_name Group name (required, non-empty)
#' @param strategy_type Strategy type (must be in allowed list)
#' @param account_number Account number (required, non-empty)
#' @param members Tibble with columns: symbol, role (optional, can be empty)
#' @return List with valid (logical) and errors (character vector)
#' @noRd
validate_group_definition <- function(group_name, strategy_type, account_number, members = NULL) {
  errors <- c()

  # Check group name
  if (is.null(group_name) || nchar(trimws(group_name)) == 0) {
    errors <- c(errors, "Group name is required")
  }

  # Check strategy type
  valid_strategies <- get_strategy_types()
  if (is.null(strategy_type) || !strategy_type %in% valid_strategies) {
    errors <- c(errors, sprintf("Strategy type must be one of: %s", paste(valid_strategies, collapse = ", ")))
  }

  # Check account number
  if (is.null(account_number) || nchar(trimws(account_number)) == 0) {
    errors <- c(errors, "Account number is required")
  }

  # Check members if provided
  if (!is.null(members) && nrow(members) > 0) {
    # Validate required columns
    required_cols <- c("symbol", "role")
    missing_cols <- setdiff(required_cols, names(members))
    if (length(missing_cols) > 0) {
      errors <- c(errors, sprintf("Members must have columns: %s", paste(missing_cols, collapse = ", ")))
    } else {
      # Validate roles
      valid_roles <- get_position_roles()
      invalid_roles <- members %>%
        filter(!role %in% valid_roles) %>%
        pull(role) %>%
        unique()

      if (length(invalid_roles) > 0) {
        errors <- c(errors, sprintf("Invalid roles: %s. Must be one of: %s",
                                    paste(invalid_roles, collapse = ", "),
                                    paste(valid_roles, collapse = ", ")))
      }

      # Check for duplicate symbols
      duplicate_symbols <- members %>%
        group_by(symbol) %>%
        filter(n() > 1) %>%
        pull(symbol) %>%
        unique()

      if (length(duplicate_symbols) > 0) {
        errors <- c(errors, sprintf("Duplicate symbols in members: %s", paste(duplicate_symbols, collapse = ", ")))
      }

      # Warn if account_number doesn't match (if members have account_number column)
      if ("account_number" %in% names(members)) {
        mismatched_accounts <- members %>%
          filter(account_number != !!account_number) %>%
          pull(symbol)

        if (length(mismatched_accounts) > 0) {
          errors <- c(errors, sprintf("Members have different account than group: %s", paste(mismatched_accounts, collapse = ", ")))
        }
      }
    }
  }

  list(
    valid = length(errors) == 0,
    errors = errors
  )
}

################################################################################
# ID GENERATION
################################################################################

#' Generate unique group identifier
#'
#' Creates a unique ID based on strategy, account, and timestamp
#'
#' @param strategy_type Strategy type string
#' @param account_number Account number
#' @return Character unique group ID
#' @noRd
generate_group_id <- function(strategy_type, account_number) {
  # Sanitize strategy type for ID (remove spaces, special chars)
  strategy_clean <- strategy_type %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^A-Za-z0-9_]", "") %>%
    toupper()

  # Create timestamp component
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

  # Random suffix to ensure uniqueness
  random_suffix <- sample(1000:9999, 1)

  sprintf("%s_%s_%s_%s", strategy_clean, account_number, timestamp, random_suffix)
}

################################################################################
# NAME GENERATION
################################################################################

#' Generate standardized group name from members and strategy
#'
#' Creates consistent group names following the pattern:
#' - With options: "{Strategy} - {Ticker} - {Expiry} @ ${Strike}"
#' - Stock-only: "{Strategy} - {Ticker}"
#' - "Other" strategy: Returns NULL (requires manual naming)
#'
#' @param members Tibble with columns: symbol, role, (optional) allocated_quantity
#' @param strategy_type Strategy type string
#' @return Character group name or NULL for "Other" strategy
#' @noRd
generate_standard_group_name <- function(members, strategy_type) {
  # "Other" strategy requires manual naming (catch-all category)
  if (strategy_type == "Other") {
    return(NULL)
  }

  # Extract member components (DRY helper function)
  member_info <- extract_member_components(members)

  # Generate name based on whether options exist
  if (!is.null(member_info$expiry) && !is.null(member_info$strike)) {
    # Has option: full format with expiry and strike
    expiry_formatted <- format(member_info$expiry, "%b %Y")
    sprintf("%s - %s - %s @ $%.0f",
            strategy_type,
            member_info$ticker,
            expiry_formatted,
            member_info$strike)
  } else {
    # Stock-only or no option details: simple format
    sprintf("%s - %s", strategy_type, member_info$ticker)
  }
}

#' Extract ticker, expiry, and strike from members tibble
#'
#' Helper function to parse member information for naming.
#' Uses existing parse_option_symbol() and parse_option_details() functions.
#'
#' @param members Tibble with symbol and role columns
#' @return List with ticker, expiry (Date or NULL), strike (numeric or NULL)
#' @noRd
extract_member_components <- function(members) {
  # Get stock symbol (underlying ticker)
  stock_members <- members %>% filter(role == "underlying_stock")
  ticker <- if (nrow(stock_members) > 0) {
    stock_members$symbol[1]
  } else {
    # Fallback: extract from option symbol (supports both covered calls and CSPs)
    option_members <- members %>% filter(role %in% c("short_call", "short_put"))
    if (nrow(option_members) > 0) {
      parse_option_symbol(option_members$symbol[1])
    } else {
      members$symbol[1]  # Last resort
    }
  }

  # Get option details if short call or short put exists (supports CSPs)
  option_members <- members %>% filter(role %in% c("short_call", "short_put"))
  if (nrow(option_members) > 0) {
    option_details <- parse_option_details(option_members$symbol[1])
    list(
      ticker = ticker,
      expiry = option_details$expiry,
      strike = option_details$strike
    )
  } else {
    # Stock-only position
    list(
      ticker = ticker,
      expiry = NULL,
      strike = NULL
    )
  }
}

#' Get missing members from a group
#'
#' Compares group members to current positions and returns missing symbols
#'
#' @param group_members Tibble with group member definitions
#' @param current_positions Tibble with current positions
#' @return Character vector of missing symbols
#' @noRd
get_missing_members <- function(group_members, current_positions) {
  if (nrow(group_members) == 0) {
    return(character())
  }

  # Find members that don't exist in current positions
  missing <- group_members %>%
    anti_join(
      current_positions %>% select(account_number, symbol),
      by = c("account_number", "symbol")
    ) %>%
    pull(symbol)

  return(missing)
}

#' Auto-close stale position groups
#'
#' Detects groups where members are no longer in the positions table and
#' automatically closes them. Before closing, attempts to link any unlinked
#' trade activities for those tickers to capture final transactions.
#'
#' @param current_positions Tibble with current position data
#' @return Integer count of groups auto-closed
#' @noRd
auto_close_stale_groups <- function(current_positions) {
  # Get all open groups
  all_groups <- get_all_groups() %>%
    filter(status == "open")

  if (nrow(all_groups) == 0) {
    log_debug("Auto-Close: No open groups found")
    return(0)
  }

  closed_count <- 0

  for (i in seq_len(nrow(all_groups))) {
    group <- all_groups[i, ]
    group_id <- group$group_id

    # Get group members
    members <- get_group_members(group_id)

    if (nrow(members) == 0) {
      log_warn("Auto-Close: Group {group_id} has no members, skipping")
      next
    }

    # Check for missing members
    missing <- get_missing_members(members, current_positions)

    if (length(missing) > 0) {
      log_info("Auto-Close: Group {group_id} has missing members: {paste(missing, collapse = ', ')}")

      # Auto-link unlinked activities before closing
      # Get unique tickers from members (underlying stock ticker)
      tickers <- members %>%
        mutate(ticker = if_else(
          is_option_symbol(symbol),
          parse_option_symbol(symbol),
          symbol
        )) %>%
        pull(ticker) %>%
        unique() %>%
        na.omit()

      account_number <- members$account_number[1]

      # Link unlinked activities for each ticker
      for (ticker in tickers) {
        unlinked <- get_unlinked_activities_for_ticker(ticker, account_number)

        if (nrow(unlinked) > 0) {
          log_info("Auto-Close: Linking {nrow(unlinked)} unlinked activities for {ticker} to group {group_id}")

          for (j in seq_len(nrow(unlinked))) {
            link_activity_to_group(unlinked$activity_id[j], group_id)
          }
        }
      }

      # Auto-close the group
      auto_close_group(group_id)
      closed_count <- closed_count + 1
    }
  }

  if (closed_count > 0) {
    log_info("Auto-Close: Closed {closed_count} stale group{if (closed_count > 1) 's' else ''}")
  }

  return(closed_count)
}

#' Detect multi-group scenario from selected positions
#'
#' Analyzes selected positions to determine if multiple groups should be created
#' (e.g., one stock position with multiple option contracts having different expiries).
#' Returns suggested group structure with auto-calculated share allocation.
#'
#' @param selected_positions Tibble with position data
#' @return List with is_multi_group (logical) and suggested_groups (list of group specs)
#' @noRd
detect_multi_group_scenario <- function(selected_positions) {
  # Default: single group
  result <- list(
    is_multi_group = FALSE,
    suggested_groups = list()
  )

  if (nrow(selected_positions) == 0) {
    return(result)
  }

  # Check basic validation first
  validation <- validate_selection_for_grouping(selected_positions)
  if (!validation$valid) {
    return(result)
  }

  # Separate stock and options
  stock_positions <- selected_positions %>%
    filter(!is_option_symbol(symbol))

  option_positions <- selected_positions %>%
    filter(is_option_symbol(symbol))

  # Must have exactly 1 stock and at least 2 options for multi-group
  if (nrow(stock_positions) != 1 || nrow(option_positions) < 2) {
    return(result)
  }

  # Extract expiry dates AND strike prices from option symbols
  option_positions <- option_positions %>%
    mutate(
      expiry_date = map_chr(symbol, function(sym) {
        parsed <- parse_option_details(sym)
        if (!is.null(parsed$expiry)) {
          as.character(parsed$expiry)
        } else {
          NA_character_
        }
      }),
      strike_price = map_dbl(symbol, function(sym) {
        parsed <- parse_option_details(sym)
        if (!is.null(parsed$strike)) {
          parsed$strike
        } else {
          NA_real_
        }
      })
    ) %>%
    filter(!is.na(expiry_date) & !is.na(strike_price)) %>%
    mutate(expiry_strike_combo = paste(expiry_date, strike_price, sep = "_"))

  # If all options have same expiry+strike combo, it's single group
  unique_combos <- unique(option_positions$expiry_strike_combo)
  if (length(unique_combos) <= 1) {
    return(result)
  }

  # Multi-group detected! Create suggested groups
  ticker <- validation$ticker
  stock_qty <- stock_positions$open_quantity[1]

  suggested_groups <- map(unique_combos, function(combo) {
    # Get options for this expiry+strike combination
    combo_options <- option_positions %>%
      filter(expiry_strike_combo == combo)

    # Calculate allocated shares (each option = 100 shares)
    allocated_shares <- abs(sum(combo_options$open_quantity)) * 100

    # Generate group name with strike price
    expiry_date <- combo_options$expiry_date[1]
    strike <- combo_options$strike_price[1]
    expiry_formatted <- format(as.Date(expiry_date), "%b %Y")
    group_name <- sprintf("%s - %s @ $%.0f", ticker, expiry_formatted, strike)

    list(
      group_name = group_name,
      expiry_date = expiry_date,
      strike_price = strike,
      allocated_shares = allocated_shares,
      stock_symbol = stock_positions$symbol[1],
      option_symbols = combo_options$symbol
    )
  })

  # Validate total allocation doesn't exceed stock quantity
  total_allocated <- sum(map_dbl(suggested_groups, ~ .$allocated_shares))
  if (total_allocated > stock_qty) {
    log_warn("Multi-group detection: Allocated shares ({total_allocated}) exceed stock quantity ({stock_qty})")
    return(result)  # Return single group if allocation is invalid
  }

  result$is_multi_group <- TRUE
  result$suggested_groups <- suggested_groups
  result$stock_quantity <- stock_qty

  log_info("Multi-group detected: {length(suggested_groups)} groups for {ticker}")

  return(result)
}
