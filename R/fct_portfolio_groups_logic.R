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
      sym
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
#' @param symbol Position symbol
#' @param account_number Account number
#' @return Character group name or NULL if not in any group
#' @deprecated Use build_group_name_lookup() for batch operations.
#'             This function is inefficient when called repeatedly.
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
      group_name = character()
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
      group_name = character()
    ))
  }

  # Join to get group names
  lookup <- all_members %>%
    left_join(
      all_groups %>% select(group_id, group_name, account_number),
      by = "group_id"
    ) %>%
    select(account_number = account_number.y, symbol, group_name)

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
