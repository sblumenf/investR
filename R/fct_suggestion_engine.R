#' Suggestion Engine
#'
#' Generates grouping suggestions from pattern matches. Creates suggestion
#' records with auto-generated names and reasoning for user review.
#'
#' @name suggestion-engine
#' @import dplyr
#' @importFrom purrr map_chr
#' @importFrom logger log_info log_warn log_debug
NULL

################################################################################
# PUBLIC ENTRY POINTS
################################################################################

#' Manually run pattern matching on existing activities
#'
#' Re-analyzes unprocessed activities without fetching new ones from Questrade.
#' Useful for re-generating suggestions after accidental rejection or data fixes.
#'
#' @return Integer count of new suggestions created
#' @export
run_pattern_matching <- function() {
  log_info("Manual Pattern Matching: Starting analysis of unprocessed activities")

  tryCatch({
    # Get unprocessed activities only (DRY - reuse existing function)
    unprocessed <- get_unprocessed_activities()

    if (nrow(unprocessed) == 0) {
      log_info("Manual Pattern Matching: No unprocessed activities found")
      return(0)
    }

    log_info("Manual Pattern Matching: Found {nrow(unprocessed)} unprocessed activities")

    # Get current positions for delayed covered call detection (DRY - reuse existing function)
    current_positions <- get_latest_positions()

    # Generate suggestions from patterns (DRY - reuse existing core function)
    suggestion_count <- generate_suggestions_from_patterns(unprocessed, current_positions)

    # Mark activities as processed (DRY - reuse existing function)
    activity_ids <- unprocessed$activity_id
    mark_activities_processed(activity_ids)

    log_info("Manual Pattern Matching: Generated {suggestion_count} new suggestions")
    return(suggestion_count)

  }, error = function(e) {
    log_error("Manual Pattern Matching: Failed - {e$message}")
    return(0)
  })
}

################################################################################
# INTERNAL SUGGESTION GENERATION
################################################################################

#' Generate suggestions from pattern matches
#'
#' Orchestrates pattern matching and suggestion creation for unprocessed
#' activities. This is the internal entry point called by both fetch_all_activities()
#' and run_pattern_matching().
#'
#' @param activities Tibble with unprocessed activities
#' @param current_positions Tibble with latest positions (for delayed covered calls)
#' @return Integer count of suggestions created
#' @noRd
generate_suggestions_from_patterns <- function(activities, current_positions) {
  if (nrow(activities) == 0) {
    log_debug("Suggestion Engine: No unprocessed activities to analyze")
    return(0)
  }

  suggestion_count <- 0

  # Pattern 1: Same-day stock + option
  same_day_matches <- detect_same_day_strategy(activities)

  if (nrow(same_day_matches) > 0) {
    # Check for ambiguous matches first
    ambiguous <- check_ambiguous_match(same_day_matches)

    if (nrow(ambiguous) > 0) {
      # Create clarification suggestions for ambiguous matches
      for (i in seq_len(nrow(ambiguous))) {
        create_ambiguous_suggestion(ambiguous[i, ], activities)
        suggestion_count <- suggestion_count + 1
      }
    }

    # Create suggestions for unambiguous matches
    unambiguous <- same_day_matches %>%
      anti_join(
        ambiguous %>%
          tidyr::unnest(stock_activity_ids) %>%
          tidyr::unnest(option_activity_ids),
        by = c("stock_activity_id" = "stock_activity_ids",
               "option_activity_id" = "option_activity_ids")
      )

    if (nrow(unambiguous) > 0) {
      for (i in seq_len(nrow(unambiguous))) {
        create_same_day_suggestion(unambiguous[i, ], activities)
        suggestion_count <- suggestion_count + 1
      }
    }
  }

  # Pattern 2: Dividend capture (overnight hold)
  div_capture_matches <- detect_dividend_capture(activities)

  if (nrow(div_capture_matches) > 0) {
    for (i in seq_len(nrow(div_capture_matches))) {
      create_dividend_capture_suggestion(div_capture_matches[i, ], activities)
      suggestion_count <- suggestion_count + 1
    }
  }

  # Pattern 3: Delayed covered call
  delayed_call_matches <- detect_delayed_covered_call(activities, current_positions)

  if (nrow(delayed_call_matches) > 0) {
    for (i in seq_len(nrow(delayed_call_matches))) {
      create_delayed_covered_call_suggestion(delayed_call_matches[i, ], activities)
      suggestion_count <- suggestion_count + 1
    }
  }

  # Pattern 4: Late dividends
  all_groups <- get_all_groups()

  if (nrow(all_groups) > 0) {
    closed_groups <- all_groups %>% filter(status == "closed")

    if (nrow(closed_groups) > 0) {
      late_div_matches <- detect_late_dividends(activities, closed_groups)

      if (nrow(late_div_matches) > 0) {
        for (i in seq_len(nrow(late_div_matches))) {
          create_late_dividend_suggestion(late_div_matches[i, ], activities)
          suggestion_count <- suggestion_count + 1
        }
      }
    }
  }

  log_info("Suggestion Engine: Generated {suggestion_count} suggestions")
  return(suggestion_count)
}

################################################################################
# SUGGESTION CREATION FUNCTIONS
################################################################################

#' Create suggestion for same-day strategy position
#'
#' @param match Single row from same_day_matches
#' @param activities Full activities tibble (to get transaction details)
#' @return Character suggestion_id
#' @noRd
create_same_day_suggestion <- function(match, activities) {
  # Get activity details
  stock <- activities %>% filter(activity_id == match$stock_activity_id)
  option <- activities %>% filter(activity_id == match$option_activity_id)

  # Generate name and reasoning
  group_name <- sprintf("%s - %s",
                       match$ticker,
                       format(as.Date(match$trade_date), "%b %Y"))
  reasoning <- sprintf(
    "Bought %g shares of %s and sold %g %s call option on %s (same day). This pattern indicates an implemented strategy position.",
    stock$quantity,
    match$ticker,
    abs(option$quantity),
    match$ticker,
    match$trade_date
  )

  # Save suggestion
  suggestion_id <- save_suggestion(
    pattern_type = "same_day_strategy",
    activity_ids = c(match$stock_activity_id, match$option_activity_id),
    suggested_group_name = group_name,
    suggested_strategy_type = "Dynamic Covered Calls",  # Default, user can change
    reasoning = reasoning
  )

  return(suggestion_id)
}

#' Create suggestion for dividend capture position
#'
#' @param match Single row from div_capture_matches
#' @param activities Full activities tibble
#' @return Character suggestion_id
#' @noRd
create_dividend_capture_suggestion <- function(match, activities) {
  # Get activity details
  buy <- activities %>% filter(activity_id == match$buy_activity_id)
  sell <- activities %>% filter(activity_id == match$sell_activity_id)

  # Generate name and reasoning
  group_name <- sprintf("%s Dividend Capture - %s to %s",
                       match$ticker,
                       format(as.Date(match$buy_date), "%b %d"),
                       format(as.Date(match$sell_date), "%b %d"))

  reasoning <- sprintf(
    "Bought %g shares of %s on %s, sold on %s (overnight hold). This pattern indicates a dividend capture strategy.",
    buy$quantity,
    match$ticker,
    match$buy_date,
    match$sell_date
  )

  # Save suggestion
  suggestion_id <- save_suggestion(
    pattern_type = "dividend_capture",
    activity_ids = c(match$buy_activity_id, match$sell_activity_id),
    suggested_group_name = group_name,
    suggested_strategy_type = "Weekly Dividend Capture",  # Default
    reasoning = reasoning
  )

  return(suggestion_id)
}

#' Create suggestion for delayed covered call
#'
#' @param match Single row from delayed_call_matches
#' @param activities Full activities tibble
#' @return Character suggestion_id
#' @noRd
create_delayed_covered_call_suggestion <- function(match, activities) {
  # Get activity details
  option <- activities %>% filter(activity_id == match$option_activity_id)

  # Generate name and reasoning
  group_name <- sprintf("%s Covered Call - %s",
                       match$ticker,
                       format(as.Date(option$trade_date), "%b %Y"))

  reasoning <- sprintf(
    "Sold %g %s call option on %s. You currently own %g shares at avg price $%.2f (purchased before transaction history). This pattern indicates a traditional covered call position.",
    abs(option$quantity),
    match$ticker,
    format(as.Date(option$trade_date), "%b %d"),
    match$position_quantity,
    match$position_avg_price
  )

  # Save suggestion
  suggestion_id <- save_suggestion(
    pattern_type = "delayed_covered_call",
    activity_ids = c(match$option_activity_id),
    suggested_group_name = group_name,
    suggested_strategy_type = "Dividend Aristocrats",  # Default
    reasoning = reasoning
  )

  return(suggestion_id)
}

#' Create suggestion for late dividend
#'
#' @param match Single row from late_div_matches
#' @param activities Full activities tibble
#' @return Character suggestion_id
#' @noRd
create_late_dividend_suggestion <- function(match, activities) {
  # Get activity details
  dividend <- activities %>% filter(activity_id == match$dividend_activity_id)

  # Generate reasoning
  reasoning <- sprintf(
    "Dividend of $%.2f received for %s on %s. This may belong to your closed dividend capture group.",
    abs(dividend$net_amount),
    match$ticker,
    match$dividend_date
  )

  # Save suggestion
  suggestion_id <- save_suggestion(
    pattern_type = "late_dividend",
    activity_ids = c(match$dividend_activity_id),
    suggested_group_name = paste("Link to group:", match$potential_group_id),
    suggested_strategy_type = "N/A",  # Not creating new group, just linking
    reasoning = reasoning
  )

  return(suggestion_id)
}

#' Create suggestion for ambiguous match
#'
#' @param match Single row from ambiguous matches
#' @param activities Full activities tibble
#' @return Character suggestion_id
#' @noRd
create_ambiguous_suggestion <- function(match, activities) {
  # Get all involved activity IDs
  stock_ids <- match$stock_activity_ids[[1]]
  option_ids <- match$option_activity_ids[[1]]
  all_ids <- c(stock_ids, option_ids)

  # Generate reasoning
  reasoning <- sprintf(
    "Found %d stock purchases and %d option sales for %s on %s. Multiple matches detected - please select which transactions belong together.",
    length(stock_ids),
    length(option_ids),
    match$ticker,
    match$trade_date
  )

  # Save suggestion
  suggestion_id <- save_suggestion(
    pattern_type = "ambiguous_match",
    activity_ids = all_ids,
    suggested_group_name = sprintf("%s - Needs Clarification", match$ticker),
    suggested_strategy_type = "Dynamic Covered Calls",  # Default
    reasoning = reasoning
  )

  return(suggestion_id)
}

################################################################################
# NAME GENERATION
################################################################################

#' Generate auto group name
#'
#' Creates a descriptive group name based on pattern type and transaction details
#'
#' @param pattern_type Type of pattern detected
#' @param ticker Stock ticker symbol
#' @param date_info Date or date range (character)
#' @return Character group name
#' @noRd
generate_group_name <- function(pattern_type, ticker, date_info) {
  if (pattern_type == "same_day_strategy") {
    sprintf("%s Strategy Position - %s", ticker, format(as.Date(date_info), "%b %d"))
  } else if (pattern_type == "dividend_capture") {
    # Date range handled in create_dividend_capture_suggestion
    sprintf("%s Dividend Capture", ticker)
  } else if (pattern_type == "delayed_covered_call") {
    sprintf("%s Covered Call", ticker)
  } else {
    sprintf("%s Position", ticker)
  }
}
