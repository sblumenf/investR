#!/usr/bin/env Rscript
#
# Backfill Income Projections for Existing Position Groups
#
# This script generates income projections for position groups that were
# created before the income projection feature was implemented.
#
# Usage: Rscript dev/backfill_income_projections.R

library(dplyr)

# Load package
devtools::load_all()

# Get all existing groups
all_groups <- get_all_groups()

if (nrow(all_groups) == 0) {
  message("No position groups found")
  quit(save = "no", status = 0)
}

message(sprintf("Found %d position groups", nrow(all_groups)))

# Counter for results
success_count <- 0
skip_count <- 0
fail_count <- 0

# Process each group
for (i in 1:nrow(all_groups)) {
  group <- all_groups[i, ]

  message(sprintf("\n[%d/%d] Processing: %s (%s)",
                  i, nrow(all_groups),
                  group$group_name,
                  group$group_id))

  # Check if projections already exist
  existing_flows <- get_group_cash_flows(group$group_id)

  if (nrow(existing_flows) > 0) {
    message(sprintf("  ⊘ Skipping - already has %d cash flow events", nrow(existing_flows)))
    skip_count <- skip_count + 1
    next
  }

  # Get group members
  members <- get_group_members(group$group_id)

  if (nrow(members) == 0) {
    message("  ⊘ Skipping - no members found")
    skip_count <- skip_count + 1
    next
  }

  # Generate projections
  result <- generate_initial_projections(
    group_id = group$group_id,
    members = members,
    account_number = group$account_number
  )

  if (result) {
    # Get newly created projections
    new_flows <- get_group_cash_flows(group$group_id)
    message(sprintf("  ✓ Success - generated %d cash flow events", nrow(new_flows)))
    success_count <- success_count + 1
  } else {
    message("  ✗ Failed to generate projections")
    fail_count <- fail_count + 1
  }
}

# Summary
message("\n" = paste(rep("=", 60), collapse = ""))
message("BACKFILL SUMMARY")
message(paste(rep("=", 60), collapse = ""))
message(sprintf("Total groups:    %d", nrow(all_groups)))
message(sprintf("Success:         %d", success_count))
message(sprintf("Skipped:         %d", skip_count))
message(sprintf("Failed:          %d", fail_count))
message(paste(rep("=", 60), collapse = ""))
