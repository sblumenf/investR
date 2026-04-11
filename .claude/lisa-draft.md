# Specification Draft: Covered Call Roll Transaction Support

*Interview in progress - Started: 2026-04-10*

## Overview
Support the "covered call roll" transaction pattern in investR: a Buy-to-Close (BTC) on an existing short call combined with a Sell-to-Open (STO) on a new call with a later expiry date. The WMT Dividend Aristocrat position triggered this work — the user rolled from a June 2027 covered call to a December 2027 or January 2028 covered call.

## Context (from prior research)
The research agent (April 9) identified that the codebase does NOT currently have roll detection, member update, or projection regeneration logic — the function names found (`detect_option_roll`, `update_group_option_member`, `regenerate_projections_after_roll`) may not exist yet. The spec must treat these as things to BUILD, not things to verify.

## Problem Statement
When a user rolls a covered call position (BTC old call + STO new call), the following must happen automatically once both transactions are linked to the position group:
1. The group's tracked option expiry date updates to the new call's expiry
2. Dividend projections are extended through the new expiry date
3. The STO premium appears as a separate actual cash flow for the month it was received
4. The cash flow projection module reflects the extended timeline

## User Decisions Captured

### Roll detection trigger
- Rolls are detected when transactions are linked to a group (same as all other transaction types)
- NO extra pairing/linking UI step needed — consistent with how other transaction types work
- If BTC and STO are linked to the same group and both reference the same underlying, roll logic fires
- No same-day requirement enforced by the user — just needs to work when both are linked

### Cash flow display
- Each STO creates its own separate, dated actual cash flow line item
- Full audit trail: original STO at inception + roll STO in April 2026 both visible separately
- No aggregation into a single cumulative total

### Screens affected
- Group card: expiry date + dividend projections + cash flows (primary)
- Cash flow projection module: must update to reflect extended timeline
- Other screens (risk dashboard, home dashboard): calculated on the fly from group data — verify they pick up changes automatically, do not build special handling

## Key Technical Facts (from research)
- Option expiry is stored implicitly in the option symbol in `position_group_members` (role = 'short_call')
- `parse_option_details(symbol)` extracts strike and expiry from the symbol string
- Dividend projections are rows in `position_group_cash_flows` with event_type = 'dividend' and status = 'projected'
- Option premiums received are rows with event_type = 'option_premium' and status = 'actual'
- The `account_activities` table stores raw transactions; `position_group_cash_flows` stores processed events
- Deduplication key for activities: account_number + description + trade_date + action + quantity + net_amount
- Blank option symbols can arrive from Questrade — enrichment function exists

## Open Questions
- Does `detect_option_roll` actually exist in the codebase, or was the research agent hallucinating function names? MUST verify before implementation.
- Which file/function actually triggers projection regeneration when a transaction is linked to a group?
- Does the dividend staleness check threshold need to be documented/configurable?

---
*Notes accumulated below*
