# Specification Draft: IV Skew Collar Screener

*Interview in progress - Started: 2026-02-26*

## Overview
Add a new collar variant "IV Skew Screener" to the existing collar strategy dropdown. This variant:
1. Fetches equity holdings from the iShares Russell 1000 ETF holdings CSV
2. Filters to Asset Class = Equity only
3. For each equity, fetches ATM call IV and ATM put IV at the closest expiry to 45 days (within 45-60 day window)
4. Computes call IV / put IV ratio per stock
5. Returns the top 20 stocks by highest IV ratio
6. Feeds those 20 into the standard collar analysis pipeline with 45-60 day target expiry
7. Displays standard collar result cards (same as other variants)

## Problem Statement
The user wants to identify collar opportunities where call IV is elevated relative to put IV (favorable skew for selling covered calls + buying protective puts). Screening the Russell 1000 universe by this ratio surfaces the most attractive collar candidates.

## Scope

### In Scope
<!-- Explicit list of what IS included in this implementation -->
- [To be filled during interview]

### Out of Scope
<!-- Explicit list of what is NOT included - future work, won't fix, etc. -->
- [To be filled during interview]

## User Stories

<!--
IMPORTANT: Each story must be small enough to complete in ONE focused coding session.
If a story is too large, break it into smaller stories.

Format each story with VERIFIABLE acceptance criteria:

### US-1: [Story Title]
**Description:** As a [user type], I want [action] so that [benefit].

**Acceptance Criteria:**
- [ ] [Specific, verifiable criterion - e.g., "API returns 200 for valid input"]
- [ ] [Another verifiable criterion - e.g., "Error message displayed for invalid email"]
- [ ] Typecheck/lint passes
- [ ] [If UI] Verify in browser

BAD criteria (too vague): "Works correctly", "Is fast", "Handles errors"
GOOD criteria: "Response time < 200ms", "Returns 404 for missing resource", "Form shows inline validation"
-->

[To be filled during interview]

## Technical Design

### Data Model
[To be filled during interview]

### API Endpoints
[To be filled during interview]

### Integration Points
[To be filled during interview]

## User Experience

### User Flows
[To be filled during interview]

### Edge Cases
[To be filled during interview]

## Requirements

### Functional Requirements
<!--
Use FR-IDs for each requirement:
- FR-1: [Requirement description]
- FR-2: [Requirement description]
-->
[To be filled during interview]

### Non-Functional Requirements
<!--
Performance, security, scalability requirements:
- NFR-1: [Requirement - e.g., "Response time < 500ms for 95th percentile"]
- NFR-2: [Requirement - e.g., "Support 100 concurrent users"]
-->
[To be filled during interview]

## Implementation Phases

<!-- Break work into 2-4 incremental milestones Ralph can complete one at a time -->

### Phase 1: [Foundation/Setup]
- [ ] [Task 1]
- [ ] [Task 2]
- **Verification:** `[command to verify phase 1]`

### Phase 2: [Core Implementation]
- [ ] [Task 1]
- [ ] [Task 2]
- **Verification:** `[command to verify phase 2]`

### Phase 3: [Integration/Polish]
- [ ] [Task 1]
- [ ] [Task 2]
- **Verification:** `[command to verify phase 3]`

<!-- Add Phase 4 if needed for complex features -->

## Definition of Done

This feature is complete when:
- [ ] All acceptance criteria in user stories pass
- [ ] All implementation phases verified
- [ ] Tests pass: `[verification command]`
- [ ] Types/lint check: `[verification command]`
- [ ] Build succeeds: `[verification command]`

## Ralph Loop Command

<!-- Generated at finalization with phases and escape hatch -->

```bash
/ralph-loop "Implement I want your help to implement a variant to the existing collar strategy. I want this variant to be an option in the doptdown of the existing list. I will rpovide you with a list of stocks to analyze as well as criteria as you ask for them per spec at docs/specs/i-want-your-help-to-implement-a-variant-to-the-existing-coll.md

PHASES:
1. [Phase 1 name]: [tasks] - verify with [command]
2. [Phase 2 name]: [tasks] - verify with [command]
3. [Phase 3 name]: [tasks] - verify with [command]

VERIFICATION (run after each phase):
- [test command]
- [lint/typecheck command]
- [build command]

ESCAPE HATCH: After 20 iterations without progress:
- Document what's blocking in the spec file under 'Implementation Notes'
- List approaches attempted
- Stop and ask for human guidance

Output <promise>COMPLETE</promise> when all phases pass verification." --max-iterations 30 --completion-promise "COMPLETE"
```

## Open Questions
[To be filled during interview]

## Implementation Notes
[To be filled during interview]

---
*Interview notes will be accumulated below as the interview progresses*
---

