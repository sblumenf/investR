# Specification Draft: dividends staleness

*Interview in progress - Started: 2026-03-04*

## Overview
[To be filled during interview]

## Problem Statement
[To be filled during interview]

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
/ralph-loop "Implement dividends staleness per spec at docs/specs/dividends-staleness.md

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

## Interview Notes

### Decisions Made
1. **Shared helper**: Create `is_dividend_stale(dividends)` in `utils_market_data.R` — reusable by all current and future strategies
2. **Visibility**: Excluded stocks get a summary log message (e.g., "Filtered out 3 stocks with stale dividends: HE, XIFR, ...")
3. **Scope**: All existing and future variants of Aristocrats and Collar strategies

### Affected Pipelines (from research)
1. **Aristocrats** (all variants): `get_stock_data()` in `fct_aristocrats_analysis.R:338`
2. **Collar** (all variants): `analyze_collar_single()` in `fct_collar_analysis.R:35`
3. **ETF Collar**: `fct_etf_collar_analysis.R` — calls `analyze_collar_single()`
4. **ETF Covered Calls**: uses `get_stock_data()` via `utils_covered_calls_shared.R`

### TBD
- Threshold configuration
- Non-dividend collar variants behavior
- Whether to also update the existing projection guard to use the shared helper

