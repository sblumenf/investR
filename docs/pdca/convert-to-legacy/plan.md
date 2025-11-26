# Convert to Legacy - PLAN Phase

## Problem Statement

### Initial Hypothesis

When a covered call option expires worthless (stock price below strike at expiration), the system's projected cash flows become invalid:

- **Projected option_gain**: Assumes stock will be called away at strike price → **INVALID** when call expires worthless
- **Projected dividends**: May no longer be relevant if user wants to manage position manually

**User Pain Point:** No way to remove invalid projections without deleting the entire position group.

### Desired Outcome

Enable users to convert Dynamic Covered Calls positions to Legacy mode, removing automatic projections while preserving actual transaction history.

## Expected Outcomes (Quantitative)

### Success Metrics

1. **Functional Correctness**
   - 100% of projected events deleted during conversion
   - 100% of actual events preserved during conversion
   - Zero partial conversions (atomic transactions)

2. **User Experience**
   - Conversion completes in <1 second for typical position
   - Clear confirmation modal showing exactly what will be deleted
   - Success rate >99% (failures only from database errors)

3. **Code Quality**
   - Test coverage >90% for conversion functions
   - Zero SQL injection vulnerabilities
   - All edge cases handled gracefully

4. **Data Integrity**
   - Audit trail created for every conversion
   - Transaction rollback on any error
   - No orphaned database records

### Measurable Goals

| Metric | Target | Measurement Method |
|--------|--------|-------------------|
| Conversion speed | <1 second | Benchmark with 10 projected events |
| Test coverage | >90% | Test suite execution |
| User errors | <1% | Error rate from validation failures |
| Data consistency | 100% | Transaction integrity tests |

## Implementation Approach

### Architecture Design

**Three-Layer Pattern:**

1. **Database Layer** - Core business logic
   - Validation function: `can_convert_to_legacy()`
   - Preview function: `preview_legacy_conversion()`
   - Conversion function: `convert_to_legacy_covered_call()`

2. **UI Layer** - User interface
   - Button on group cards (conditional visibility)
   - Confirmation modal with preview
   - Success/error notifications

3. **Test Layer** - Quality assurance
   - Unit tests for each function
   - Integration tests for full workflow
   - Edge case and failure scenario tests

### Database Design

**Tables Modified:**
- `position_groups` - Update `strategy_type` field
- `position_group_cash_flows` - Delete projected events
- `projection_recalculations` - Insert audit log entry

**Transaction Strategy:**
- Single atomic transaction for all changes
- Validation before any modifications
- Rollback on any error
- Commit only when all steps succeed

### UI/UX Design

**Button Placement:**
- Location: Open group cards, action button row
- Visibility: Only for Dynamic Covered Calls with projected events
- Style: Info color (blue), archive icon
- Label: "Convert to Legacy"

**Confirmation Flow:**
1. User clicks button
2. System shows modal with:
   - Explanation of what will happen
   - Breakdown of events to be deleted (by type)
   - Total amount to be removed
   - Warning about permanence
3. User confirms or cancels
4. System executes conversion
5. Success notification + card refresh

### Testing Strategy

**Test Categories:**
1. **Validation Tests** - All eligibility rules
2. **Preview Tests** - Correct breakdown calculation
3. **Conversion Tests** - Successful execution
4. **Failure Tests** - Rollback and error handling
5. **Integration Tests** - End-to-end workflow
6. **Edge Cases** - Unusual data scenarios
7. **Safety Tests** - Transaction integrity

**Test Data:**
- Valid Dynamic Covered Calls with projections
- Already-Legacy groups (should reject)
- Closed groups (should reject)
- Groups without projections (should reject)
- Groups with mixed actual/projected events
- Non-existent group IDs

## Risks and Mitigation

### Technical Risks

| Risk | Probability | Impact | Mitigation Strategy |
|------|-------------|--------|---------------------|
| **Partial conversion** | Medium | High | Use database transactions, test rollback thoroughly |
| **Data loss** | Low | Critical | Preserve actual events, comprehensive tests, audit logging |
| **SQL injection** | Low | Critical | Parameterized queries only, code review |
| **Race conditions** | Low | Medium | Single-threaded Shiny, database locks |
| **Performance issues** | Low | Low | Indexed queries, benchmark with large datasets |

### User Experience Risks

| Risk | Probability | Impact | Mitigation Strategy |
|------|-------------|--------|---------------------|
| **Accidental conversion** | Medium | Medium | Confirmation modal with detailed preview, no undo |
| **Confusion about Legacy mode** | High | Low | Clear documentation, modal explanation |
| **Lost projected data** | Low | Low | Show what will be deleted, warn about permanence |
| **Button not visible** | Low | Low | Clear visibility rules, test on various scenarios |

### Mitigation Actions

**Before Implementation:**
- Review database schema for constraints
- Design transaction flow with rollback points
- Create comprehensive test plan
- Document expected behavior for edge cases

**During Implementation:**
- Write tests first (TDD approach)
- Use parameterized SQL queries
- Log all operations for debugging
- Handle all error cases explicitly

**After Implementation:**
- Manual testing with real portfolio data
- Performance benchmarking
- Code review for security issues
- User documentation with screenshots

## Dependencies

### Technical Dependencies
- DuckDB transaction support (ACID properties)
- Shiny reactive system (modal management)
- Existing portfolio groups infrastructure
- Cash flow projection system

### Feature Dependencies
- Portfolio groups must exist
- Cash flow events must be tracked
- Group status (open/closed) must be implemented
- Strategy types must support Dynamic/Legacy distinction

### Documentation Dependencies
- User guide for feature usage
- Technical architecture documentation
- PDCA documentation (this document)
- Pattern documentation for reusability

## Timeline Estimate

### Development Phases

| Phase | Estimated Time | Deliverables |
|-------|---------------|--------------|
| **Database Functions** | 2 hours | 3 functions + basic tests |
| **UI Implementation** | 2 hours | Button + modal + handlers |
| **Comprehensive Testing** | 3 hours | 19 tests covering all scenarios |
| **Documentation** | 2 hours | User guide + technical docs |
| **Manual Testing** | 1 hour | Real data validation |
| **Total** | **10 hours** | Complete feature |

### Actual Implementation
- **Date:** January 25, 2025
- **Duration:** ~8 hours (20% faster than estimate)
- **Iterations:** 2 (initial implementation + test refinement)

## Success Criteria

### Definition of Done

Feature is complete when:

1. ✓ All three database functions implemented and tested
2. ✓ UI button appears only for eligible groups
3. ✓ Confirmation modal shows accurate preview
4. ✓ Conversion executes atomically (all or nothing)
5. ✓ Actual events preserved, projected events deleted
6. ✓ Audit log entry created for every conversion
7. ✓ Test suite passes with >90% coverage
8. ✓ User guide documentation complete
9. ✓ Technical architecture documentation complete
10. ✓ Manual testing validates behavior

### Acceptance Tests

**User Story:** As a portfolio manager, I want to convert expired covered call positions to Legacy mode so that invalid projections don't mislead my analysis.

**Acceptance Criteria:**

**Given** an open Dynamic Covered Calls position with projected events
**When** I click "Convert to Legacy" and confirm
**Then**
- Strategy type changes to "Legacy Covered Call"
- All projected events are deleted
- All actual events are preserved
- Audit log entry is created
- Success notification is shown
- Card refreshes with new status

**Given** a position already in Legacy mode
**When** I view the position card
**Then** "Convert to Legacy" button is not displayed

**Given** a closed position
**When** I view the position card
**Then** "Convert to Legacy" button is not displayed

**Given** a Dynamic position with no projected events
**When** I view the position card
**Then** "Convert to Legacy" button is not displayed

## Alternatives Considered

### Option 1: Manual Projection Deletion
**Approach:** Allow users to delete individual projected events

**Pros:**
- More granular control
- Can keep some projections, delete others

**Cons:**
- Complex UI (multiple selection)
- Easy to make mistakes
- Doesn't solve strategy type confusion
- More code to maintain

**Decision:** Rejected - all-or-nothing conversion is clearer

### Option 2: Strategy Type Toggle
**Approach:** Simple toggle switch between Dynamic and Legacy

**Pros:**
- Very simple UI
- Reversible operation

**Cons:**
- Doesn't handle projected events
- Unclear what happens to existing projections
- Potentially confusing behavior

**Decision:** Rejected - need to handle projections explicitly

### Option 3: Automatic Conversion on Expiration
**Approach:** System auto-converts when option expires worthless

**Pros:**
- No user action required
- Prevents invalid projections automatically

**Cons:**
- Requires option expiration tracking
- May convert when user wants to roll position
- Reduces user control
- More complex logic

**Decision:** Rejected - user should control conversion timing

### Chosen Approach: Explicit Conversion with Preview
**Rationale:**
- Clear user intent (explicit button click)
- Preview prevents surprises
- Atomic transaction prevents partial state
- Reversible via close + recreate if needed
- Simple implementation

## References

### Related Features
- Portfolio Groups system
- Cash flow projection system
- Position group status management
- Strategy type definitions

### Similar Implementations
- Close Group feature (atomic transaction pattern)
- Delete Group feature (confirmation modal pattern)
- Cash flow deletion (preserve actual, delete projected)

### Technical Resources
- DuckDB transaction documentation
- Shiny modal dialog documentation
- R database best practices (parameterized queries)
- Test-driven development patterns
