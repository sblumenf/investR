# Convert to Legacy Covered Call - User Guide

## Overview

The **Convert to Legacy** feature allows you to switch a Dynamic Covered Calls position to Legacy Covered Call mode when the automatic projections are no longer valid or useful.

### When to Use This Feature

Convert to Legacy when:

- **Call option expired worthless** - Stock price stayed below strike, so the "profit at expiration" projection is invalid
- **You want manual control** - Prefer to track cash flows only when they actually occur
- **Projections are misleading** - Automatic dividend/option gain estimates no longer reflect reality
- **Position evolved** - Your strategy changed from systematic covered calls to opportunistic management

### What's the Difference?

| Feature | Dynamic Covered Calls | Legacy Covered Call |
|---------|----------------------|---------------------|
| **Dividend Projections** | Automatic quarterly/monthly estimates | Manual tracking only |
| **Option Gain Projection** | Assumes assignment at expiration | No automatic projection |
| **Cash Flow Display** | Shows both projected and actual | Shows actual events only |
| **Best For** | Active covered call positions | Expired/manual positions |

## Step-by-Step Instructions

### 1. Locate the Position Group

Navigate to the **Portfolio Groups** page and find the Dynamic Covered Calls position you want to convert.

**Requirements:**
- Group must have "Dynamic Covered Calls" strategy type
- Group must be in "OPEN" status
- Group must have projected cash flow events

### 2. Click "Convert to Legacy" Button

On the position card, locate the **Convert to Legacy** button (blue, with archive icon).

**Button Location:** Bottom right of the card, between "Analyze Risk" and "Close Group" buttons.

**If button is missing:**
- Group is already Legacy Covered Call (no conversion needed)
- Group is closed (cannot convert closed positions)
- Group has no projected events (nothing to remove)

### 3. Review Confirmation Modal

The system displays a confirmation dialog showing:

#### What Will Happen
- Strategy type changes from "Dynamic Covered Calls" → "Legacy Covered Call"
- All projected cash flow events permanently deleted
- No new automatic projections generated
- Manual tracking required for future dividends/premiums

#### Events to Be Deleted
The modal shows a detailed breakdown by event type:

**Example:**
```
Events to be deleted:
• Profit at Expiration: 1 event totaling $500.00
• Dividend Payments: 4 events totaling $160.00

Total projected amount: $660.00
```

**Event Types:**
- **Profit at Expiration** - Projected option gain assuming assignment
- **Dividend Payments** - Future estimated dividend payments
- **Option Premiums** - Any projected premium income

### 4. Confirm Conversion

**To proceed:**
- Click **"Convert to Legacy"** button (blue, bottom right)
- Changes take effect immediately
- Success notification appears
- Card refreshes to show new Legacy status

**To cancel:**
- Click **"Cancel"** button or click outside modal
- No changes made to position

### 5. Verify Conversion Success

After conversion:

**✓ Strategy badge changes:** "Dynamic Covered Calls" → "Legacy Covered Call"

**✓ Projected events removed:** Cash Flows section now shows only actual (realized) events

**✓ Button disappears:** "Convert to Legacy" button no longer displayed

**✓ Actual events preserved:** All realized dividends, option premiums, and transactions remain intact

## What Gets Deleted

### Permanently Removed
- All projected dividend payments
- All projected option gains (profit at expiration)
- Any other projected cash flow events
- Total count and amounts shown in confirmation

### Preserved Data
- All actual (realized) cash flow events
- All transaction history
- All group members
- Cost basis calculations
- Account information
- Group creation date and metadata

## Common Scenarios

### Scenario 1: Call Expired Worthless
**Situation:** Sold AAPL $150 call, stock at $145 at expiration

**Problem:** System projects $500 gain assuming assignment, but call expired worthless

**Solution:**
1. Let call expire (no action needed in broker)
2. Convert to Legacy in investR
3. Manually record new option premium when you sell next call

### Scenario 2: Rolling Strategy Changed
**Situation:** Initially planned systematic covered calls, now managing opportunistically

**Problem:** Don't want automatic projections for positions managed case-by-case

**Solution:**
1. Convert to Legacy for manual control
2. Record dividends when received
3. Record option premiums when new calls are sold

### Scenario 3: Dividend Policy Changed
**Situation:** Company cut dividend, automatic projections now wrong

**Problem:** Projected dividends based on old dividend rate

**Solution:**
1. Convert to Legacy to remove old projections
2. Wait for company to announce new dividend policy
3. Manually record dividends at new rate when received

## After Conversion

### How Legacy Mode Works

**No Automatic Projections:**
- System will NOT generate dividend estimates
- System will NOT project option assignment gains
- You must manually record cash flows as they occur

**Manual Recording Required:**

| Event Type | When to Record | How to Record |
|------------|---------------|---------------|
| Dividend Received | After payment date | Cash flow event: "dividend" |
| New Option Sold | After sell order fills | Activity already recorded from broker import |
| Option Expired | After expiration date | No recording needed (just let it expire) |

### How to Return to Dynamic Mode

**Currently not supported** - Conversion is one-way only.

**Workaround:** Create a new Dynamic Covered Calls group:
1. Close the Legacy group
2. Create new Dynamic Covered Calls group
3. System will generate fresh projections

## Frequently Asked Questions

### Q: Can I undo the conversion?
**A:** No, conversion is permanent. However, you can close the Legacy group and create a new Dynamic group if needed.

### Q: Will this affect my P&L calculations?
**A:** No. P&L is calculated from actual transactions only. Projected events never affect realized returns.

### Q: What happens to dividends I already received?
**A:** They are preserved. Only *projected* (future) events are deleted. All *actual* (realized) events remain.

### Q: Can I convert a closed group?
**A:** No. Only open groups can be converted. Closed groups already have finalized P&L.

### Q: Will I lose transaction history?
**A:** No. All trades, stock purchases, option sales, etc. are preserved. Only projected cash flows are removed.

### Q: How do I record dividends in Legacy mode?
**A:** Currently via direct database entry or broker import. UI for manual cash flow entry coming in future release.

### Q: What if I convert by accident?
**A:** Contact support or manually re-add projected events via database if needed. Best practice: review confirmation modal carefully before clicking "Convert to Legacy".

### Q: Can I convert other strategy types?
**A:** No. Only "Dynamic Covered Calls" positions can be converted to "Legacy Covered Call". Other strategies don't have this option.

## Troubleshooting

### Button Not Appearing

**Problem:** "Convert to Legacy" button not visible on card

**Solutions:**
1. **Check strategy type** - Must be "Dynamic Covered Calls" (not "Legacy Covered Call" or "Other")
2. **Check status** - Group must be "OPEN" (not "CLOSED")
3. **Check projections** - Group must have at least one projected cash flow event
4. **Refresh page** - Browser cache may be stale

### Conversion Fails

**Problem:** Clicked "Convert to Legacy" but error occurred

**Solutions:**
1. **Check logs** - Error details in application logs
2. **Verify permissions** - Database must be writable
3. **Check for concurrent changes** - Another user may have modified group
4. **Retry** - Refresh page and try again

**If problem persists:** Contact support with group ID and error message.

## Related Documentation

- [Portfolio Groups Overview](./portfolio-groups-overview.md)
- [Dynamic vs Legacy Strategies](./strategy-types.md)
- [Cash Flow Tracking](./cash-flow-tracking.md)
- [Technical Architecture](../technical/convert-to-legacy-architecture.md)
