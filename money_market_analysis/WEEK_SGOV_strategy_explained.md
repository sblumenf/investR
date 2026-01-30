# The WEEK/SGOV Dividend Capture Strategy: A Beginner's Guide

## What Is This Strategy?

This strategy switches between two investment funds at specific times to try to capture more dividend income. However, it's important to understand both the benefits AND the costs before deciding if it's right for you.

---

## First, Let's Understand Some Basic Concepts

### What's an ETF?

An ETF (Exchange-Traded Fund) is like a basket of investments you can buy and sell like a regular stock. The two ETFs we use are:

```
+------------------+-------------------------------------------+
|       WEEK       |                   SGOV                    |
+------------------+-------------------------------------------+
| Pays you money   | Pays you money                            |
| EVERY WEEK       | ONCE A MONTH                              |
| (small amounts)  | (bigger amounts)                          |
+------------------+-------------------------------------------+
```

Both hold super-safe U.S. Treasury bills (government IOUs), so your money is very secure.

---

### What's a Dividend?

A dividend is money the fund pays you just for owning it. It's like interest the fund pays you for letting it hold your money.

```
    You own shares of WEEK
              |
              v
    +------------------+
    |   WEEK Fund      |
    |                  |
    |  "Thanks for     |
    |   investing!     |
    |   Here's $47"    |
    +------------------+
              |
              v
    Money deposited in your account!
```

---

### What's an "Ex-Dividend Date"?

This is the **cutoff date** to qualify for the dividend. If you own shares on this date, you get the dividend. If you don't, you miss it.

```
Timeline:
=========

   March 30         March 31          April 1
      |                |                 |
      v                v                 v
 +---------+     +-----------+     +-----------+
 | Regular |     | EX-DIV    |     | Dividend  |
 |   Day   |     |   DATE    |     |   PAID    |
 +---------+     +-----------+     +-----------+
                       ^
                       |
              Own shares on THIS day
              to get paid on April 1!
```

---

## IMPORTANT: The Ex-Dividend Price Drop

Here's something many beginners don't realize: **stock prices DROP on the ex-dividend date by approximately the dividend amount.**

Why? Because the dividend is "baked into" the price before ex-div. Once it's paid out, the stock is worth less.

```
EXAMPLE: SGOV in March 2025
===========================

March 28 (day before ex-div):
  SGOV price: $100.67  <-- Price INCLUDES upcoming ~$0.35/share dividend

March 31 (ex-div date):
  You qualify for dividend!

April 4 (when you sell):
  SGOV price: $100.41  <-- Price DROPPED because dividend paid out

Price drop per share: $100.67 - $100.41 = $0.26
```

**This means you don't "make" the full dividend - part of it is offset by the price drop.**

---

## The Strategy Explained

### The Basic Idea

```
WEEK weekly ex-div:   W   W   W   W   W   W   W   W   W
                      |   |   |   |   |   |   |   |   |
Timeline:    ----+----+---+---+---+---+---+---+---+---+----
                 |               |                    |
SGOV monthly:    S               S                    S

LEGEND:
  W = WEEK ex-dividend day (happens every week)
  S = SGOV ex-dividend day (last business day of each month)
```

We try to:
1. Hold WEEK most of the time to collect weekly dividends
2. Switch to SGOV before its monthly ex-div to collect the larger monthly dividend
3. Switch back to WEEK before the next weekly ex-div

---

### The Overlap Problem

Three times during our analysis period, WEEK and SGOV have their ex-dividend on the **same day**:

```
OVERLAP DATES (2025):
=====================
  - March 31
  - June 30
  - August 29

On these days, you can only be in ONE fund.
We choose SGOV because its dividend is larger (~$0.35 vs ~$0.08).

THIS MEANS: We MISS 3 WEEK dividends!
```

So we don't get "all WEEK dividends plus all SGOV dividends" - we have to choose on overlap days.

---

## What Actually Happened: Real Numbers

### Investment Period
- **Start:** March 6, 2025
- **End:** December 2, 2025
- **Initial Investment:** $60,000

### The Switches

We made 18 trades (9 round trips to SGOV and back):

```
+------------+----------------------------------------------+
|    Date    |               What Happened                  |
+------------+----------------------------------------------+
| Mar 6      | BUY WEEK @ $100.025 (599.85 shares)          |
| Mar 28     | SELL WEEK @ $100.065 --> BUY SGOV @ $100.67  |
| Apr 4      | SELL SGOV @ $100.41 --> BUY WEEK @ $100.08   |
| Apr 29     | SELL WEEK @ $100.01 --> BUY SGOV @ $100.66   |
| May 2      | SELL SGOV @ $100.40 --> BUY WEEK @ $100.065  |
| May 29     | SELL WEEK @ $100.025 --> BUY SGOV @ $100.68  |
| May 30     | SELL SGOV @ $100.71 --> BUY WEEK @ $100.06   |
| Jun 27     | SELL WEEK @ $100.073 --> BUY SGOV @ $100.69  |
| Jul 3      | SELL SGOV @ $100.42 --> BUY WEEK @ $100.056  |
| Jul 30     | SELL WEEK @ $100.04 --> BUY SGOV @ $100.71   |
| Aug 1      | SELL SGOV @ $100.39 --> BUY WEEK @ $100.073  |
| Aug 28     | SELL WEEK @ $100.07 --> BUY SGOV @ $100.68   |
| Sep 5      | SELL SGOV @ $100.44 --> BUY WEEK @ $100.107  |
| Sep 29     | SELL WEEK @ $100.02 --> BUY SGOV @ $100.70   |
| Oct 3      | SELL SGOV @ $100.42 --> BUY WEEK @ $100.05   |
| Oct 30     | SELL WEEK @ $100.025 --> BUY SGOV @ $100.69  |
| Oct 31     | SELL SGOV @ $100.72 --> BUY WEEK @ $100.055  |
| Nov 26     | SELL WEEK @ $100.00 --> BUY SGOV @ $100.64   |
| Nov 28     | SELL SGOV @ $100.67 --> BUY WEEK @ $100.03   |
+------------+----------------------------------------------+
```

### Dividends Actually Received

```
WEEK Dividends: 36 payments (NOT 39 - we missed 3 on overlap days)
=================================================================
| Date       | Shares   | Per Share | Amount  |
|------------|----------|-----------|---------|
| Mar 11     | 599.8500 | $0.068    | $40.79  |
| Mar 18     | 599.8500 | $0.080    | $47.99  |
| Mar 25     | 599.8500 | $0.079    | $47.39  |
| Apr 8      | 598.2112 | $0.078    | $46.66  |
| Apr 15     | 598.2112 | $0.078    | $46.66  |
| Apr 22     | 598.2112 | $0.078    | $46.66  |
| Apr 29     | 598.2112 | $0.079    | $47.26  |
| ...        | ...      | ...       | ...     |
| Dec 2      | 589.2695 | $0.071    | $41.84  |
|------------|----------|-----------|---------|
| TOTAL      |          |           |$1,641.81|


SGOV Dividends: 9 payments (one for each month)
===============================================
| Ex-Div Date | Payment Date | Shares   | Per Share | Amount  |
|-------------|--------------|----------|-----------|---------|
| Mar 31      | Apr 1        | 596.2451 | $0.346    | $206.30 |
| Apr 30      | May 1        | 594.3483 | $0.335    | $199.11 |
| May 30      | Jun 2        | 592.4584 | $0.358    | $212.10 |
| Jun 30      | Jul 1        | 592.6531 | $0.347    | $205.65 |
| Jul 31      | Aug 1        | 590.8520 | $0.362    | $213.89 |
| Aug 29      | Sep 2        | 589.1324 | $0.360    | $212.09 |
| Sep 30      | Oct 1        | 587.1007 | $0.347    | $203.72 |
| Oct 31      | Nov 3        | 585.3800 | $0.348    | $203.71 |
| Nov 28      | Dec 1        | 585.5233 | $0.313    | $183.27 |
|-------------|--------------|----------|-----------|---------|
| TOTAL       |              |          |           |$1,839.84|
```

---

## The True Cost: Capital Erosion

Every time we switch to SGOV and back, we lose money due to price movements:

```
EXAMPLE: March/April Round Trip
===============================

Step 1: Sell WEEK, Buy SGOV (March 28)
  Sell 599.85 WEEK shares @ $100.065 = $60,023.99
  Buy SGOV @ $100.67 = 596.2451 shares

Step 2: Hold SGOV through ex-div (March 31)
  Entitled to dividend: 596.2451 x $0.346 = $206.30

Step 3: Sell SGOV, Buy WEEK (April 4)
  Sell 596.2451 SGOV shares @ $100.41 = $59,868.97
  Buy WEEK @ $100.08 = 598.2112 shares

WHAT HAPPENED:
  Started with:  $60,023.99 (in WEEK value)
  Ended with:    $59,868.97 (in WEEK value)
  Capital Loss:  $155.02

  Dividend Received: $206.30

  NET GAIN from this cycle: $206.30 - $155.02 = $51.28
```

**Key insight:** We didn't "make" the full $206.30 dividend. After the price drop, the net gain was only about $51.

---

## Honest Accounting: Where Did The Money Go?

```
STARTING POINT (March 6, 2025)
==============================
  Investment: $60,000.00


ENDING POINT (December 2, 2025)
===============================
  Stock Value:      $58,950.52
  Dividends:       + $3,481.65
                   -----------
  TOTAL VALUE:      $62,432.17


THE BREAKDOWN:
==============
  Capital Change:   $58,950.52 - $60,000 = -$1,049.48  (LOSS)
  Dividends:        +$3,481.65                         (GAIN)
                                           -----------
  NET PROFIT:       +$2,432.17


WHERE THE CAPITAL WENT:
=======================
  - Each SGOV round trip cost ~$100-200 in price slippage
  - 9 round trips = ~$1,049 in total capital erosion
  - This is partially offset by the SGOV dividends we captured
```

---

## Comparison: Was It Worth It?

### If You Just Held WEEK (Buy and Hold)

```
March 6:  Buy 599.85 shares @ $100.025 = $60,000
Dec 2:    Value = 599.85 shares @ $100.04 = $60,008.99
          Dividends received: ~$1,800
          TOTAL: ~$61,809
          RETURN: 3.01%
```

### If You Just Held SGOV (Buy and Hold)

```
March 6:  Buy ~596 shares @ $100.67 = $60,000
Dec 2:    Value = ~596 shares @ $100.44 = ~$59,862
          Dividends received: ~$2,006
          TOTAL: ~$61,868
          RETURN: 3.11%
```

### The Rotation Strategy (What We Did)

```
March 6:  Buy WEEK with $60,000
Dec 2:    Stock Value: $58,950.52
          Dividends:   $3,481.65
          TOTAL:       $62,432.17
          RETURN:      4.05%
```

### Side-by-Side Comparison

```
+------------------+-------------+------------+------------+--------+
|    Strategy      | Stock Value | Dividends  | Total      | Return |
+------------------+-------------+------------+------------+--------+
| Hold WEEK        | $60,009     | $1,800     | $61,809    | 3.01%  |
| Hold SGOV        | $59,862     | $2,006     | $61,868    | 3.11%  |
| WEEK/SGOV Rotate | $58,951     | $3,482     | $62,432    | 4.05%  |
+------------------+-------------+------------+------------+--------+
                         ^            ^
                         |            |
                    Lower value   Higher dividends
                    (due to       (captured both
                     switching)    weekly & monthly)
```

**Bottom Line:** The rotation strategy earned about **$564 more** than just holding SGOV, and about **$623 more** than just holding WEEK. The extra dividends captured outweighed the capital erosion from switching.

---

## The Trade-Offs Explained Honestly

### What You Gain
```
+ Captured 36 weekly WEEK dividends: $1,642
+ Captured 9 monthly SGOV dividends: $1,840
+ Total dividends: $3,482 (vs ~$2,006 for SGOV-only)
```

### What You Lose
```
- Missed 3 WEEK dividends on overlap days: ~$141
- Capital erosion from 18 trades: ~$1,049
- More complexity and effort
- Potential tax implications from frequent trading
```

### Net Result
```
Extra dividends captured:     +$1,476 (vs SGOV buy-and-hold)
Capital erosion:              -$912   (vs SGOV buy-and-hold)
                              -------
Net benefit of strategy:      +$564
```

---

## Why The Strategy Still Works (Despite The Costs)

The math works because:

1. **SGOV dividend > Price drop**: While SGOV drops after ex-div, it doesn't drop by the FULL dividend amount. We capture a net gain each cycle.

2. **WEEK dividends compound**: While holding WEEK between switches, we collect weekly dividends that a SGOV-only investor misses.

3. **Volume matters**: With $60,000 invested, even small percentage gains add up to real money.

```
SIMPLIFIED VIEW:
================

SGOV buy-and-hold gets:     ~$2,000 in dividends
                            + small capital gain
                            = ~$1,868 profit

Rotation strategy gets:     ~$3,482 in dividends
                            - ~$1,050 capital loss
                            = ~$2,432 profit

Extra profit from rotating: ~$564
```

---

## Is This Strategy For You?

### Realistic Assessment

```
PROS:
  [+] Higher total return (4.05% vs 3.11% for SGOV)
  [+] Both funds are very safe (U.S. Treasury bills)
  [+] Mechanical strategy - no guessing required

CONS:
  [-] Requires 18 trades over 9 months
  [-] Must track ex-dividend dates carefully
  [-] Capital erosion from each switch (~$100-150 per round trip)
  [-] You WILL miss some WEEK dividends (3 in our period)
  [-] Tax complexity from frequent trading
  [-] More things can go wrong (miss a trade, bad timing, etc.)

THE HONEST QUESTION:
  Is ~$564 extra profit worth 18 trades and the added complexity?

  On $60,000 invested, that's less than 1% extra return.
  You'll need to decide if the effort is worth it for you.
```

---

## Key Dates Reference

SGOV ex-dividend: **Last business day of each month**

```
2025 SGOV Ex-Dividend Dates:
============================
January   --> Jan 31    July      --> Jul 31
February  --> Feb 28    August    --> Aug 29
March     --> Mar 31    September --> Sep 30
April     --> Apr 30    October   --> Oct 31
May       --> May 30    November  --> Nov 28
June      --> Jun 30    December  --> Dec 31

OVERLAP DATES (when WEEK ex-div falls on same day):
  March 31, June 30, August 29

  On these days, choose SGOV (bigger dividend)
```

---

## Summary

```
+------------------------------------------------------------------+
|                    THE HONEST SUMMARY                             |
+------------------------------------------------------------------+
|                                                                  |
| The WEEK/SGOV rotation strategy DOES outperform buy-and-hold:    |
|                                                                  |
|   - Rotation: 4.05% return ($2,432 profit)                       |
|   - SGOV only: 3.11% return ($1,868 profit)                      |
|   - WEEK only: 3.01% return ($1,809 profit)                      |
|                                                                  |
| BUT it's not "free money":                                       |
|                                                                  |
|   - You lose ~$1,050 in capital from switching                   |
|   - You miss 3 WEEK dividends on overlap days                    |
|   - Price drops after ex-div offset some dividend gains          |
|   - Requires active management (18 trades)                       |
|                                                                  |
| The extra ~$564 profit comes from:                               |
|   - Net gains on SGOV captures (dividend > price drop)           |
|   - WEEK dividends collected between switches                    |
|                                                                  |
+------------------------------------------------------------------+
```

---

*This explanation is for educational purposes only. Past performance doesn't guarantee future results. The analysis assumes no trading commissions and doesn't account for taxes. Consult a financial advisor before investing.*
