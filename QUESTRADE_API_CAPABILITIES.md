# Questrade API Capabilities

## 🔑 YOUR API KEY PERMISSIONS

**Accounts Accessible:** 3 accounts
- RRSP (Registered Retirement Savings Plan)
- SRRSP (Spousal RRSP)
- LIRA (Locked-In Retirement Account)

**Permission Level:** READ-ONLY (Account Data + Market Data)
- ✓ You CAN read all account and market data
- ✗ You CANNOT place trades (requires Questrade partner status)

---

## 📊 WHAT YOU CAN BUILD

### **1. PORTFOLIO TRACKING & ANALYTICS**

**Account Balances**
- See cash balances across all 3 accounts
- Track total portfolio value in real-time
- Monitor currency balances (CAD/USD)

**Current Holdings/Positions**
- View all stocks, ETFs, and options you own
- See quantity, current price, and market value
- Track open P&L (profit/loss) on each position
- Calculate total portfolio allocation

**Performance Tracking**
- Monitor daily/weekly/monthly returns
- Compare performance across your 3 accounts
- Track which account is performing best

---

### **2. TRANSACTION HISTORY & TAX REPORTING**

**Account Activities (Transactions)**
- View all trades, deposits, withdrawals
- Track dividend payments and interest income
- Monitor fees and commissions paid
- Export data for tax preparation (T-slips)

**Trade History**
- See all executed trades (buys/sells)
- Review execution prices and timestamps
- Calculate realized gains/losses
- Track trading patterns and frequency

**Order History**
- View all orders (filled, cancelled, pending)
- Analyze order types used (market, limit, stop)
- Review order modifications

---

### **3. MARKET DATA & RESEARCH TOOLS**

**Real-Time Stock Quotes**
- Get current bid/ask prices for any stock or ETF
- See last trade price and volume
- Monitor day high/low and 52-week ranges
- Track percentage change and volatility

**Historical Price Data (OHLC Candles)**
- Download historical prices (open, high, low, close)
- Choose intervals: 1min, 5min, 15min, 1hour, 1day
- Analyze price trends and patterns
- Build custom technical indicators

**Symbol Search**
- Search for stocks/ETFs by ticker
- Get symbol IDs needed for other API calls
- Find Canadian (.TO) and US stocks

**Options Chain Data**
- Get all available options for any stock
- See strike prices and expiration dates
- View calls and puts separately
- Access greeks (delta, gamma, theta, vega)
- Check open interest and volume

---

### **4. AUTOMATED ALERTS & MONITORING**

**Price Alerts**
- Monitor when stocks hit target prices
- Get notified when holdings drop/rise X%
- Track stop-loss levels

**Portfolio Alerts**
- Alert when portfolio value crosses thresholds
- Monitor dividend payment dates
- Track when positions become profitable

**Balance Monitoring**
- Check if cash balance is low
- Monitor margin usage (if applicable)
- Track contribution room

---

### **5. DIVIDEND INCOME TRACKING**

**Dividend Dashboard**
- Track all dividend payments across accounts
- Calculate annual dividend income
- Project future dividend payments
- Monitor dividend yield on holdings

**Ex-Dividend Calendars**
- See upcoming ex-dividend dates
- Plan dividend capture strategies
- Track dividend reinvestment

---

### **6. OPTIONS STRATEGY ANALYSIS**

**Covered Call Opportunities**
- Identify stocks you own that have liquid options
- Calculate premium income potential
- Find optimal strike prices and expirations
- Track annualized return on covered calls

**Cash-Secured Put Analysis**
- Find stocks worth selling puts on
- Calculate income from put selling
- Monitor cash required for assignment

**Options Portfolio Tracking**
- Track all open option positions
- Monitor time decay (theta)
- Calculate potential max profit/loss

---

### **7. RISK MANAGEMENT DASHBOARDS**

**Position Sizing**
- See % allocation to each holding
- Identify concentration risk
- Ensure diversification

**Drawdown Analysis**
- Track maximum drawdown on portfolio
- Calculate peak-to-trough losses
- Monitor recovery time

**Currency Exposure**
- Track CAD vs USD exposure
- Monitor forex impact on returns
- Plan currency hedging

---

### **8. TAX OPTIMIZATION TOOLS**

**Tax-Loss Harvesting**
- Identify positions with unrealized losses
- Plan tax-loss selling strategies
- Track superficial loss rules (30-day)

**Account Comparison**
- Compare RRSP vs LIRA performance
- Optimize which securities go in which account
- Track contribution history

**Capital Gains Tracking**
- Calculate realized gains/losses
- Track adjusted cost base (ACB)
- Prepare for tax filing

---

### **9. CUSTOM SCREENERS & SCANNERS**

**Holdings Screener**
- Filter your holdings by sector, yield, P/L
- Identify underperformers
- Find rebalancing opportunities

**Options Screener**
- Find best covered call candidates from your holdings
- Screen for high-premium opportunities
- Filter by liquidity (open interest/volume)

---

### **10. INTEGRATION & AUTOMATION**

**Shiny Dashboards** (what you're building!)
- Real-time portfolio visualizations
- Interactive charts and tables
- Custom reporting

**Data Export**
- Export to CSV/Excel for further analysis
- Feed data into other tools
- Create custom reports

**Scheduled Updates**
- Auto-refresh portfolio data hourly/daily
- Email/SMS alerts on triggers
- Automated daily summaries

---

## ❌ WHAT YOU CANNOT DO

**Trading Operations** (requires Questrade Partner status)
- Place buy/sell orders
- Modify or cancel orders
- Execute options strategies
- Rebalance portfolio automatically

**Note:** For trading, you need to apply as a Questrade partner developer by emailing [email protected]

---

## 💡 RECOMMENDED NEXT FEATURES FOR YOUR APP

Based on your current covered calls/dividend strategies:

1. **Real-Time Portfolio Tracker** - See all 3 accounts in one dashboard
2. **Covered Call Scanner** - Automatically find best CC opportunities from your holdings
3. **Dividend Income Tracker** - Monitor all dividend payments and project annual income
4. **Tax-Loss Harvesting Tool** - Identify positions to sell before year-end
5. **Options P&L Tracker** - Track performance of your options strategies
6. **Automated Alerts** - Get notified when stocks hit target prices or dividends are paid

---

## 📋 ACCESSIBLE API ENDPOINTS

### Account Endpoints
- `GET /v1/accounts` - List all accounts
- `GET /v1/accounts/:id/balances` - Account balances
- `GET /v1/accounts/:id/positions` - Current positions/holdings
- `GET /v1/accounts/:id/orders` - Order history
- `GET /v1/accounts/:id/executions` - Trade executions
- `GET /v1/accounts/:id/activities` - Account activities (dividends, trades, etc.)

### Market Data Endpoints
- `GET /v1/time` - Server time
- `GET /v1/symbols/search` - Search for stocks/ETFs by ticker
- `GET /v1/symbols/:id` - Get symbol details
- `GET /v1/symbols/:id/options` - Get options chain for a symbol
- `GET /v1/markets` - Market information
- `GET /v1/markets/candles/:id` - Historical OHLC price data
- `GET /v1/markets/quotes/:id` - Real-time stock quotes

---

**Generated:** 2025-01-05
**API Documentation:** https://www.questrade.com/api/documentation
