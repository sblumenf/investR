# Common Pitfalls to Avoid

## Option Symbol Formats (CRITICAL)
Two formats exist in production:
- Legacy: "ALB17Dec27C55.00" (ticker + day + month + year + C/P + strike)
- New: "AAPL250119C150" (ticker + YYMMDD + C/P + strike)
→ Use parse_option_details(), don't parse manually

## Questrade API Issues
- Symbol field sometimes blank for options
- Use description field for uniqueness constraint
- 401 errors require immediate token refresh

## Risk Simulation Consistency
- Portfolio stress tests MUST use same baseline (purchase_price) as position-level
- Use shared_risk_engine.R for all simulations

## Cash Flow Projections
Different strategies have different projection rules:
- Covered calls: dividends + option gains
- Cash-secured puts: premium only (no dividends)
- Legacy covered calls: actual only (no projections)
→ Always check strategy_type before generating

## Database
- Always use on.exit() for connection cleanup
- Use transactions for multi-step operations
- Never silently fail - log and inform user
