# Questrade API Integration

## Token Management
- Cached in ~/.investR_tokens.json
- Auto-refresh if < 30 min old (60s safety buffer)
- Falls back to .Renviron QUESTRADE_REFRESH_TOKEN

## Key Functions
- get_questrade_auth(): Token retrieval with caching
- fetch_questrade_quote(): Stock quotes
- fetch_questrade_options(): Options chains

## 401 Recovery
1. Log clear error with solution
2. Attempt refresh with stored refresh token
3. Provide path to generate new token if still fails
4. NEVER silently fail

## Fallback
- If Questrade fails → Yahoo Finance fallback
- Track fallbacks with get_fallback_summary()
- Always inform user which data came from fallback
