# Logging & Error Handling

## Logger Package Levels
```r
log_info("Starting analysis")     # Normal flow
log_warn("Fallback triggered")    # Expected but notable
log_error("API failed: {msg}")    # Error, recoverable
log_debug("Query: {nrow} rows")   # Dev info
log_success("Complete")           # Major milestone
```

## Error Handling Pattern
```r
tryCatch({
  result <- risky_operation()
}, error = function(e) {
  log_error("Operation failed: {e$message}")
  NULL  # Safe default
})
```

## User Communication
- Never silently fail
- Always inform user of fallbacks/errors
- Use notifications (showNotification) for important events
