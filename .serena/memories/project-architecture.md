# investR Architecture

## Scale
- 56,290 lines of R code
- 36 Shiny modules, 38 business logic functions, 47 utilities
- 175 exported functions

## File Naming (CRITICAL)
| Prefix | Purpose |
|--------|---------|
| `page_*.R` | Brochure page definitions |
| `mod_*.R` | Shiny modules (UI + Server) |
| `fct_*.R` | Business logic (pure, testable) |
| `utils_*.R` | Utilities and helpers |

## Module Pattern
- mod_*_ui() + mod_*_server() in same file
- Server returns reactive list for composition
- Pages compose modules, don't contain logic
- Always use NS(id) for namespacing

## Separation of Concerns
- fct_*.R: Pure functions, no side effects, testable
- Database functions: Isolated I/O in fct_*_database.R
- Shiny modules: Reactive wrappers calling fct_* functions
