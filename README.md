# investR

> A production-grade multi-page Shiny application built with golem and Brochure

## Overview

This is a scaffold for a robust, production-ready Shiny application that follows R package development best practices. It combines:

- **golem**: Framework for building production-grade Shiny apps as R packages
- **Brochure**: Multi-page Shiny application support with independent sessions per page

## Project Structure

```
investR/
├── R/                      # Application code
│   ├── app_config.R       # Configuration management
│   ├── app_ui.R           # External resources
│   ├── run_app.R          # Main app launcher (uses brochureApp)
│   ├── page_home.R        # Home page (/)
│   ├── page_about.R       # About page (/about)
│   └── _disable_autoload.R
├── dev/                    # Development scripts
│   ├── 01_start.R         # Initial setup
│   ├── 02_dev.R           # Development helpers
│   └── 03_deploy.R        # Deployment configuration
├── inst/                   # Installed files
│   ├── golem-config.yml   # App configuration
│   └── app/www/           # External resources (CSS, JS, images)
├── tests/                  # Testing infrastructure
│   └── testthat/
├── DESCRIPTION            # Package metadata
└── NAMESPACE              # Package namespace
```

## Getting Started

### 1. Install Dependencies

```r
# Install required packages
install.packages(c("golem", "brochure", "shiny", "config", "devtools"))
```

### 2. Initial Setup

Open and run the setup script:

```r
# Run initial setup (only once)
source("dev/01_start.R")
```

This will:
- Configure package metadata
- Set up testing infrastructure
- Initialize git repository (optional)
- Add development dependencies

### 3. Run the Application

```r
# Load the package
devtools::load_all()

# Run the app
run_app()
```

Or use:

```r
# Run in development mode with auto-reload
golem::run_dev()
```

## Development Workflow

### Adding New Pages

Create a new page file in `R/`:

```r
# R/page_newpage.R
page_newpage <- function() {
  brochure::page(
    href = "/newpage",
    ui = function(request) {
      tagList(
        fluidPage(
          h1("New Page"),
          # Your UI here
        )
      )
    },
    server = function(input, output, session) {
      # Your server logic here
    }
  )
}
```

Then add it to `run_app()` in `R/run_app.R`:

```r
brochureApp(
  golem_add_external_resources(),
  page_home(),
  page_about(),
  page_newpage(),  # Add your new page
  ...
)
```

### Adding Modules

For complex pages, use Shiny modules:

```r
# Add a new module
golem::add_module(name = "mymodule", with_test = TRUE)

# This creates R/mod_mymodule.R and tests/testthat/test-mod_mymodule.R
```

### Adding Business Logic Functions

```r
# Add a function file
golem::add_fct("helpers", with_test = TRUE)

# This creates R/fct_helpers.R and tests/testthat/test-fct_helpers.R
```

### Adding Utility Functions

```r
# Add utilities
golem::add_utils("helpers", with_test = TRUE)

# This creates R/utils_helpers.R and tests/testthat/test-utils_helpers.R
```

### Adding External Resources

```r
# Add CSS
golem::add_css_file("custom")

# Add JavaScript
golem::add_js_file("script")

# Add JS handlers
golem::add_js_handler("handlers")
```

Files will be created in `inst/app/www/` and automatically loaded.

## Key Differences from Standard Golem Apps

This app uses **Brochure** for multi-page support, which means:

1. **No `app_server.R` file**: Each page has its own server function
2. **Modified `app_ui.R`**: Only contains `golem_add_external_resources()`
3. **Uses `brochureApp()` instead of `shinyApp()`** in `run_app.R`
4. **Independent sessions**: Each page runs in its own Shiny session
5. **Page-based routing**: Navigation uses `href` paths (e.g., `/`, `/about`)

## Configuration

Edit `inst/golem-config.yml` to manage environment-specific settings:

```yaml
default:
  golem_name: investR
  golem_version: 0.0.0.9000
  app_prod: no

production:
  app_prod: yes
  # Add production-specific config here

dev:
  golem_wd: !expr here::here()
```

Access config values in your code:

```r
get_golem_config("app_prod")
```

## Testing

Run tests:

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-golem-recommended.R")

# Check package
devtools::check()
```

## Deployment

See `dev/03_deploy.R` for deployment options:

- Docker containers
- RStudio Connect / Posit Connect
- shinyapps.io
- Shiny Server

**Important**: When deploying Brochure apps, be aware that URL routing may differ between development and production environments.

## Additional Resources

- [golem documentation](https://thinkr-open.github.io/golem/)
- [Engineering Production-Grade Shiny Apps](https://engineering-shiny.org/)
- [Brochure package](https://github.com/ColinFay/brochure)
- [Multi-page Shiny with Brochure](https://colinfay.me/brochure-r-package/)

## License

MIT License - see LICENSE file for details