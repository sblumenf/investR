## Development Day-to-Day ----

## Add modules ----
## Create a module infrastructure in R/
# Note: For Brochure apps, modules can be used within pages
golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module
golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_* files
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)

## Add Brochure pages ----
## Use this to add new pages to your Brochure app
## This creates R/page_*.R files
# Example:
# source("dev/create_page.R")
# create_page("dashboard", href = "/dashboard")
# create_page("analytics", href = "/analytics")

## External resources
## Add one line by file in inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

## Vignette ----
usethis::use_vignette("investR")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

## Documentation ----
## Run this when you're ready to document your package
devtools::document()
devtools::check()