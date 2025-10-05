## Fill in the DESCRIPTION ----
## Add metadata and dependencies to DESCRIPTION file
golem::fill_desc(
  pkg_name = "investR",
  pkg_title = "Investment Research Shiny Application",
  pkg_description = "A production-grade Shiny application built with golem and Brochure for multi-page investment research capabilities.",
  author_first_name = "First",
  author_last_name = "Last",
  author_email = "first.last@example.com",
  repo_url = "https://github.com/yourusername/investR"
)

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license("investR authors")
usethis::use_readme_rmd(open = FALSE)
usethis::use_code_of_conduct(contact = "first.last@example.com")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon(path = "inst/app/www/favicon.ico")

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## External resources
## Creates inst/app/www/
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
attachment::att_amend_desc()

## Add Brochure dependency explicitly ----
usethis::use_package("brochure")
usethis::use_package("config")
usethis::use_package("golem")
usethis::use_package("shiny")
usethis::use_package("htmltools")
usethis::use_package("pkgload")

## Document and reload ----
golem::document_and_reload()

## Run the application ----
# run_app()