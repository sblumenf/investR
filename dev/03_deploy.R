## Deployment ----

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile_with_renv()

## If you want to deploy to ShinyProxy
golem::add_dockerfile_with_renv_shinyproxy()

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_rstudioconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Posit Connect ----
rsconnect::writeManifest()

## Deploy to Posit Connect or shinyapps.io ----
## Once you have run the appropriate `add_*()` function
## from above, you can deploy your app with:

# rsconnect::deployApp(
#   appFiles = c(
#     "R/",
#     "inst/",
#     "data/",
#     "NAMESPACE",
#     "DESCRIPTION",
#     "app.R"
#   ),
#   appName = "investR",
#   appTitle = "Investment Research",
#   launch.browser = FALSE,
#   forceUpdate = TRUE
# )

## Note on Brochure deployment ----
## When deploying Brochure apps, be aware that:
## 1. URL routing may differ between development and production
## 2. On platforms like shinyapps.io, the base URL includes the app name
##    (e.g., username.shinyapps.io/investR/)
## 3. You may need to configure href paths accordingly
## 4. Test URL navigation thoroughly after deployment

## Build package ----
## Build the package as a tar.gz for manual installation
devtools::build()

## Check package ----
## Run checks before deployment
devtools::check()

## Install package ----
## Install the package locally for testing
devtools::install()