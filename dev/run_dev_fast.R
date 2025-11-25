# Fast development mode - skips full package installation
# Use this for rapid iteration during development

# Set options
options(golem.app.prod = FALSE)
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages
golem::detach_all_attached()

# Install package for parallel workers
message("Installing package for parallel worker access...")
devtools::install(quick = TRUE, upgrade = "never", quiet = TRUE)

# Load package without installing (much faster!)
devtools::load_all()

# Run the application
run_app()
