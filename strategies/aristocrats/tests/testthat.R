library(testthat)

# Get the correct path to source files
test_dir <- getwd()
if (basename(test_dir) == "testthat") {
  source_dir <- file.path(dirname(dirname(test_dir)))
} else if (basename(test_dir) == "tests") {
  source_dir <- dirname(test_dir)
} else {
  source_dir <- file.path(test_dir, "strategies")
}

# Load all source files
source(file.path(source_dir, "config.R"))
source(file.path(source_dir, "utils.R"))
source(file.path(source_dir, "metrics.R"))

# Run tests
test_dir(file.path(dirname(test_dir), "testthat"))