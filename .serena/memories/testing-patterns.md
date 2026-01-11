# Testing Patterns (testthat)

## Test Organization
- test-fct_*.R: Unit tests for business logic
- test-mod_*.R: Shiny module integration tests
- test-utils_*.R: Utility function tests

## Unit Test Pattern
```r
test_that("function does X", {
  result <- my_function(input)
  expect_equal(result$field, expected_value)
})
```

## Shiny Module Test Pattern
```r
testServer(mod_example_server, args = list(id = "test"), {
  returned <- session$getReturned()
  expect_true(is.reactive(returned$results))
})
```

## Isolation
- Use temp in-memory databases
- Mock external APIs
- withr::with_options() for config isolation
- Each test completely independent
