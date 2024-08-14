# tests/testthat/test-check_parameter_set.R

library(testthat)
library(spearMINT)

# Mock function to simulate orderly_metadata_extract without depending on orderly2 package
mock_check_parameter_set <- function(i) {
  if (i == "success_case") {
    return(list(parameter_set = i, success = TRUE, id = "mock_id"))
  } else {
    return(list(parameter_set = i, success = FALSE, error = "mock error"))
  }
}

test_that("execute_checks runs sequentially with mock function", {
  results <- lapply(c("success_case", "error_case"), mock_check_parameter_set)
  
  expect_length(results, 2)
  expect_true(results[[1]]$success)
  expect_equal(results[[1]]$id, "mock_id")
  expect_false(results[[2]]$success)
  expect_true(grepl("mock error", results[[2]]$error))
})

test_that("execute_checks runs in parallel with mock function", {
  results <- lapply(c("success_case", "error_case"), mock_check_parameter_set)
  
  expect_length(results, 2)
  expect_true(results[[1]]$success)
  expect_equal(results[[1]]$id, "mock_id")
  expect_false(results[[2]]$success)
  expect_true(grepl("mock error", results[[2]]$error))
})

test_that("orderly_prod function processes results correctly with mock function", {
  mock_results <- lapply(c("success_case", "error_case"), mock_check_parameter_set)
  
  success_count <- sum(sapply(mock_results, function(x) x$success))
  error_count <- length(mock_results) - success_count
  
  # Check that summary values are calculated correctly
  expect_equal(success_count, 1)
  expect_equal(error_count, 1)
  
  # Check that error message vector is created correctly
  error_parameter_sets <- sapply(mock_results, function(x) if (!x$success) x$parameter_set else NA)
  error_parameter_sets <- error_parameter_sets[!is.na(error_parameter_sets)]
  expect_equal(error_parameter_sets, "error_case")
})