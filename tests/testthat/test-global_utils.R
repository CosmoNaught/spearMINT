# test-generate_param_name.R

library(testthat)
library(spearMINT)

test_that("generate_param_name works correctly", {
  # Test basic cases
  expect_equal(generate_param_name(1), "A1")
  expect_equal(generate_param_name(1001), "B1")
  expect_equal(generate_param_name(26001), "AA1")
  expect_equal(generate_param_name(27001), "AB1")
  
  # Test edge cases
  expect_equal(generate_param_name(0), "A0")
  expect_equal(generate_param_name(999), "A999")
  expect_equal(generate_param_name(1000), "B0")
  expect_equal(generate_param_name(27002), "AB2")
  
  # Test larger indices
  expect_equal(generate_param_name(52001), "BA1")
  expect_equal(generate_param_name(53001), "BB1")
  expect_equal(generate_param_name(78001), "CA1")
  expect_equal(generate_param_name(79001), "CB1")
  
  # Test a very large index
  expect_equal(generate_param_name(1000001), "NAM1")
})