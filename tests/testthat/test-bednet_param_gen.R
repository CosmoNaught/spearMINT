# spearMINT/tests/testthat/test-bednet_param_gen.R

library(testthat)
library(spearMINT)

test_that("load_itn_data loads and transforms data correctly", {
  # Mock data for testing
  mock_data <- data.frame(
    ERG_d_ITN0 = c(0.1, 0.2),
    ERG_r_ITN0 = c(0.3, 0.4),
    gamman = c(0.5, 0.6),
    bioassay_surv = c(0.7, 0.8)
  )
  write.csv(mock_data, "mock_data.csv", row.names = FALSE)

  result <- load_itn_data("mock_data.csv", "mock_net")

  expect_equal(result$net_type, rep("mock_net", 2))
  expect_equal(result$dn0, c(0.1, 0.2))
  expect_equal(result$rn0, c(0.3, 0.4))

  # Clean up
  unlink("mock_data.csv")
})

test_that("combine_itn_data combines data correctly", {
  # Mock data for testing
  mock_data1 <- data.frame(
    ERG_d_ITN0 = c(0.1),
    ERG_r_ITN0 = c(0.3),
    gamman = c(0.5),
    bioassay_surv = c(0.7)
  )
  write.csv(mock_data1, "mock_data1.csv", row.names = FALSE)

  mock_data2 <- data.frame(
    ERG_d_ITN0 = c(0.2),
    ERG_r_ITN0 = c(0.4),
    gamman = c(0.6),
    bioassay_surv = c(0.8)
  )
  write.csv(mock_data2, "mock_data2.csv", row.names = FALSE)

  file_paths <- list(
    "net1" = "mock_data1.csv",
    "net2" = "mock_data2.csv"
  )

  result <- combine_itn_data(file_paths)

  expect_equal(nrow(result), 2)
  expect_equal(result$net_type, c("net1", "net2"))

  # Clean up
  unlink("mock_data1.csv")
  unlink("mock_data2.csv")
})

test_that("save_itn_data saves data correctly", {
  # Mock data for testing
  mock_data <- data.frame(
    dn0 = c(0.1, 0.2),
    rn0 = c(0.3, 0.4),
    gamman = c(0.5, 0.6),
    bioassay_surv = c(0.7, 0.8),
    net_type = c("net1", "net2")
  )

  save_itn_data(mock_data, "mock_output.RDS")

  result <- readRDS("mock_output.RDS")

  expect_equal(result, mock_data)

  # Clean up
  unlink("mock_output.RDS")
})
