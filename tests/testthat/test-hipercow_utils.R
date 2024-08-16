# test-hipercow.R

library(testthat)

# Load the testthat package
library(testthat)

# Test for the list_r_files function
test_that("list_r_files correctly lists R files recursively", {
  
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  
  # Create some dummy files in the temporary directory
  r_file_1 <- file.path(temp_dir, "file1.R")
  r_file_2 <- file.path(temp_dir, "file2.R")
  non_r_file <- file.path(temp_dir, "file.txt")
  
  # Create subdirectories
  sub_dir <- file.path(temp_dir, "subfolder")
  src_dir <- file.path(temp_dir, "src")
  dir.create(sub_dir)
  dir.create(src_dir)
  
  # Create some dummy files in the subdirectories
  r_file_3 <- file.path(sub_dir, "file3.R")
  r_file_4 <- file.path(src_dir, "file1.R")
  r_file_5 <- file.path(src_dir, "file2.R")
  non_r_file_2 <- file.path(sub_dir, "file2.txt")
  
  # Write to these files to actually create them
  file.create(r_file_1, r_file_2, non_r_file, r_file_3, r_file_4, r_file_5, non_r_file_2)
  
  # Run the function to list R files
  r_files <- list_r_files(temp_dir)
  
  # Get expected .R files dynamically
  expected_files <- list.files(path = temp_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  
  # Test if the function returns the correct files
  expect_equal(sort(r_files), sort(expected_files))
  
  # Clean up - remove the created files and directories
  unlink(temp_dir, recursive = TRUE)
})



test_that("extract_packages works correctly", {
  # Create a temporary R file with some library and require calls
  temp_file <- tempfile(fileext = ".R")
  writeLines(c(
    "library(dplyr)",
    "require(stringr)",
    "library('utils')",
    "require(\"stringr\")"
  ), temp_file)
  
  # Test extract_packages function
  packages <- unique(extract_packages(temp_file)) # Get unique packages
  expect_equal(sort(packages), sort(c("dplyr", "stringr", "utils")))
})


test_that("check_package_in_file works correctly", {
  # Create a temporary pkgdepends.txt file
  pkg_file <- tempfile()
  writeLines(c(
    "dplyr",
    "stringr"
  ), pkg_file)
  
  # Test check_package_in_file function
  expect_true(check_package_in_file("dplyr", pkg_file))
  expect_false(check_package_in_file("utils", pkg_file))
})

test_that("find_missing_packages works correctly", {
  # Create a temporary directory and some R files with library calls
  temp_dir <- tempdir()
  unlink(temp_dir, recursive = TRUE) # Clean the directory before the test
  dir.create(temp_dir)
  
  file.create(file.path(temp_dir, "file1.R"))
  writeLines("library(dplyr)", file.path(temp_dir, "file1.R"))
  file.create(file.path(temp_dir, "file2.R"))
  writeLines("require(stringr)", file.path(temp_dir, "file2.R"))
  file.create(file.path(temp_dir, "file3.R"))
  writeLines("library(utils)", file.path(temp_dir, "file3.R"))
  
  # Create a temporary pkgdepends.txt file
  pkg_file <- tempfile()
  writeLines(c(
    "dplyr",
    "stringr"
  ), pkg_file)
  
  # Test find_missing_packages function
  missing_packages <- find_missing_packages(temp_dir, pkg_file)
  expect_equal(missing_packages, "utils")
})

test_that("hipercow_check_pkgdepends works without errors", {
  # Check if running in a non-HPC environment and skip the test if so
  if (!Sys.getenv("HPC_ENVIRONMENT") == "TRUE") {
    skip("Skipping test: not in HPC environment.")
  }
  
  # The rest of your test code here...
  
  temp_dir <- tempdir()
  unlink(temp_dir, recursive = TRUE) # Clean the directory before the test
  dir.create(temp_dir)
  dir.create(file.path(temp_dir, "src"))
  
  file.create(file.path(temp_dir, "src/file1.R"))
  writeLines("library(dplyr)", file.path(temp_dir, "src/file1.R"))
  file.create(file.path(temp_dir, "src/file2.R"))
  writeLines("require(stringr)", file.path(temp_dir, "src/file2.R"))
  
  # Create a temporary pkgdepends.txt file
  pkg_file <- file.path(temp_dir, "pkgdepends.txt")
  writeLines(c("dplyr"), pkg_file)
  
  # Initialize the hipercow project root
  hipercow_init(temp_dir)
  
  # Configure a hipercow driver
  hipercow_configure(driver = "windows", root = temp_dir)
  
  # Set the working directory to the temp directory
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd))
  
  # Test hipercow_check_pkgdepends function
  expect_message(hipercow_check_pkgdepends(), "The following packages are missing from pkgdepends.txt:")
})
