#' List All R Files Recursively
#'
#' This helper function lists all R files within a specified directory, including subdirectories.
#'
#' @param dir A character string specifying the directory to search for R files.
#' @return A character vector containing the full paths of all R files found.
#' @keywords internal
list_r_files <- function(dir) {
  list.files(path = dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
}

#' Extract Package Names from R Script
#'
#' This helper function extracts package names from `library()` and `require()` calls in an R script.
#'
#' @param file A character string specifying the path to an R file.
#' @return A character vector of package names used in the script.
#' @keywords internal
#' @importFrom stringr str_extract_all str_replace_all
extract_packages <- function(file) {
  content <- readLines(file, warn = FALSE) # Read the content of the file
  # Use regex to find library() and require() calls and extract package names
  library_calls <- str_extract_all(content, "library\\(([^)]+)\\)|require\\(([^)]+)\\)") %>%
    unlist() %>%
    str_replace_all(c("library\\(" = "", "require\\(" = "", "\\)" = "", "\"" = "", "'" = ""))
  
  library_calls <- trimws(library_calls) # Trim any whitespace around package names
  return(library_calls)
}

#' Check if a Package Exists in pkgdepends.txt
#'
#' This helper function checks whether a given package is listed in the `pkgdepends.txt` file.
#'
#' @param pkg A character string specifying the package name to check.
#' @param file A character string specifying the path to the `pkgdepends.txt` file.
#' @return A logical value: `TRUE` if the package is found in the file, `FALSE` otherwise.
#' @keywords internal
check_package_in_file <- function(pkg, file) {
  content <- readLines(file, warn = FALSE) # Read the content of the file
  # Create a regex pattern to match the package name ignoring prefixes/suffixes
  pattern <- paste0("(^|/)", pkg, "(@|$)")
  # Return TRUE if any line matches the pattern, otherwise FALSE
  any(grepl(pattern, content))
}

#' Find Missing Packages in R Scripts
#'
#' This helper function processes all R files in a specified directory, 
#' identifies packages that are not listed in the `pkgdepends.txt` file, 
#' and returns a list of those missing packages.
#'
#' @param src_dir A character string specifying the path to the directory containing R scripts.
#' @param pkg_file A character string specifying the path to the `pkgdepends.txt` file.
#' @return A character vector of missing package names.
#' @keywords internal
find_missing_packages <- function(src_dir, pkg_file) {
  r_files <- list_r_files(src_dir) # Get all R files
  
  all_packages <- c()
  
  # Loop through each R file and extract packages
  for (file in r_files) {
    packages <- extract_packages(file)
    all_packages <- c(all_packages, packages)
  }
  
  unique_packages <- sort(unique(all_packages)) # Remove duplicates and sort the package names
  
  missing_packages <- c()
  
  # Check each package if it's in the pkgdepends.txt file
  for (pkg in unique_packages) {
    if (!check_package_in_file(pkg, pkg_file)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  return(missing_packages)
}

#' Check for Missing Packages in pkgdepends.txt
#'
#' This function checks all R files in the "src" directory for `library()` and `require()` calls, 
#' identifies any packages that are not listed in the `pkgdepends.txt` file, and prompts the user to 
#' add any missing packages.
#'
#' @return NULL. The function may prompt the user to take actions such as adding missing packages 
#' to `pkgdepends.txt` or reprovisioning the hipercow directory.
#' @export
#' @import stringr
#' @import dplyr
#' @importFrom hipercow hipercow_provision
hipercow_check_pkgdepends <- function() {
  working_dir <- getwd() # Get the current working directory
  src_dir <- file.path(working_dir, "src") # Define the src directory path
  pkg_file <- file.path(working_dir, "pkgdepends.txt") # Define the path to pkgdepends.txt
  
  missing_packages <- find_missing_packages(src_dir, pkg_file)
  
  # Print the list of missing packages
  if (length(missing_packages) > 0) {
    cat("The following packages are missing from pkgdepends.txt:\n")
    print(missing_packages)
    
    # Ask the user if they want to add the missing packages
    answer <- readline(prompt = "Would you like to add them to pkgdepends.txt? (yes/no): ")
    
    if (tolower(answer) == "yes") {
      # Open file connection to append
      con <- file(pkg_file, open = "a")
      # Add newline before writing the new packages
      write("\n", file = con)
      write(missing_packages, file = con, ncolumns = 1)
      close(con)
      cat("Missing packages have been added to pkgdepends.txt.\n")
      
      # Ask if the user wants to reprovision the hipercow directory
      reprovision_answer <- readline(prompt = "Would you like to reprovision your hipercow directory? (yes/no): ")
      
      if (tolower(reprovision_answer) == "yes") {
        cat("Reprovisioning hipercow directory...\n")
        hipercow::hipercow_provision() # Call the reprovision function from hipercow
      } else {
        cat("Reprovisioning skipped.\n")
      }
      
    } else {
      cat("No changes were made to pkgdepends.txt.\n")
    }
  } else {
    cat("No packages are missing from pkgdepends.txt.\n")
  }
}