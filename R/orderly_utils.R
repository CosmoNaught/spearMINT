#' Check a Single Parameter Set
#'
#' This function checks a single `parameter_set` by dynamically constructing a query 
#' and running it using the `orderly2::orderly_metadata_extract` function.
#' 
#' @param i An individual parameter set to check.
#' 
#' @return A list containing the `parameter_set`, a success flag, and either the `id` or an error message.
#' 
#' @keywords internal
#' @export
check_parameter_set <- function(i) {
  tryCatch({
    # Dynamically construct the query
    query <- substitute(latest(parameter:parameter_set == val), list(val = i))
    
    # Run the orderly2 function with the dynamically constructed query
    result <- orderly2::orderly_metadata_extract(query, name = "simulation_launch")
    
    return(list(parameter_set = i, success = TRUE, id = result$id))
  }, error = function(e) {
    return(list(parameter_set = i, success = FALSE, error = conditionMessage(e)))
  })
}

#' Execute Parameter Set Checks
#'
#' This function executes the `check_parameter_set` function for a range of parameter sets, either sequentially or in parallel.
#'
#' @param start The starting index of the parameter sets to check.
#' @param end The ending index of the parameter sets to check.
#' @param parallel Logical. If `TRUE`, the checks are run in parallel using multiple cores; otherwise, they are run sequentially.
#' 
#' @return A list of results for each parameter set, including success flags and any errors.
#' 
#' @keywords internal
#' @export
execute_checks <- function(start, end, parallel) {
  if (parallel) {
    num_cores <- parallel::detectCores() - 1  # Use all but one core
    cl <- parallel::makeCluster(num_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)  # Ensure the cluster is stopped even if there's an error
    results <- parallel::parLapply(cl, start:end, check_parameter_set)
  } else {
    results <- lapply(start:end, check_parameter_set)
  }
  return(results)
}

#' Run Parameter Set Checks
#'
#' This function runs checks on a range of parameter sets and provides a detailed report on the results.
#' It can execute the checks either sequentially or in parallel.
#'
#' @param start The starting index of the parameter sets to check (default is 1).
#' @param end The ending index of the parameter sets to check (default is 10,000).
#' @param verbose Logical. If `TRUE`, detailed output of the process is provided (default is `TRUE`).
#' @param parallel Logical. If `TRUE`, the checks are run in parallel using multiple cores; otherwise, they are run sequentially (default is `TRUE`).
#' 
#' @return None. This function prints a summary of the results and detailed information if `verbose` is `TRUE`.
#' 
#' @export
prod <- function(start = 1, end = 10000, verbose = TRUE, parallel = TRUE) {

  t0 <- Sys.time()

  # Perform the checks
  results <- execute_checks(start, end, parallel)
  
  # Process results
  success_count <- sum(sapply(results, function(x) x$success))
  error_count <- length(results) - success_count
  
  # Extract the parameter sets that led to errors
  error_parameter_sets <- sapply(results, function(x) if (!x$success) x$parameter_set else NA)
  error_parameter_sets <- error_parameter_sets[!is.na(error_parameter_sets)]
  
  t1 <- Sys.time()

  elapsed_time <- as.numeric(difftime(t1, t0, units = "secs"))

  # Verbose output of detailed information
  if (verbose) {
    if (success_count > 0) {
      cat("\nSuccesses:\n")
      for (res in results) {
        if (res$success) {
          cat("parameter_set:", res$parameter_set, "- id:", res$id, "\n")
        }
      }
    }
    
    if (error_count > 0) {
      cat("\nErrors:\n")
      for (res in results) {
        if (!res$success) {
          cat("parameter_set:", res$parameter_set, "- Error:", res$error, "\n")
        }
      }
    }
    cat(end - start + 1, "total packets processed in:", elapsed_time, "seconds\n")
  }

  # Print the results summary
  cat("Number of successful parameter_set found:", success_count, "\n")
  cat("Number of errors encountered:", error_count, "\n")
  
  # Print the vector of parameter sets that led to errors
  if (length(error_parameter_sets) == 0) error_parameter_sets = 0 else error_parameter_sets
  
  cat("Parameter sets leading to errors:", paste(error_parameter_sets, collapse = ", "), "\n")

}
