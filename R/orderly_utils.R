#' Check a Single Parameter Set
#'
#' This function checks a single `parameter_set` by dynamically constructing a query 
#' and running it using the `orderly2::orderly_metadata_extract` function.
#' 
#' @param i An individual parameter set to check.
#' 
#' @return A list containing the `parameter_set`, a success flag, and either the `id` or an error message.
#' @importFrom orderly2 orderly_metadata_extract
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
#' This function executes the `check_parameter_set` function for a vector of parameter sets, either sequentially or in parallel.
#'
#' @param indices A vector of indices of the parameter sets to check.
#' @param parallel Logical. If `TRUE`, the checks are run in parallel using multiple cores; otherwise, they are run sequentially.
#' 
#' @return A list of results for each parameter set, including success flags and any errors.
#' 
#' @keywords internal
#' @export
execute_checks <- function(indices, parallel) {
  if (parallel) {
    num_cores <- parallel::detectCores() - 1  # Use all but one core
    cl <- parallel::makeCluster(num_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)  # Ensure the cluster is stopped even if there's an error
    results <- parallel::parLapply(cl, indices, check_parameter_set)
  } else {
    results <- lapply(indices, check_parameter_set)
  }
  return(results)
}


#' Run Parameter Set Checks
#'
#' This function runs checks on a vector of parameter sets and provides a detailed report on the results.
#' It can execute the checks either sequentially or in parallel.
#'
#' @param indices A vector of indices of the parameter sets to check. Default is `1:10000`.
#' @param verbose Logical. If `TRUE`, detailed output of the process is provided (default is `TRUE`).
#' @param parallel Logical. If `TRUE`, the checks are run in parallel using multiple cores; otherwise, they are run sequentially (default is `TRUE`).
#' @param store_output Logical. If `TRUE`, the function will store the IDs of the successful checks and the parameter sets that encountered errors (default is `FALSE`).
#' 
#' @return A list containing the success summary, error summary, and optionally the IDs and error parameter sets if `store_output` is `TRUE`.
#' 
#' @export
orderly_prod <- function(indices = 1:10000, verbose = TRUE, parallel = TRUE, store_output = FALSE) {

  t0 <- Sys.time()

  # Perform the checks
  results <- execute_checks(indices, parallel)
  
  # Process results
  success_results <- lapply(results, function(x) if (x$success) list(parameter_set = x$parameter_set, id = x$id))
  success_results <- Filter(Negate(is.null), success_results)  # Filter out NULL values
  
  error_results <- lapply(results, function(x) if (!x$success) list(parameter_set = x$parameter_set, error = x$error))
  error_results <- Filter(Negate(is.null), error_results)  # Filter out NULL values

  success_count <- length(success_results)
  error_count <- length(error_results)
  
  t1 <- Sys.time()
  elapsed_time <- as.numeric(difftime(t1, t0, units = "secs"))

  # Verbose output of detailed information
  if (verbose) {
    if (success_count > 0) {
      cat("\nSuccesses:\n")
      for (res in success_results) {
        cat("parameter_set:", res$parameter_set, "- id:", res$id, "\n")
      }
    }
    
    if (error_count > 0) {
      cat("\nErrors:\n")
      for (res in error_results) {
        cat("parameter_set:", res$parameter_set, "- Error:", res$error, "\n")
      }
    }
    cat(length(indices), "total packets processed in:", elapsed_time, "seconds\n")
  }

  # Print the results summary
  cat("Number of successful parameter_set found:", success_count, "\n")
  cat("Number of errors encountered:", error_count, "\n")
  
  # Store output if requested
  if (store_output) {
    return(list(
      report_IDs = success_results,
      error_parameter_set = sapply(error_results, function(x) x$parameter_set)
    ))
  } else {
    invisible(NULL)  # Return nothing if store_output is FALSE
  }
}
