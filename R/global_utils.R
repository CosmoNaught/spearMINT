#' Generate Parameter Name
#'
#' This function generates a parameter name based on a given index. The parameter name consists of a combination
#' of letters and numbers. The function handles multi-letter prefixes (e.g., AA, AB, ...) once the index exceeds
#' the size of the alphabet.
#'
#' @param index An integer representing the index for which to generate a parameter name.
#' @return A string representing the generated parameter name.
#' @examples
#' generate_param_name(1)    # Returns "A1"
#' generate_param_name(1001) # Returns "B1"
#' generate_param_name(26001) # Returns "Z1"
#' generate_param_name(27001) # Returns "AA1"
#' @export
# Function to generate parameter names
generate_param_name <- function(index) {
  # Determine the "base" alphabet size
  alphabet_size <- length(LETTERS)
  
  # Calculate which "block" the index is in
  block <- (index %/% 1000)
  
  # Calculate the specific index within the current block
  number <- index %% 1000
  
  # Calculate the letter sequence for the block
  letter1 <- LETTERS[(block %% alphabet_size) + 1]
  
  # For multi-letter prefix (e.g., AA, AB, ... after Z)
  letter2 <- ""
  if (block >= alphabet_size) {
    letter2 <- LETTERS[(block %/% alphabet_size)]
  }
  
  # Combine letters and number
  param_name <- paste0(letter2, letter1, number)
  
  return(param_name)
}

#' Flatten Nested Lists (Internal)
#'
#' @param x List to flatten
#' @param parent_key Parent key for naming
#' @return Flattened list
#' @export
flatten_list <- function(x, parent_key = "") {
  out <- list()
  for (nm in names(x)) {
    val <- x[[nm]]
    new_name <- if (parent_key == "") nm else paste0(parent_key, "_", nm)
    if (is.list(val) && !is.null(names(val))) {
      deeper <- flatten_list(val, new_name)
      out <- c(out, deeper)
    } else {
      out[[new_name]] <- val
    }
  }
  out
}

#' Manage DuckDB Connection
#' 
#' @description
#' Handles DuckDB connection creation and cleanup. This function standardizes
#' the connection pattern used across MINTed and segMINT packages.
#' 
#' @param con Optional existing DuckDB connection object
#' @param raw_db_path Path to .duckdb file (used if con is NULL)
#' @param read_only Logical, whether connection should be read-only. Default TRUE.
#' 
#' @return List with:
#'   \item{con}{DuckDB connection object}
#'   \item{should_close}{Logical indicating if caller should close connection}
#' 
#' @export
#' @importFrom DBI dbConnect
#' @importFrom duckdb duckdb
get_duck_connection <- function(con = NULL, raw_db_path = NULL, read_only = TRUE) {
  if (is.null(con)) {
    if (is.null(raw_db_path)) {
      stop("Either con or raw_db_path must be provided")
    }
    if (read_only && !file.exists(raw_db_path)) {
      stop(sprintf("Database file not found: %s", raw_db_path))
    }
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = raw_db_path, read_only = read_only)
    should_close <- TRUE
  } else {
    should_close <- FALSE
  }
  
  list(con = con, should_close = should_close)
}

#' Build Database Key Filter Clause
#' 
#' @description
#' Creates SQL WHERE clause for parameter_index or global_index filtering.
#' Standardizes the pattern used across multiple query functions.
#' 
#' @param con DuckDB connection for proper quoting
#' @param parameter_index Integer index (optional)
#' @param global_index Character index (optional)
#' 
#' @return Character string with SQL WHERE clause (without 'WHERE' keyword)
#' 
#' @export
#' @importFrom DBI dbQuoteString
build_key_clause <- function(con, parameter_index = NULL, global_index = NULL) {
  if (is.null(parameter_index) && is.null(global_index)) {
    stop("Provide either parameter_index OR global_index")
  }
  
  if (!is.null(parameter_index) && !is.null(global_index)) {
    warning("Both parameter_index and global_index supplied; using parameter_index.")
    global_index <- NULL
  }
  
  if (!is.null(parameter_index)) {
    sprintf("parameter_index = %d", as.integer(parameter_index))
  } else {
    sprintf("global_index = %s", as.character(DBI::dbQuoteString(con, global_index)))
  }
}