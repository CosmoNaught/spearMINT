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