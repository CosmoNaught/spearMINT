% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global_utils.R
\name{generate_param_name}
\alias{generate_param_name}
\title{Generate Parameter Name}
\usage{
generate_param_name(index)
}
\arguments{
\item{index}{An integer representing the index for which to generate a parameter name.}
}
\value{
A string representing the generated parameter name.
}
\description{
This function generates a parameter name based on a given index. The parameter name consists of a combination
of letters and numbers. The function handles multi-letter prefixes (e.g., AA, AB, ...) once the index exceeds
the size of the alphabet.
}
\examples{
generate_param_name(1)    # Returns "A1"
generate_param_name(1001) # Returns "B1"
generate_param_name(26001) # Returns "Z1"
generate_param_name(27001) # Returns "AA1"
}
