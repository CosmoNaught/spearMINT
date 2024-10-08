% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orderly_utils.R
\name{check_parameter_set}
\alias{check_parameter_set}
\title{Check a Single Parameter Set}
\usage{
check_parameter_set(i)
}
\arguments{
\item{i}{An individual parameter set to check.}
}
\value{
A list containing the \code{parameter_set}, a success flag, and either the \code{id} or an error message.
}
\description{
This function checks a single \code{parameter_set} by dynamically constructing a query
and running it using the \code{orderly2::orderly_metadata_extract} function.
}
\keyword{internal}
