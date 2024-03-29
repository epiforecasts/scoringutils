% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-input-helpers.R
\name{test_columns_present}
\alias{test_columns_present}
\title{Test whether all column names are present in a data.frame}
\usage{
test_columns_present(data, columns)
}
\arguments{
\item{data}{A data.frame or similar to be checked}

\item{columns}{A character vector of column names to check}
}
\value{
Returns TRUE if all columns are present and FALSE otherwise
}
\description{
The function checks whether all column names are present. If
one or more columns are missing, the function returns FALSE. If all columns
are present, the function returns TRUE.
}
\keyword{internal_input_check}
