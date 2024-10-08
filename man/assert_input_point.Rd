% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics-point.R
\name{assert_input_point}
\alias{assert_input_point}
\title{Assert that inputs are correct for point forecast}
\usage{
assert_input_point(observed, predicted)
}
\arguments{
\item{observed}{Input to be checked. Should be a numeric vector with the
observed values of size n.}

\item{predicted}{Input to be checked. Should be a numeric vector with the
predicted values of size n.}
}
\value{
Returns NULL invisibly if the assertion was successful and throws an
error otherwise.
}
\description{
Function assesses whether the inputs correspond to the
requirements for scoring point forecasts.
}
\keyword{internal_input_check}
