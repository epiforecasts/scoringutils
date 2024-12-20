% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics-ordinal.R
\name{assert_input_ordinal}
\alias{assert_input_ordinal}
\title{Assert that inputs are correct for ordinal forecasts}
\usage{
assert_input_ordinal(observed, predicted, predicted_label)
}
\arguments{
\item{observed}{Input to be checked. Should be an ordered factor of length n
with N levels holding the observed values. n is the number of observations
and N is the number of possible outcomes the observed values can assume.}

\item{predicted}{Input to be checked. Should be nxN matrix of predicted
probabilities, n (number of rows) being the number of data points and N
(number of columns) the number of possible outcomes the observed values
can assume.
If \code{observed} is just a single number, then predicted can just be a
vector of size N.
Values represent the probability that the corresponding value
in \code{observed} will be equal to factor level referenced in \code{predicted_label}.}

\item{predicted_label}{Ordered factor of length N with N levels, where N is
the number of possible outcomes the observed values can assume.}
}
\value{
Returns NULL invisibly if the assertion was successful and throws an
error otherwise.
}
\description{
Function assesses whether the inputs correspond to the
requirements for scoring ordinal forecasts.
}
\keyword{internal_input_check}
