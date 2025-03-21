% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics-nominal.R
\name{assert_input_categorical}
\alias{assert_input_categorical}
\title{Assert that inputs are correct for categorical forecasts}
\usage{
assert_input_categorical(observed, predicted, predicted_label, ordered = NA)
}
\arguments{
\item{observed}{Input to be checked. Should be a factor of length n with
N levels holding the observed values. n is the number of observations and
N is the number of possible outcomes the observed values can assume.}

\item{predicted}{Input to be checked. Should be nxN matrix of predicted
probabilities, n (number of rows) being the number of data points and N
(number of columns) the number of possible outcomes the observed values
can assume.
If \code{observed} is just a single number, then predicted can just be a
vector of size N.
Values represent the probability that the corresponding value
in \code{observed} will be equal to the factor level referenced in
\code{predicted_label}.}

\item{predicted_label}{Factor of length N with N levels, where N is the
number of possible outcomes the observed values can assume.}

\item{ordered}{Value indicating whether factors have to be ordered or not.
Defaults to \code{NA}, which means that the check is not performed.}
}
\value{
Returns NULL invisibly if the assertion was successful and throws an
error otherwise.
}
\description{
Function assesses whether the inputs correspond to the
requirements for scoring categorical, i.e. either nominal or ordinal
forecasts.
}
\keyword{internal_input_check}
