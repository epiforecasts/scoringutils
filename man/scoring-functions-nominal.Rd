% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics-nominal.R
\name{logs_categorical}
\alias{logs_categorical}
\title{Log score for categorical outcomes}
\usage{
logs_categorical(observed, predicted, predicted_label)
}
\arguments{
\item{observed}{Factor of length n with N levels holding the
observed values.}

\item{predicted}{nxN matrix of predictive probabilities, n (number of rows)
being the number of observations and N (number of columns) the number of
possible outcomes.}

\item{predicted_label}{Factor of length N, denoting the outcome
that the probabilities in \code{predicted} correspond to.}
}
\value{
A numeric vector of size n with log scores
}
\description{
\strong{Log score for categorical (nominal or ordinal) outcomes}

The Log Score is the negative logarithm of the probability
assigned to the observed value. It is a proper scoring rule. Small values
are better (best is zero, worst is infinity).
}
\section{Input format}{
\if{html}{
  \out{<div style="text-align: left">}
  \figure{metrics-nominal.png}{options: style="width:750px;max-width:100\%;"}
  \out{</div><p>}
  Overview of required input format for nominal forecasts
}
\if{latex}{
  \figure{metrics-nominal.png}
}
}

\examples{
factor_levels <- c("one", "two", "three")
predicted_label <- factor(c("one", "two", "three"), levels = factor_levels)
observed <- factor(c("one", "three", "two"), levels = factor_levels)
predicted <- matrix(
  c(0.8, 0.1, 0.1,
    0.1, 0.2, 0.7,
    0.4, 0.4, 0.2),
  nrow = 3,
  byrow = TRUE
)
logs_categorical(observed, predicted, predicted_label)
}
\seealso{
Other log score functions: 
\code{\link{logs_sample}()},
\code{\link{scoring-functions-binary}}
}
\concept{log score functions}
\keyword{metric}
