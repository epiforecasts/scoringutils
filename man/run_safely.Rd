% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score.R
\name{run_safely}
\alias{run_safely}
\title{Run a function safely}
\usage{
run_safely(..., fun, metric_name)
}
\arguments{
\item{...}{Arguments to pass to \code{fun}.}

\item{fun}{A function to execute.}

\item{metric_name}{A character string with the name of the metric. Used to
provide a more informative warning message in case \code{fun} errors.}
}
\value{
The result of \code{fun} or \code{NULL} if \code{fun} errors
}
\description{
This is a wrapper/helper function designed to run a function safely
when it is not completely clear what arguments could be passed to the
function.

All named arguments in \code{...} that are not accepted by \code{fun} are removed.
All unnamed arguments are passed on to the function. In case \code{fun} errors,
the error will be converted to a warning and \code{run_safely} returns \code{NULL}.

\code{run_safely} can be useful when constructing functions to be used as
metrics in \code{\link[=score]{score()}}.
}
\examples{
f <- function(x) {x}
scoringutils:::run_safely(2, fun = f, metric_name = "f")
scoringutils:::run_safely(2, y = 3, fun = f, metric_name = "f")
scoringutils:::run_safely(fun = f, metric_name = "f")
scoringutils:::run_safely(y = 3, fun = f, metric_name = "f")
}
\keyword{internal}
