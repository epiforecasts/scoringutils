% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_-functions.R
\name{get_duplicate_forecasts}
\alias{get_duplicate_forecasts}
\title{Find duplicate forecasts}
\usage{
get_duplicate_forecasts(data, counts = FALSE)
}
\arguments{
\item{data}{A data.frame as used for \code{\link[=score]{score()}}}

\item{counts}{Should the output show the number of duplicates per forecast
unit instead of the individual duplicated rows? Default is \code{FALSE}.}
}
\value{
A data.frame with all rows for which a duplicate forecast was found
}
\description{
Helper function to identify duplicate forecasts, i.e.
instances where there is more than one forecast for the same prediction
target.
}
\examples{
example <- rbind(example_quantile, example_quantile[1000:1010])
get_duplicate_forecasts(example)
}
\keyword{check-forecasts}
