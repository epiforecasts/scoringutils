% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_-functions.R
\name{get_metrics}
\alias{get_metrics}
\title{Get names of the metrics that were used for scoring}
\usage{
get_metrics(scores, error = FALSE)
}
\arguments{
\item{scores}{A data.table with an attribute \code{metrics}.}

\item{error}{Throw an error if there is no attribute called \code{metrics}?
Default is FALSE.}
}
\value{
Character vector with the names of the scoring rules that were used
for scoring or \code{NULL} if no scores were computed previously.
}
\description{
When applying a scoring rule via \code{\link[=score]{score()}}, the names of the scoring rules
become column names of the
resulting data.table. In addition, an attribute \code{metrics} will be
added to the output, holding the names of the scores as a vector.
This is done so that a function like \code{\link[=get_forecast_unit]{get_forecast_unit()}} can still
identify which columns are part of the forecast unit and which hold a score.

\code{get_metrics()} access and returns this attribute. If there is no
attribute, the function will return NULL. Users can control whether the
function should error instead via the \code{error} argument.

\code{get_metrics()} also checks whether the names of the scores stored in
the attribute are column names of the data and will throw a warning if not.
This can happen if you rename columns after scoring. You can either run
\code{\link[=score]{score()}} again, specifying names for the scoring rules manually, or you
can update the attribute manually using
\code{attr(scores, "metrics") <- c("names", "of", "your", "scores")} (the
order does not matter).
}
\keyword{check-forecasts}
