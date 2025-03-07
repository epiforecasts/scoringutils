% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-forecast-sample.R
\name{get_metrics.forecast_sample}
\alias{get_metrics.forecast_sample}
\title{Get default metrics for sample-based forecasts}
\usage{
\method{get_metrics}{forecast_sample}(x, select = NULL, exclude = NULL, ...)
}
\arguments{
\item{x}{A forecast object (a validated data.table with predicted and
observed values, see \code{\link[=as_forecast_binary]{as_forecast_binary()}}).}

\item{select}{A character vector of scoring rules to select from the list. If
\code{select} is \code{NULL} (the default), all possible scoring rules are returned.}

\item{exclude}{A character vector of scoring rules to exclude from the list.
If \code{select} is not \code{NULL}, this argument is ignored.}

\item{...}{unused}
}
\description{
For sample-based forecasts, the default scoring rules are:
\itemize{
\item "crps" = \code{\link[=crps_sample]{crps_sample()}}
\item "overprediction" = \code{\link[=overprediction_sample]{overprediction_sample()}}
\item "underprediction" = \code{\link[=underprediction_sample]{underprediction_sample()}}
\item "dispersion" = \code{\link[=dispersion_sample]{dispersion_sample()}}
\item "log_score" = \code{\link[=logs_sample]{logs_sample()}}
\item "dss" = \code{\link[=dss_sample]{dss_sample()}}
\item "mad" = \code{\link[=mad_sample]{mad_sample()}}
\item "bias" = \code{\link[=bias_sample]{bias_sample()}}
\item "ae_median" = \code{\link[=ae_median_sample]{ae_median_sample()}}
\item "se_mean" = \code{\link[=se_mean_sample]{se_mean_sample()}}
}
}
\section{Input format}{
\if{html}{
  \out{<div style="text-align: left">}
  \figure{metrics-sample.png}{options: style="width:750px;max-width:100\%;"}
  \out{</div><p>}
  Overview of required input format for sample-based forecasts
}
\if{latex}{
  \figure{metrics-sample.png}
}
}

\examples{
get_metrics(example_sample_continuous, exclude = "mad")
}
\seealso{
Other get_metrics functions: 
\code{\link{get_metrics}()},
\code{\link{get_metrics.forecast_binary}()},
\code{\link{get_metrics.forecast_nominal}()},
\code{\link{get_metrics.forecast_ordinal}()},
\code{\link{get_metrics.forecast_point}()},
\code{\link{get_metrics.forecast_quantile}()},
\code{\link{get_metrics.scores}()}
}
\concept{get_metrics functions}
\keyword{handle-metrics}
