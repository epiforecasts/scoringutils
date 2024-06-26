% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_-functions.R
\name{get_forecast_type}
\alias{get_forecast_type}
\title{Infer forecast type from data}
\usage{
get_forecast_type(data)
}
\arguments{
\item{data}{A data.frame (or similar) with predicted and observed values.
See the details section of \code{\link[=as_forecast]{as_forecast()}} for additional information
on required input formats.}
}
\value{
Character vector of length one with either "binary", "quantile",
"sample" or "point".
}
\description{
Helper function to infer the forecast type based on a
data.frame or similar with forecasts and observed values. See the details
section below for information on the different forecast types.
}
\section{Forecast types and input formats}{
Various different forecast types / forecast formats are supported. At the
moment, those are:
\itemize{
\item point forecasts
\item binary forecasts ("soft binary classification")
\item Probabilistic forecasts in a quantile-based format (a forecast is
represented as a set of predictive quantiles)
\item Probabilistic forecasts in a sample-based format (a forecast is represented
as a set of predictive samples)
}

Forecast types are determined based on the columns present in the input data.
Here is an overview of the required format for each forecast type:
\if{html}{
  \out{<div style="text-align: left">}
  \figure{required-inputs.png}{options: style="width:750px;max-width:100\%;"}
  \out{</div>}
}
\if{latex}{
  \figure{required-inputs.png}
}

\emph{All forecast types} require a data.frame or similar with columns \code{observed}
\code{predicted}, and \code{model}.

\emph{Point forecasts} require a column \code{observed} of type numeric and a column
\code{predicted} of type numeric.

\emph{Binary forecasts} require a column \code{observed} of type factor with exactly
two levels and a column \code{predicted} of type numeric with probabilities,
corresponding to the probability that \code{observed} is equal to the second
factor level. See details \link[=brier_score]{here} for more information.

\emph{Quantile-based forecasts} require a column \code{observed} of type numeric,
a column \code{predicted} of type numeric, and a column \code{quantile_level} of type
numeric with quantile-levels (between 0 and 1).

\emph{Sample-based forecasts} require a column \code{observed} of type numeric,
a column \code{predicted} of type numeric, and a column \code{sample_id} of type
numeric with sample indices.

For more information see the vignettes and the example data
(\link{example_quantile}, \link{example_sample_continuous}, \link{example_sample_discrete},
\code{\link[=example_point]{example_point()}}, and \link{example_binary}).
}

\keyword{check-forecasts}
