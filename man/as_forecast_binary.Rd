% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-forecast-binary.R
\name{as_forecast_binary}
\alias{as_forecast_binary}
\alias{as_forecast_binary.default}
\title{Create a \code{forecast} object for binary forecasts}
\usage{
as_forecast_binary(data, ...)

\method{as_forecast_binary}{default}(
  data,
  forecast_unit = NULL,
  observed = NULL,
  predicted = NULL,
  ...
)
}
\arguments{
\item{data}{A data.frame (or similar) with predicted and observed values.
See the details section of for additional information
on the required input format.}

\item{...}{Unused}

\item{forecast_unit}{(optional) Name of the columns in \code{data} (after
any renaming of columns) that denote the unit of a
single forecast. See \code{\link[=get_forecast_unit]{get_forecast_unit()}} for details.
If \code{NULL} (the default), all columns that are not required columns are
assumed to form the unit of a single forecast. If specified, all columns
that are not part of the forecast unit (or required columns) will be removed.}

\item{observed}{(optional) Name of the column in \code{data} that contains the
observed values. This column will be renamed to "observed".}

\item{predicted}{(optional) Name of the column in \code{data} that contains the
predicted values. This column will be renamed to "predicted".}
}
\value{
A \code{forecast} object of class \code{forecast_binary}
}
\description{
Process and validate a data.frame (or similar) or similar with forecasts
and observations. If the input passes all input checks, those functions will
be converted to a \code{forecast} object. A forecast object is a \code{data.table} with
a class \code{forecast} and an additional class that depends on the forecast type.

The arguments \code{observed}, \code{predicted}, etc. make it possible to rename
existing columns of the input data to match the required columns for a
forecast object. Using the argument \code{forecast_unit}, you can specify
the columns that uniquely identify a single forecast (and thereby removing
other, unneeded columns. See section "Forecast Unit" below for details).
}
\section{Required input}{
The input needs to be a data.frame or similar with the following columns:
\itemize{
\item \code{observed}: \code{factor} with exactly two levels representing the observed
values. The highest factor level is assumed to be the reference level.
This means that corresponding value in \code{predicted} represent the
probability that the observed value is equal to the highest factor level.
\item \code{predicted}: \code{numeric} with predicted probabilities, representing
the probability that the corresponding value in \code{observed} is equal to
the highest available factor level.
}

For convenience, we recommend an additional column \code{model} holding the name
of the forecaster or model that produced a prediction, but this is not
strictly necessary.

See the \link{example_binary} data set for an example.
}

\section{Forecast unit}{
In order to score forecasts, \code{scoringutils} needs to know which of the rows
of the data belong together and jointly form a single forecasts. This is
easy e.g. for point forecast, where there is one row per forecast. For
quantile or sample-based forecasts, however, there are multiple rows that
belong to a single forecast.

The \emph{forecast unit} or \emph{unit of a single forecast} is then described by the
combination of columns that uniquely identify a single forecast.
For example, we could have forecasts made by different models in various
locations at different time points, each for several weeks into the future.
The forecast unit could then be described as
\code{forecast_unit = c("model", "location", "forecast_date", "forecast_horizon")}.
\code{scoringutils} automatically tries to determine the unit of a single
forecast. It uses all existing columns for this, which means that no columns
must be present that are unrelated to the forecast unit. As a very simplistic
example, if you had an additional row, "even", that is one if the row number
is even and zero otherwise, then this would mess up scoring as \code{scoringutils}
then thinks that this column was relevant in defining the forecast unit.

In order to avoid issues, we recommend setting the forecast unit explicitly,
using the \code{forecast_unit} argument. This will simply drop unneeded columns,
while making sure that all necessary, 'protected columns' like "predicted"
or "observed" are retained.
}

\examples{
as_forecast_binary(
  example_binary,
  predicted = "predicted",
  forecast_unit = c("model", "target_type", "target_end_date",
                    "horizon", "location")
)
}
\seealso{
Other functions to create forecast objects: 
\code{\link{as_forecast_nominal}()},
\code{\link{as_forecast_ordinal}()},
\code{\link{as_forecast_point}()},
\code{\link{as_forecast_quantile}()},
\code{\link{as_forecast_sample}()}
}
\concept{functions to create forecast objects}
\keyword{as_forecast}
\keyword{transform}
