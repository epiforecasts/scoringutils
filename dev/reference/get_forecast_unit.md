# Get unit of a single forecast

Helper function to get the unit of a single forecast, i.e. the column
names that define where a single forecast was made for. This just takes
all columns that are available in the data and subtracts the columns
that are protected, i.e. those returned by
[`get_protected_columns()`](https://epiforecasts.io/scoringutils/dev/reference/get_protected_columns.md)
as well as the names of the metrics that were specified during scoring,
if any.

## Usage

``` r
get_forecast_unit(data)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

## Value

A character vector with the column names that define the unit of a
single forecast

## Forecast unit

In order to score forecasts, `scoringutils` needs to know which of the
rows of the data belong together and jointly form a single forecast.
This is easy e.g. for point forecast, where there is one row per
forecast. For quantile or sample-based forecasts, however, there are
multiple rows that belong to a single forecast. (For a multivariate
forecast, several univariate forecasts are pooled together to form a
joint forecast. In the multivariate case, "forecast unit" still refers
to the forecast unit of the univariate forecasts that are pooled
together to form the multivariate forecast.)

The *forecast unit* or *unit of a single forecast* is then described by
the combination of columns that uniquely identify a single forecast. For
example, we could have forecasts made by different models in various
locations at different time points, each for several weeks into the
future. The forecast unit could then be described as
`forecast_unit = c("model", "location", "forecast_date", "forecast_horizon")`.
`scoringutils` automatically tries to determine the unit of a single
forecast. It uses all existing columns for this, which means that no
columns must be present that are unrelated to the forecast unit. As a
very simplistic example, if you had an additional row, "even", that is
one if the row number is even and zero otherwise, then this would mess
up scoring as `scoringutils` then thinks that this column was relevant
in defining the forecast unit.

In order to avoid issues, we recommend setting the forecast unit
explicitly, using the `forecast_unit` argument. This will simply drop
unneeded columns, while making sure that all necessary, 'protected
columns' like "predicted" or "observed" are retained.
