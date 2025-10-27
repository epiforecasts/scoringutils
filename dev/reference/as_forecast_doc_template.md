# General information on creating a `forecast` object

Process and validate a data.frame (or similar) or similar with forecasts
and observations. If the input passes all input checks, those functions
will be converted to a `forecast` object. A forecast object is a
`data.table` with a class `forecast` and an additional class that
depends on the forecast type.

The arguments `observed`, `predicted`, etc. make it possible to rename
existing columns of the input data to match the required columns for a
forecast object. Using the argument `forecast_unit`, you can specify the
columns that uniquely identify a single forecast (and thereby removing
other, unneeded columns. See section "Forecast Unit" below for details).

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

- forecast_unit:

  (optional) Name of the columns in `data` (after any renaming of
  columns) that denote the unit of a single forecast. See
  [`get_forecast_unit()`](https://epiforecasts.io/scoringutils/dev/reference/get_forecast_unit.md)
  for details. If `NULL` (the default), all columns that are not
  required columns are assumed to form the unit of a single forecast. If
  specified, all columns that are not part of the forecast unit (or
  required columns) will be removed.

- observed:

  (optional) Name of the column in `data` that contains the observed
  values. This column will be renamed to "observed".

- predicted:

  (optional) Name of the column in `data` that contains the predicted
  values. This column will be renamed to "predicted".

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
