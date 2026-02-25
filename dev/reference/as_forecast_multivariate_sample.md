# Create a `forecast` object for sample-based multivariate forecasts

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

## Usage

``` r
as_forecast_multivariate_sample(data, ...)

# Default S3 method
as_forecast_multivariate_sample(
  data,
  joint_across = NULL,
  forecast_unit = NULL,
  observed = NULL,
  predicted = NULL,
  sample_id = NULL,
  ...
)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

- ...:

  Unused

- joint_across:

  Character vector with columns names that define the variables which
  are forecasted jointly. Conceptually, several univariate forecasts are
  pooled together to form a single multivariate forecasts. For example,
  if you have a column `country` and want to define a multivariate
  forecast for several countries at once, you could set
  `joint_across = "country"`.

- forecast_unit:

  (optional) Name of the columns in `data` (after any renaming of
  columns) that denote the unit of a single univariate (!) forecast. See
  [`get_forecast_unit()`](https://epiforecasts.io/scoringutils/dev/reference/get_forecast_unit.md)
  for details. If `NULL` (the default), all columns that are not
  required columns are assumed to form the unit of a single forecast. If
  specified, all columns that are not part of the forecast unit (or
  required columns) will be removed. Multivariate forecasts are defined
  by a) specifying the univariate forecast unit (i.e. the unit of a
  single forecast if that forecast were univariate) and b) specifying
  which variables are pooled together to form a multivariate forecast.

- observed:

  (optional) Name of the column in `data` that contains the observed
  values. This column will be renamed to "observed".

- predicted:

  (optional) Name of the column in `data` that contains the predicted
  values. This column will be renamed to "predicted".

- sample_id:

  (optional) Name of the column in `data` that contains the sample id.
  This column will be renamed to "sample_id".

## Value

A `forecast` object of class `forecast_sample`

## Target format

The input for all further scoring needs to be a data.frame or similar
with the following columns:

- `observed`: Column of type `numeric` with observed values.

- `predicted`: Column of type `numeric` with predicted values. Predicted
  values represent random samples from the predictive distribution.

- `sample_id`: Column of any type with unique identifiers (unique within
  a single forecast) for each sample.

- `mv_group_id`: Column of any type with unique identifiers (unique
  within a single forecast) for each multivariate group. This column is
  created automatically using the `forecast_unit` and the `joint_across`
  arguments.

For convenience, we recommend an additional column `model` holding the
name of the forecaster or model that produced a prediction, but this is
not strictly necessary.

See the
[example_sample_continuous](https://epiforecasts.io/scoringutils/dev/reference/example_sample_continuous.md)
and
[example_sample_discrete](https://epiforecasts.io/scoringutils/dev/reference/example_sample_discrete.md)
data set for an example

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

## See also

Other functions to create forecast objects:
[`as_forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_binary.md),
[`as_forecast_multivariate_point()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_multivariate_point.md),
[`as_forecast_nominal()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_nominal.md),
[`as_forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_ordinal.md),
[`as_forecast_point()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_point.md),
[`as_forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_quantile.md),
[`as_forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_sample.md)
