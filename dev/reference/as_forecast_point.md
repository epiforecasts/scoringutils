# Create a `forecast` object for point forecasts

When converting a `forecast_quantile` object into a `forecast_point`
object, the 0.5 quantile is extracted and returned as the point
forecast.

## Usage

``` r
as_forecast_point(data, ...)

# Default S3 method
as_forecast_point(
  data,
  forecast_unit = NULL,
  observed = NULL,
  predicted = NULL,
  ...
)

# S3 method for class 'forecast_quantile'
as_forecast_point(data, ...)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

- ...:

  Unused

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

## Value

A `forecast` object of class `forecast_point`

## Target format

The input for all further scoring needs to be a data.frame or similar
with the following columns:

- `observed`: Column of type `numeric` with observed values.

- `predicted`: Column of type `numeric` with predicted values.

For convenience, we recommend an additional column `model` holding the
name of the forecaster or model that produced a prediction, but this is
not strictly necessary.

See the
[example_point](https://epiforecasts.io/scoringutils/dev/reference/example_point.md)
data set for an example.

## See also

Other functions to create forecast objects:
[`as_forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_binary.md),
[`as_forecast_multivariate_sample()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_multivariate_sample.md),
[`as_forecast_nominal()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_nominal.md),
[`as_forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_ordinal.md),
[`as_forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_quantile.md),
[`as_forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_sample.md)
