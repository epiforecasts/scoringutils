# Create a `forecast` object for binary forecasts

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
as_forecast_binary(data, ...)

# Default S3 method
as_forecast_binary(
  data,
  forecast_unit = NULL,
  observed = NULL,
  predicted = NULL,
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

A `forecast` object of class `forecast_binary`

## Target format

The input for all further scoring needs to be a data.frame or similar
with the following columns:

- `observed`: `factor` with exactly two levels representing the observed
  values. The highest factor level is assumed to be the reference level.
  This means that corresponding value in `predicted` represent the
  probability that the observed value is equal to the highest factor
  level.

- `predicted`: `numeric` with predicted probabilities, representing the
  probability that the corresponding value in `observed` is equal to the
  highest available factor level.

For convenience, we recommend an additional column `model` holding the
name of the forecaster or model that produced a prediction, but this is
not strictly necessary.

See the
[example_binary](https://epiforecasts.io/scoringutils/dev/reference/example_binary.md)
data set for an example.

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
[`as_forecast_multivariate_sample()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_multivariate_sample.md),
[`as_forecast_nominal()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_nominal.md),
[`as_forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_ordinal.md),
[`as_forecast_point()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_point.md),
[`as_forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_quantile.md),
[`as_forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_sample.md)

## Examples

``` r
as_forecast_binary(
  example_binary,
  predicted = "predicted",
  forecast_unit = c("model", "target_type", "target_end_date",
                    "horizon", "location")
)
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#> Forecast type: binary
#> Forecast unit:
#> model, target_type, target_end_date, horizon, and location
#> 
#>       predicted observed                 model target_type target_end_date
#>           <num>   <fctr>                <char>      <char>          <Date>
#>    1:        NA     <NA>                  <NA>       Cases      2021-01-02
#>    2:        NA     <NA>                  <NA>      Deaths      2021-01-02
#>    3:        NA     <NA>                  <NA>       Cases      2021-01-09
#>    4:        NA     <NA>                  <NA>      Deaths      2021-01-09
#>    5:        NA     <NA>                  <NA>       Cases      2021-01-16
#>   ---                                                                     
#> 1027:     0.250        0 EuroCOVIDhub-baseline      Deaths      2021-07-24
#> 1028:     0.475        0       UMass-MechBayes      Deaths      2021-07-24
#> 1029:     0.450        0       UMass-MechBayes      Deaths      2021-07-24
#> 1030:     0.375        0  epiforecasts-EpiNow2      Deaths      2021-07-24
#> 1031:     0.300        0  epiforecasts-EpiNow2      Deaths      2021-07-24
#>       horizon location
#>         <num>   <char>
#>    1:      NA       DE
#>    2:      NA       DE
#>    3:      NA       DE
#>    4:      NA       DE
#>    5:      NA       DE
#>   ---                 
#> 1027:       2       IT
#> 1028:       3       IT
#> 1029:       2       IT
#> 1030:       3       IT
#> 1031:       2       IT
```
