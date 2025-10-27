# Create a `forecast` object for quantile-based forecasts

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
as_forecast_quantile(data, ...)

# Default S3 method
as_forecast_quantile(
  data,
  forecast_unit = NULL,
  observed = NULL,
  predicted = NULL,
  quantile_level = NULL,
  ...
)

# S3 method for class 'forecast_sample'
as_forecast_quantile(
  data,
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
  type = 7,
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

- quantile_level:

  (optional) Name of the column in `data` that contains the quantile
  level of the predicted values. This column will be renamed to
  "quantile_level". Only applicable to quantile-based forecasts.

- probs:

  A numeric vector of quantile levels for which quantiles will be
  computed. Corresponds to the `probs` argument in
  [`quantile()`](https://rdrr.io/r/stats/quantile.html).

- type:

  Type argument passed down to the quantile function. For more
  information, see
  [`quantile()`](https://rdrr.io/r/stats/quantile.html).

## Value

A `forecast` object of class `forecast_quantile`

## Target format

The input for all further scoring needs to be a data.frame or similar
with the following columns:

- `observed`: Column of type `numeric` with observed values.

- `predicted`: Column of type `numeric` with predicted values. Predicted
  values represent quantiles of the predictive distribution.

- `quantile_level`: Column of type `numeric`, denoting the quantile
  level of the corresponding predicted value. Quantile levels must be
  between 0 and 1.

For convenience, we recommend an additional column `model` holding the
name of the forecaster or model that produced a prediction, but this is
not strictly necessary.

See the
[example_quantile](https://epiforecasts.io/scoringutils/dev/reference/example_quantile.md)
data set for an example.

## Converting from `forecast_sample` to `forecast_quantile`

When creating a `forecast_quantile` object from a `forecast_sample`
object, the quantiles are estimated by computing empircal quantiles from
the samples via [`quantile()`](https://rdrr.io/r/stats/quantile.html).
Note that empirical quantiles are a biased estimator for the true
quantiles in particular in the tails of the distribution and when the
number of available samples is low.

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
[`as_forecast_multivariate_sample()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_multivariate_sample.md),
[`as_forecast_nominal()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_nominal.md),
[`as_forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_ordinal.md),
[`as_forecast_point()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_point.md),
[`as_forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_sample.md)

## Examples

``` r
as_forecast_quantile(
  example_quantile,
  predicted = "predicted",
  forecast_unit = c("model", "target_type", "target_end_date",
                    "horizon", "location")
)
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#> Forecast type: quantile
#> Forecast unit:
#> model, target_type, target_end_date, horizon, and location
#> 
#> Key: <location, target_end_date, target_type>
#>        observed quantile_level predicted                model target_type
#>           <num>          <num>     <int>               <char>      <char>
#>     1:   127300             NA        NA                 <NA>       Cases
#>     2:     4534             NA        NA                 <NA>      Deaths
#>     3:   154922             NA        NA                 <NA>       Cases
#>     4:     6117             NA        NA                 <NA>      Deaths
#>     5:   110183             NA        NA                 <NA>       Cases
#>    ---                                                                   
#> 20541:       78          0.850       352 epiforecasts-EpiNow2      Deaths
#> 20542:       78          0.900       397 epiforecasts-EpiNow2      Deaths
#> 20543:       78          0.950       499 epiforecasts-EpiNow2      Deaths
#> 20544:       78          0.975       611 epiforecasts-EpiNow2      Deaths
#> 20545:       78          0.990       719 epiforecasts-EpiNow2      Deaths
#>        target_end_date horizon location
#>                 <Date>   <num>   <char>
#>     1:      2021-01-02      NA       DE
#>     2:      2021-01-02      NA       DE
#>     3:      2021-01-09      NA       DE
#>     4:      2021-01-09      NA       DE
#>     5:      2021-01-16      NA       DE
#>    ---                                 
#> 20541:      2021-07-24       2       IT
#> 20542:      2021-07-24       2       IT
#> 20543:      2021-07-24       2       IT
#> 20544:      2021-07-24       2       IT
#> 20545:      2021-07-24       2       IT
```
