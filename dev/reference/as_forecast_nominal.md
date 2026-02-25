# Create a `forecast` object for nominal forecasts

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
as_forecast_nominal(data, ...)

# Default S3 method
as_forecast_nominal(
  data,
  forecast_unit = NULL,
  observed = NULL,
  predicted = NULL,
  predicted_label = NULL,
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

- predicted_label:

  (optional) Name of the column in `data` that denotes the outcome to
  which a predicted probability corresponds to. This column will be
  renamed to "predicted_label".

## Value

A `forecast` object of class `forecast_nominal`

## Details

Nominal forecasts are a form of categorical forecasts and represent a
generalisation of binary forecasts to multiple outcomes. The possible
outcomes that the observed values can assume are not ordered.

## Target format

The input for all further scoring needs to be a data.frame or similar
with the following columns:

- `observed`: Column with observed values of type `factor` with N
  levels, where N is the number of possible outcomes. The levels of the
  factor represent the possible outcomes that the observed values can
  assume.

- `predicted`: `numeric` column with predicted probabilities. The values
  represent the probability that the observed value is equal to the
  factor level denoted in `predicted_label`. Note that forecasts must be
  complete, i.e. there must be a probability assigned to every possible
  outcome and those probabilities must sum to one.

- `predicted_label`: `factor` with N levels, denoting the outcome that
  the probabilities in `predicted` correspond to.

For convenience, we recommend an additional column `model` holding the
name of the forecaster or model that produced a prediction, but this is
not strictly necessary.

See the
[example_nominal](https://epiforecasts.io/scoringutils/dev/reference/example_nominal.md)
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
[`as_forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_binary.md),
[`as_forecast_multivariate_point()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_multivariate_point.md),
[`as_forecast_multivariate_sample()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_multivariate_sample.md),
[`as_forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_ordinal.md),
[`as_forecast_point()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_point.md),
[`as_forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_quantile.md),
[`as_forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_sample.md)

## Examples

``` r
as_forecast_nominal(
  na.omit(example_nominal),
  predicted = "predicted",
  forecast_unit = c("model", "target_type", "target_end_date",
                    "horizon", "location")
)
#> Forecast type: nominal
#> Forecast unit:
#> model, target_type, target_end_date, horizon, and location
#> 
#>       observed predicted_label predicted                 model target_type
#>         <fctr>          <fctr>     <num>                <char>      <char>
#>    1:      low             low     0.525 EuroCOVIDhub-ensemble       Cases
#>    2:      low             low     0.075 EuroCOVIDhub-baseline       Cases
#>    3:      low             low     0.150  epiforecasts-EpiNow2       Cases
#>    4:   medium             low     0.100 EuroCOVIDhub-ensemble      Deaths
#>    5:   medium             low     0.275 EuroCOVIDhub-baseline      Deaths
#>   ---                                                                     
#> 2657:      low          medium     0.300 EuroCOVIDhub-baseline      Deaths
#> 2658:   medium          medium     0.850       UMass-MechBayes      Deaths
#> 2659:      low          medium     0.825       UMass-MechBayes      Deaths
#> 2660:   medium          medium     0.275  epiforecasts-EpiNow2      Deaths
#> 2661:      low          medium     0.375  epiforecasts-EpiNow2      Deaths
#>       target_end_date horizon location
#>                <Date>   <num>   <char>
#>    1:      2021-05-08       1       DE
#>    2:      2021-05-08       1       DE
#>    3:      2021-05-08       1       DE
#>    4:      2021-05-08       1       DE
#>    5:      2021-05-08       1       DE
#>   ---                                 
#> 2657:      2021-07-24       2       IT
#> 2658:      2021-07-24       3       IT
#> 2659:      2021-07-24       2       IT
#> 2660:      2021-07-24       3       IT
#> 2661:      2021-07-24       2       IT
```
