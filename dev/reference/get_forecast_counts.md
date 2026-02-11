# Count number of available forecasts

Given a data set with forecasts, this function counts the number of
available forecasts. The level of grouping can be specified using the
`by` argument (e.g. to count the number of forecasts per model, or the
number of forecasts per model and location). This is useful to determine
whether there are any missing forecasts.

## Usage

``` r
get_forecast_counts(
  forecast,
  by = get_forecast_unit(forecast),
  collapse = c("quantile_level", "sample_id")
)
```

## Arguments

- forecast:

  A forecast object (a validated data.table with predicted and observed
  values).

- by:

  character vector or `NULL` (the default) that denotes the categories
  over which the number of forecasts should be counted. By default this
  will be the unit of a single forecast (i.e. all available columns
  (apart from a few "protected" columns such as 'predicted' and
  'observed') plus "quantile_level" or "sample_id" where present).

- collapse:

  character vector (default: `c("quantile_level", "sample_id"`) with
  names of categories for which the number of rows should be collapsed
  to one when counting. For example, a single forecast is usually
  represented by a set of several quantiles or samples and collapsing
  these to one makes sure that a single forecast only gets counted once.
  Setting `collapse = c()` would mean that all quantiles / samples would
  be counted as individual forecasts.

## Value

A data.table with columns as specified in `by` and an additional column
"count" with the number of forecasts.

## Examples

``` r
example_quantile |>
  as_forecast_quantile() |>
  get_forecast_counts(by = c("model", "target_type"))
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#> Key: <model, target_type>
#>                    model target_type count
#>                   <char>      <char> <int>
#> 1: EuroCOVIDhub-baseline       Cases   128
#> 2: EuroCOVIDhub-baseline      Deaths   128
#> 3: EuroCOVIDhub-ensemble       Cases   128
#> 4: EuroCOVIDhub-ensemble      Deaths   128
#> 5:       UMass-MechBayes       Cases     0
#> 6:       UMass-MechBayes      Deaths   128
#> 7:  epiforecasts-EpiNow2       Cases   128
#> 8:  epiforecasts-EpiNow2      Deaths   119
```
