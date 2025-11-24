# Get default metrics for sample-based forecasts

For sample-based multivariate forecasts, the default scoring rules are:

- "energy_score" =
  [`energy_score_multivariate()`](https://epiforecasts.io/scoringutils/dev/reference/energy_score_multivariate.md)

## Usage

``` r
# S3 method for class 'forecast_sample_multivariate'
get_metrics(x, select = NULL, exclude = NULL, ...)
```

## Arguments

- x:

  A forecast object (a validated data.table with predicted and observed
  values, see
  [`as_forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_binary.md)).

- select:

  A character vector of scoring rules to select from the list. If
  `select` is `NULL` (the default), all possible scoring rules are
  returned.

- exclude:

  A character vector of scoring rules to exclude from the list. If
  `select` is not `NULL`, this argument is ignored.

- ...:

  unused

## Input format

![](figures/metrics-sample.png)

Overview of required input format for sample-based forecasts

## See also

Other get_metrics functions:
[`get_metrics()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.md),
[`get_metrics.forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_binary.md),
[`get_metrics.forecast_nominal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_nominal.md),
[`get_metrics.forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_ordinal.md),
[`get_metrics.forecast_point()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_point.md),
[`get_metrics.forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_quantile.md),
[`get_metrics.forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample.md),
[`get_metrics.scores()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.scores.md)

## Examples

``` r
example <- as_forecast_multivariate_sample(
  example_sample_continuous, joint_across = c("location", "location_name")
)
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
get_metrics(example)
#> $energy_score
#> function (observed, predicted, mv_group_id, w = NULL) 
#> {
#>     assert_input_multivariate_sample(observed, predicted, mv_group_id)
#>     unique_groups <- unique(mv_group_id)
#>     energy_score <- vapply(unique_groups, function(group) {
#>         idx <- which(mv_group_id == group)
#>         es_sample(y = observed[idx], dat = predicted[idx, , drop = FALSE], 
#>             w = w)
#>     }, numeric(1))
#>     names(energy_score) <- unique_groups
#>     return(energy_score)
#> }
#> <bytecode: 0x559930468098>
#> <environment: namespace:scoringutils>
#> 
```
