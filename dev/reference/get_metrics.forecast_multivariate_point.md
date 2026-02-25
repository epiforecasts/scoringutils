# Get default metrics for multivariate point forecasts

For multivariate point forecasts, the default scoring rule is:

- "variogram_score" =
  [`variogram_score_multivariate_point()`](https://epiforecasts.io/scoringutils/dev/reference/variogram_score_multivariate_point.md)

## Usage

``` r
# S3 method for class 'forecast_multivariate_point'
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

![](figures/metrics-binary-point.png)

Overview of required input format for binary and point forecasts

## See also

Other get_metrics functions:
[`get_metrics()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.md),
[`get_metrics.forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_binary.md),
[`get_metrics.forecast_multivariate_sample()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_multivariate_sample.md),
[`get_metrics.forecast_nominal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_nominal.md),
[`get_metrics.forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_ordinal.md),
[`get_metrics.forecast_point()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_point.md),
[`get_metrics.forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_quantile.md),
[`get_metrics.forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample.md),
[`get_metrics.scores()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.scores.md)

## Examples

``` r
data <- data.frame(
  observed = c(1, 2, 3),
  predicted = c(1.1, 2.2, 3.3),
  target = c("a", "b", "c"),
  model = "m1",
  date = "2020-01-01"
)
ex <- as_forecast_multivariate_point(
  data,
  forecast_unit = c("model", "date", "target"),
  joint_across = "target"
)
get_metrics(ex)
#> $variogram_score
#> function (observed, predicted, mv_group_id, w_vs = NULL, p = 0.5) 
#> {
#>     assert_numeric(observed, min.len = 1)
#>     assert_numeric(as.vector(predicted), min.len = 1)
#>     assert_numeric(mv_group_id, len = length(observed))
#>     unique_groups <- unique(mv_group_id)
#>     vs <- vapply(unique_groups, function(group) {
#>         idx <- which(mv_group_id == group)
#>         scoringRules::vs_sample(y = observed[idx], dat = predicted[idx, 
#>             , drop = FALSE], w_vs = w_vs, p = p)
#>     }, numeric(1))
#>     names(vs) <- unique_groups
#>     return(vs)
#> }
#> <bytecode: 0x56291d316ad0>
#> <environment: namespace:scoringutils>
#> 
```
