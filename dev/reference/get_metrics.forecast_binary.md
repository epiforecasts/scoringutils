# Get default metrics for binary forecasts

For binary forecasts, the default scoring rules are:

- "brier_score" =
  [`brier_score()`](https://epiforecasts.io/scoringutils/dev/reference/scoring-functions-binary.md)

- "log_score" =
  [`logs_binary()`](https://epiforecasts.io/scoringutils/dev/reference/scoring-functions-binary.md)

## Usage

``` r
# S3 method for class 'forecast_binary'
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

## Value

A list of scoring functions.

## Input format

![](figures/metrics-binary-point.png)

Overview of required input format for binary and point forecasts

## See also

Other get_metrics functions:
[`get_metrics()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.md),
[`get_metrics.forecast_nominal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_nominal.md),
[`get_metrics.forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_ordinal.md),
[`get_metrics.forecast_point()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_point.md),
[`get_metrics.forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_quantile.md),
[`get_metrics.forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample.md),
[`get_metrics.forecast_sample_multivariate()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample_multivariate.md),
[`get_metrics.scores()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.scores.md)

## Examples

``` r
get_metrics(example_binary)
#> $brier_score
#> function (observed, predicted) 
#> {
#>     assert_input_binary(observed, predicted)
#>     observed <- as.numeric(observed) - 1
#>     brierscore <- (observed - predicted)^2
#>     return(brierscore)
#> }
#> <bytecode: 0x56520e9647c0>
#> <environment: namespace:scoringutils>
#> 
#> $log_score
#> function (observed, predicted) 
#> {
#>     assert_input_binary(observed, predicted)
#>     observed <- as.numeric(observed) - 1
#>     logs <- -log(1 - abs(observed - predicted))
#>     return(logs)
#> }
#> <bytecode: 0x56520e9677b8>
#> <environment: namespace:scoringutils>
#> 
get_metrics(example_binary, select = "brier_score")
#> $brier_score
#> function (observed, predicted) 
#> {
#>     assert_input_binary(observed, predicted)
#>     observed <- as.numeric(observed) - 1
#>     brierscore <- (observed - predicted)^2
#>     return(brierscore)
#> }
#> <bytecode: 0x56520e9647c0>
#> <environment: namespace:scoringutils>
#> 
get_metrics(example_binary, exclude = "log_score")
#> $brier_score
#> function (observed, predicted) 
#> {
#>     assert_input_binary(observed, predicted)
#>     observed <- as.numeric(observed) - 1
#>     brierscore <- (observed - predicted)^2
#>     return(brierscore)
#> }
#> <bytecode: 0x56520e9647c0>
#> <environment: namespace:scoringutils>
#> 
```
