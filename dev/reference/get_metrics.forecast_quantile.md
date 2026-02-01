# Get default metrics for quantile-based forecasts

For quantile-based forecasts, the default scoring rules are:

- "wis" =
  [`wis()`](https://epiforecasts.io/scoringutils/dev/reference/wis.md)

- "overprediction" =
  [`overprediction_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/wis.md)

- "underprediction" =
  [`underprediction_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/wis.md)

- "dispersion" =
  [`dispersion_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/wis.md)

- "bias" =
  [`bias_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/bias_quantile.md)

- "interval_coverage_50" =
  [`interval_coverage()`](https://epiforecasts.io/scoringutils/dev/reference/interval_coverage.md)

- "interval_coverage_90" = purrr::partial( interval_coverage,
  interval_range = 90 )

- "ae_median" =
  [`ae_median_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/ae_median_quantile.md)

Note: The `interval_coverage_90` scoring rule is created by modifying
[`interval_coverage()`](https://epiforecasts.io/scoringutils/dev/reference/interval_coverage.md),
making use of the function
[`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html).
This construct allows the function to deal with arbitrary arguments in
`...`, while making sure that only those that
[`interval_coverage()`](https://epiforecasts.io/scoringutils/dev/reference/interval_coverage.md)
can accept get passed on to it. `interval_range = 90` is set in the
function definition, as passing an argument `interval_range = 90` to
[`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)
would mean it would also get passed to `interval_coverage_50`.

## Usage

``` r
# S3 method for class 'forecast_quantile'
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

![](figures/metrics-quantile.png)

Overview of required input format for quantile-based forecasts

## See also

Other get_metrics functions:
[`get_metrics()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.md),
[`get_metrics.forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_binary.md),
[`get_metrics.forecast_nominal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_nominal.md),
[`get_metrics.forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_ordinal.md),
[`get_metrics.forecast_point()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_point.md),
[`get_metrics.forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample.md),
[`get_metrics.forecast_sample_multivariate()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample_multivariate.md),
[`get_metrics.scores()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.scores.md)

## Examples

``` r
get_metrics(example_quantile, select = "wis")
#> $wis
#> function (observed, predicted, quantile_level, separate_results = FALSE, 
#>     weigh = TRUE, count_median_twice = FALSE, na.rm = FALSE) 
#> {
#>     assert_input_quantile(observed, predicted, quantile_level)
#>     reformatted <- quantile_to_interval(observed, predicted, 
#>         quantile_level)
#>     interval_ranges <- get_range_from_quantile(quantile_level[quantile_level != 
#>         0.5])
#>     complete_intervals <- duplicated(interval_ranges) | duplicated(interval_ranges, 
#>         fromLast = TRUE)
#>     if (!all(complete_intervals) && !isTRUE(na.rm)) {
#>         incomplete <- quantile_level[quantile_level != 0.5][!complete_intervals]
#>         cli_abort(c(`!` = "Not all quantile levels specified form symmetric prediction\n        intervals.\n        The following quantile levels miss a corresponding lower/upper bound:\n        {.val {incomplete}}.\n        You can drop incomplete prediction intervals using `na.rm = TRUE`."))
#>     }
#>     assert_logical(separate_results, len = 1)
#>     assert_logical(weigh, len = 1)
#>     assert_logical(count_median_twice, len = 1)
#>     assert_logical(na.rm, len = 1)
#>     if (separate_results) {
#>         cols <- c("wis", "dispersion", "underprediction", "overprediction")
#>     }
#>     else {
#>         cols <- "wis"
#>     }
#>     reformatted[, `:=`(eval(cols), do.call(interval_score, list(observed = observed, 
#>         lower = lower, upper = upper, interval_range = interval_range, 
#>         weigh = weigh, separate_results = separate_results)))]
#>     if (count_median_twice) {
#>         reformatted[, `:=`(weight, 1)]
#>     }
#>     else {
#>         reformatted[, `:=`(weight, ifelse(interval_range == 0, 
#>             0.5, 1))]
#>     }
#>     reformatted <- reformatted[, lapply(.SD, weighted.mean, na.rm = na.rm, 
#>         w = weight), by = "forecast_id", .SDcols = colnames(reformatted) %like% 
#>         paste(cols, collapse = "|")]
#>     if (separate_results) {
#>         return(list(wis = reformatted$wis, dispersion = reformatted$dispersion, 
#>             underprediction = reformatted$underprediction, overprediction = reformatted$overprediction))
#>     }
#>     else {
#>         return(reformatted$wis)
#>     }
#> }
#> <bytecode: 0x5602e2ba6ce8>
#> <environment: namespace:scoringutils>
#> 
```
