# Get metrics

Generic function to to obtain default metrics available for scoring or
metrics that were used for scoring.

- If called on a `forecast` object it returns a list of functions that
  can be used for scoring.

- If called on a `scores` object (see
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)),
  it returns a character vector with the names of the metrics that were
  used for scoring.

See the documentation for the actual methods in the `See Also` section
below for more details. Alternatively call
`?get_metrics.<forecast_type>` or
[`?get_metrics.scores`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.scores.md).

## Usage

``` r
get_metrics(x, ...)
```

## Arguments

- x:

  A `forecast` or `scores` object.

- ...:

  Additional arguments passed to the method.

## See also

Other get_metrics functions:
[`get_metrics.forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_binary.md),
[`get_metrics.forecast_nominal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_nominal.md),
[`get_metrics.forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_ordinal.md),
[`get_metrics.forecast_point()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_point.md),
[`get_metrics.forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_quantile.md),
[`get_metrics.forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample.md),
[`get_metrics.forecast_sample_multivariate()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample_multivariate.md),
[`get_metrics.scores()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.scores.md)
