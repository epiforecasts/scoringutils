# Get names of the metrics that were used for scoring

When applying a scoring rule via
[`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md),
the names of the scoring rules become column names of the resulting
data.table. In addition, an attribute `metrics` will be added to the
output, holding the names of the scores as a vector.

This is done so that functions like
[`get_forecast_unit()`](https://epiforecasts.io/scoringutils/dev/reference/get_forecast_unit.md)
or
[`summarise_scores()`](https://epiforecasts.io/scoringutils/dev/reference/summarise_scores.md)
can still identify which columns are part of the forecast unit and which
hold a score.

[`get_metrics()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.md)
accesses and returns the `metrics` attribute. If there is no attribute,
the function will return `NULL` (or, if `error = TRUE` will produce an
error instead). In addition, it checks the column names of the input for
consistency with the data stored in the `metrics` attribute.

**Handling a missing or inconsistent `metrics` attribute**:

If the metrics attribute is missing or is not consistent with the column
names of the data.table, you can either

- run
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)
  again, specifying names for the scoring rules manually, or

- add/update the attribute manually using
  `attr(scores, "metrics") <- c("names", "of", "your", "scores")` (the
  order does not matter).

## Usage

``` r
# S3 method for class 'scores'
get_metrics(x, error = FALSE, ...)
```

## Arguments

- x:

  A `scores` object, (a data.table with an attribute `metrics` as
  produced by
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)).

- error:

  Throw an error if there is no attribute called `metrics`? Default is
  FALSE.

- ...:

  unused

## Value

Character vector with the names of the scoring rules that were used for
scoring.

## See also

Other get_metrics functions:
[`get_metrics()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.md),
[`get_metrics.forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_binary.md),
[`get_metrics.forecast_nominal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_nominal.md),
[`get_metrics.forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_ordinal.md),
[`get_metrics.forecast_point()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_point.md),
[`get_metrics.forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_quantile.md),
[`get_metrics.forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample.md),
[`get_metrics.forecast_sample_multivariate()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample_multivariate.md)
