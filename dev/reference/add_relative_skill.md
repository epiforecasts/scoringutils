# Add relative skill scores based on pairwise comparisons

Adds a columns with relative skills computed by running pairwise
comparisons on the scores. For more information on the computation of
relative skill, see
[`get_pairwise_comparisons()`](https://epiforecasts.io/scoringutils/dev/reference/get_pairwise_comparisons.md).
Relative skill will be calculated for the aggregation level specified in
`by`.

## Usage

``` r
add_relative_skill(
  scores,
  compare = "model",
  by = NULL,
  metric = intersect(c("wis", "crps", "brier_score"), names(scores)),
  baseline = NULL,
  ...
)
```

## Arguments

- scores:

  An object of class `scores` (a data.table with scores and an
  additional attribute `metrics` as produced by
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)).

- compare:

  Character vector with a single colum name that defines the elements
  for the pairwise comparison. For example, if this is set to "model"
  (the default), then elements of the "model" column will be compared.

- by:

  Character vector with column names that define further grouping levels
  for the pairwise comparisons. By default this is `NULL` and there will
  be one relative skill score per distinct entry of the column selected
  in `compare`. If further columns are given here, for example,
  `by = "location"` with `compare = "model"`, then one separate relative
  skill score is calculated for every model in every location.

- metric:

  A string with the name of the metric for which a relative skill shall
  be computed. By default this is either "crps", "wis" or "brier_score"
  if any of these are available.

- baseline:

  A string with the name of a model. If a baseline is given, then a
  scaled relative skill with respect to the baseline will be returned.
  By default (`NULL`), relative skill will not be scaled with respect to
  a baseline model.

- ...:

  Additional arguments for the comparison between two models. See
  [`compare_forecasts()`](https://epiforecasts.io/scoringutils/dev/reference/compare_forecasts.md)
  for more information.
