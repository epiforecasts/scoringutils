# Do pairwise comparison for one set of forecasts

This function does the pairwise comparison for one set of forecasts, but
multiple models involved. It gets called from
[`get_pairwise_comparisons()`](https://epiforecasts.io/scoringutils/dev/reference/get_pairwise_comparisons.md).
[`get_pairwise_comparisons()`](https://epiforecasts.io/scoringutils/dev/reference/get_pairwise_comparisons.md)
splits the data into arbitrary subgroups specified by the user (e.g. if
pairwise comparison should be done separately for different forecast
targets) and then the actual pairwise comparison for that subgroup is
managed from `pairwise_comparison_one_group()`. In order to actually do
the comparison between two models over a subset of common forecasts it
calls
[`compare_forecasts()`](https://epiforecasts.io/scoringutils/dev/reference/compare_forecasts.md).

## Usage

``` r
pairwise_comparison_one_group(
  scores,
  metric,
  baseline,
  compare = "model",
  by,
  ...
)
```

## Arguments

- scores:

  An object of class `scores` (a data.table with scores and an
  additional attribute `metrics` as produced by
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)).

- metric:

  A string with the name of the metric for which a relative skill shall
  be computed. By default this is either "crps", "wis" or "brier_score"
  if any of these are available.

- baseline:

  A string with the name of a model. If a baseline is given, then a
  scaled relative skill with respect to the baseline will be returned.
  By default (`NULL`), relative skill will not be scaled with respect to
  a baseline model.

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

- ...:

  Additional arguments for the comparison between two models. See
  [`compare_forecasts()`](https://epiforecasts.io/scoringutils/dev/reference/compare_forecasts.md)
  for more information.

## Value

A data.table with the results of pairwise comparisons containing the
mean score ratios (`mean_scores_ratio`), unadjusted (`pval`) and
adjusted (`adj_pval`) p-values, and relative skill values of each model
(`..._relative_skill`). If a baseline model is given then the scaled
relative skill is reported as well (`..._scaled_relative_skill`).
