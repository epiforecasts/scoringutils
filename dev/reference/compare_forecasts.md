# Compare a subset of common forecasts

This function compares two comparators based on the subset of forecasts
for which both comparators have made a prediction. It gets called from
[`pairwise_comparison_one_group()`](https://epiforecasts.io/scoringutils/dev/reference/pairwise_comparison_one_group.md),
which handles the comparison of multiple comparators on a single set of
forecasts (there are no subsets of forecasts to be distinguished).
[`pairwise_comparison_one_group()`](https://epiforecasts.io/scoringutils/dev/reference/pairwise_comparison_one_group.md)
in turn gets called from from
[`get_pairwise_comparisons()`](https://epiforecasts.io/scoringutils/dev/reference/get_pairwise_comparisons.md)
which can handle pairwise comparisons for a set of forecasts with
multiple subsets, e.g. pairwise comparisons for one set of forecasts,
but done separately for two different forecast targets.

## Usage

``` r
compare_forecasts(
  scores,
  compare = "model",
  name_comparator1,
  name_comparator2,
  metric,
  one_sided = FALSE,
  test_type = c("non_parametric", "permutation", NULL),
  n_permutations = 999
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

- name_comparator1:

  Character, name of the first comparator

- name_comparator2:

  Character, name of the comparator to compare against

- metric:

  A string with the name of the metric for which a relative skill shall
  be computed. By default this is either "crps", "wis" or "brier_score"
  if any of these are available.

- one_sided:

  Boolean, default is `FALSE`, whether two conduct a one-sided instead
  of a two-sided test to determine significance in a pairwise
  comparison.

- test_type:

  Character, either "non_parametric" (the default), "permutation", or
  NULL. This determines which kind of test shall be conducted to
  determine p-values. If NULL, no test will be conducted and p-values
  will be NA.

- n_permutations:

  Numeric, the number of permutations for a permutation test. Default is
  999.

## Value

A list with mean score ratios and p-values for the comparison between
two comparators

## Author

Johannes Bracher, <johannes.bracher@kit.edu>

Nikos Bosse <nikosbosse@gmail.com>
