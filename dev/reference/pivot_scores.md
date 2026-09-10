# Pivot scores into a forecast unit by comparator matrix

Pivots a set of scores into a matrix with one row per forecast unit
(excluding the `compare` column) and one column per comparator. Entries
are the values of `metric` and are `NA` where a comparator did not
provide a forecast for a given forecast unit.

The function is used by
[`pairwise_comparison_one_group()`](https://epiforecasts.io/scoringutils/dev/reference/pairwise_comparison_one_group.md)
to align the scores of all comparators once, rather than once per pair
of comparators. Exact duplicate rows are dropped silently; rows that
share a forecast unit and comparator but are not otherwise identical
raise an error, as the scores could then not be pivoted unambiguously.

## Usage

``` r
pivot_scores(scores, compare = "model", metric)
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

- metric:

  A string with the name of the metric to pivot. Unlike in
  [`get_pairwise_comparisons()`](https://epiforecasts.io/scoringutils/dev/reference/get_pairwise_comparisons.md),
  there is no default: the caller must supply a single metric present in
  `scores`.

## Value

A numeric matrix with one row per forecast unit and one column per
comparator. Column names are the comparators (as character).
