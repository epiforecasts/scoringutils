# Compare two aligned vectors of scores

Computes the mean score ratio and, optionally, a p-value for two vectors
of scores that have already been aligned, i.e. where `values_x[i]` and
`values_y[i]` are the scores of two comparators for the same forecast
unit. This is the shared computational core of
[`pairwise_comparison_one_group()`](https://epiforecasts.io/scoringutils/dev/reference/pairwise_comparison_one_group.md)
and
[`compare_forecasts()`](https://epiforecasts.io/scoringutils/dev/reference/compare_forecasts.md).

## Usage

``` r
compare_scores(
  values_x,
  values_y,
  one_sided = FALSE,
  test_type = c("non_parametric", "permutation", NULL),
  n_permutations = 999
)
```

## Arguments

- values_x:

  Numeric vector of scores of the first comparator.

- values_y:

  Numeric vector of scores of the second comparator, aligned with
  `values_x`.

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
