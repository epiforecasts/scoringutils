# Simple permutation test

The implementation of the permutation test follows the function
`permutationTest` from the `surveillance` package by Michael Höhle,
Andrea Riebler and Michaela Paul. The function compares two vectors of
scores. It computes the mean of each vector independently and then takes
either the difference or the ratio of the two. This observed difference
or ratio is compared against the same test statistic based on
permutations of the original data.

Used in
[`get_pairwise_comparisons()`](https://epiforecasts.io/scoringutils/dev/reference/get_pairwise_comparisons.md).

## Usage

``` r
permutation_test(
  scores1,
  scores2,
  n_permutation = 999,
  one_sided = FALSE,
  comparison_mode = c("difference", "ratio")
)
```

## Arguments

- scores1:

  Vector of scores to compare against another vector of scores.

- scores2:

  A second vector of scores to compare against the first

- n_permutation:

  The number of replications to use for a permutation test. More
  replications yield more exact results, but require more computation.

- one_sided:

  Whether or not to compute a one-sided test. Default is `FALSE`.

- comparison_mode:

  How to compute the test statistic for the comparison of the two
  scores. Should be either "difference" or "ratio".

## Value

p-value of the permutation test
