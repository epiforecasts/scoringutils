# Variogram score for multivariate point forecasts

Compute the variogram score for multivariate point forecasts, treating
each point forecast as a single-sample ensemble. This is a thin wrapper
around
[`variogram_score_multivariate()`](https://epiforecasts.io/scoringutils/dev/reference/variogram_score_multivariate.md)
with `w = NULL`.

See
[`variogram_score_multivariate()`](https://epiforecasts.io/scoringutils/dev/reference/variogram_score_multivariate.md)
for details on the variogram score and its parameters.

## Usage

``` r
variogram_score_multivariate_point(
  observed,
  predicted,
  mv_group_id,
  w_vs = NULL,
  p = 0.5
)
```

## Arguments

- observed:

  A vector with observed values of size n

- predicted:

  Numeric matrix with one column, where each row corresponds to a target
  within a multivariate group.

- mv_group_id:

  Numeric vector of length n with ids indicating the grouping of
  predicted values. Conceptually, each row of the `predicted` matrix
  *could* be seen as a separate (univariate) forecast. The grouping id
  then groups several of those forecasts together, treating them as a
  single multivariate forecast.

- w_vs:

  Optional non-negative weight matrix for the pairwise comparisons
  between targets. Entry `w_vs[i, j]` controls the importance of the
  pair (i, j) in the score. Must be a symmetric square matrix with rows
  and columns equal to the number of targets within each multivariate
  group. If `NULL` (the default), all pairs are weighted equally.

- p:

  Numeric, order of the variogram score. This controls how pairwise
  differences are scaled: the score compares \|y_i - y_j\|^p across
  targets. Lower values of `p` give less weight to large differences,
  making the score more robust to outliers. Typical choices are 0.5 (the
  default) and 1.

## Value

A named numeric vector of scores, one per multivariate group. Lower
values are better.

## References

Scheuerer, M. and Hamill, T.M. (2015). Variogram-Based Proper Scoring
Rules for Probabilistic Forecasts of Multivariate Quantities. *Monthly
Weather Review*, 143, 1321-1334.
