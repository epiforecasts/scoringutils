# Variogram score for multivariate forecasts

Compute the variogram score for each multivariate group defined by
`mv_group_id`. The variogram score (Scheuerer and Hamill, 2015) assesses
whether a forecast captures the correlation structure across the targets
being forecast jointly (e.g. locations, age groups). For each pair of
targets (i, j), it compares the observed absolute difference \|y_i -
y_j\|^p against the expected absolute difference under the forecast
distribution. A forecast that misspecifies correlations between targets
will predict pairwise differences that do not match the observations,
resulting in a higher score.

The score is computed using
[`scoringRules::vs_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_multiv.html).

## Usage

``` r
variogram_score_multivariate(
  observed,
  predicted,
  mv_group_id,
  w = NULL,
  w_vs = NULL,
  p = 0.5
)
```

## Arguments

- observed:

  A vector with observed values of size n

- predicted:

  nxN matrix of predictive samples, n (number of rows) being the number
  of data points and N (number of columns) the number of Monte Carlo
  samples. Alternatively, if n = 1, `predicted` can just be a vector of
  size n.

- mv_group_id:

  Numeric vector of length n with ids indicating the grouping of
  predicted values. Conceptually, each row of the `predicted` matrix
  *could* be seen as a separate (univariate) forecast. The grouping id
  then groups several of those forecasts together, treating them as a
  single multivariate forecast.

- w:

  Optional numeric vector of weights for forecast samples (length equal
  to the number of columns of `predicted`). If `NULL` (the default),
  equal weights are used.

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
