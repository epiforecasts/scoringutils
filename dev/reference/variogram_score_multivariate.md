# Variogram score for multivariate forecasts

Compute the variogram score for multivariate forecasts. The variogram
score (Scheuerer and Hamill, 2015) evaluates the dependence structure of
multivariate forecasts by comparing predicted pairwise differences
against observed pairwise differences.

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

  numeric vector of weights for forecast draws (length equal to number
  of columns of `dat`)

- w_vs:

  Optional non-negative weight matrix. If not `NULL`, must be a square
  matrix with dimensions equal to the number of targets within each
  multivariate group.

- p:

  Numeric, order of the variogram score. Typical choices are 0.5
  (default, more robust) and 1.

## Value

A named numeric vector of scores, one per multivariate group. Lower
values are better.

## References

Scheuerer, M. and Hamill, T.M. (2015). Variogram-Based Proper Scoring
Rules for Probabilistic Forecasts of Multivariate Quantities. *Monthly
Weather Review*, 143, 1321-1334.
