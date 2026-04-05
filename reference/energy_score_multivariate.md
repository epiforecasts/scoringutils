# Energy score for multivariate forecasts

Compute the energy score (Gneiting et al., 2008) for each multivariate
group defined by `mv_group_id`. The energy score is a multivariate
generalisation of the CRPS that measures both calibration and sharpness
of the forecast distribution.

The score is computed using
[`scoringRules::es_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_multiv.html).

## Usage

``` r
energy_score_multivariate(observed, predicted, mv_group_id, w = NULL)
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

## Value

A named numeric vector of scores, one per multivariate group. Lower
values are better.

## References

Gneiting, T., Stanberry, L.I., Grimit, E.P., Held, L. and Johnson, N.A.
(2008). Assessing probabilistic forecasts of multivariate quantities,
with an application to ensemble predictions of surface winds. *TEST*,
17, 211-235.
