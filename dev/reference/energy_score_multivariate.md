# Energy score for multivariate forecasts

Compute the multivariate energy score (see
[scoringRules::es_sample](https://rdrr.io/pkg/scoringRules/man/scores_sample_multiv.html))
for each group defined by `mv_group_id`.

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

  numeric vector of weights for forecast draws (length equal to number
  of columns of `dat`)
