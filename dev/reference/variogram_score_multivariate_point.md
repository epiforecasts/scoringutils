# Variogram score for multivariate point forecasts

Compute the variogram score (see
[`scoringRules::vs_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_multiv.html))
for each group defined by `mv_group_id`, treating each point forecast as
a single-sample ensemble.

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

  Numeric vector of observed values.

- predicted:

  Numeric matrix with one column, where each row corresponds to a target
  within a multivariate group.

- mv_group_id:

  Numeric vector of length equal to `length(observed)` with group
  identifiers.

- w_vs:

  Numeric matrix of weights for the variogram score. See
  [`scoringRules::vs_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_multiv.html)
  for details.

- p:

  Numeric, order of the variogram score. Defaults to 0.5. See
  [`scoringRules::vs_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_multiv.html)
  for details.

## Value

A named numeric vector of scores, one per multivariate group. Lower
values are better.
