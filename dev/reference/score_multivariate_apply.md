# Apply multivariate metrics to grouped forecast data

Shared helper used by score methods for multivariate forecast classes.
Identifies the grouping columns, builds a unique metadata table, calls
[`apply_metrics()`](https://epiforecasts.io/scoringutils/dev/reference/apply_metrics.md),
and reorders columns.

## Usage

``` r
score_multivariate_apply(dt, metrics, observed, predicted, mv_group_id)
```

## Arguments

- dt:

  A data.table containing at least `.mv_group_id` and the grouping
  columns.

- metrics:

  Named list of metric functions.

- observed:

  Numeric vector of observed values.

- predicted:

  Matrix of predicted values.

- mv_group_id:

  Integer vector of group identifiers.

## Value

A data.table of scores.
