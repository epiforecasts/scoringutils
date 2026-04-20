# Ensure multivariate grouping is set

Shared helper for multivariate forecast constructors. Applies
[`set_grouping()`](https://epiforecasts.io/scoringutils/dev/reference/set_grouping.md)
when `joint_across` is provided, or checks that `.mv_group_id` already
exists.

## Usage

``` r
ensure_mv_grouping(data, joint_across)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  "Target format" section in Details for additional information on the
  required input format.

- joint_across:

  Character vector with columns names that define the variables which
  are forecasted jointly. Conceptually, several univariate forecasts are
  pooled together to form a single multivariate forecasts. For example,
  if you have a column `country` and want to define a multivariate
  forecast for several countries at once, you could set
  `joint_across = "country"`.

## Value

A data.table with a `.mv_group_id` column.
