# Set grouping

Helper function to set the grouping of a forecast.

## Usage

``` r
set_grouping(data, joint_across)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

- joint_across:

  Character vector with columns names that define the variables which
  are forecasted jointly. Conceptually, several univariate forecasts are
  pooled together to form a single multivariate forecasts. For example,
  if you have a column `country` and want to define a multivariate
  forecast for several countries at once, you could set
  `joint_across = "country"`.

## Value

A data.table with an additional column `.mv_group_id` that contains the
group id for each row.
