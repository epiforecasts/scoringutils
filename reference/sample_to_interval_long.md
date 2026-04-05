# Change data from a sample-based format to a long interval range format

Transform data from a format that is based on predictive samples to a
format based on interval ranges.

## Usage

``` r
sample_to_interval_long(
  data,
  interval_range = c(0, 50, 90),
  type = 7,
  keep_quantile_col = TRUE
)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

- type:

  Type argument passed down to the quantile function. For more
  information, see
  [`quantile()`](https://rdrr.io/r/stats/quantile.html).

- keep_quantile_col:

  keep quantile_level column, default is TRUE

## Value

A data.table in a long interval interval range format
