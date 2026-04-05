# Get interval range belonging to a quantile

Every quantile can be thought of either as the lower or the upper bound
of a symmetric central prediction interval. This helper function returns
the range of the central prediction interval to which the quantile
belongs.

Due to numeric instability that sometimes occurred in the past, ranges
are rounded to 10 decimal places. This is not a problem for the vast
majority of use cases, but it is something to be aware of.

## Usage

``` r
get_range_from_quantile(quantile_level)
```

## Arguments

- quantile_level:

  A numeric vector of quantile levels of size N.

## Value

a numeric vector of interval ranges of size N
