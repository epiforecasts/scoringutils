# Get grouping for a multivariate forecast

Helper function to get the grouping for a multivariate forecast.

## Usage

``` r
get_grouping(forecast)
```

## Arguments

- forecast:

  A forecast object (a validated data.table with predicted and observed
  values).

## Value

A character vector with the names of the columns that define the
grouping.
