# Validation common to all forecast types

The function runs input checks that apply to all input data, regardless
of forecast type. The function

- asserts that the forecast is a data.table which has columns `observed`
  and `predicted`

- checks the forecast type and forecast unit

- checks there are no duplicate forecasts

- if appropriate, checks the number of samples / quantiles is the same
  for all forecasts.

## Usage

``` r
assert_forecast_generic(data, verbose = TRUE)
```

## Arguments

- data:

  A data.table with forecasts and observed values that should be
  validated.

- verbose:

  Logical. If `FALSE` (default is `TRUE`), no messages and warnings will
  be created.

## Value

returns the input
