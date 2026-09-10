# Check that observed values are constant within each forecast unit

Helper function that checks that all rows belonging to the same forecast
(as defined by the forecast unit) share a single observed value. Rows
where `observed` is `NA` are ignored. If the observed values are
constant within each forecast unit, the function returns `TRUE` and a
string with an error message otherwise.

## Usage

``` r
check_observed_constant(data, forecast_unit)
```

## Arguments

- data:

  A data.frame or similar to be checked

- forecast_unit:

  Character vector denoting the unit of a single forecast.

## Value

Returns TRUE if the check was successful and a string with an error
message otherwise.
