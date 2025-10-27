# Check that all forecasts have the same number of rows

Helper function that checks the number of rows (corresponding e.g to
quantiles or samples) per forecast. If the number of quantiles or
samples is the same for all forecasts, it returns TRUE and a string with
an error message otherwise.

## Usage

``` r
check_number_per_forecast(data, forecast_unit)
```

## Arguments

- data:

  A data.frame or similar to be checked

- forecast_unit:

  Character vector denoting the unit of a single forecast.

## Value

Returns TRUE if the check was successful and a string with an error
message otherwise.
