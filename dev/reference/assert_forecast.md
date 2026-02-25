# Assert that input is a forecast object and passes validations

Assert that an object is a forecast object (i.e. a `data.table` with a
class `forecast` and an additional class `forecast_<type>` corresponding
to the forecast type).

See the corresponding `assert_forecast_<type>` functions for more
details on the required input formats.

## Usage

``` r
# S3 method for class 'forecast_binary'
assert_forecast(forecast, forecast_type = NULL, verbose = TRUE, ...)

# S3 method for class 'forecast_multivariate_point'
assert_forecast(forecast, forecast_type = NULL, verbose = TRUE, ...)

# S3 method for class 'forecast_multivariate_sample'
assert_forecast(forecast, forecast_type = NULL, verbose = TRUE, ...)

# S3 method for class 'forecast_sample_multivariate'
assert_forecast(forecast, forecast_type = NULL, verbose = TRUE, ...)

# S3 method for class 'forecast_point'
assert_forecast(forecast, forecast_type = NULL, verbose = TRUE, ...)

# S3 method for class 'forecast_quantile'
assert_forecast(forecast, forecast_type = NULL, verbose = TRUE, ...)

# S3 method for class 'forecast_sample'
assert_forecast(forecast, forecast_type = NULL, verbose = TRUE, ...)

assert_forecast(forecast, forecast_type = NULL, verbose = TRUE, ...)

# Default S3 method
assert_forecast(forecast, forecast_type = NULL, verbose = TRUE, ...)
```

## Arguments

- forecast:

  A forecast object (a validated data.table with predicted and observed
  values).

- forecast_type:

  (optional) The forecast type you expect the forecasts to have. If the
  forecast type as determined by `scoringutils` based on the input does
  not match this, an error will be thrown. If `NULL` (the default), the
  forecast type will be inferred from the data.

- verbose:

  Logical. If `FALSE` (default is `TRUE`), no messages and warnings will
  be created.

- ...:

  Currently unused. You *cannot* pass additional arguments to scoring
  functions via `...`. See the *Customising metrics* section below for
  details on how to use
  [`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html)
  to pass arguments to individual metrics.

## Value

Returns `NULL` invisibly.

## Examples

``` r
forecast <- as_forecast_binary(example_binary)
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
assert_forecast(forecast)
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
```
