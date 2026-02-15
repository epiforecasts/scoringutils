# Test whether an object is a forecast object

Test whether an object is a forecast object.

You can test for a specific `forecast_<type>` class using the
appropriate `is_forecast_<type>` function.

## Usage

``` r
is_forecast_binary(x)

is_forecast_multivariate_sample(x)

is_forecast_sample_multivariate(x)

is_forecast_nominal(x)

is_forecast_ordinal(x)

is_forecast_point(x)

is_forecast_quantile(x)

is_forecast_sample(x)

is_forecast(x)
```

## Arguments

- x:

  An R object.

## Value

*`is_forecast`*: `TRUE` if the object is of class `forecast`, `FALSE`
otherwise.

*`is_forecast_<type>*`*: `TRUE` if the object is of class `forecast_*`
in addition to class `forecast`, `FALSE` otherwise.

## Examples

``` r
forecast_binary <- as_forecast_binary(example_binary)
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
is_forecast(forecast_binary)
#> [1] TRUE
```
