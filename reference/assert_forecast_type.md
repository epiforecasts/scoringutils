# Assert that forecast type is as expected

Assert that forecast type is as expected

## Usage

``` r
assert_forecast_type(data, actual = get_forecast_type(data), desired = NULL)
```

## Arguments

- data:

  A forecast object.

- actual:

  The actual forecast type of the data

- desired:

  The desired forecast type of the data

## Value

Returns NULL invisibly if the assertion was successful and throws an
error otherwise.
