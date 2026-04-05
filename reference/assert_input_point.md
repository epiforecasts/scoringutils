# Assert that inputs are correct for point forecast

Function assesses whether the inputs correspond to the requirements for
scoring point forecasts.

## Usage

``` r
assert_input_point(observed, predicted)
```

## Arguments

- observed:

  Input to be checked. Should be a numeric vector with the observed
  values of size n.

- predicted:

  Input to be checked. Should be a numeric vector with the predicted
  values of size n.

## Value

Returns NULL invisibly if the assertion was successful and throws an
error otherwise.
