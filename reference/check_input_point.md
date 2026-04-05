# Check that inputs are correct for point forecast

Function assesses whether the inputs correspond to the requirements for
scoring point forecasts.

## Usage

``` r
check_input_point(observed, predicted)
```

## Arguments

- observed:

  Input to be checked. Should be a numeric vector with the observed
  values of size n.

- predicted:

  Input to be checked. Should be a numeric vector with the predicted
  values of size n.

## Value

Returns TRUE if the check was successful and a string with an error
message otherwise.
