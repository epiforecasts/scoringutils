# Assert that inputs are correct for interval-based forecast

Function assesses whether the inputs correspond to the requirements for
scoring interval-based forecasts.

## Usage

``` r
assert_input_interval(observed, lower, upper, interval_range)
```

## Arguments

- observed:

  Input to be checked. Should be a numeric vector with the observed
  values of size n.

- lower:

  Input to be checked. Should be a numeric vector of size n that holds
  the predicted value for the lower bounds of the prediction intervals.

- upper:

  Input to be checked. Should be a numeric vector of size n that holds
  the predicted value for the upper bounds of the prediction intervals.

- interval_range:

  Input to be checked. Should be a vector of size n that denotes the
  interval range in percent. E.g. a value of 50 denotes a (25%, 75%)
  prediction interval.

## Value

Returns NULL invisibly if the assertion was successful and throws an
error otherwise.
