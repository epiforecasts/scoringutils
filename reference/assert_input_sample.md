# Assert that inputs are correct for sample-based forecast

Function assesses whether the inputs correspond to the requirements for
scoring sample-based forecasts.

## Usage

``` r
assert_input_sample(observed, predicted)
```

## Arguments

- observed:

  Input to be checked. Should be a numeric vector with the observed
  values of size n.

- predicted:

  Input to be checked. Should be a numeric nxN matrix of predictive
  samples, n (number of rows) being the number of data points and N
  (number of columns) the number of samples per forecast. If `observed`
  is just a single number, then predicted values can just be a vector of
  size N.

## Value

Returns NULL invisibly if the assertion was successful and throws an
error otherwise.
