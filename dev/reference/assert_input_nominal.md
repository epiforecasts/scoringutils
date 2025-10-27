# Assert that inputs are correct for nominal forecasts

Function assesses whether the inputs correspond to the requirements for
scoring nominal forecasts.

## Usage

``` r
assert_input_nominal(observed, predicted, predicted_label)
```

## Arguments

- observed:

  Input to be checked. Should be an unordered factor of length n with N
  levels holding the observed values. n is the number of observations
  and N is the number of possible outcomes the observed values can
  assume.

- predicted:

  Input to be checked. Should be nxN matrix of predicted probabilities,
  n (number of rows) being the number of data points and N (number of
  columns) the number of possible outcomes the observed values can
  assume. If `observed` is just a single number, then predicted can just
  be a vector of size N. Values represent the probability that the
  corresponding value in `observed` will be equal to the factor level
  referenced in `predicted_label`.

- predicted_label:

  Unordered factor of length N with N levels, where N is the number of
  possible outcomes the observed values can assume.

## Value

Returns NULL invisibly if the assertion was successful and throws an
error otherwise.
