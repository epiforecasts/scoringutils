# Assert that inputs are correct for categorical forecasts

Function assesses whether the inputs correspond to the requirements for
scoring categorical, i.e. either nominal or ordinal forecasts.

## Usage

``` r
assert_input_categorical(observed, predicted, predicted_label, ordered = NA)
```

## Arguments

- observed:

  Input to be checked. Should be a factor of length n with N levels
  holding the observed values. n is the number of observations and N is
  the number of possible outcomes the observed values can assume.

- predicted:

  Input to be checked. Should be nxN matrix of predicted probabilities,
  n (number of rows) being the number of data points and N (number of
  columns) the number of possible outcomes the observed values can
  assume. If `observed` is just a single number, then predicted can just
  be a vector of size N. Values represent the probability that the
  corresponding value in `observed` will be equal to the factor level
  referenced in `predicted_label`.

- predicted_label:

  Factor of length N with N levels, where N is the number of possible
  outcomes the observed values can assume.

- ordered:

  Value indicating whether factors have to be ordered or not. Defaults
  to `NA`, which means that the check is not performed.

## Value

Returns NULL invisibly if the assertion was successful and throws an
error otherwise.
