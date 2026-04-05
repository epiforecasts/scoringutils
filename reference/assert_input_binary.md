# Assert that inputs are correct for binary forecast

Function assesses whether the inputs correspond to the requirements for
scoring binary forecasts.

## Usage

``` r
assert_input_binary(observed, predicted)
```

## Arguments

- observed:

  Input to be checked. Should be a factor of length n with exactly two
  levels, holding the observed values. The highest factor level is
  assumed to be the reference level. This means that `predicted`
  represents the probability that the observed value is equal to the
  highest factor level.

- predicted:

  Input to be checked. `predicted` should be a vector of length n,
  holding probabilities. Alternatively, `predicted` can be a matrix of
  size n x 1. Values represent the probability that the corresponding
  value in `observed` will be equal to the highest available factor
  level.

## Value

Returns NULL invisibly if the assertion was successful and throws an
error otherwise.
