# Assert Inputs Have Matching Dimensions

Function assesses whether input dimensions match. In the following, n is
the number of observations / forecasts. Scalar values may be repeated to
match the length of the other input. Allowed options are therefore:

- `observed` is vector of length 1 or length n

- `predicted` is:

  - a vector of of length 1 or length n

  - a matrix with n rows and 1 column

## Usage

``` r
assert_dims_ok_point(observed, predicted)
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
