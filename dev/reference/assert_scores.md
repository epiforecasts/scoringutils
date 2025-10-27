# Validate an object of class `scores`

This function validates an object of class `scores`, checking that it
has the correct class and that it has a `metrics` attribute.

## Usage

``` r
assert_scores(scores)
```

## Arguments

- scores:

  A data.table or similar with scores as produced by
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md).

## Value

Returns `NULL` invisibly
