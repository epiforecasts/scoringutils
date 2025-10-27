# Create an object of class `scores` from data

This convenience function wraps
[`new_scores()`](https://epiforecasts.io/scoringutils/dev/reference/new_scores.md)
and validates the `scores` object.

## Usage

``` r
as_scores(scores, metrics)
```

## Arguments

- scores:

  A data.table or similar with scores as produced by
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md).

- metrics:

  A character vector with the names of the scores (i.e. the names of the
  scoring rules used for scoring).

## Value

An object of class `scores`
