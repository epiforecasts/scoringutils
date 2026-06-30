# Assert that a strategy has the expected signature

Internal helper used by
[`filter_scores()`](https://epiforecasts.io/scoringutils/dev/reference/filter_scores.md)
and
[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md)
to check that a user-supplied strategy function has at least the
required named formals. This catches common mistakes early (e.g.
forgetting the `compare` argument) without constraining the strategy
author to a specific internal type.

## Usage

``` r
assert_strategy(strategy, required)
```

## Arguments

- strategy:

  A function.

- required:

  Character vector of formal names that `strategy` must accept.

## Value

`invisible(NULL)`. Called for its side effect of erroring when the check
fails.
