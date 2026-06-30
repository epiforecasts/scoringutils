# Build grid of missing model-target combinations

Internal function that detects missing model x target combinations by
comparing observed data against the complete grid of all compare-column
values crossed with all observed target combinations.

## Usage

``` r
build_missing_grid(scores, compare = "model")
```

## Arguments

- scores:

  A `scores` object (data.table with a `metrics` attribute as produced
  by
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)).

- compare:

  Character string (default `"model"`) naming the column whose values
  are compared against each target.

## Value

A `data.table` with forecast-unit columns for each missing combination.
Zero rows if nothing is missing.
