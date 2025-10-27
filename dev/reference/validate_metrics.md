# Validate metrics

This function validates whether the list of metrics is a list of valid
functions.

The function is used in
[`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)
to make sure that all metrics are valid functions.

## Usage

``` r
validate_metrics(metrics)
```

## Arguments

- metrics:

  A named list with metrics. Every element should be a scoring function
  to be applied to the data.

## Value

A named list of metrics, with those filtered out that are not valid
functions
