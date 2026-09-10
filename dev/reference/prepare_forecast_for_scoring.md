# Prepare a forecast object for scoring

This function performs the input preparation steps shared by all
[`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)
methods: it validates and cleans the forecast object (removing rows with
missing values), determines the forecast unit, validates the metrics,
and converts the forecast to a plain `data.table`.

The metrics are validated within this function so that the lazy default
`metrics = get_metrics(forecast)` of the
[`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)
methods is forced before those methods rebind `forecast` to a plain
`data.table` (there is no `get_metrics.default()`, so forcing the
default after that rebind would fail). The default is thereby evaluated
on the original forecast object passed to
[`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md),
i.e. before rows with missing values are removed. This makes no
difference for the built-in
[`get_metrics()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.md)
methods, which do not inspect the data.

## Usage

``` r
prepare_forecast_for_scoring(forecast, metrics)
```

## Arguments

- forecast:

  A forecast object (a validated data.table with predicted and observed
  values).

- metrics:

  A named list of scoring functions. See
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)
  for details.

## Value

A list with three elements: `forecast` (the cleaned forecast as a plain
`data.table`), `metrics` (the validated list of metrics) and
`forecast_unit` (a character vector with the columns that define the
unit of a single forecast).
