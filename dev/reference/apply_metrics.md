# Apply a list of functions to a data table of forecasts

This helper function applies scoring rules (stored as a list of
functions) to a data table of forecasts. `apply_metrics` is used within
[`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)
to apply all scoring rules to the data. Scoring rules are wrapped in
[`run_safely()`](https://epiforecasts.io/scoringutils/dev/reference/run_safely.md)
to catch errors and to make sure that only arguments are passed to the
scoring rule that are actually accepted by it.

## Usage

``` r
apply_metrics(forecast, metrics, ...)
```

## Arguments

- forecast:

  A forecast object (a validated data.table with predicted and observed
  values).

- metrics:

  A named list of scoring functions. Each element should be a function
  reference, not a function call. For example, use
  `list("crps" = crps_sample)` rather than
  `list("crps" = crps_sample())`. Names will be used as column names in
  the output. See
  [`get_metrics()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.md)
  for more information on the default metrics used. See the *Customising
  metrics* section below for information on how to pass custom arguments
  to scoring functions.

- ...:

  Additional arguments to be passed to the scoring rules. Note that this
  is currently not used, as all calls to `apply_scores` currently avoid
  passing arguments via `...` and instead expect that the metrics
  directly be modified using
  [`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html).

## Value

A data table with the forecasts and the calculated metrics.
