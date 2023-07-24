# scoringutils 1.1.7

## Package updates

- Made imputation of the median in `bias_range()` and `bias_quantile()` more obvious to the user as this may cause unexpected behaviour.

## Bug fixes

- Fixed a bug in `get_prediction_type()` which led to it being unable to correctly detect integer (instead categorising them as continuous) forecasts when the input was a matrix. This issue impacted `bias_sample()`.


# scoringutils 1.1.6

## Feature updates
- Added a new argument, `across`, to `summarise_scores()`. This argument allows the user to summarise scores across different forecast units as an alternative to specifying `by`. See the documentation for `summarise_scores()` for more details and an example use case.

# scoringutils 1.1.5

## Feature updates
- Added a new function, `set_forecast_unit()` that allows the user to set the forecast unit manually. The function removes all columns that are not relevant for uniquely identifying a single forecast. If not done manually, `scoringutils` attempts to determine the unit of a single automatically by simply assuming that all column names are
relevant to determine the forecast unit. This can lead to unexpected behaviour, so setting the forecast unit explicitly can help make the code easier to debug and easier to read (see issue #268). 
When used as part of a workflow, `set_forecast_unit()` can be directly piped into `check_forecasts()` to
check everything is in order.

# scoringutils 1.1.4

## Package updates
- added links to the scoringutils paper [Evaluating Forecasts with scoringutils in R](https://arxiv.org/abs/2205.07090) to the package. 
- updated citation formatting to comply with newer standards. 

# scoringutils 1.1.3

## Package updates

- Added a warning to `interval_score()` if the interval range is between 0 and 1. Thanks to @adrian-lison (see #277) for the suggestion.

## Package updates

- Switched to a linting GitHub Action that only triggers on changes. Inspired by @bisaloo recent contribution to the [`epinowcast` package](https://github.com/epinowcast/epinowcast/pull/220).
- Updated package linters to be more extensive. Inspired by @bisaloo recent contribution to the [`epinowcast` package](https://github.com/epinowcast/epinowcast/pull/220).
- Resolved all flagged linting issues across the package.

# scoringutils 1.1.2

## Feature updates

- Added a new function, `transform_forecasts()` to make it easy to transform forecasts before scoring them, as suggested in Bosse et al. (2023), https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1.
- Added a function, `log_shift()` that implements the default transformation function. The function allows add an offset before applying the logarithm.

# scoringutils 1.1.1

- Added a small change to `interval_score()` which explicitly converts the logical vector to a numeric one. This should happen implicitly anyway, but is now done explicitly in order to avoid issues that may come up if the input vector has a type that doesn't allow the implicit conversion.  

# scoringutils 1.1.0

A minor update to the package with some bug fixes and minor changes.

## Feature updates

## Package updates

- Removed the on attach message which warned of breaking changes in `1.0.0`.
- Renamed the `metric` argument of `summarise_scores()` to `relative_skill_metric`. This argument is now deprecated and will be removed in a future version of the package. Please use the new argument instead.
- Updated the documentation for `score()` and related functions to make the soft requirement for a `model` column in the input data more explicit.
- Updated the documentation for `score()`, `pairwise_comparison()` and `summarise_scores()` to make it clearer what the unit of a single forecast is that is required for computations
- Simplified the function `plot_pairwise_comparison()` which now only supports plotting mean score ratios or p-values and removed the hybrid option to print both at the same time.

## Bug fixes

- Missing baseline forecasts in `pairwise_comparison()` now trigger an explicit and informative error message.
- The requirements table in the getting started vignette is now correct.
- Added support for an optional `sample` column when using a quantile forecast format. Previously this resulted in an error.

# scoringutils 1.0.0

Major update to the package and most package functions with lots of breaking changes.

## Feature updates

- New and updated Readme and vignette.
- The proposed scoring workflow was reworked. Functions were changed so they
can easily be piped and have simplified arguments and outputs.

### New functions and function changes

- The function `eval_forecasts()` was replaced by a function `score()` with a much reduced set of function arguments.
- Functionality to summarise scores and to add relative skill scores was moved
to a function `summarise_scores()`
- New function `check_forecasts()` to analyse input data before scoring
- New function `correlation()` to compute correlations between different metrics
- New function `add_coverage()` to add coverage for specific central prediction intervals.
- New function `avail_forecasts()` allows to visualise the number of available forecasts.
- New function `find_duplicates()` to find duplicate forecasts which cause an error.
- All plotting functions were renamed to begin with `plot_`. Arguments were
simplified.
- The function `pit()` now works based on data.frames. The old `pit` function
was renamed to `pit_sample()`. PIT p-values were removed entirely.
- The function `plot_pit()` now works directly with input as produced by `pit()`
- Many data-handling functions were removed and input types for `score()` were
restricted to sample-based, quantile-based or binary forecasts.
- The function `brier_score()` now returns all brier scores, rather than taking
the mean before returning an output.
- `crps()`, `dss()` and `logs()` were renamed to `crps_sample()`, `dss_sample()`, and
`logs_sample()`

### Bug fixes

- Testing was expanded
- Minor bugs were fixed, for example a bug in the `sample_to_quantile()` function
(https://github.com/epiforecasts/scoringutils/pull/223)

### Package data updated

- Package data is now based on forecasts submitted to the European Forecast Hub
(https://covid19forecasthub.eu/).
- All example data files were renamed to begin with `example_`.
- A new data set, `summary_metrics` was included that contains a summary of the metrics implemented in `scoringutils`.

## Other breaking changes

- The 'sharpness' component of the weighted interval score was renamed to dispersion. This was done to make it more clear what the component represents and to maintain consistency with what is used in other places.

# scoringutils 0.1.8

## Feature updates

- Added a function `check_forecasts()` that runs some basic checks on the
input data and provides feedback.

# scoringutils 0.1.7.2

## Package updates

- Minor bug fixes (previously, 'interval_score' needed to be among the selected metrics).
- All data.tables are now returned as `table[]` rather than as `table`, such that they don't have to be called twice to display the contents.

# scoringutils 0.1.7

## Feature updates

- Added a function, `pairwise_comparison()` that runs pairwise comparisons between models on the output of `eval_forecasts()`
- Added functionality to compute relative skill within `eval_forecasts()`.
- Added a function to visualise pairwise comparisons.

## Package updates

- The WIS definition change introduced in version 0.1.5 was partly corrected such that the difference in weighting is only introduced when summarising over scores from different interval ranges.
- "sharpness" was renamed to 'mad' in the output of [score()] for sample-based
forecasts.

# scoringutils 0.1.

## Feature updates

- `eval_forecasts()` can now handle a separate forecast and truth data set as 
as input.
- `eval_forecasts()` now supports scoring point forecasts along side quantiles
in a quantile-based format. Currently the only metric used is the absolute error.

## Package updates

- Many functions, especially `eval_forecasts()` got a major rewrite. While 
functionality should be unchanged, the code should now be easier to maintain
- Some of the data-handling functions got renamed, but old names are supported
as well for now.

# scoringutils 0.1.5

## Package updates

- Changed the default definition of the weighted interval score. Previously, 
the median prediction was counted twice, but is no only counted once. If you 
want to go back to the old behaviour, you can call the interval_score function
with the argument `count_median_twice = FALSE`.

# scoringutils 0.1.4

## Feature updates

- Added basic plotting functionality to visualise scores. You can now
easily obtain diagnostic plots based on scores as produced by `score`.
- `correlation_plot()` shows correlation between metrics.
- `plot_ranges()` shows contribution of different prediction intervals to some chosen metric.
- `plot_heatmap()` visualises scores as heatmap.
- `plot_score_table()` shows a coloured summary table of scores.

## package updates

- Renamed "calibration" to "coverage".
- Renamed "true_values" to "true_value" in data.frames.
- Renamed "predictions" to "prediction" in data.frames.
- Renamed "is_overprediction" to "overprediction".
- Renamed "is_underprediction" to "underprediction".

# scoringutils 0.1.3

## (Potentially) Breaking changes

- The by argument in `score` now has a slightly changed meaning. It now denotes the lowest possible grouping unit, i.e. the unit of one observation and needs to be specified explicitly. The default is now `NULL`. The reason for
this change is that most metrics need scoring on the observation level and this the most consistent implementation of this principle. The pit function receives
its grouping now from `summarise_by`. In a similar spirit, `summarise_by` has to
be specified explicitly and e.g. doesn't assume anymore that you want 'range'
to be included.
- For the interval score, `weigh = TRUE` is now the default option.
- Renamed true_values to true_value and predictions to prediction.

## Feature updates

- Updated quantile evaluation metrics in `score`. Bias as well as calibration now take all quantiles into account.
- Included option to summarise scores according to a `summarise_by` argument in `score()` The summary can return the mean, the standard deviation as well
as an arbitrary set of quantiles.
- `score()` can now return pit histograms.
- Switched to `ggplot2` for plotting.

# scoringutils 0.1.2

## (Potentially) Breaking changes

- All scores in score were consistently renamed to lower case. `Interval_score` is now `interval_score`, `CRPS` is now `crps` etc.

## Feature updates

- Included support for grouping scores according to a vector of column names
in `score()`.
- Included support for passing down arguments to lower-level functions in `score()`
- Included support for three new metrics to score quantiles with `score()`: bias, sharpness and calibration

## Package updates

- Example data now has a horizon column to illustrate the use of grouping.
- Documentation updated to explain the above listed changes.

# scoringutils 0.1.1

## Feature updates

- Included support for a long as well as wide input formats for quantile forecasts that are scored with `score()`.

## Package updates

- Updated documentation for the `score()`.
- Added badges to the `README`.
