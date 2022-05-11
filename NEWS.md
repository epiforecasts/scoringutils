# scoringutils 1.0.0

Major update to the package and most package functions with lots of breaking changes. 

## Feature updates
- new and updated Readme and vignette
- the proposed scoring workflow was reworked. Functions were changed so they 
can easily be piped and have simplified arguments and outputs. 

### new functions and function changes
- the function `eval_forecasts()` was replaced by a function `score()` with a 
much reduced set of function arguments. 
- Functionality to summarise scores and to add relative skill scores was moved
to a function `summarise_scores()`
- new function `check_forecasts()` to analyse input data before scoring
- new function `correlation()` to compute correlations between different metrics
- new function `add_coverage()` to add coverage for specific central prediction
intervals
- new function `avail_forecasts()` allows to visualise the number of available
forecasts
- new function `find_duplicates()` to find duplicate forecasts which cause an 
error
- all plotting functions were renamed to begin with `plot_`. Arguments were 
simplified
- the function `pit()` now works based on data.frames. The old `pit` function 
was renamed to `pit_sample()`. PIT p-values were removed entirely. 
- the function `plot_pit()` now works directly with input as produced by `pit()`
- many data-handling functions were removed and input types for `score()` were
restricted to sample-based, quantile-based or binary forecasts. 
- the function `brier_score()` now returns all brier scores, rather than taking
the mean before returning an output. 
- `crps`, `dss` and `logs` were renamed to `crps_sample()`, `dss_sample()`, and
`logs_sample()`

### Bug fixes
- Testing was expanded
- minor bugs were fixed, for example a bug in the sample_to_quantile function
(https://github.com/epiforecasts/scoringutils/pull/223)

### package data updated
- package data is now based on forecasts submitted to the European Forecast Hub
(https://covid19forecasthub.eu/). 
- all example data files were renamed to begin with `example_`
- a new data set, `summary_metrics` was included that contains a summary of the 
metrics implemented in `scoringutils`

## Other breaking changes
- The 'sharpness' component of the weighted interval score was renamed to 
dispersion. This was done to make it more clear what the component represents 
and to maintain consistency with what is used in other places. 

# scoringutils 0.1.8

## Feature updates
- now added a function `check_forecasts()` that runs some basic checks on the
input data and provides feedback

# scoringutils 0.1.7.2

## Package updates
- minor bug fixes (previously, 'interval_score' needed to be among the 
selected metrics)
- all data.tables are now returned as `table[]` rather than as `table`, 
such that they don't have to be called twice to display the contents. 

# scoringutils 0.1.7

## Feature updates
- added a function, `pairwise_comparison()` that runs pairwise comparisons 
between models on the output of `eval_forecasts()`
- added functionality to compute relative skill within `eval_forecasts()`
- added a function to visualise pairwise comparisons

## Package updates
- The WIS definition change introduced in version 0.1.5 was partly corrected
such that the difference in weighting is only introduced when summarising 
over scores from different interval ranges
- "sharpness" was renamed to 'mad' in the output of [score()] for sample-based
forecasts. 

# scoringutils 0.1.

## Feature updates
- `eval_forecasts()` can now handle a separate forecast and truth data set as 
as input
- `eval_forecasts()` now supports scoring point forecasts along side quantiles
in a quantile-based format. Currently the only metric used is the absolute error

## Package updates
- Many functions, especially `eval_forecasts()` got a major rewrite. While 
functionality should be unchanged, the code should now be easier to maintain
- Some of the data-handling functions got renamed, but old names are supported
as well for now. 


# scoringutils 0.1.5

## Package updates
- changed the default definition of the weighted interval score. Previously, 
the median prediction was counted twice, but is no only counted once. If you 
want to go back to the old behaviour, you can call the interval_score function
with the argument `count_median_twice = FALSE`. 

# scoringutils 0.1.4

## Feature updates
- we added basic plotting functionality to visualise scores. You can now
easily obtain diagnostic plots based on scores as produced by `score`.
- `correlation_plot` shows correlation between metrics
- `plot_ranges` shows contribution of different prediction intervals to some 
chosen metric
- `plot_heatmap` visualises scores as heatmap
- `plot_score_table` shows a coloured summary table of scores

## package updates
- renamed "calibration" to "coverage"
- renamed "true_values" to "true_value" in data.frames
- renamed "predictions" to "prediction" in data.frames
- renamed "is_overprediction" to "overprediction"
- renamed "is_underprediction" to "underprediction"

# scoringutils 0.1.3

## (Potentially) Breaking changes
- the by argument in `score` now has a slightly changed meaning. It 
now denotes the lowest possible grouping unit, i.e. the unit of one observation
and needs to be specified explicitly. The default is now `NULL`. The reason for
this change is that most metrics need scoring on the observation level and this 
the most consistent implementation of this principle. The pit function receives
its grouping now from `summarise_by`. In a similar spirit, `summarise_by` has to
be specified explicitly and e.g. doesn't assume anymore that you want 'range'
to be included. 
- for the interval score, `weigh = TRUE` is now the default option.
- (potentially planned) rename true_values to true_value and predictions to prediction. 

## Feature updates
- updated quantile evaluation metrics in `score`. Bias as well as 
calibration now take all quantiles into account
- Included option to summarise scores according to a `summarise_by` argument in 
`score` The summary can return the mean, the standard deviation as well
as an arbitrary set of quantiles. 
- `score` can now return pit histograms. 
- switched to ggplot2 for plotting

# scoringutils 0.1.2

## (Potentially) Breaking changes
- all scores in score were consistently renamed to lower case. 
Interval_score is now interval_score, CRPS is now crps etc. 

## Feature updates
- included support for grouping scores according to a vector of column names
in `score`
- included support for passing down arguments to lower-level functions in 
`score`
- included support for three new metrics to score quantiles with 
`score`: bias, sharpness and calibration

## Package updates
- example data now has a horizon column to illustrate the use of grouping
- documentation updated to explain the above listed changes

# scoringutils 0.1.1

## Feature updates
- included support for a long as well as wide input formats for 
quantile forecasts that are scored with `score`

## Package updates
- updated documentation for the `score`
- added badges to the Readme

