# scoringutils 0.2.0

## Breaking changes
- The 'sharpness' component of the weighted interval score was renamed to 
dispersion. This was done to make it more clear what the component represents 
and to maintain consistency with what is used in other places. 
- Several changes have been introduced to [eval_forecasts()] and other functions: 
  - the verbose argument has to be removed from almost all functions
  - the `by` argument has been removed from [eval_forecasts()]. Instead, all 
  functions now expect you to remove any additional columns beforehand to 
  avoid confusion. 
  - PIT plots have been removed from [eval_forecasts()]. Instead the function
  [pit_df()] can now be used on a `data.frame` and its output can then be 
  passed to [hist_PIT()]
  - the `interval_score_arguments` were now replaced by `...` in [eval_forecasts()]
  - the argument `summarised = TRUE` was dropped. If you want no summarising 
  at all, please let `summarise_by` be equal to all avaialable column names 
  plus 'sample' or 'quantile' and 'range'
- the function [pit()] now returns PIT values rather than p-values from an 
Anderson-Darling test for uniformity of the PIT values. The AD test is 
sometimes not reliable in practice and we do not recommend its use in most cases. 
The function also does not return plots anymore. Instead, plots can be generated
by running [hist_PIT()]. 
- Old legacy functions for transforming data between different formats were
removed
  
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
easily obtain diagnostic plots based on scores as produced by `eval_forecasts`.
- `correlation_plot` shows correlation between metrics
- `range_plot` shows contribution of different prediction intervals to some 
chosen metric
- `score_heatmap` visualises scores as heatmap
- `score_table` shows a coloured summary table of scores

## package updates
- renamed "calibration" to "coverage"
- renamed "true_values" to "true_value" in data.frames
- renamed "predictions" to "prediction" in data.frames
- renamed "is_overprediction" to "overprediction"
- renamed "is_underprediction" to "underprediction"

# scoringutils 0.1.3

## (Potentially) Breaking changes
- the by argument in `eval_forecasts` now has a slightly changed meaning. It 
now denotes the lowest possible grouping unit, i.e. the unit of one observation
and needs to be specified explicitly. The default is now `NULL`. The reason for
this change is that most metrics need scoring on the observation level and this 
the most consistent implementation of this principle. The pit function receives
its grouping now from `summarise_by`. In a similar spirit, `summarise_by` has to
be specificed explicitly and e.g. doesn't assume anymore that you want 'range'
to be included. 
- for the interval score, `weigh = TRUE` is now the default option.
- (potentially planned) rename true_values to true_value and predictions to prediction. 

## Feature updates
- updated quantile evaluation metrics in `eval_forecasts`. Bias as well as 
calibration now take all quantiles into account
- Included option to summarise scores according to a `summarise_by` argument in 
`eval_forecasts` The summary can return the mean, the standard deviation as well
as an arbitrary set of quantiles. 
- `eval_forecasts` can now return pit histograms. 
- switched to ggplot2 for plotting

# scoringutils 0.1.2

## (Potentially) Breaking changes
- all scores in eval_forecasts were consistently renamed to lower case. 
Interval_score is now interval_score, CRPS is now crps etc. 

## Feature updates
- included support for grouping scores according to a vector of column names
in `eval_forecasts`
- included support for passing down arguments to lower-level functions in 
`eval_forecasts`
- included support for three new metrics to score quantiles with 
`eval_forecasts`: bias, sharpness and calibration

## Package updates
- example data now has a horizon column to illustrate the use of grouping
- documentation updated to explain the above listed changes

# scoringutils 0.1.1

## Feature updates
- included support for a long as well as wide input formats for 
quantile forecasts that are scored with `eval_forecasts`

## Package updates
- updated documentation for the `eval_forecasts`
- added badges to the Readme

