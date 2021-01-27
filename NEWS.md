## scoringutils 0.1.7
### Package updates
- The WIS definition change introduced in version 0.1.5 was partly corrected
such that the difference in weighting is only introduced when summarising 
over scores from different interval ranges

## scoringutils 0.1.6
## Feature updates
- `eval_forecasts()` can now handle a separate forecast and truth data set as 
as input
- `eval_forecasts()` now supports scoring point forecasts along side quantiles
in a quantile-based format. Currently the only metric used is the absoluter error

### Package updates
- Many functions, especially `eval_forecasts()` got a major rewrite. While 
functionality should be unchanged, the code should now be easier to maintain
- Some of the data-handling functions got renamed, but old names are supported
as well for now. 


## scoringutils 0.1.5
### Package updates
- changed the default definition of the weighted interval score. Previously, 
the median prediction was counted twice, but is no only counted once. If you 
want to go back to the old behaviour, you can call the interval_score fucntion
with the agument `count_median_twice = FALSE`. 

## scoringutils 0.1.4

### Feature updates
- we added basic plotting functionality to visualise scores. You can now
easily obtain diagnostic plots based on scores as produced by `eval_forecasts`.
- `correlation_plot` shows correlation between metrics
- `range_plot` shows contribution of different prediction intervals to some 
chosen metric
- `score_heatmap` visualises scores as heatmap
- `score_table` shows a coloured summary table of scores

### package updates
- renamed "calibration" to "coverage"
- renamed "true_values" to "true_value" in data.frames
- renamed "predictions" to "prediction" in data.frames
- renamed "is_overprediction" to "overprediction"
- renamed "is_underprediction" to "underprediction"

## scoringutils 0.1.3

### (Potentially) Breaking changes
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

### Feature updates
- updated quantile evaluation metrics in `eval_forecasts`. Bias as well as 
calibration now take all quantiles into account
- Included option to summarise scores according to a `summarise_by` argument in 
`eval_forecasts` The summary can return the mean, the standard deviation as well
as an arbitrary set of quantiles. 
- `eval_forecasts` can now return pit histograms. 
- switched to ggplot2 for plotting

## scoringutils 0.1.2

### (Potentially) Breakting changes
- all scores in eval_forecasts were consistently renamed to lower case. 
Interval_score is now interval_score, CRPS is now crps etc. 

### Feature updates
- included support for grouping scores according to a vector of column names
in `eval_forecasts`
- included support for passing down arguments to lower-level functions in 
`eval_forecasts`
- included support for three new metrics to score quantiles with 
`eval_forecasts`: bias, sharpness and calibration

### Package updates
- example data now has a horizon column to illustrate the use of grouping
- documentation updated to explain the above listed changes

## scoringutils 0.1.1

### Feature updates
- included support for a long as well as wide input formats for 
quantile forecasts that are scored with `eval_forecasts`

### Package updates
- updated documentation for the `eval_forecasts`
- added badges to the Readme

