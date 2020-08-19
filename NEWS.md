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

