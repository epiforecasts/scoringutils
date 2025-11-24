# Get default metrics for point forecasts

For point forecasts, the default scoring rules are:

- "ae_point" = [ae()](https://rdrr.io/pkg/Metrics/man/ae.html)

- "se_point" = [se()](https://rdrr.io/pkg/Metrics/man/se.html)

- "ape" = [ape()](https://rdrr.io/pkg/Metrics/man/ape.html)

A note of caution: Every scoring rule for a point forecast is implicitly
minimised by a specific aspect of the predictive distribution (see
Gneiting, 2011).

The mean squared error, for example, is only a meaningful scoring rule
if the forecaster actually reported the mean of their predictive
distribution as a point forecast. If the forecaster reported the median,
then the mean absolute error would be the appropriate scoring rule. If
the scoring rule and the predictive task do not align, the results will
be misleading.

Failure to respect this correspondence can lead to grossly misleading
results! Consider the example in the section below.

## Usage

``` r
# S3 method for class 'forecast_point'
get_metrics(x, select = NULL, exclude = NULL, ...)
```

## Arguments

- x:

  A forecast object (a validated data.table with predicted and observed
  values, see
  [`as_forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_binary.md)).

- select:

  A character vector of scoring rules to select from the list. If
  `select` is `NULL` (the default), all possible scoring rules are
  returned.

- exclude:

  A character vector of scoring rules to exclude from the list. If
  `select` is not `NULL`, this argument is ignored.

- ...:

  unused

## Input format

![](figures/metrics-binary-point.png)

Overview of required input format for binary and point forecasts

## References

Making and Evaluating Point Forecasts, Gneiting, Tilmann, 2011, Journal
of the American Statistical Association.

## See also

Other get_metrics functions:
[`get_metrics()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.md),
[`get_metrics.forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_binary.md),
[`get_metrics.forecast_nominal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_nominal.md),
[`get_metrics.forecast_ordinal()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_ordinal.md),
[`get_metrics.forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_quantile.md),
[`get_metrics.forecast_sample()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample.md),
[`get_metrics.forecast_sample_multivariate()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.forecast_sample_multivariate.md),
[`get_metrics.scores()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.scores.md)

## Examples

``` r
get_metrics(example_point, select = "ape")
#> $ape
#> function (actual, predicted) 
#> {
#>     return(ae(actual, predicted)/abs(actual))
#> }
#> <bytecode: 0x564946269dc0>
#> <environment: namespace:Metrics>
#> 

library(magrittr)
set.seed(123)
n <- 500
observed <- rnorm(n, 5, 4)^2

predicted_mu <- mean(observed)
predicted_not_mu <- predicted_mu - rnorm(n, 10, 2)

df <- data.frame(
  model = rep(c("perfect", "bad"), each = n),
  predicted = c(rep(predicted_mu, n), predicted_not_mu),
  observed = rep(observed, 2),
  id = rep(1:n, 2)
) %>%
  as_forecast_point()
score(df) %>%
  summarise_scores()
#>      model ae_point se_point      ape
#>     <char>    <num>    <num>    <num>
#> 1: perfect 34.64686 2145.813 3543.184
#> 2:     bad 32.34199 2238.566 2692.868
```
