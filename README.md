scoringutils: Utilities for Scoring and Assessing Predictions
================

[![R-CMD-check](https://github.com/epiforecasts/scoringutils/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/scoringutils/actions)
[![codecov](https://codecov.io/gh/epiforecasts/scoringutils/branch/master/graphs/badge.svg)](https://codecov.io/gh/epiforecasts/scoringutils/)
[![CRAN_Release_Badge](https://www.r-pkg.org/badges/version-ago/scoringutils)](https://CRAN.R-project.org/package=scoringutils)
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.7-green.svg?style=flat)](https://github.com/epiforecasts/scoringutils)
[![metacran
downloads](http://cranlogs.r-pkg.org/badges/grand-total/scoringutils)](https://cran.r-project.org/package=scoringutils)
<!-- badges: end -->

# Introduction and Overview of Functionality

The `scoringutils` package provides a collection of metrics and proper
scoring rules that make it simple to score forecasts against the true
observed values. Predictions can either be automatically scored from a
`data.frame` using the function `eval_forecasts`. Alternatively,
evaluation metrics can be accessed directly using lower level functions
within a vector/matrix framework.

Predictions can be handled in various formats: `scoringutils` can handle
probabilistic forecasts in either a sample based or a quantile based
format. For more detail on the expected input formats please see below.
True values can be integer, continuous or binary.

In addition to automatic scoring, `scoringutils` offers a variety of
plots and visualisations.

# Scoring Forecasts Automatically

Most of the time, the `eval_forecasts` function will be able to do the
entire evaluation for you. The idea is simple, yet flexible.

All you need to do is to pass in a `data.frame` that has a column called
`prediction` and one called `true_value`. Depending on the exact input
format, additional columns like `sample`, `quantile` or `range` and
`boundary` are needed. Additional columns may be present to indicate a
grouping of forecasts. For example, we could have forecasts made by
different models in various locations at different time points, each for
several weeks into the future. In this case, we would have additional
columns called for example `model`, `date`, `forecast_date`,
`forecast_horizon` and `location`.

Using the `by` argument you need to specify the *unit of a single
forecast*. In this example here we would set
`by = c("model", "date", "forecast_date", "forecast_horizon", "location")`
(note: if we want to be pedantic, there is a small duplication as the
information of “date” is already included in the combination of
“forecast_date” and “forecast_horizon”. But as long as there isn’t some
weird shift, this doesn’t matter for the purpose of grouping our
observations). If you don’t specify `by` (i.e. `by = NULL`),
`scoringutils` will automatically use all appropriate present columns.
Note that you don’t need to include columns such as `quantile` or
`sample` in the `by` argument, as several quantiles / samples make up
one forecast.

Using the `summarise_by` argument you can now choose categories to
aggregate over. If you were only interested in scores for the different
models, you would specify `summarise_by = c("model")`. If you wanted to
have scores for every model in every location, you would need to specify
`summarise_by = c("model", "location")`. If you wanted to have one score
per quantile or one per prediction interval range, you could specify
something like `summarise_by = c("model", "quantile")` or
`summarise_by = c("model", "quantile", "range")` (note again that some
information is duplicated in quantile and range, but this doesn’t really
matter for grouping purposes). When aggregating, `eval_forecasts` takes
the mean according to the group defined in `summarise_by` (i.e. in this
example, if `summarise_by = c("model", "location")`, scores will be
averaged over all forecast dates, forecast horizons and quantiles to
yield one score per model and location). In addition to the mean, you
can also obtain the standard deviation of the scores over which you
average or any desired quantile (e.g. the median in addition to the
mean) by specifying `sd = TRUE` and `quantiles = c(0.5)`.

## Example Evaluation

Here is an example of an evaluation using the example data included in
the package. The data comes from a set of [Covid-19 short-term forecasts
in the UK](https://github.com/epiforecasts/covid19.forecasts.uk).

``` r
library(scoringutils)
#> Note: The definition of the weighted interval score has slightly changed in version 0.1.5. If you want to use the old definition, use the argument `count_median_twice = TRUE` in the function `eval_forecasts()`
library(data.table)
```

``` r
data <- scoringutils::quantile_example_data
print(data, 3, 3)
#>       value_date     value_type geography          value_desc true_value
#>    1: 2020-05-04   hospital_inc   England Hospital admissions       1043
#>    2: 2020-05-04  hospital_prev   England Total beds occupied      10648
#>    3: 2020-05-11   hospital_inc   England Hospital admissions        743
#>   ---                                                                   
#> 5150: 2020-08-03 death_inc_line     Wales              Deaths          1
#> 5151: 2020-08-03 death_inc_line     Wales              Deaths          1
#> 5152: 2020-08-03 death_inc_line     Wales              Deaths          1
#>                    model creation_date quantile prediction horizon
#>    1:               <NA>          <NA>       NA         NA      NA
#>    2:               <NA>          <NA>       NA         NA      NA
#>    3:               <NA>          <NA>       NA         NA      NA
#>   ---                                                             
#> 5150:           SIRCOVID    2020-07-13     0.85          4      21
#> 5151: DetSEIRwithNB MCMC    2020-07-13     0.90          2      21
#> 5152:           SIRCOVID    2020-07-13     0.90          6      21

scores <- scoringutils::eval_forecasts(data, 
                                       summarise_by = c("model", "quantile", "range"))
print(scores, 3, 3)
#>                  model quantile range interval_score sharpness underprediction
#>  1: DetSEIRwithNB MCMC     0.50     0       54.45528  0.000000     54.16260163
#>  2: DetSEIRwithNB MCMC     0.45    10       53.96138  6.310976     47.42276423
#>  3: DetSEIRwithNB MCMC     0.55    10       53.96138  6.310976     47.42276423
#> ---                                                                           
#> 55:           SIRCOVID     0.90    80       18.18000 17.368889      0.15555556
#> 56:           SIRCOVID     0.05    90       11.69444 11.661111      0.03333333
#> 57:           SIRCOVID     0.95    90       11.69444 11.661111      0.03333333
#>     overprediction  coverage coverage_deviation       bias      aem
#>  1:      0.2926829 0.3170732         0.31707317 -0.2333333 54.45528
#>  2:      0.2276423 0.4308943         0.33089431 -0.2333333 54.45528
#>  3:      0.2276423 0.4308943         0.33089431 -0.2333333 54.45528
#> ---                                                                
#> 55:      0.6555556 0.9333333         0.13333333  0.2255556 42.90000
#> 56:      0.0000000 0.9888889         0.08888889  0.2255556 42.90000
#> 57:      0.0000000 0.9888889         0.08888889  0.2255556 42.90000
#>     quantile_coverage
#>  1:         0.4959350
#>  2:         0.4308943
#>  3:         0.5691057
#> ---                  
#> 55:         0.9777778
#> 56:         0.2666667
#> 57:         0.9888889
```

<!-- Using an appropriate level of summary, we can easily use the output for visualisation. The `scoringutils` package offers some built-in functions to help get a sense of the data -->

``` r
scores <- scoringutils::eval_forecasts(data, 
                             summarise_by = c("model"))
scoringutils::score_table(scores)
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

Given this level of aggregation, not all metrics may make sense. In this
case, for example, averaging over different quantiles to compute
quantile coverage does not make much sense. If you like, you can select
specific metrics for the visualisation.

Let us look at calibration:

``` r
scores <- scoringutils::eval_forecasts(data, 
                             summarise_by = c("model", "range", "quantile"))
scoringutils::interval_coverage(scores) + 
  ggplot2::ggtitle("Interval Coverage")

scoringutils::quantile_coverage(scores) + 
  ggplot2::ggtitle("Quantile Coverage")
```

<img src="man/figures/unnamed-chunk-5-1.png" width="50%" /><img src="man/figures/unnamed-chunk-5-2.png" width="50%" />

Let us look at the individual components of the weighted interval score:

``` r
scores <- scoringutils::eval_forecasts(data, 
                             summarise_by = c("model", "value_desc"))
scoringutils::wis_components(scores, facet_formula = ~ value_desc)
```

![](man/figures/unnamed-chunk-6-1.png)<!-- -->

We can also look at contributions to different metrics by range:

``` r
scores <- scoringutils::eval_forecasts(data, 
                             summarise_by = c("model", "range", "value_desc"))
scoringutils::range_plot(scores, y = "interval_score", 
                         facet_formula = ~ value_desc)
```

![](man/figures/unnamed-chunk-7-1.png)<!-- -->

We can also visualise metrics using a heatmap:

``` r
scores <- scoringutils::eval_forecasts(data, 
                             summarise_by = c("model", "horizon"))
scores <- scores[, horizon := as.factor(horizon)]
scoringutils::score_heatmap(scores, 
                            x = "horizon", metric = "bias")
```

![](man/figures/unnamed-chunk-8-1.png)<!-- -->

### Expected Input Formats

The `eval_forecasts` function is designed to work with various different
input formats. The following formats are currently supported:

quantile forecasts in either a plain quantile format or in a format that
specifies interval ranges and the boundary of a given interval range.

``` r
print(scoringutils::quantile_example_data, 3, 3)
#>       value_date     value_type geography          value_desc true_value
#>    1: 2020-05-04   hospital_inc   England Hospital admissions       1043
#>    2: 2020-05-04  hospital_prev   England Total beds occupied      10648
#>    3: 2020-05-11   hospital_inc   England Hospital admissions        743
#>   ---                                                                   
#> 5150: 2020-08-03 death_inc_line     Wales              Deaths          1
#> 5151: 2020-08-03 death_inc_line     Wales              Deaths          1
#> 5152: 2020-08-03 death_inc_line     Wales              Deaths          1
#>                    model creation_date quantile prediction horizon
#>    1:               <NA>          <NA>       NA         NA      NA
#>    2:               <NA>          <NA>       NA         NA      NA
#>    3:               <NA>          <NA>       NA         NA      NA
#>   ---                                                             
#> 5150:           SIRCOVID    2020-07-13     0.85          4      21
#> 5151: DetSEIRwithNB MCMC    2020-07-13     0.90          2      21
#> 5152:           SIRCOVID    2020-07-13     0.90          6      21
print(scoringutils::range_example_data_long, 3, 3)
#>       value_date     value_type geography          value_desc true_value
#>    1: 2020-05-04   hospital_inc   England Hospital admissions       1043
#>    2: 2020-05-04  hospital_prev   England Total beds occupied      10648
#>    3: 2020-05-11   hospital_inc   England Hospital admissions        743
#>   ---                                                                   
#> 5417: 2020-07-27 death_inc_line     Wales              Deaths          1
#> 5418: 2020-08-03 death_inc_line     Wales              Deaths          1
#> 5419: 2020-08-03 death_inc_line     Wales              Deaths          1
#>                    model creation_date prediction horizon boundary range
#>    1:               <NA>          <NA>         NA      NA     <NA>    NA
#>    2:               <NA>          <NA>         NA      NA     <NA>    NA
#>    3:               <NA>          <NA>         NA      NA     <NA>    NA
#>   ---                                                                   
#> 5417:           SIRCOVID    2020-07-13          1      14    upper     0
#> 5418: DetSEIRwithNB MCMC    2020-07-13          0      21    upper     0
#> 5419:           SIRCOVID    2020-07-13          1      21    upper     0
print(scoringutils::range_example_data_wide, 3, 3)
#>      value_date     value_type        geography          value_desc true_value
#>   1: 2020-05-04 death_inc_line          England              Deaths        448
#>   2: 2020-05-04 death_inc_line Northern Ireland              Deaths          9
#>   3: 2020-05-04 death_inc_line         Scotland              Deaths         40
#>  ---                                                                          
#> 344: 2020-08-03  hospital_prev          England Total beds occupied        784
#> 345: 2020-08-03  hospital_prev         Scotland Total beds occupied        265
#> 346: 2020-08-03       icu_prev         Scotland   ICU beds occupied          3
#>                   model creation_date horizon lower_0 lower_10 lower_20
#>   1:               <NA>          <NA>      NA      NA       NA       NA
#>   2:               <NA>          <NA>      NA      NA       NA       NA
#>   3:               <NA>          <NA>      NA      NA       NA       NA
#>  ---                                                                   
#> 344:               <NA>          <NA>      NA      NA       NA       NA
#> 345:               <NA>          <NA>      NA      NA       NA       NA
#> 346: DetSEIRwithNB MCMC    2020-07-13      21       2        2        2
#>      lower_30 lower_40 lower_50 lower_60 lower_70 lower_80 lower_90 upper_0
#>   1:       NA       NA       NA       NA       NA       NA       NA      NA
#>   2:       NA       NA       NA       NA       NA       NA       NA      NA
#>   3:       NA       NA       NA       NA       NA       NA       NA      NA
#>  ---                                                                       
#> 344:       NA       NA       NA       NA       NA       NA       NA      NA
#> 345:       NA       NA       NA       NA       NA       NA       NA      NA
#> 346:        2        2        1        1        1        1        0       2
#>      upper_10 upper_20 upper_30 upper_40 upper_50 upper_60 upper_70 upper_80
#>   1:       NA       NA       NA       NA       NA       NA       NA       NA
#>   2:       NA       NA       NA       NA       NA       NA       NA       NA
#>   3:       NA       NA       NA       NA       NA       NA       NA       NA
#>  ---                                                                        
#> 344:       NA       NA       NA       NA       NA       NA       NA       NA
#> 345:       NA       NA       NA       NA       NA       NA       NA       NA
#> 346:        3        3        3        3        4        4        4        5
#>      upper_90
#>   1:       NA
#>   2:       NA
#>   3:       NA
#>  ---         
#> 344:       NA
#> 345:       NA
#> 346:        6
```

sample based format with either continuous or integer values

``` r
print(scoringutils::integer_example_data, 3, 3)
#>        value_date     value_type geography          value_desc    model
#>     1: 2020-05-04   hospital_inc   England Hospital admissions     <NA>
#>     2: 2020-05-04  hospital_prev   England Total beds occupied     <NA>
#>     3: 2020-05-11   hospital_inc   England Hospital admissions     <NA>
#>    ---                                                                 
#> 13427: 2020-08-03 death_inc_line     Wales              Deaths SIRCOVID
#> 13428: 2020-08-03 death_inc_line     Wales              Deaths SIRCOVID
#> 13429: 2020-08-03 death_inc_line     Wales              Deaths SIRCOVID
#>        creation_date horizon prediction sample true_value
#>     1:          <NA>      NA         NA     NA       1043
#>     2:          <NA>      NA         NA     NA      10648
#>     3:          <NA>      NA         NA     NA        743
#>    ---                                                   
#> 13427:    2020-07-13      21          0     48          1
#> 13428:    2020-07-13      21          0     49          1
#> 13429:    2020-07-13      21          0     50          1
print(scoringutils::continuous_example_data, 3, 3)
#>        value_date     value_type geography          value_desc    model
#>     1: 2020-05-04   hospital_inc   England Hospital admissions     <NA>
#>     2: 2020-05-04  hospital_prev   England Total beds occupied     <NA>
#>     3: 2020-05-11   hospital_inc   England Hospital admissions     <NA>
#>    ---                                                                 
#> 13427: 2020-08-03 death_inc_line     Wales              Deaths SIRCOVID
#> 13428: 2020-08-03 death_inc_line     Wales              Deaths SIRCOVID
#> 13429: 2020-08-03 death_inc_line     Wales              Deaths SIRCOVID
#>        creation_date horizon   prediction sample true_value
#>     1:          <NA>      NA           NA     NA       1043
#>     2:          <NA>      NA           NA     NA      10648
#>     3:          <NA>      NA           NA     NA        743
#>    ---                                                     
#> 13427:    2020-07-13      21 0.3340917507     48          1
#> 13428:    2020-07-13      21 0.3540187438     49          1
#> 13429:    2020-07-13      21 0.0001998965     50          1
```

forecasts in a binary format:

``` r
print(scoringutils::binary_example_data, 3, 3)
#>      value_date     value_type geography          value_desc              model
#>   1: 2020-05-04   hospital_inc   England Hospital admissions               <NA>
#>   2: 2020-05-04  hospital_prev   England Total beds occupied               <NA>
#>   3: 2020-05-11   hospital_inc   England Hospital admissions               <NA>
#>  ---                                                                           
#> 344: 2020-07-27 death_inc_line     Wales              Deaths           SIRCOVID
#> 345: 2020-08-03 death_inc_line     Wales              Deaths DetSEIRwithNB MCMC
#> 346: 2020-08-03 death_inc_line     Wales              Deaths           SIRCOVID
#>      creation_date horizon prediction true_value
#>   1:          <NA>      NA         NA         NA
#>   2:          <NA>      NA         NA         NA
#>   3:          <NA>      NA         NA         NA
#>  ---                                            
#> 344:    2020-07-13      14       0.34          0
#> 345:    2020-07-13      21       0.22          1
#> 346:    2020-07-13      21       0.26          0
```

It also offers functionality to convert between these formats. For more
information have a look at the documentation of the following functions:

``` r
scoringutils::sample_to_quantile() # convert from sample based to quantile format
scoringutils::range_long_to_quantile() # convert from range format to plain quantile
scoringutils::quantile_to_range_long() # convert the other way round
scoringutils::range_wide_to_long() # convert range based format from wide to long
scoringutils::range_long_to_wide() # convert the other way round
```

# Scoring Forecasts Directly

A variety of metrics and scoring rules can also be accessed directly
through the `scoringutils` package.

The following gives an overview of (most of) the implemented metrics.

## Bias

The function `bias` determines bias from predictive Monte-Carlo samples,
automatically recognising whether forecasts are continuous or integer
valued.

For continuous forecasts, Bias is measured as
*B*<sub>*t*</sub>(*P*<sub>*t*</sub>,*x*<sub>*t*</sub>) = 1 − 2 ⋅ (*P*<sub>*t*</sub>(*x*<sub>*t*</sub>))

where *P*<sub>*t*</sub> is the empirical cumulative distribution
function of the prediction for the true value *x*<sub>*t*</sub>.
Computationally, *P*<sub>*t*</sub>(*x*<sub>*t*</sub>) is just calculated
as the fraction of predictive samples for *x*<sub>*t*</sub> that are
smaller than *x*<sub>*t*</sub>.

For integer valued forecasts, Bias is measured as

*B*<sub>*t*</sub>(*P*<sub>*t*</sub>,*x*<sub>*t*</sub>) = 1 − (*P*<sub>*t*</sub>(*x*<sub>*t*</sub>)+*P*<sub>*t*</sub>(*x*<sub>*t*</sub>+1))

to adjust for the integer nature of the forecasts. In both cases, Bias
can assume values between -1 and 1 and is 0 ideally.

``` r
## integer valued forecasts
true_values <- rpois(30, lambda = 1:30)
predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
bias(true_values, predictions)
#>  [1] -0.700 -0.520 -0.460 -0.665  0.305  0.780  0.750 -0.600 -0.935  0.975
#> [11] -0.410 -0.380 -0.135  0.080  0.580 -0.310 -0.380 -0.195  0.375  0.700
#> [21]  0.545 -0.010  0.080 -0.440  0.885  0.310  0.530 -0.945 -0.045 -0.635

## continuous forecasts
true_values <- rnorm(30, mean = 1:30)
predictions <- replicate(200, rnorm(30, mean = 1:30))
bias(true_values, predictions)
#>  [1] -0.17  0.42  0.53 -0.70  0.71 -0.49 -0.71  0.28 -0.51  0.53  0.92  0.52
#> [13]  0.31 -0.92  0.03  0.75 -0.66  0.59  0.48 -0.83 -0.99 -0.66 -0.57  0.54
#> [25]  0.43 -0.33 -0.27 -0.39  0.58 -0.17
```

## Sharpness

Sharpness is the ability of the model to generate predictions within a
narrow range. It is a data-independent measure, and is purely a feature
of the forecasts themselves.

Shaprness of predictive samples corresponding to one single true value
is measured as the normalised median of the absolute deviation from the
median of the predictive samples. For details, see `?stats::mad`

``` r
predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
sharpness(predictions)
#>  [1] 1.4826 1.4826 1.4826 1.4826 1.4826 2.9652 2.9652 2.9652 2.9652 2.9652
#> [11] 4.4478 2.9652 2.9652 2.9652 4.4478 4.4478 3.7065 4.4478 4.4478 4.4478
#> [21] 5.9304 4.4478 4.4478 4.4478 4.4478 4.4478 4.4478 5.9304 4.4478 5.9304
```

## Calibration

Calibration or reliability of forecasts is the ability of a model to
correctly identify its own uncertainty in making predictions. In a model
with perfect calibration, the observed data at each time point look as
if they came from the predictive probability distribution at that time.

Equivalently, one can inspect the probability integral transform of the
predictive distribution at time t,

*u*<sub>*t*</sub> = *F*<sub>*t*</sub>(*x*<sub>*t*</sub>)

where *x*<sub>*t*</sub> is the observed data point at time
*t* in *t*<sub>1</sub>, …, *t*<sub>*n*</sub>, n being the number of
forecasts, and *F*<sub>*t*</sub> is the (continuous) predictive
cumulative probability distribution at time t. If the true probability
distribution of outcomes at time t is *G*<sub>*t*</sub> then the
forecasts *F*<sub>*t*</sub> are said to be ideal if
*F*<sub>*t*</sub> = *G*<sub>*t*</sub> at all times *t*. In that case,
the probabilities ut are distributed uniformly.

In the case of discrete outcomes such as incidence counts, the PIT is no
longer uniform even when forecasts are ideal. In that case a randomised
PIT can be used instead:

*u*<sub>*t*</sub> = *P*<sub>*t*</sub>(*k*<sub>*t*</sub>) + *v* ⋅ (*P*<sub>*t*</sub>(*k*<sub>*t*</sub>)−*P*<sub>*t*</sub>(*k*<sub>*t*</sub>−1))

where *k*<sub>*t*</sub> is the observed count, *P*<sub>*t*</sub>(*x*) is
the predictive cumulative probability of observing incidence *k* at time
*t*, *P*<sub>*t*</sub>(−1) = 0 by definition and *v* is standard uniform
and independent of *k*. If *P*<sub>*t*</sub> is the true cumulative
probability distribution, then *u*<sub>*t*</sub> is standard uniform.

The function checks whether integer or continuous forecasts were
provided. It then applies the (randomised) probability integral and
tests the values *u*<sub>*t*</sub> for uniformity using the
Anderson-Darling test.

As a rule of thumb, there is no evidence to suggest a forecasting model
is miscalibrated if the p-value found was greater than a threshold of
*p* \>  = 0.1, some evidence that it was miscalibrated if
0.01 \< *p* \< 0.1, and good evidence that it was miscalibrated if
*p* \<  = 0.01. In this context it should be noted, though, that
uniformity of the PIT is a necessary but not sufficient condition of
calibration. It should als be noted that the test only works given
sufficient samples, otherwise the Null hypothesis will often be rejected
outright.

## Continuous Ranked Probability Score (CRPS)

Wrapper around the `crps_sample` function from the `scoringRules`
package. For more information look at the manuals from the
`scoringRules` package. The function can be used for continuous as well
as integer valued forecasts. Smaller values are better.

``` r
true_values <- rpois(30, lambda = 1:30)
predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
crps(true_values, predictions)
#>  [1] 0.633225 0.522550 0.396400 2.935500 0.646975 1.092100 1.673725 1.208975
#>  [9] 2.349250 1.457050 1.492775 3.949725 1.190300 3.185700 5.178925 0.968050
#> [17] 0.979150 1.072325 2.183100 4.927400 1.109600 1.740250 1.685275 1.841800
#> [25] 1.937725 1.789300 2.097750 1.441450 7.593550 8.196575
```

## Dawid-Sebastiani Score (DSS)

Wrapper around the `dss_sample` function from the `scoringRules`
package. For more information look at the manuals from the
`scoringRules` package. The function can be used for continuous as well
as integer valued forecasts. Smaller values are better.

``` r
true_values <- rpois(30, lambda = 1:30)
predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
dss(true_values, predictions)
#>  [1] -0.1800783  1.0913706  1.0079771  3.4442881  1.3031143  1.8876494
#>  [7]  2.3585774  2.2110750  2.3802473  2.6640302  3.0230465  7.3333000
#> [13]  2.7466030  2.8338357  2.8143720  2.8633432  2.8711060  3.3624147
#> [19]  3.0109214  3.0643456  3.1705285  3.9281835  3.3284476 16.2662208
#> [25]  3.2660274  3.5280882  3.9380496  4.4371102  3.4380811  3.6981317
```

## Log Score

Wrapper around the `log_sample` function from the `scoringRules`
package. For more information look at the manuals from the
`scoringRules` package. The function should not be used for integer
valued forecasts. While Log Scores are in principle possible for integer
valued forecasts they require a kernel density estimate which is not
well defined for discrete values. Smaller values are better.

``` r
true_values <- rnorm(30, mean = 1:30)
predictions <- replicate(200, rnorm(n = 30, mean = 1:30))
logs(true_values, predictions)
#>  [1] 1.2611949 2.0085114 0.9560384 1.0689759 1.0129317 1.4904568 2.9205285
#>  [8] 0.8460895 1.4281170 0.9316350 0.9953321 1.0236646 0.9766526 1.8764237
#> [15] 1.6515511 1.0918181 2.1622704 1.5427550 0.8694894 1.5452538 0.9868115
#> [22] 0.9044315 1.3446104 1.9701726 1.1217898 1.2175854 1.4064990 1.0417268
#> [29] 1.1115333 0.8998055
```

## Brier Score

The Brier score is a proper score rule that assesses the accuracy of
probabilistic binary predictions. The outcomes can be either 0 or 1, the
predictions must be a probability that the true outcome will be 1.

The Brier Score is then computed as the mean squared error between the
probabilistic prediction and the true outcome.

``` r
true_values <- sample(c(0,1), size = 30, replace = TRUE)
predictions <- runif(n = 30, min = 0, max = 1)

brier_score(true_values, predictions)
#> [1] 0.3554911
```

## Interval Score

The Interval Score is a Proper Scoring Rule to score quantile
predictions, following Gneiting and Raftery (2007). Smaller values are
better.

The score is computed as

![interval_score](man/figures/interval_score.png)

where 1() is the indicator function and *α* is the decimal value that
indicates how much is outside the prediction interval. To improve
usability, the user is asked to provide an interval range in percentage
terms, i.e. interval_range = 90 (percent) for a 90 percent prediction
interval. Correspondingly, the user would have to provide the 5% and 95%
quantiles (the corresponding alpha would then be 0.1). No specific
distribution is assumed, but the range has to be symmetric (i.e you
can’t use the 0.1 quantile as the lower bound and the 0.7 quantile as
the upper). Setting `weigh = TRUE` will weigh the score by
$\\frac{\\alpha}{2}$ such that the Interval Score converges to the CRPS
for increasing number of quantiles.

``` r
true_values <- rnorm(30, mean = 1:30)
interval_range <- 90
alpha <- (100 - interval_range) / 100
lower <- qnorm(alpha/2, rnorm(30, mean = 1:30))
upper <- qnorm((1- alpha/2), rnorm(30, mean = 1:30))

interval_score(true_values = true_values,
               lower = lower,
               upper = upper,
               interval_range = interval_range)
#>  [1] 0.1435602 0.1363273 0.1214869 0.3309745 0.1909975 0.1015329 0.1686463
#>  [8] 0.1605127 0.2402594 0.1066244 0.7533624 0.5321044 0.1682648 0.2013887
#> [15] 0.3317460 0.2161267 0.3294493 0.1764297 0.9422938 0.2414125 2.0005948
#> [22] 0.1053030 0.1262531 0.2189997 0.1860936 0.1452726 0.1516566 0.2706347
#> [29] 0.1062714 0.2404143
```
