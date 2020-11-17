scoringutils: Utilities for Scoring and Assessing Predictions
================

[![R-CMD-check](https://github.com/epiforecasts/scoringutils/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/scoringutils/actions)
[![codecov](https://codecov.io/gh/epiforecasts/scoringutils/branch/master/graphs/badge.svg)](https://codecov.io/gh/epiforecasts/scoringutils/)
[![CRAN\_Release\_Badge](https://www.r-pkg.org/badges/version-ago/scoringutils)](https://CRAN.R-project.org/package=scoringutils)
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.4-green.svg?style=flat)](https://github.com/epiforecasts/scoringutils)
[![metacran
downloads](http://cranlogs.r-pkg.org/badges/grand-total/scoringutils)](https://cran.r-project.org/package=scoringutils)
<!-- badges: end -->

The scoringutils package provides a collection of metrics and proper
scoring rules that make it simple to score forecasts against the true
observed values.

Installation
============

The stable version of `scoringutils` is on CRAN, but is outdated now. We
do not recommend using it. Please install the current development
version from github using

``` r
remotes::install_github("epiforecasts/scoringutils")
```

Introduction and Overview of Functionality
==========================================

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

Scoring Forecasts Automatically
===============================

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
“forecast\_date” and “forecast\_horizon”. But as long as there isn’t
some weird shift, this doesn’t matter for the purpose of grouping our
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

Example Evaluation
------------------

Here is an example of an evaluation using toy data:

``` r
library(scoringutils)
library(data.table)
```

``` r
data <- scoringutils::quantile_example_data_plain
print(data, 3, 3)
#>      true_value id  model prediction horizon quantile
#>   1:   2.659261  1 model1 -0.6448536       1     0.05
#>   2:   2.659261  1 model1  0.3255102       1     0.25
#>   3:   2.659261  1 model1  1.0000000       1     0.50
#>  ---                                                 
#> 598:  30.189608 30 model2 31.2242353       2     0.50
#> 599:  30.189608 30 model2 31.3873685       2     0.95
#> 600:  30.189608 30 model2 30.6399809       2     0.75
scoringutils::eval_forecasts(data, 
                             summarise_by = c("model", "quantile", "range"))
#>      model quantile range interval_score sharpness underprediction
#>  1: model1     0.50     0      0.8269027 0.0000000     0.304369095
#>  2: model1     0.25    50      0.7760589 0.3044214     0.179681560
#>  3: model1     0.75    50      0.7760589 0.3044214     0.179681560
#>  4: model1     0.05    90      0.2658170 0.1523911     0.024935181
#>  5: model1     0.95    90      0.2658170 0.1523911     0.024935181
#>  6: model2     0.50     0      0.9779030 0.0000000     0.350926228
#>  7: model2     0.25    50      0.6787509 0.3566315     0.072721303
#>  8: model2     0.75    50      0.6787509 0.3566315     0.072721303
#>  9: model2     0.05    90      0.2721723 0.1606143     0.008071852
#> 10: model2     0.95    90      0.2721723 0.1606143     0.008071852
#>     overprediction  coverage coverage_deviation      bias       aem
#>  1:     0.52253362 0.0000000         0.00000000 0.1566667 0.8269027
#>  2:     0.29195591 0.4000000        -0.10000000 0.1566667 0.8269027
#>  3:     0.29195591 0.4000000        -0.10000000 0.1566667 0.8269027
#>  4:     0.08849074 0.8166667        -0.08333333 0.1566667 0.8269027
#>  5:     0.08849074 0.8166667        -0.08333333 0.1566667 0.8269027
#>  6:     0.62697674 0.0000000         0.00000000 0.2233333 0.9779030
#>  7:     0.24939811 0.5333333         0.03333333 0.2233333 0.9779030
#>  8:     0.24939811 0.5333333         0.03333333 0.2233333 0.9779030
#>  9:     0.10348616 0.8500000        -0.05000000 0.2233333 0.9779030
#> 10:     0.10348616 0.8500000        -0.05000000 0.2233333 0.9779030
#>     quantile_coverage
#>  1:         0.5500000
#>  2:         0.4000000
#>  3:         0.7833333
#>  4:         0.1333333
#>  5:         0.9500000
#>  6:         0.6166667
#>  7:         0.3333333
#>  8:         0.8666667
#>  9:         0.1500000
#> 10:         0.9833333
```

Using an appropriate level of summary, we can easily use the output for
visualisation. The `scoringutils` package offers some built-in functions
to help get a sense of the data

``` r
scoringutils::plot_predictions(data, x = "id", range = c(0, 90), 
                               facet_formula = ~ model)
```

![](man/figures/unnamed-chunk-3-1.png)<!-- -->

(The data is just randomly generated values. We plan to add real example
data to make these illustrations more useful in the future)

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
                             summarise_by = c("model"))
scoringutils::wis_components(scores)
```

![](man/figures/unnamed-chunk-6-1.png)<!-- -->

We can also look at contributions to different metrics by range:

``` r
scores <- scoringutils::eval_forecasts(data, 
                             summarise_by = c("model", "range"))
scoringutils::range_plot(scores, y = "interval_score")
```

![](man/figures/unnamed-chunk-7-1.png)<!-- -->

We can also visualise metrics using a heatmap:

``` r
scores <- scoringutils::eval_forecasts(data, 
                             summarise_by = c("model", "horizon"))
scores[, horizon := as.factor(horizon)]
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
print(scoringutils::quantile_example_data_plain, 3, 3)
#>      true_value id  model prediction horizon quantile
#>   1:   2.659261  1 model1 -0.6448536       1     0.05
#>   2:   2.659261  1 model1  0.3255102       1     0.25
#>   3:   2.659261  1 model1  1.0000000       1     0.50
#>  ---                                                 
#> 598:  30.189608 30 model2 31.2242353       2     0.50
#> 599:  30.189608 30 model2 31.3873685       2     0.95
#> 600:  30.189608 30 model2 30.6399809       2     0.75
print(scoringutils::quantile_example_data_long, 3, 3)
#>      true_value id  model prediction boundary range horizon
#>   1:   2.659261  1 model1 -0.6448536    lower    90       1
#>   2:   2.659261  1 model1  0.3255102    lower    50       1
#>   3:   2.659261  1 model1  1.0000000    lower     0       1
#>  ---                                                       
#> 718:  30.189608 30 model2 31.3873685    upper    90       2
#> 719:  30.189608 30 model2 30.6399809    upper    50       2
#> 720:  30.189608 30 model2 31.2576984    upper     0       2
```

sample based format with either continuous or integer values

``` r
print(scoringutils::integer_example_data, 3, 3)
#> # A tibble: 6,000 x 6
#> # Groups:   id [30]
#>       id model  true_value sample prediction horizon
#>    <int> <chr>       <dbl>  <int>      <dbl>   <dbl>
#>  1     1 model1          6      1          5       1
#>  2     1 model1          6      2          4       1
#>  3     1 model1          6      3          3       1
#>  4     1 model1          6      4          3       1
#>  5     1 model1          6      5          4       1
#>  6     1 model1          6      6          4       1
#>  7     1 model1          6      7          5       1
#>  8     1 model1          6      8          4       1
#>  9     1 model1          6      9          4       1
#> 10     1 model1          6     10          6       1
#> # … with 5,990 more rows
print(scoringutils::continuous_example_data, 3, 3)
#>       id  model  true_value sample   prediction horizon
#>    1:  1 model1  0.03007379      1 -0.203426069       1
#>    2:  1 model1  0.03007379      2  0.007621269       1
#>    3:  1 model1  0.03007379      3 -2.086657003       1
#>   ---                                                  
#> 5998: 30 model2 -2.93749990     48 -0.079900522       2
#> 5999: 30 model2 -2.93749990     49 -1.178524017       2
#> 6000: 30 model2 -2.93749990     50  0.638750918       2
```

forecasts in a binary format:

``` r
print(scoringutils::binary_example_data, 3, 3)
#> # A tibble: 120 x 5
#> # Groups:   id, model [60]
#>       id model  horizon prediction true_value
#>    <int> <fct>    <dbl>      <dbl>      <dbl>
#>  1     1 model1       1    0.746            0
#>  2     1 model1       2    0.522            0
#>  3     1 model2       1    0.00958          0
#>  4     1 model2       2    0.00671          0
#>  5     2 model1       1    0.730            0
#>  6     2 model1       2    0.511            0
#>  7     2 model2       1    0.0274           0
#>  8     2 model2       2    0.0192           0
#>  9     3 model1       1    0.543            0
#> 10     3 model1       2    0.380            0
#> # … with 110 more rows
```

It also offers functionality to convert between these formats. For more
information have a look at the documentation of the following functions:

``` r
scoringutils::sample_to_quantile() # convert from sample based to quantile format
scoringutils::range_to_quantile() # convert from range format to plain quantile
scoringutils::quantile_to_range() # convert the other way round
scoringutils::quantile_to_long() # convert range based format from wide to long
scoringutils::quantile_to_wide() # convert the other way round
```

Scoring Forecasts Directly
==========================

A variety of metrics and scoring rules can also be accessed directly
through the `scoringutils` package.

The following gives an overview of (most of) the implemented metrics.

Bias
----

The function `bias` determines bias from predictive Monte-Carlo samples,
automatically recognising whether forecasts are continuous or integer
valued.

For continuous forecasts, Bias is measured as
*B*<sub>*t*</sub>(*P*<sub>*t*</sub>, *x*<sub>*t*</sub>) = 1 − 2 ⋅ (*P*<sub>*t*</sub>(*x*<sub>*t*</sub>))

where *P*<sub>*t*</sub> is the empirical cumulative distribution
function of the prediction for the true value *x*<sub>*t*</sub>.
Computationally, *P*<sub>*t*</sub>(*x*<sub>*t*</sub>) is just calculated
as the fraction of predictive samples for *x*<sub>*t*</sub> that are
smaller than *x*<sub>*t*</sub>.

For integer valued forecasts, Bias is measured as

*B*<sub>*t*</sub>(*P*<sub>*t*</sub>, *x*<sub>*t*</sub>) = 1 − (*P*<sub>*t*</sub>(*x*<sub>*t*</sub>) + *P*<sub>*t*</sub>(*x*<sub>*t*</sub> + 1))

to adjust for the integer nature of the forecasts. In both cases, Bias
can assume values between -1 and 1 and is 0 ideally.

``` r
## integer valued forecasts
true_values <- rpois(30, lambda = 1:30)
predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
bias(true_values, predictions)
#>  [1] -0.010 -0.800  0.405 -0.380  0.800  0.280  0.425 -0.050 -0.635 -0.675
#> [11]  0.210  0.355  0.825 -0.815  0.540 -0.170 -0.685  0.765 -0.760  0.640
#> [21]  0.345  0.725 -0.695 -0.055  0.840 -0.010 -0.865 -1.000  0.385  0.795

## continuous forecasts
true_values <- rnorm(30, mean = 1:30)
predictions <- replicate(200, rnorm(30, mean = 1:30))
bias(true_values, predictions)
#>  [1] -0.76  0.55  0.27 -0.36  0.79  0.16  0.89  0.93  0.62  0.37 -0.64 -0.32
#> [13]  0.79 -0.03 -0.96 -0.43 -0.55 -0.98 -0.47 -0.74  0.01  0.78 -0.95  0.55
#> [25]  0.62 -0.47  0.37 -0.57 -0.83  0.63
```

Sharpness
---------

Sharpness is the ability of the model to generate predictions within a
narrow range. It is a data-independent measure, and is purely a feature
of the forecasts themselves.

Shaprness of predictive samples corresponding to one single true value
is measured as the normalised median of the absolute deviation from the
median of the predictive samples. For details, see `?stats::mad`

``` r
predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
sharpness(predictions)
#>  [1] 1.4826 1.4826 1.4826 1.4826 2.9652 2.9652 2.9652 2.9652 2.9652 2.9652
#> [11] 2.9652 3.7065 2.9652 2.9652 3.7065 3.7065 3.7065 4.4478 4.4478 4.4478
#> [21] 4.4478 4.4478 5.1891 4.4478 4.4478 4.4478 5.1891 5.9304 5.9304 5.9304
```

Calibration
-----------

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

*u*<sub>*t*</sub> = *P*<sub>*t*</sub>(*k*<sub>*t*</sub>) + *v* ⋅ (*P*<sub>*t*</sub>(*k*<sub>*t*</sub>) − *P*<sub>*t*</sub>(*k*<sub>*t*</sub> − 1))

where *k*<sub>*t*</sub> is the observed count, *P*<sub>*t*</sub>(*x*) is
the predictive cumulative probability of observing incidence *k* at time
*t*, *P*<sub>*t*</sub>( − 1) = 0 by definition and *v* is standard
uniform and independent of *k*. If *P*<sub>*t*</sub> is the true
cumulative probability distribution, then *u*<sub>*t*</sub> is standard
uniform.

The function checks whether integer or continuous forecasts were
provided. It then applies the (randomised) probability integral and
tests the values *u*<sub>*t*</sub> for uniformity using the
Anderson-Darling test.

As a rule of thumb, there is no evidence to suggest a forecasting model
is miscalibrated if the p-value found was greater than a threshold of
*p* &gt;  = 0.1, some evidence that it was miscalibrated if
0.01 &lt; *p* &lt; 0.1, and good evidence that it was miscalibrated if
*p* &lt;  = 0.01. In this context it should be noted, though, that
uniformity of the PIT is a necessary but not sufficient condition of
calibration. It should als be noted that the test only works given
sufficient samples, otherwise the Null hypothesis will often be rejected
outright.

Continuous Ranked Probability Score (CRPS)
------------------------------------------

Wrapper around the `crps_sample` function from the `scoringRules`
package. For more information look at the manuals from the
`scoringRules` package. The function can be used for continuous as well
as integer valued forecasts. Smaller values are better.

``` r
true_values <- rpois(30, lambda = 1:30)
predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
crps(true_values, predictions)
#>  [1] 0.704075 0.311000 0.432350 2.026400 0.696425 1.286075 0.673100 1.230400
#>  [9] 1.143925 0.843700 1.150525 0.875475 1.782625 0.900250 1.183550 2.282225
#> [17] 1.519975 3.286200 1.902250 1.444550 2.609925 4.159200 1.717425 1.222200
#> [25] 4.761275 3.114750 5.268500 4.391125 2.247625 1.219675
```

Dawid-Sebastiani Score (DSS)
----------------------------

Wrapper around the `dss_sample` function from the `scoringRules`
package. For more information look at the manuals from the
`scoringRules` package. The function can be used for continuous as well
as integer valued forecasts. Smaller values are better.

``` r
true_values <- rpois(30, lambda = 1:30)
predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
dss(true_values, predictions)
#>  [1] 0.1043611 0.8888282 1.5083081 2.2574257 2.5124626 1.9721862 2.3620773
#>  [8] 2.9454208 3.2478488 2.4677004 2.5026360 2.3575467 2.4369080 2.8268165
#> [15] 2.7914715 3.2860262 3.7733067 5.4649830 3.9147155 3.7486418 3.1407649
#> [22] 4.6635521 3.3183931 3.5593709 3.6031710 3.3189119 3.3872755 4.2149458
#> [29] 3.7143660 5.5206435
```

Log Score
---------

Wrapper around the `log_sample` function from the `scoringRules`
package. For more information look at the manuals from the
`scoringRules` package. The function should not be used for integer
valued forecasts. While Log Scores are in principle possible for integer
valued foreasts they require a kernel density estimate which is not well
defined for discrete values. Smaller values are better.

``` r
true_values <- rnorm(30, mean = 1:30)
predictions <- replicate(200, rnorm(n = 30, mean = 1:30))
logs(true_values, predictions)
#>  [1] 0.8780070 1.2341316 1.1023236 1.1012004 0.9709626 1.0700818 1.0594070
#>  [8] 1.7880585 1.2927519 0.8751655 1.1405355 1.0779380 1.4176212 0.7565269
#> [15] 1.2051364 1.1889050 1.1754868 1.3193320 0.9459058 2.6253908 1.6844723
#> [22] 1.8298744 0.9674385 1.8864392 0.8140657 1.0949883 1.1532910 1.0966391
#> [29] 1.2514795 1.0004319
```

Brier Score
-----------

The Brier score is a proper score rule that assesses the accuracy of
probabilistic binary predictions. The outcomes can be either 0 or 1, the
predictions must be a probability that the true outcome will be 1.

The Brier Score is then computed as the mean squared error between the
probabilistic prediction and the true outcome.

$$\\text{Brier\_Score} = \\frac{1}{N} \\sum\_{t = 1}^{n} (\\text{prediction}\_t - \\text{outcome}\_t)^2$$

``` r
true_values <- sample(c(0,1), size = 30, replace = TRUE)
predictions <- runif(n = 30, min = 0, max = 1)

brier_score(true_values, predictions)
#> [1] 0.2751849
```

Interval Score
--------------

The Interval Score is a Proper Scoring Rule to score quantile
predictions, following Gneiting and Raftery (2007). Smaller values are
better.

The score is computed as

$$ \\text{score} = (\\text{upper} - \\text{lower}) + \\\\
\\frac{2}{\\alpha} \\cdot (\\text{lower} - \\text{true\_value}) \\cdot 1(\\text{true\_values} &lt; \\text{lower}) + \\\\
\\frac{2}{\\alpha} \\cdot (\\text{true\_value} - \\text{upper}) \\cdot
1(\\text{true\_value} &gt; \\text{upper})$$

where 1() is the indicator function and *α* is the decimal value that
indicates how much is outside the prediction interval. To improve
usability, the user is asked to provide an interval range in percentage
terms, i.e. interval\_range = 90 (percent) for a 90 percent prediction
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
#>  [1] 0.23074512 0.32158513 0.13772108 0.30220573 0.17881437 0.69729367
#>  [7] 0.14821784 1.52278644 0.21188499 0.04990424 0.08890472 0.20200402
#> [13] 0.25466474 0.20046919 0.11756582 0.89191235 0.14182934 0.23710429
#> [19] 0.21261377 1.82674018 0.18465405 0.19252317 0.29736976 0.19512091
#> [25] 0.09765623 0.22994483 0.27190154 0.14759534 0.14447785 0.54087104
```
