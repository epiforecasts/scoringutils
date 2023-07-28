scoringutils: Utilities for Scoring and Assessing Predictions
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/epiforecasts/scoringutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiforecasts/scoringutils/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/epiforecasts/scoringutils/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiforecasts/scoringutils)
[![CRAN_Release_Badge](https://www.r-pkg.org/badges/version-ago/scoringutils)](https://CRAN.R-project.org/package=scoringutils)
![GitHub R package
version](https://img.shields.io/github/r-package/v/epiforecasts/scoringutils)
[![metacran
downloads](http://cranlogs.r-pkg.org/badges/grand-total/scoringutils)](https://cran.r-project.org/package=scoringutils)
<!-- badges: end -->

The `scoringutils` package provides a collection of metrics and proper
scoring rules and aims to make it simple to score probabilistic
forecasts against the true observed values.

You can find additional information and examples in the papers
[Evaluating Forecasts with scoringutils in
R](https://arxiv.org/abs/2205.07090) [Scoring epidemiological forecasts
on transformed
scales](https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1)
as well as the Vignettes ([Getting
started](https://epiforecasts.io/scoringutils/articles/scoringutils.html),
[Details on the metrics
implemented](https://epiforecasts.io/scoringutils/articles/metric-details.html)
and [Scoring forecasts
directly](https://epiforecasts.io/scoringutils/articles/scoring-forecasts-directly.html)).

The `scoringutils` package offers convenient automated forecast
evaluation through the function `score()`. The function operates on
data.frames (it uses `data.table` internally for speed and efficiency)
and can easily be integrated in a workflow based on `dplyr` or
`data.table`. It also provides experienced users with a set of reliable
lower-level scoring metrics operating on vectors/matrices they can build
upon in other applications. In addition it implements a wide range of
flexible plots designed to cover many use cases.

Where available `scoringutils` depends on functionality from
`scoringRules` which provides a comprehensive collection of proper
scoring rules for predictive probability distributions represented as
sample or parametric distributions. For some forecast types, such as
quantile forecasts, `scoringutils` also implements additional metrics
for evaluating forecasts. On top of providing an interface to the proper
scoring rules implemented in `scoringRules` and natively, `scoringutils`
also offers utilities for summarising and visualising forecasts and
scores, and to obtain relative scores between models which may be useful
for non-overlapping forecasts and forecasts across scales.

Predictions can be handled in various formats: `scoringutils` can handle
probabilistic forecasts in either a sample based or a quantile based
format. For more detail on the expected input formats please see below.
True values can be integer, continuous or binary, and appropriate scores
for each of these value types are selected automatically.

## Installation

Install the CRAN version of this package using:

``` r
install.packages("scoringutils")
```

Install the stable development version of the package with:

``` r
install.packages("scoringutils", repos = "https://epiforecasts.r-universe.dev")
```

Install the unstable development from GitHub using the following,

``` r
remotes::install_github("epiforecasts/scoringutils", dependencies = TRUE)
```

## Quick start

In this quick start guide we explore some of the functionality of the
`scoringutils` package using quantile forecasts from the [ECDC
forecasting hub](https://covid19forecasthub.eu/) as an example. For more
detailed documentation please see the package vignettes, and individual
function documentation.

### Plotting forecasts

As a first step to evaluating the forecasts we visualise them. For the
purposes of this example here we make use of `plot_predictions()` to
filter the available forecasts for a single model, and forecast date.

``` r
example_quantile %>%
  make_NA(what = "truth", 
          target_end_date >= "2021-07-15", 
          target_end_date < "2021-05-22"
  ) %>%
  make_NA(what = "forecast",
          model != "EuroCOVIDhub-ensemble", 
          forecast_date != "2021-06-28"
  ) %>%
  plot_predictions(
    x = "target_end_date",
    by = c("target_type", "location")
  ) +
  facet_wrap(target_type ~ location, ncol = 4, scales = "free") 
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

### Scoring forecasts

Forecasts can be easily and quickly scored using the `score()` function.
`score()` automatically tries to determine the `forecast_unit`, i.e. the
set of columns that uniquely defines a single forecast, by taking all
column names of the data into account. However, it is recommended to set
the forecast unit manually using `set_forecast_unit()` as this may help
to avoid errors, especially when scoringutils is used in automated
pipelines. The function `set_forecast_unit()` will simply drop unneeded
columns. To verify everything is in order, the function
`check_forecasts()` should be used. The result of that check can then
passed directly into `score()`. `score()` returns unsummarised scores,
which in most cases is not what the user wants. Here we make use of
additional functions from `scoringutils` to add empirical
coverage-levels (`add_coverage()`), and scores relative to a baseline
model (here chosen to be the EuroCOVIDhub-ensemble model). See the
getting started vignette for more details. Finally we summarise these
scores by model and target type.

``` r
example_quantile %>%
  set_forecast_unit(c("location", "target_end_date", "target_type", "horizon", "model")) %>%
  check_forecasts() %>%
  score() %>%
  add_coverage(ranges = c(50, 90), by = c("model", "target_type")) %>%
  summarise_scores(
    by = c("model", "target_type"),
    relative_skill = TRUE,
    baseline = "EuroCOVIDhub-ensemble"
  ) %>%
  summarise_scores(
    fun = signif, 
    digits = 2
  ) %>%
  kable()
#> The following messages were produced when checking inputs:
#> 1.  144 values for `prediction` are NA in the data provided and the corresponding rows were removed. This may indicate a problem if unexpected.
```

| model                 | target_type | interval_score | dispersion | underprediction | overprediction | coverage_deviation |    bias | ae_median | coverage_50 | coverage_90 | relative_skill | scaled_rel_skill |
|:----------------------|:------------|---------------:|-----------:|----------------:|---------------:|-------------------:|--------:|----------:|------------:|------------:|---------------:|-----------------:|
| EuroCOVIDhub-baseline | Cases       |          28000 |       4100 |         10000.0 |        14000.0 |             -0.110 |  0.0980 |     38000 |        0.33 |        0.82 |           1.30 |              1.6 |
| EuroCOVIDhub-baseline | Deaths      |            160 |         91 |             2.1 |           66.0 |              0.120 |  0.3400 |       230 |        0.66 |        1.00 |           2.30 |              3.8 |
| EuroCOVIDhub-ensemble | Cases       |          18000 |       3700 |          4200.0 |        10000.0 |             -0.098 | -0.0560 |     24000 |        0.39 |        0.80 |           0.82 |              1.0 |
| EuroCOVIDhub-ensemble | Deaths      |             41 |         30 |             4.1 |            7.1 |              0.200 |  0.0730 |        53 |        0.88 |        1.00 |           0.60 |              1.0 |
| UMass-MechBayes       | Deaths      |             53 |         27 |            17.0 |            9.0 |             -0.023 | -0.0220 |        78 |        0.46 |        0.88 |           0.75 |              1.3 |
| epiforecasts-EpiNow2  | Cases       |          21000 |       5700 |          3300.0 |        12000.0 |             -0.067 | -0.0790 |     28000 |        0.47 |        0.79 |           0.95 |              1.2 |
| epiforecasts-EpiNow2  | Deaths      |             67 |         32 |            16.0 |           19.0 |             -0.043 | -0.0051 |       100 |        0.42 |        0.91 |           0.98 |              1.6 |

`scoringutils` contains additional functionality to transform forecasts,
to summarise scores at different levels, to visualise them, and to
explore the forecasts themselves. See the package vignettes and function
documentation for more information.

You may want to score forecasts based on transformations of the original
data in order to obtain a more complete evaluation (see [this
paper](https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1)
for more information). This can be done using the function
`transform_forecasts()`. In the following example, we truncate values at
0 and use the function `log_shift()` to add 1 to all values before
applying the natural logarithm.

``` r
example_quantile %>%
 .[, true_value := ifelse(true_value < 0, 0, true_value)] %>%
  transform_forecasts(append = TRUE, fun = log_shift, offset = 1) %>%
  score %>%
  summarise_scores(by = c("model", "target_type", "scale")) %>%
  head()
#> The following messages were produced when checking inputs:
#> 1.  288 values for `prediction` are NA in the data provided and the corresponding rows were removed. This may indicate a problem if unexpected.
#>                    model target_type   scale interval_score   dispersion
#> 1: EuroCOVIDhub-baseline       Cases     log   1.169972e+00    0.4373146
#> 2: EuroCOVIDhub-baseline       Cases natural   2.209046e+04 4102.5009443
#> 3: EuroCOVIDhub-ensemble       Cases     log   5.500974e-01    0.1011850
#> 4: EuroCOVIDhub-ensemble       Cases natural   1.155071e+04 3663.5245788
#> 5:  epiforecasts-EpiNow2       Cases     log   6.005778e-01    0.1066329
#> 6:  epiforecasts-EpiNow2       Cases natural   1.443844e+04 5664.3779484
#>    underprediction overprediction coverage_deviation        bias    ae_median
#> 1:    3.521964e-01      0.3804607        -0.10940217  0.09726562 1.185905e+00
#> 2:    1.028497e+04   7702.9836957        -0.10940217  0.09726562 3.208048e+04
#> 3:    1.356563e-01      0.3132561        -0.09785326 -0.05640625 7.410484e-01
#> 4:    4.237177e+03   3650.0047554        -0.09785326 -0.05640625 1.770795e+04
#> 5:    1.858699e-01      0.3080750        -0.06660326 -0.07890625 7.656591e-01
#> 6:    3.260356e+03   5513.7058424        -0.06660326 -0.07890625 2.153070e+04
```

## Citation

If using `scoringutils` in your work please consider citing it using the
output of `citation("scoringutils")`:

    #> To cite scoringutils in publications use the following. If you use the
    #> CRPS, DSS, or Log Score, please also cite scoringRules.
    #> 
    #>   Nikos I. Bosse, Hugo Gruson, Sebastian Funk, Anne Cori, Edwin van
    #>   Leeuwen, and Sam Abbott (2022). Evaluating Forecasts with
    #>   scoringutils in R, arXiv. DOI: 10.48550/ARXIV.2205.07090
    #> 
    #> To cite scoringRules in publications use:
    #> 
    #>   Alexander Jordan, Fabian Krueger, Sebastian Lerch (2019). Evaluating
    #>   Probabilistic Forecasts with scoringRules. Journal of Statistical
    #>   Software, 90(12), 1-37. DOI 10.18637/jss.v090.i12
    #> 
    #> To see these entries in BibTeX format, use 'print(<citation>,
    #> bibtex=TRUE)', 'toBibtex(.)', or set
    #> 'options(citation.bibtex.max=999)'.

## How to make a bug report or feature request

Please briefly describe your problem and what output you expect in an
[issue](https://github.com/epiforecasts/scoringutils/issues). If you
have a question, please don’t open an issue. Instead, ask on our [Q and
A
page](https://github.com/epiforecasts/scoringutils/discussions/categories/q-a).

## Contributing

We welcome contributions and new contributors! We particularly
appreciate help on priority problems in the
[issues](https://github.com/epiforecasts/scoringutils/issues). Please
check and add to the issues, and/or add a [pull
request](https://github.com/epiforecasts/scoringutils/pulls).

## Code of Conduct

Please note that the `scoringutils` project is released with a
[Contributor Code of
Conduct](https://epiforecasts.io/scoringutils/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
