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

**Note**: This documentation refers to the development version of
`scoringutils`. You can also view the [documentation of the stable
version](https://epiforecasts.io/scoringutils).

The `scoringutils` package provides a collection of metrics and proper
scoring rules and aims to make it simple to score probabilistic
forecasts against observed values.

A good starting point for those wishing to use `scoringutils` are the
vignettes on ([Getting
started](https://epiforecasts.io/scoringutils/articles/scoringutils.html),
[Details on the metrics
implemented](https://epiforecasts.io/scoringutils/articles/metric-details.html)
and [Scoring forecasts
directly](https://epiforecasts.io/scoringutils/articles/scoring-forecasts-directly.html)).

For a detailed description of the package, its rationale and design,
usage examples and how it relates to other packages in the R ecosystem,
please see the corresponding paper:

> Nikos I. Bosse, Hugo Gruson, Anne Cori, Edwin van Leeuwen, Sebastian
> Funk and Sam Abbott (2022).
> *`Evaluating Forecasts with scoringutils in R`*. arXiv:2205.07090
> <https://doi.org/10.48550/arXiv.2205.07090>

For further details on the specific issue of transforming forecasts for
scoring see:

> Nikos I. Bosse, Sam Abbott, Anne Cori, Edwin van Leeuwen, Johannes
> Bracher\* and Sebastian Funk\* (\*: equal contribution) (2023).
> *`Scoring epidemiological forecasts on transformed scales`*, PLoS
> Comput Biol 19(8): e1011393
> <https://doi.org/10.1371/journal.pcbi.1011393>

## Package overview

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
`validate_forecast()` should be used. The result of that check can then
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
  as_forecast() %>%
  add_coverage() %>%
  score() %>%
  summarise_scores(
    by = c("model", "target_type")
  ) %>%
  add_pairwise_comparison(
    baseline = "EuroCOVIDhub-ensemble"
  ) %>%
  summarise_scores(
    fun = signif, 
    digits = 2
  ) %>%
  kable()
#> Some rows containing NA values may be removed. This is fine if not unexpected.
#> Some rows containing NA values may be removed. This is fine if not unexpected.
#> Some rows containing NA values may be removed. This is fine if not unexpected.
```

| model                 | target_type |   wis | overprediction | underprediction | dispersion |    bias | interval_coverage_50 | interval_coverage_90 | interval_coverage_deviation | ae_median | relative_skill | scaled_rel_skill |
|:----------------------|:------------|------:|---------------:|----------------:|-----------:|--------:|---------------------:|---------------------:|----------------------------:|----------:|---------------:|-----------------:|
| EuroCOVIDhub-baseline | Cases       | 28000 |        14000.0 |         10000.0 |       4100 |  0.0980 |                 0.33 |                 0.82 |                      -0.120 |     38000 |           1.30 |              1.6 |
| EuroCOVIDhub-baseline | Deaths      |   160 |           66.0 |             2.1 |         91 |  0.3400 |                 0.66 |                 1.00 |                       0.120 |       230 |           2.30 |              3.8 |
| EuroCOVIDhub-ensemble | Cases       | 18000 |        10000.0 |          4200.0 |       3700 | -0.0560 |                 0.39 |                 0.80 |                      -0.100 |     24000 |           0.82 |              1.0 |
| EuroCOVIDhub-ensemble | Deaths      |    41 |            7.1 |             4.1 |         30 |  0.0730 |                 0.88 |                 1.00 |                       0.200 |        53 |           0.60 |              1.0 |
| UMass-MechBayes       | Deaths      |    53 |            9.0 |            17.0 |         27 | -0.0220 |                 0.46 |                 0.88 |                      -0.025 |        78 |           0.75 |              1.3 |
| epiforecasts-EpiNow2  | Cases       | 21000 |        12000.0 |          3300.0 |       5700 | -0.0790 |                 0.47 |                 0.79 |                      -0.070 |     28000 |           0.95 |              1.2 |
| epiforecasts-EpiNow2  | Deaths      |    67 |           19.0 |            16.0 |         32 | -0.0051 |                 0.42 |                 0.91 |                      -0.045 |       100 |           0.98 |              1.6 |

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
 .[, observed := ifelse(observed < 0, 0, observed)] %>%
  transform_forecasts(append = TRUE, fun = log_shift, offset = 1) %>%
  score %>%
  summarise_scores(by = c("model", "target_type", "scale")) %>%
  head()
#> Some rows containing NA values may be removed. This is fine if not unexpected.
#>                    model target_type   scale         wis overprediction
#> 1: EuroCOVIDhub-ensemble       Cases natural 11550.70664    3650.004755
#> 2: EuroCOVIDhub-baseline       Cases natural 22090.45747    7702.983696
#> 3:  epiforecasts-EpiNow2       Cases natural 14438.43943    5513.705842
#> 4: EuroCOVIDhub-ensemble      Deaths natural    41.42249       7.138247
#> 5: EuroCOVIDhub-baseline      Deaths natural   159.40387      65.899117
#> 6:       UMass-MechBayes      Deaths natural    52.65195       8.978601
#>    underprediction dispersion        bias interval_coverage_50
#> 1:     4237.177310 3663.52458 -0.05640625            0.3906250
#> 2:    10284.972826 4102.50094  0.09726562            0.3281250
#> 3:     3260.355639 5664.37795 -0.07890625            0.4687500
#> 4:        4.103261   30.18099  0.07265625            0.8750000
#> 5:        2.098505   91.40625  0.33906250            0.6640625
#> 6:       16.800951   26.87239 -0.02234375            0.4609375
#>    interval_coverage_90 interval_coverage_deviation   ae_median
#> 1:            0.8046875                 -0.10230114 17707.95312
#> 2:            0.8203125                 -0.11437500 32080.48438
#> 3:            0.7890625                 -0.06963068 21530.69531
#> 4:            1.0000000                  0.20380682    53.13281
#> 5:            1.0000000                  0.12142045   233.25781
#> 6:            0.8750000                 -0.02488636    78.47656
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
