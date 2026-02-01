# Evaluate forecasts

`score()` applies a selection of scoring metrics to a forecast object.
`score()` is a generic that dispatches to different methods depending on
the class of the input data.

See
[`as_forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_binary.md),
[`as_forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_quantile.md)
etc. for information on how to create a forecast object.

See
[`get_forecast_unit()`](https://epiforecasts.io/scoringutils/dev/reference/get_forecast_unit.md)
for more information on the concept of a forecast unit.

For additional help and examples, check out the paper [Evaluating
Forecasts with scoringutils in R](https://arxiv.org/abs/2205.07090).

## Usage

``` r
# S3 method for class 'forecast_binary'
score(forecast, metrics = get_metrics(forecast), ...)

# S3 method for class 'forecast_sample_multivariate'
score(forecast, metrics = get_metrics(forecast), ...)

# S3 method for class 'forecast_nominal'
score(forecast, metrics = get_metrics(forecast), ...)

# S3 method for class 'forecast_ordinal'
score(forecast, metrics = get_metrics(forecast), ...)

# S3 method for class 'forecast_point'
score(forecast, metrics = get_metrics(forecast), ...)

# S3 method for class 'forecast_quantile'
score(forecast, metrics = get_metrics(forecast), ...)

# S3 method for class 'forecast_sample'
score(forecast, metrics = get_metrics(forecast), ...)

score(forecast, metrics, ...)
```

## Arguments

- forecast:

  A forecast object (a validated data.table with predicted and observed
  values).

- metrics:

  A named list of scoring functions. Each element should be a function
  reference, not a function call. For example, use
  `list("crps" = crps_sample)` rather than
  `list("crps" = crps_sample())`. Names will be used as column names in
  the output. See
  [`get_metrics()`](https://epiforecasts.io/scoringutils/dev/reference/get_metrics.md)
  for more information on the default metrics used. See the *Customising
  metrics* section below for information on how to pass custom arguments
  to scoring functions.

- ...:

  Currently unused. You *cannot* pass additional arguments to scoring
  functions via `...`. See the *Customising metrics* section below for
  details on how to use
  [`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html)
  to pass arguments to individual metrics.

## Value

An object of class `scores`. This object is a data.table with
unsummarised scores (one score per forecast) and has an additional
attribute `metrics` with the names of the metrics used for scoring. See
[`summarise_scores()`](https://epiforecasts.io/scoringutils/dev/reference/summarise_scores.md))
for information on how to summarise scores.

## Details

**Customising metrics**

If you want to pass arguments to a scoring function, you need change the
scoring function itself via e.g.
[`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html)
and pass an updated list of functions with your custom metric to the
`metrics` argument in `score()`. For example, to use
[`interval_coverage()`](https://epiforecasts.io/scoringutils/dev/reference/interval_coverage.md)
with `interval_range = 90`, you would define a new function, e.g.
`interval_coverage_90 <- purrr::partial(interval_coverage, interval_range = 90)`
and pass this new function to `metrics` in `score()`.

Note that if you want to pass a variable as an argument, you can unquote
it with `!!` to make sure the value is evaluated only once when the
function is created. Consider the following example:

    custom_arg <- "foo"
    print1 <- purrr::partial(print, x = custom_arg)
    print2 <- purrr::partial(print, x = !!custom_arg)

    custom_arg <- "bar"
    print1() # prints 'bar'
    print2() # prints 'foo'

## References

Bosse NI, Gruson H, Cori A, van Leeuwen E, Funk S, Abbott S (2022)
Evaluating Forecasts with scoringutils in R.
[doi:10.48550/arXiv.2205.07090](https://doi.org/10.48550/arXiv.2205.07090)

## Author

Nikos Bosse <nikosbosse@gmail.com>

## Examples

``` r
library(magrittr) # pipe operator

validated <- as_forecast_quantile(example_quantile)
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
score(validated) %>%
  summarise_scores(by = c("model", "target_type"))
#>                    model target_type         wis overprediction underprediction
#>                   <char>      <char>       <num>          <num>           <num>
#> 1: EuroCOVIDhub-ensemble       Cases 17943.82383   10043.121943     4237.177310
#> 2: EuroCOVIDhub-baseline       Cases 28483.57465   14096.100883    10284.972826
#> 3:  epiforecasts-EpiNow2       Cases 20831.55662   11906.823030     3260.355639
#> 4: EuroCOVIDhub-ensemble      Deaths    41.42249       7.138247        4.103261
#> 5: EuroCOVIDhub-baseline      Deaths   159.40387      65.899117        2.098505
#> 6:       UMass-MechBayes      Deaths    52.65195       8.978601       16.800951
#> 7:  epiforecasts-EpiNow2      Deaths    66.64282      18.892583       15.893314
#>    dispersion        bias interval_coverage_50 interval_coverage_90   ae_median
#>         <num>       <num>                <num>                <num>       <num>
#> 1: 3663.52458 -0.05640625            0.3906250            0.8046875 24101.07031
#> 2: 4102.50094  0.09796875            0.3281250            0.8203125 38473.60156
#> 3: 5664.37795 -0.07890625            0.4687500            0.7890625 27923.81250
#> 4:   30.18099  0.07265625            0.8750000            1.0000000    53.13281
#> 5:   91.40625  0.33906250            0.6640625            1.0000000   233.25781
#> 6:   26.87239 -0.02234375            0.4609375            0.8750000    78.47656
#> 7:   31.85692 -0.00512605            0.4201681            0.9075630   104.74790

# set forecast unit manually (to avoid issues with scoringutils trying to
# determine the forecast unit automatically)
example_quantile %>%
  as_forecast_quantile(
    forecast_unit = c(
      "location", "target_end_date", "target_type", "horizon", "model"
    )
  ) %>%
  score()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#>      location target_end_date target_type horizon                 model
#>        <char>          <Date>      <char>   <num>                <char>
#>   1:       DE      2021-05-08       Cases       1 EuroCOVIDhub-ensemble
#>   2:       DE      2021-05-08       Cases       1 EuroCOVIDhub-baseline
#>   3:       DE      2021-05-08       Cases       1  epiforecasts-EpiNow2
#>   4:       DE      2021-05-08      Deaths       1 EuroCOVIDhub-ensemble
#>   5:       DE      2021-05-08      Deaths       1 EuroCOVIDhub-baseline
#>  ---                                                                   
#> 883:       IT      2021-07-24      Deaths       2 EuroCOVIDhub-baseline
#> 884:       IT      2021-07-24      Deaths       3       UMass-MechBayes
#> 885:       IT      2021-07-24      Deaths       2       UMass-MechBayes
#> 886:       IT      2021-07-24      Deaths       3  epiforecasts-EpiNow2
#> 887:       IT      2021-07-24      Deaths       2  epiforecasts-EpiNow2
#>               wis overprediction underprediction  dispersion  bias
#>             <num>          <num>           <num>       <num> <num>
#>   1:  7990.854783   2.549870e+03       0.0000000 5440.985217  0.50
#>   2: 16925.046957   1.527583e+04       0.0000000 1649.220870  0.95
#>   3: 25395.960870   1.722226e+04       0.0000000 8173.700000  0.90
#>   4:    53.880000   0.000000e+00       0.6086957   53.271304 -0.10
#>   5:    46.793043   2.130435e+00       0.0000000   44.662609  0.30
#>  ---                                                              
#> 883:    80.336957   3.608696e+00       0.0000000   76.728261  0.20
#> 884:     4.881739   4.347826e-02       0.0000000    4.838261  0.10
#> 885:    25.581739   1.782609e+01       0.0000000    7.755652  0.80
#> 886:    19.762609   5.478261e+00       0.0000000   14.284348  0.50
#> 887:    66.161739   4.060870e+01       0.0000000   25.553043  0.90
#>      interval_coverage_50 interval_coverage_90 ae_median
#>                    <lgcl>               <lgcl>     <num>
#>   1:                 TRUE                 TRUE     12271
#>   2:                FALSE                FALSE     25620
#>   3:                FALSE                 TRUE     44192
#>   4:                 TRUE                 TRUE        14
#>   5:                 TRUE                 TRUE        15
#>  ---                                                    
#> 883:                 TRUE                 TRUE        53
#> 884:                 TRUE                 TRUE         1
#> 885:                FALSE                 TRUE        46
#> 886:                 TRUE                 TRUE        26
#> 887:                FALSE                 TRUE       108

# forecast formats with different metrics
if (FALSE) { # \dontrun{
score(as_forecast_binary(example_binary))
score(as_forecast_quantile(example_quantile))
score(as_forecast_point(example_point))
score(as_forecast_sample(example_sample_discrete))
score(as_forecast_sample(example_sample_continuous))
} # }

# passing a subset of metrics using select_metrics()
# (the preferred approach for selecting from default metrics)
example_sample_continuous %>%
  as_forecast_sample() %>%
  score(metrics = select_metrics(
    get_metrics(as_forecast_sample(example_sample_continuous)),
    select = c("crps", "mad")
  ))
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#>      location location_name target_end_date target_type forecast_date
#>        <char>        <char>          <Date>      <char>        <Date>
#>   1:       DE       Germany      2021-05-08       Cases    2021-05-03
#>   2:       DE       Germany      2021-05-08       Cases    2021-05-03
#>   3:       DE       Germany      2021-05-08       Cases    2021-05-03
#>   4:       DE       Germany      2021-05-08      Deaths    2021-05-03
#>   5:       DE       Germany      2021-05-08      Deaths    2021-05-03
#>  ---                                                                 
#> 883:       IT         Italy      2021-07-24      Deaths    2021-07-12
#> 884:       IT         Italy      2021-07-24      Deaths    2021-07-05
#> 885:       IT         Italy      2021-07-24      Deaths    2021-07-12
#> 886:       IT         Italy      2021-07-24      Deaths    2021-07-05
#> 887:       IT         Italy      2021-07-24      Deaths    2021-07-12
#>                      model horizon         crps         mad
#>                     <char>   <num>        <num>       <num>
#>   1: EuroCOVIDhub-ensemble       1  7482.975177 17641.24334
#>   2: EuroCOVIDhub-baseline       1 20371.250988 19341.68942
#>   3:  epiforecasts-EpiNow2       1 24810.424753 32348.79978
#>   4: EuroCOVIDhub-ensemble       1    67.510511   267.13585
#>   5: EuroCOVIDhub-baseline       1    86.462930   397.09371
#>  ---                                                       
#> 883: EuroCOVIDhub-baseline       2    66.515150   168.03093
#> 884:       UMass-MechBayes       3     6.616381    24.76457
#> 885:       UMass-MechBayes       2    29.446723    39.96542
#> 886:  epiforecasts-EpiNow2       3    28.863742   107.93293
#> 887:  epiforecasts-EpiNow2       2    49.998906    93.54245

# passing a custom list of metrics manually
# make sure to pass the function itself, not the result of calling it,
# i.e. use `crps_sample` (correct) instead of `crps_sample()` (incorrect)
example_sample_continuous %>%
  as_forecast_sample() %>%
  score(metrics = list("crps" = crps_sample, "mad" = mad_sample))
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#>      location location_name target_end_date target_type forecast_date
#>        <char>        <char>          <Date>      <char>        <Date>
#>   1:       DE       Germany      2021-05-08       Cases    2021-05-03
#>   2:       DE       Germany      2021-05-08       Cases    2021-05-03
#>   3:       DE       Germany      2021-05-08       Cases    2021-05-03
#>   4:       DE       Germany      2021-05-08      Deaths    2021-05-03
#>   5:       DE       Germany      2021-05-08      Deaths    2021-05-03
#>  ---                                                                 
#> 883:       IT         Italy      2021-07-24      Deaths    2021-07-12
#> 884:       IT         Italy      2021-07-24      Deaths    2021-07-05
#> 885:       IT         Italy      2021-07-24      Deaths    2021-07-12
#> 886:       IT         Italy      2021-07-24      Deaths    2021-07-05
#> 887:       IT         Italy      2021-07-24      Deaths    2021-07-12
#>                      model horizon         crps         mad
#>                     <char>   <num>        <num>       <num>
#>   1: EuroCOVIDhub-ensemble       1  7482.975177 17641.24334
#>   2: EuroCOVIDhub-baseline       1 20371.250988 19341.68942
#>   3:  epiforecasts-EpiNow2       1 24810.424753 32348.79978
#>   4: EuroCOVIDhub-ensemble       1    67.510511   267.13585
#>   5: EuroCOVIDhub-baseline       1    86.462930   397.09371
#>  ---                                                       
#> 883: EuroCOVIDhub-baseline       2    66.515150   168.03093
#> 884:       UMass-MechBayes       3     6.616381    24.76457
#> 885:       UMass-MechBayes       2    29.446723    39.96542
#> 886:  epiforecasts-EpiNow2       3    28.863742   107.93293
#> 887:  epiforecasts-EpiNow2       2    49.998906    93.54245

# multivariate forecasts
if (FALSE) { # \dontrun{
score(example_multivariate_sample)
} # }
```
