# Filter scores

Filter a `scores` object using a supplied strategy function.
`filter_scores()` is responsible for preserving the `scores` class and
the `metrics` attribute; the strategy is responsible only for the
filtering logic.

Strategies are constructed by helpers such as
[`filter_to_intersection()`](https://epiforecasts.io/scoringutils/dev/reference/filter_to_intersection.md)
and
[`filter_to_include()`](https://epiforecasts.io/scoringutils/dev/reference/filter_to_include.md)
and can also be user-defined. A strategy is a function with signature
`function(scores, compare)` that returns a filtered data.table with the
same columns as its input.

## Usage

``` r
filter_scores(scores, strategy = filter_to_intersection(), compare = "model")
```

## Arguments

- scores:

  An object of class `scores` (a data.table with an additional `metrics`
  attribute as produced by
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)).

- strategy:

  A strategy function. See Description for the expected signature.
  Default:
  [`filter_to_intersection()`](https://epiforecasts.io/scoringutils/dev/reference/filter_to_intersection.md).

- compare:

  Character string (default `"model"`) naming the column whose values
  are compared when deciding which target combinations to keep.

## Value

A `scores` object with the same class and `metrics` attribute as the
input, with rows filtered according to `strategy`.

## See also

[`filter_to_intersection()`](https://epiforecasts.io/scoringutils/dev/reference/filter_to_intersection.md),
[`filter_to_include()`](https://epiforecasts.io/scoringutils/dev/reference/filter_to_include.md),
[`vignette("handling-missing-forecasts")`](https://epiforecasts.io/scoringutils/dev/articles/handling-missing-forecasts.md)

## Examples

``` r
scores <- example_quantile |>
  as_forecast_quantile() |>
  score()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.

# Keep only targets covered by every model (the default)
filter_scores(scores)
#> ℹ Filtered out 411 rows.
#> ℹ 476 of 887 rows remaining.
#> Key: <location, target_end_date, target_type, location_name, forecast_date, horizon>
#>      location target_end_date target_type location_name forecast_date
#>        <char>          <Date>      <char>        <char>        <Date>
#>   1:       DE      2021-05-08      Deaths       Germany    2021-05-03
#>   2:       DE      2021-05-08      Deaths       Germany    2021-05-03
#>   3:       DE      2021-05-08      Deaths       Germany    2021-05-03
#>   4:       DE      2021-05-08      Deaths       Germany    2021-05-03
#>   5:       DE      2021-05-15      Deaths       Germany    2021-05-03
#>  ---                                                                 
#> 472:       IT      2021-07-24      Deaths         Italy    2021-07-05
#> 473:       IT      2021-07-24      Deaths         Italy    2021-07-12
#> 474:       IT      2021-07-24      Deaths         Italy    2021-07-12
#> 475:       IT      2021-07-24      Deaths         Italy    2021-07-12
#> 476:       IT      2021-07-24      Deaths         Italy    2021-07-12
#>                      model horizon       wis overprediction underprediction
#>                     <char>   <num>     <num>          <num>           <num>
#>   1: EuroCOVIDhub-ensemble       1  53.88000       0.000000       0.6086957
#>   2: EuroCOVIDhub-baseline       1  46.79304       2.130435       0.0000000
#>   3:       UMass-MechBayes       1 116.12174       0.000000      39.0434783
#>   4:  epiforecasts-EpiNow2       1  80.35652       1.043478       0.0000000
#>   5: EuroCOVIDhub-ensemble       2 120.57870      49.652174       0.0000000
#>  ---                                                                       
#> 472:  epiforecasts-EpiNow2       3  19.76261       5.478261       0.0000000
#> 473: EuroCOVIDhub-ensemble       2  18.65870       5.304348       0.0000000
#> 474: EuroCOVIDhub-baseline       2  80.33696       3.608696       0.0000000
#> 475:       UMass-MechBayes       2  25.58174      17.826087       0.0000000
#> 476:  epiforecasts-EpiNow2       2  66.16174      40.608696       0.0000000
#>      dispersion  bias interval_coverage_50 interval_coverage_90 ae_median
#>           <num> <num>               <lgcl>               <lgcl>     <num>
#>   1:  53.271304  -0.1                 TRUE                 TRUE        14
#>   2:  44.662609   0.3                 TRUE                 TRUE        15
#>   3:  77.078261  -0.5                 TRUE                 TRUE       208
#>   4:  79.313043   0.1                 TRUE                 TRUE        24
#>   5:  70.926522   0.6                FALSE                 TRUE       210
#>  ---                                                                     
#> 472:  14.284348   0.5                 TRUE                 TRUE        26
#> 473:  13.354348   0.4                 TRUE                 TRUE        30
#> 474:  76.728261   0.2                 TRUE                 TRUE        53
#> 475:   7.755652   0.8                FALSE                 TRUE        46
#> 476:  25.553043   0.9                FALSE                 TRUE       108

# Keep targets covered by at least 75% of models
filter_scores(
  scores,
  strategy = filter_to_intersection(min_coverage = 0.75)
)
#> ℹ No rows filtered. Returning scores unchanged.
#>      location target_end_date target_type location_name forecast_date
#>        <char>          <Date>      <char>        <char>        <Date>
#>   1:       DE      2021-05-08       Cases       Germany    2021-05-03
#>   2:       DE      2021-05-08       Cases       Germany    2021-05-03
#>   3:       DE      2021-05-08       Cases       Germany    2021-05-03
#>   4:       DE      2021-05-08      Deaths       Germany    2021-05-03
#>   5:       DE      2021-05-08      Deaths       Germany    2021-05-03
#>  ---                                                                 
#> 883:       IT      2021-07-24      Deaths         Italy    2021-07-12
#> 884:       IT      2021-07-24      Deaths         Italy    2021-07-05
#> 885:       IT      2021-07-24      Deaths         Italy    2021-07-12
#> 886:       IT      2021-07-24      Deaths         Italy    2021-07-05
#> 887:       IT      2021-07-24      Deaths         Italy    2021-07-12
#>                      model horizon          wis overprediction underprediction
#>                     <char>   <num>        <num>          <num>           <num>
#>   1: EuroCOVIDhub-ensemble       1  7990.854783   2.549870e+03       0.0000000
#>   2: EuroCOVIDhub-baseline       1 16925.046957   1.527583e+04       0.0000000
#>   3:  epiforecasts-EpiNow2       1 25395.960870   1.722226e+04       0.0000000
#>   4: EuroCOVIDhub-ensemble       1    53.880000   0.000000e+00       0.6086957
#>   5: EuroCOVIDhub-baseline       1    46.793043   2.130435e+00       0.0000000
#>  ---                                                                          
#> 883: EuroCOVIDhub-baseline       2    80.336957   3.608696e+00       0.0000000
#> 884:       UMass-MechBayes       3     4.881739   4.347826e-02       0.0000000
#> 885:       UMass-MechBayes       2    25.581739   1.782609e+01       0.0000000
#> 886:  epiforecasts-EpiNow2       3    19.762609   5.478261e+00       0.0000000
#> 887:  epiforecasts-EpiNow2       2    66.161739   4.060870e+01       0.0000000
#>       dispersion  bias interval_coverage_50 interval_coverage_90 ae_median
#>            <num> <num>               <lgcl>               <lgcl>     <num>
#>   1: 5440.985217  0.50                 TRUE                 TRUE     12271
#>   2: 1649.220870  0.95                FALSE                FALSE     25620
#>   3: 8173.700000  0.90                FALSE                 TRUE     44192
#>   4:   53.271304 -0.10                 TRUE                 TRUE        14
#>   5:   44.662609  0.30                 TRUE                 TRUE        15
#>  ---                                                                      
#> 883:   76.728261  0.20                 TRUE                 TRUE        53
#> 884:    4.838261  0.10                 TRUE                 TRUE         1
#> 885:    7.755652  0.80                FALSE                 TRUE        46
#> 886:   14.284348  0.50                 TRUE                 TRUE        26
#> 887:   25.553043  0.90                FALSE                 TRUE       108

# Keep only targets covered by a named model
filter_scores(
  scores,
  strategy = filter_to_include("EuroCOVIDhub-baseline")
)
#> ℹ No rows filtered. Returning scores unchanged.
#>      location target_end_date target_type location_name forecast_date
#>        <char>          <Date>      <char>        <char>        <Date>
#>   1:       DE      2021-05-08       Cases       Germany    2021-05-03
#>   2:       DE      2021-05-08       Cases       Germany    2021-05-03
#>   3:       DE      2021-05-08       Cases       Germany    2021-05-03
#>   4:       DE      2021-05-08      Deaths       Germany    2021-05-03
#>   5:       DE      2021-05-08      Deaths       Germany    2021-05-03
#>  ---                                                                 
#> 883:       IT      2021-07-24      Deaths         Italy    2021-07-12
#> 884:       IT      2021-07-24      Deaths         Italy    2021-07-05
#> 885:       IT      2021-07-24      Deaths         Italy    2021-07-12
#> 886:       IT      2021-07-24      Deaths         Italy    2021-07-05
#> 887:       IT      2021-07-24      Deaths         Italy    2021-07-12
#>                      model horizon          wis overprediction underprediction
#>                     <char>   <num>        <num>          <num>           <num>
#>   1: EuroCOVIDhub-ensemble       1  7990.854783   2.549870e+03       0.0000000
#>   2: EuroCOVIDhub-baseline       1 16925.046957   1.527583e+04       0.0000000
#>   3:  epiforecasts-EpiNow2       1 25395.960870   1.722226e+04       0.0000000
#>   4: EuroCOVIDhub-ensemble       1    53.880000   0.000000e+00       0.6086957
#>   5: EuroCOVIDhub-baseline       1    46.793043   2.130435e+00       0.0000000
#>  ---                                                                          
#> 883: EuroCOVIDhub-baseline       2    80.336957   3.608696e+00       0.0000000
#> 884:       UMass-MechBayes       3     4.881739   4.347826e-02       0.0000000
#> 885:       UMass-MechBayes       2    25.581739   1.782609e+01       0.0000000
#> 886:  epiforecasts-EpiNow2       3    19.762609   5.478261e+00       0.0000000
#> 887:  epiforecasts-EpiNow2       2    66.161739   4.060870e+01       0.0000000
#>       dispersion  bias interval_coverage_50 interval_coverage_90 ae_median
#>            <num> <num>               <lgcl>               <lgcl>     <num>
#>   1: 5440.985217  0.50                 TRUE                 TRUE     12271
#>   2: 1649.220870  0.95                FALSE                FALSE     25620
#>   3: 8173.700000  0.90                FALSE                 TRUE     44192
#>   4:   53.271304 -0.10                 TRUE                 TRUE        14
#>   5:   44.662609  0.30                 TRUE                 TRUE        15
#>  ---                                                                      
#> 883:   76.728261  0.20                 TRUE                 TRUE        53
#> 884:    4.838261  0.10                 TRUE                 TRUE         1
#> 885:    7.755652  0.80                FALSE                 TRUE        46
#> 886:   14.284348  0.50                 TRUE                 TRUE        26
#> 887:   25.553043  0.90                FALSE                 TRUE       108
```
