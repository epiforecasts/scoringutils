# Filter to target combinations meeting a coverage threshold

Strategy for
[`filter_scores()`](https://epiforecasts.io/scoringutils/dev/reference/filter_scores.md)
that keeps target combinations covered by at least `min_coverage` of the
values in the `compare` column. With the default `min_coverage = 1`,
only target combinations present for every compare value are kept
(strict intersection across the full set).

To restrict to the targets covered by a named subset of compare values
instead of by a proportion, use
[`filter_to_include()`](https://epiforecasts.io/scoringutils/dev/reference/filter_to_include.md).

## Usage

``` r
filter_to_intersection(min_coverage = 1)
```

## Arguments

- min_coverage:

  Numeric between 0 and 1 (default `1`). Minimum proportion of compare
  values that must cover a target combination for it to be kept.

## Value

A strategy function for
[`filter_scores()`](https://epiforecasts.io/scoringutils/dev/reference/filter_scores.md).
Intended to be passed to
[`filter_scores()`](https://epiforecasts.io/scoringutils/dev/reference/filter_scores.md)
rather than called directly —
[`filter_scores()`](https://epiforecasts.io/scoringutils/dev/reference/filter_scores.md)
is where the `scores` class and `metrics` attribute are preserved.

## See also

[`filter_scores()`](https://epiforecasts.io/scoringutils/dev/reference/filter_scores.md),
[`filter_to_include()`](https://epiforecasts.io/scoringutils/dev/reference/filter_to_include.md)

## Examples

``` r
scores <- example_quantile |>
  as_forecast_quantile() |>
  score()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
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
```
