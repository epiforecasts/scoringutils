# Impute with mean observed score

Strategy for
[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md)
that fills each missing metric with the mean observed value for that
metric within the same target combination across all values of the
`compare` column. Target combinations with no non-NA observations are
filled with `NA_real_`.

## Usage

``` r
impute_mean_score()
```

## Value

A strategy function for
[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md).

## See also

[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md),
[`impute_worst_score()`](https://epiforecasts.io/scoringutils/dev/reference/impute_worst_score.md)

## Examples

``` r
scores <- example_quantile |>
  as_forecast_quantile() |>
  score()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.

impute_missing_scores(scores, strategy = impute_mean_score())
#> ℹ Imputing 137 missing score rows.
#> ℹ 2 model values affected.
#>       location target_end_date target_type location_name forecast_date
#>         <char>          <Date>      <char>        <char>        <Date>
#>    1:       DE      2021-05-08       Cases       Germany    2021-05-03
#>    2:       DE      2021-05-08       Cases       Germany    2021-05-03
#>    3:       DE      2021-05-08       Cases       Germany    2021-05-03
#>    4:       DE      2021-05-08      Deaths       Germany    2021-05-03
#>    5:       DE      2021-05-08      Deaths       Germany    2021-05-03
#>   ---                                                                 
#> 1020:       IT      2021-07-17       Cases         Italy    2021-06-28
#> 1021:       IT      2021-07-17       Cases         Italy    2021-07-05
#> 1022:       IT      2021-07-17       Cases         Italy    2021-07-12
#> 1023:       IT      2021-07-24       Cases         Italy    2021-07-05
#> 1024:       IT      2021-07-24       Cases         Italy    2021-07-12
#>                       model horizon         wis overprediction underprediction
#>                      <char>   <num>       <num>          <num>           <num>
#>    1: EuroCOVIDhub-ensemble       1  7990.85478    2549.869565    0.000000e+00
#>    2: EuroCOVIDhub-baseline       1 16925.04696   15275.826087    0.000000e+00
#>    3:  epiforecasts-EpiNow2       1 25395.96087   17222.260870    0.000000e+00
#>    4: EuroCOVIDhub-ensemble       1    53.88000       0.000000    6.086957e-01
#>    5: EuroCOVIDhub-baseline       1    46.79304       2.130435    0.000000e+00
#>   ---                                                                         
#> 1020:       UMass-MechBayes       3  9429.09942       0.000000    8.184551e+03
#> 1021:       UMass-MechBayes       2  5293.94435       0.000000    3.998116e+03
#> 1022:       UMass-MechBayes       1  2527.43304       2.826087    1.608058e+03
#> 1023:       UMass-MechBayes       3 12959.05783       0.000000    1.088126e+04
#> 1024:       UMass-MechBayes       2  7690.73493     113.666667    5.053913e+03
#>       dispersion       bias interval_coverage_50 interval_coverage_90 ae_median
#>            <num>      <num>                <num>                <num>     <num>
#>    1: 5440.98522  0.5000000            1.0000000            1.0000000 12271.000
#>    2: 1649.22087  0.9500000            0.0000000            0.0000000 25620.000
#>    3: 8173.70000  0.9000000            0.0000000            1.0000000 44192.000
#>    4:   53.27130 -0.1000000            1.0000000            1.0000000    14.000
#>    5:   44.66261  0.3000000            1.0000000            1.0000000    15.000
#>   ---                                                                          
#> 1020: 1244.54870 -0.8000000            0.3333333            0.3333333 11685.333
#> 1021: 1295.82841 -0.7600000            0.3333333            0.6666667  8902.667
#> 1022:  916.54899 -0.4666667            0.3333333            1.0000000  4051.667
#> 1023: 2077.79696 -0.8266667            0.0000000            0.6666667 21995.000
#> 1024: 2523.15522 -0.4666667            0.3333333            1.0000000 12982.333
#>       .imputed
#>         <lgcl>
#>    1:    FALSE
#>    2:    FALSE
#>    3:    FALSE
#>    4:    FALSE
#>    5:    FALSE
#>   ---         
#> 1020:     TRUE
#> 1021:     TRUE
#> 1022:     TRUE
#> 1023:     TRUE
#> 1024:     TRUE
```
