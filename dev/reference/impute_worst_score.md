# Impute with worst (maximum) observed score

Strategy for
[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md)
that fills each missing metric with the worst (maximum) observed value
for that metric within the same target combination across all values of
the `compare` column. Target combinations with no non-NA observations
are filled with `NA_real_`.

## Usage

``` r
impute_worst_score()
```

## Value

A strategy function for
[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md).

## See also

[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md),
[`impute_mean_score()`](https://epiforecasts.io/scoringutils/dev/reference/impute_mean_score.md)

## Examples

``` r
scores <- example_quantile |>
  as_forecast_quantile() |>
  score()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.

impute_missing_scores(scores, strategy = impute_worst_score())
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
#> 1020:       UMass-MechBayes       3 12956.58826       0.000000    1.287265e+04
#> 1021:       UMass-MechBayes       2  7242.90913       0.000000    6.685870e+03
#> 1022:       UMass-MechBayes       1  4190.08739       8.478261    2.884739e+03
#> 1023:       UMass-MechBayes       3 17401.27217       0.000000    1.647296e+04
#> 1024:       UMass-MechBayes       2 11480.34913     341.000000    8.971000e+03
#>       dispersion  bias interval_coverage_50 interval_coverage_90 ae_median
#>            <num> <num>                <int>                <int>     <num>
#>    1: 5440.98522  0.50                    1                    1     12271
#>    2: 1649.22087  0.95                    0                    0     25620
#>    3: 8173.70000  0.90                    0                    1     44192
#>    4:   53.27130 -0.10                    1                    1        14
#>    5:   44.66261  0.30                    1                    1        15
#>   ---                                                                     
#> 1020: 3275.16522 -0.40                    1                    1     13259
#> 1021: 2406.71000 -0.50                    1                    1     10401
#> 1022: 1305.34826  0.10                    1                    1      7073
#> 1023: 3248.63174 -0.70                    0                    1     24208
#> 1024: 3331.34043  0.30                    1                    1     20967
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
