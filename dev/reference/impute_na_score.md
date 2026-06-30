# Impute with NA values

Strategy for
[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md)
that fills each missing metric with `NA_real_`.

## Usage

``` r
impute_na_score()
```

## Value

A strategy function for
[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md).

## See also

[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md)

## Examples

``` r
scores <- example_quantile |>
  as_forecast_quantile() |>
  score()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.

impute_missing_scores(scores, strategy = impute_na_score())
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
#>    1: EuroCOVIDhub-ensemble       1  7990.85478    2549.869565       0.0000000
#>    2: EuroCOVIDhub-baseline       1 16925.04696   15275.826087       0.0000000
#>    3:  epiforecasts-EpiNow2       1 25395.96087   17222.260870       0.0000000
#>    4: EuroCOVIDhub-ensemble       1    53.88000       0.000000       0.6086957
#>    5: EuroCOVIDhub-baseline       1    46.79304       2.130435       0.0000000
#>   ---                                                                         
#> 1020:       UMass-MechBayes       3          NA             NA              NA
#> 1021:       UMass-MechBayes       2          NA             NA              NA
#> 1022:       UMass-MechBayes       1          NA             NA              NA
#> 1023:       UMass-MechBayes       3          NA             NA              NA
#> 1024:       UMass-MechBayes       2          NA             NA              NA
#>       dispersion  bias interval_coverage_50 interval_coverage_90 ae_median
#>            <num> <num>                <num>                <num>     <num>
#>    1: 5440.98522  0.50                    1                    1     12271
#>    2: 1649.22087  0.95                    0                    0     25620
#>    3: 8173.70000  0.90                    0                    1     44192
#>    4:   53.27130 -0.10                    1                    1        14
#>    5:   44.66261  0.30                    1                    1        15
#>   ---                                                                     
#> 1020:         NA    NA                   NA                   NA        NA
#> 1021:         NA    NA                   NA                   NA        NA
#> 1022:         NA    NA                   NA                   NA        NA
#> 1023:         NA    NA                   NA                   NA        NA
#> 1024:         NA    NA                   NA                   NA        NA
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
