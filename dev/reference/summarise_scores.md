# Summarise scores as produced by [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)

Summarise scores as produced by
[`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md).

`summarise_scores` relies on a way to identify the names of the scores
and distinguish them from columns that denote the unit of a single
forecast. Internally, this is done via a stored attribute, `metrics`
that stores the names of the scores. This means, however, that you need
to be careful with renaming scores after they have been produced by
[`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md).
If you do, you also have to manually update the attribute by calling
`attr(scores, "metrics") <- new_names`.

## Usage

``` r
summarise_scores(scores, by = "model", fun = mean, ...)

summarize_scores(scores, by = "model", fun = mean, ...)
```

## Arguments

- scores:

  An object of class `scores` (a data.table with scores and an
  additional attribute `metrics` as produced by
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)).

- by:

  Character vector with column names to summarise scores by. Default is
  "model", i.e. scores are summarised by the "model" column.

- fun:

  A function used for summarising scores. Default is
  [`mean()`](https://rdrr.io/r/base/mean.html).

- ...:

  Additional parameters that can be passed to the summary function
  provided to `fun`. For more information see the documentation of the
  respective function.

## Value

A data.table with summarised scores. Scores are summarised according to
the names of the columns of the original data specified in `by` using
the `fun` passed to `summarise_scores()`.

## Examples

``` r
library(magrittr) # pipe operator
scores <- example_sample_continuous %>%
 as_forecast_sample() %>%
 score()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.

# get scores by model
summarise_scores(scores, by = "model")
#>                    model         bias      dss        crps overprediction
#>                   <char>        <num>    <num>       <num>          <num>
#> 1: EuroCOVIDhub-ensemble  0.009765625 16.40496  9876.95886     5281.81845
#> 2: EuroCOVIDhub-baseline  0.177734375      NaN 15309.68627     6701.83844
#> 3:  epiforecasts-EpiNow2 -0.024898785 26.10137 11901.43354     6986.16572
#> 4:       UMass-MechBayes -0.026953125 10.08582    60.19018       11.20505
#>    underprediction dispersion log_score        mad   ae_median      se_mean
#>              <num>      <num>     <num>      <num>       <num>        <num>
#> 1:      2433.20525 2161.93515 10.747811  8763.6176 12406.03100 2.103026e+09
#> 2:      5864.53649 2743.31134       Inf  9680.3792 18932.50196 2.885063e+09
#> 3:      1740.93388 3174.33393       Inf 12999.5404 14680.12285 3.152268e+09
#> 4:        19.28195   29.70318  5.941622   123.6211    79.66001 1.371418e+04

# get scores by model and target type
summarise_scores(scores, by = c("model", "target_type"))
#>                    model target_type        bias      dss        crps
#>                   <char>      <char>       <num>    <num>       <num>
#> 1: EuroCOVIDhub-ensemble       Cases -0.04648437 22.89997 19703.05522
#> 2: EuroCOVIDhub-baseline       Cases  0.03671875      NaN 30453.58346
#> 3:  epiforecasts-EpiNow2       Cases -0.03867188 40.87716 22896.51608
#> 4: EuroCOVIDhub-ensemble      Deaths  0.06601562  9.90995    50.86249
#> 5: EuroCOVIDhub-baseline      Deaths  0.31875000 12.99360   165.78907
#> 6:       UMass-MechBayes      Deaths -0.02695313 10.08582    60.19018
#> 7:  epiforecasts-EpiNow2      Deaths -0.01008403 10.20807    74.79013
#>    overprediction underprediction dispersion log_score        mad   ae_median
#>             <num>           <num>      <num>     <num>      <num>       <num>
#> 1:    10552.97603     4861.015121 4289.06407 15.633420 17385.2629 24749.39707
#> 2:    13346.03509    11727.330575 5380.21780       Inf 18982.2128 37648.01693
#> 3:    13462.90822     3346.583024 6087.02483       Inf 24929.3438 28233.04536
#> 4:       10.66088        5.395380   34.80623  5.862203   141.9723    62.66492
#> 5:       57.64179        1.742401  106.40489  6.977391   378.5457   216.98699
#> 6:       11.20505       19.281946   29.70318  5.941622   123.6211    79.66001
#> 7:       19.58555       13.849095   41.35549  6.024092   167.4829   102.18939
#>         se_mean
#>           <num>
#> 1: 4.206042e+09
#> 2: 5.769964e+09
#> 3: 6.082863e+09
#> 4: 1.080233e+04
#> 5: 1.622417e+05
#> 6: 1.371418e+04
#> 7: 3.243111e+04

# get standard deviation
summarise_scores(scores, by = "model", fun = sd)
#>                    model      bias        dss        crps overprediction
#>                   <char>     <num>      <num>       <num>          <num>
#> 1: EuroCOVIDhub-ensemble 0.5468290  14.869520 39368.24836    37275.23950
#> 2: EuroCOVIDhub-baseline 0.5457971         NA 45020.82814    39070.74445
#> 3:  epiforecasts-EpiNow2 0.6083410 108.130107 44957.07746    40776.81690
#> 4:       UMass-MechBayes 0.6221914   2.248998    49.62465       21.34675
#>    underprediction dispersion log_score        mad   ae_median      se_mean
#>              <num>      <num>     <num>      <num>       <num>        <num>
#> 1:      8634.87723 5163.42293 21.510119 19799.1620 42801.64123 1.564286e+10
#> 2:     20537.03929 3664.87255       NaN 13610.4174 49458.36446 1.760651e+10
#> 3:      8096.39644 7266.11787       NaN 29616.1714 51129.54601 2.209086e+10
#> 4:        36.98584   29.60927  1.126019   123.3465    76.09471 2.994664e+04

# round digits
summarise_scores(scores, by = "model") %>%
  summarise_scores(fun = signif, digits = 2)
#>                    model    bias   dss  crps overprediction underprediction
#>                   <char>   <num> <num> <num>          <num>           <num>
#> 1: EuroCOVIDhub-ensemble  0.0098    16  9900           5300            2400
#> 2: EuroCOVIDhub-baseline  0.1800   NaN 15000           6700            5900
#> 3:  epiforecasts-EpiNow2 -0.0250    26 12000           7000            1700
#> 4:       UMass-MechBayes -0.0270    10    60             11              19
#>    dispersion log_score   mad ae_median se_mean
#>         <num>     <num> <num>     <num>   <num>
#> 1:       2200      11.0  8800     12000 2.1e+09
#> 2:       2700       Inf  9700     19000 2.9e+09
#> 3:       3200       Inf 13000     15000 3.2e+09
#> 4:         30       5.9   120        80 1.4e+04
```
