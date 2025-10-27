# Get quantile and interval coverage values for quantile-based forecasts

For a validated forecast object in a quantile-based format (see
[`as_forecast_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_quantile.md)
for more information), this function computes:

- interval coverage of central prediction intervals

- quantile coverage for predictive quantiles

- the deviation between desired and actual coverage (both for interval
  and quantile coverage)

Coverage values are computed for a specific level of grouping, as
specified in the `by` argument. By default, coverage values are computed
per model.

**Interval coverage**

Interval coverage for a given interval range is defined as the
proportion of observations that fall within the corresponding central
prediction intervals. Central prediction intervals are symmetric around
the median and formed by two quantiles that denote the lower and upper
bound. For example, the 50% central prediction interval is the interval
between the 0.25 and 0.75 quantiles of the predictive distribution.

**Quantile coverage**

Quantile coverage for a given quantile level is defined as the
proportion of observed values that are smaller than the corresponding
predictive quantile. For example, the 0.5 quantile coverage is the
proportion of observed values that are smaller than the 0.5 quantile of
the predictive distribution. Just as above, for a single observation and
the quantile of a single predictive distribution, the value will either
be `TRUE` or `FALSE`.

**Coverage deviation**

The coverage deviation is the difference between the desired coverage
(can be either interval or quantile coverage) and the actual coverage.
For example, if the desired coverage is 90% and the actual coverage is
80%, the coverage deviation is -0.1.

## Usage

``` r
get_coverage(forecast, by = "model")
```

## Arguments

- forecast:

  A forecast object (a validated data.table with predicted and observed
  values).

- by:

  character vector that denotes the level of grouping for which the
  coverage values should be computed. By default (`"model"`), one
  coverage value per model will be returned.

## Value

A data.table with columns as specified in `by` and additional columns
for the coverage values described above

a data.table with columns "interval_coverage",
"interval_coverage_deviation", "quantile_coverage",
"quantile_coverage_deviation" and the columns specified in `by`.

## Examples

``` r
library(magrittr) # pipe operator
example_quantile %>%
  as_forecast_quantile() %>%
  get_coverage(by = "model")
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#>                     model quantile_level interval_range interval_coverage
#>                    <char>          <num>          <num>             <num>
#>  1: EuroCOVIDhub-baseline          0.500              0       0.000000000
#>  2: EuroCOVIDhub-baseline          0.450             10       0.085937500
#>  3: EuroCOVIDhub-baseline          0.550             10       0.085937500
#>  4: EuroCOVIDhub-baseline          0.400             20       0.191406250
#>  5: EuroCOVIDhub-baseline          0.600             20       0.191406250
#>  6: EuroCOVIDhub-baseline          0.350             30       0.289062500
#>  7: EuroCOVIDhub-baseline          0.650             30       0.289062500
#>  8: EuroCOVIDhub-baseline          0.300             40       0.375000000
#>  9: EuroCOVIDhub-baseline          0.700             40       0.375000000
#> 10: EuroCOVIDhub-baseline          0.250             50       0.496093750
#> 11: EuroCOVIDhub-baseline          0.750             50       0.496093750
#> 12: EuroCOVIDhub-baseline          0.200             60       0.628906250
#> 13: EuroCOVIDhub-baseline          0.800             60       0.628906250
#> 14: EuroCOVIDhub-baseline          0.150             70       0.773437500
#> 15: EuroCOVIDhub-baseline          0.850             70       0.773437500
#> 16: EuroCOVIDhub-baseline          0.100             80       0.843750000
#> 17: EuroCOVIDhub-baseline          0.900             80       0.843750000
#> 18: EuroCOVIDhub-baseline          0.050             90       0.910156250
#> 19: EuroCOVIDhub-baseline          0.950             90       0.910156250
#> 20: EuroCOVIDhub-baseline          0.025             95       0.925781250
#> 21: EuroCOVIDhub-baseline          0.975             95       0.925781250
#> 22: EuroCOVIDhub-baseline          0.010             98       0.933593750
#> 23: EuroCOVIDhub-baseline          0.990             98       0.933593750
#> 24: EuroCOVIDhub-ensemble          0.500              0       0.003906250
#> 25: EuroCOVIDhub-ensemble          0.450             10       0.148437500
#> 26: EuroCOVIDhub-ensemble          0.550             10       0.148437500
#> 27: EuroCOVIDhub-ensemble          0.400             20       0.250000000
#> 28: EuroCOVIDhub-ensemble          0.600             20       0.250000000
#> 29: EuroCOVIDhub-ensemble          0.350             30       0.386718750
#> 30: EuroCOVIDhub-ensemble          0.650             30       0.386718750
#> 31: EuroCOVIDhub-ensemble          0.300             40       0.519531250
#> 32: EuroCOVIDhub-ensemble          0.700             40       0.519531250
#> 33: EuroCOVIDhub-ensemble          0.250             50       0.632812500
#> 34: EuroCOVIDhub-ensemble          0.750             50       0.632812500
#> 35: EuroCOVIDhub-ensemble          0.200             60       0.667968750
#> 36: EuroCOVIDhub-ensemble          0.800             60       0.667968750
#> 37: EuroCOVIDhub-ensemble          0.150             70       0.753906250
#> 38: EuroCOVIDhub-ensemble          0.850             70       0.753906250
#> 39: EuroCOVIDhub-ensemble          0.100             80       0.816406250
#> 40: EuroCOVIDhub-ensemble          0.900             80       0.816406250
#> 41: EuroCOVIDhub-ensemble          0.050             90       0.902343750
#> 42: EuroCOVIDhub-ensemble          0.950             90       0.902343750
#> 43: EuroCOVIDhub-ensemble          0.025             95       0.941406250
#> 44: EuroCOVIDhub-ensemble          0.975             95       0.941406250
#> 45: EuroCOVIDhub-ensemble          0.010             98       0.968750000
#> 46: EuroCOVIDhub-ensemble          0.990             98       0.968750000
#> 47:  epiforecasts-EpiNow2          0.500              0       0.004048583
#> 48:  epiforecasts-EpiNow2          0.450             10       0.093117409
#> 49:  epiforecasts-EpiNow2          0.550             10       0.093117409
#> 50:  epiforecasts-EpiNow2          0.400             20       0.165991903
#> 51:  epiforecasts-EpiNow2          0.600             20       0.165991903
#> 52:  epiforecasts-EpiNow2          0.350             30       0.230769231
#> 53:  epiforecasts-EpiNow2          0.650             30       0.230769231
#> 54:  epiforecasts-EpiNow2          0.300             40       0.319838057
#> 55:  epiforecasts-EpiNow2          0.700             40       0.319838057
#> 56:  epiforecasts-EpiNow2          0.250             50       0.445344130
#> 57:  epiforecasts-EpiNow2          0.750             50       0.445344130
#> 58:  epiforecasts-EpiNow2          0.200             60       0.538461538
#> 59:  epiforecasts-EpiNow2          0.800             60       0.538461538
#> 60:  epiforecasts-EpiNow2          0.150             70       0.635627530
#> 61:  epiforecasts-EpiNow2          0.850             70       0.635627530
#> 62:  epiforecasts-EpiNow2          0.100             80       0.732793522
#> 63:  epiforecasts-EpiNow2          0.900             80       0.732793522
#> 64:  epiforecasts-EpiNow2          0.050             90       0.846153846
#> 65:  epiforecasts-EpiNow2          0.950             90       0.846153846
#> 66:  epiforecasts-EpiNow2          0.025             95       0.874493927
#> 67:  epiforecasts-EpiNow2          0.975             95       0.874493927
#> 68:  epiforecasts-EpiNow2          0.010             98       0.910931174
#> 69:  epiforecasts-EpiNow2          0.990             98       0.910931174
#> 70:       UMass-MechBayes          0.500              0       0.015625000
#> 71:       UMass-MechBayes          0.450             10       0.101562500
#> 72:       UMass-MechBayes          0.550             10       0.101562500
#> 73:       UMass-MechBayes          0.400             20       0.195312500
#> 74:       UMass-MechBayes          0.600             20       0.195312500
#> 75:       UMass-MechBayes          0.350             30       0.281250000
#> 76:       UMass-MechBayes          0.650             30       0.281250000
#> 77:       UMass-MechBayes          0.300             40       0.382812500
#> 78:       UMass-MechBayes          0.700             40       0.382812500
#> 79:       UMass-MechBayes          0.250             50       0.460937500
#> 80:       UMass-MechBayes          0.750             50       0.460937500
#> 81:       UMass-MechBayes          0.200             60       0.539062500
#> 82:       UMass-MechBayes          0.800             60       0.539062500
#> 83:       UMass-MechBayes          0.150             70       0.617187500
#> 84:       UMass-MechBayes          0.850             70       0.617187500
#> 85:       UMass-MechBayes          0.100             80       0.765625000
#> 86:       UMass-MechBayes          0.900             80       0.765625000
#> 87:       UMass-MechBayes          0.050             90       0.875000000
#> 88:       UMass-MechBayes          0.950             90       0.875000000
#> 89:       UMass-MechBayes          0.025             95       0.953125000
#> 90:       UMass-MechBayes          0.975             95       0.953125000
#> 91:       UMass-MechBayes          0.010             98       0.984375000
#> 92:       UMass-MechBayes          0.990             98       0.984375000
#>                     model quantile_level interval_range interval_coverage
#>     interval_coverage_deviation quantile_coverage quantile_coverage_deviation
#>                           <num>             <num>                       <num>
#>  1:                 0.000000000        0.69921875                 0.199218750
#>  2:                -0.014062500        0.65625000                 0.206250000
#>  3:                -0.014062500        0.74218750                 0.192187500
#>  4:                -0.008593750        0.58593750                 0.185937500
#>  5:                -0.008593750        0.77343750                 0.173437500
#>  6:                -0.010937500        0.52343750                 0.173437500
#>  7:                -0.010937500        0.80859375                 0.158593750
#>  8:                -0.025000000        0.46875000                 0.168750000
#>  9:                -0.025000000        0.84375000                 0.143750000
#> 10:                -0.003906250        0.36718750                 0.117187500
#> 11:                -0.003906250        0.86328125                 0.113281250
#> 12:                 0.028906250        0.25000000                 0.050000000
#> 13:                 0.028906250        0.87500000                 0.075000000
#> 14:                 0.073437500        0.13281250                -0.017187500
#> 15:                 0.073437500        0.90625000                 0.056250000
#> 16:                 0.043750000        0.08203125                -0.017968750
#> 17:                 0.043750000        0.92578125                 0.025781250
#> 18:                 0.010156250        0.04296875                -0.007031250
#> 19:                 0.010156250        0.95312500                 0.003125000
#> 20:                -0.024218750        0.03125000                 0.006250000
#> 21:                -0.024218750        0.95703125                -0.017968750
#> 22:                -0.046406250        0.03125000                 0.021250000
#> 23:                -0.046406250        0.96484375                -0.025156250
#> 24:                 0.003906250        0.53125000                 0.031250000
#> 25:                 0.048437500        0.46484375                 0.014843750
#> 26:                 0.048437500        0.60156250                 0.051562500
#> 27:                 0.050000000        0.40625000                 0.006250000
#> 28:                 0.050000000        0.65625000                 0.056250000
#> 29:                 0.086718750        0.33984375                -0.010156250
#> 30:                 0.086718750        0.72656250                 0.076562500
#> 31:                 0.119531250        0.26562500                -0.034375000
#> 32:                 0.119531250        0.76562500                 0.065625000
#> 33:                 0.132812500        0.16406250                -0.085937500
#> 34:                 0.132812500        0.79687500                 0.046875000
#> 35:                 0.067968750        0.14062500                -0.059375000
#> 36:                 0.067968750        0.80468750                 0.004687500
#> 37:                 0.053906250        0.10156250                -0.048437500
#> 38:                 0.053906250        0.85546875                 0.005468750
#> 39:                 0.016406250        0.07812500                -0.021875000
#> 40:                 0.016406250        0.89453125                -0.005468750
#> 41:                 0.002343750        0.04296875                -0.007031250
#> 42:                 0.002343750        0.94531250                -0.004687500
#> 43:                -0.008593750        0.03125000                 0.006250000
#> 44:                -0.008593750        0.97265625                -0.002343750
#> 45:                -0.011250000        0.01562500                 0.005625000
#> 46:                -0.011250000        0.98437500                -0.005625000
#> 47:                 0.004048583        0.49392713                -0.006072874
#> 48:                -0.006882591        0.43724696                -0.012753036
#> 49:                -0.006882591        0.53036437                -0.019635628
#> 50:                -0.034008097        0.39676113                -0.003238866
#> 51:                -0.034008097        0.55870445                -0.041295547
#> 52:                -0.069230769        0.36437247                 0.014372470
#> 53:                -0.069230769        0.59514170                -0.054858300
#> 54:                -0.080161943        0.31983806                 0.019838057
#> 55:                -0.080161943        0.63967611                -0.060323887
#> 56:                -0.054655870        0.26315789                 0.013157895
#> 57:                -0.054655870        0.70445344                -0.045546559
#> 58:                -0.061538462        0.20647773                 0.006477733
#> 59:                -0.061538462        0.74493927                -0.055060729
#> 60:                -0.064372470        0.14574899                -0.004251012
#> 61:                -0.064372470        0.78137652                -0.068623482
#> 62:                -0.067206478        0.10121457                 0.001214575
#> 63:                -0.067206478        0.83400810                -0.065991903
#> 64:                -0.053846154        0.06072874                 0.010728745
#> 65:                -0.053846154        0.90688259                -0.043117409
#> 66:                -0.075506073        0.04858300                 0.023582996
#> 67:                -0.075506073        0.92307692                -0.051923077
#> 68:                -0.069068826        0.02429150                 0.014291498
#> 69:                -0.069068826        0.93522267                -0.054777328
#> 70:                 0.015625000        0.50000000                 0.000000000
#> 71:                 0.001562500        0.42187500                -0.028125000
#> 72:                 0.001562500        0.52343750                -0.026562500
#> 73:                -0.004687500        0.37500000                -0.025000000
#> 74:                -0.004687500        0.57031250                -0.029687500
#> 75:                -0.018750000        0.35156250                 0.001562500
#> 76:                -0.018750000        0.61718750                -0.032812500
#> 77:                -0.017187500        0.28906250                -0.010937500
#> 78:                -0.017187500        0.66406250                -0.035937500
#> 79:                -0.039062500        0.27343750                 0.023437500
#> 80:                -0.039062500        0.71875000                -0.031250000
#> 81:                -0.060937500        0.24218750                 0.042187500
#> 82:                -0.060937500        0.78125000                -0.018750000
#> 83:                -0.082812500        0.20312500                 0.053125000
#> 84:                -0.082812500        0.82031250                -0.029687500
#> 85:                -0.034375000        0.12500000                 0.025000000
#> 86:                -0.034375000        0.87500000                -0.025000000
#> 87:                -0.025000000        0.06250000                 0.012500000
#> 88:                -0.025000000        0.93750000                -0.012500000
#> 89:                 0.003125000        0.01562500                -0.009375000
#> 90:                 0.003125000        0.96875000                -0.006250000
#> 91:                 0.004375000        0.00781250                -0.002187500
#> 92:                 0.004375000        0.99218750                 0.002187500
#>     interval_coverage_deviation quantile_coverage quantile_coverage_deviation
```
