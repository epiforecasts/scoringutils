# Helper function to interpolate the median prediction if it is not available

Internal function to interpolate the median prediction if it is not
available in the given quantile levels. This is done using linear
interpolation between the two innermost quantiles.

## Usage

``` r
interpolate_median(predicted, quantile_level)
```

## Arguments

- predicted:

  Vector of length N (corresponding to the number of quantiles) that
  holds predictions.

- quantile_level:

  Vector of of size N with the quantile levels for which predictions
  were made. Note that if this does not contain the median (0.5) then
  the median is imputed as being the mean of the two innermost
  quantiles.

## Value

scalar with the imputed median prediction

## Input format

![](figures/metrics-quantile.png)

Overview of required input format for quantile-based forecasts
