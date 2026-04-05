# Compute bias for a single vector of quantile predictions

Internal function to compute bias for a single observed value, a vector
of predicted values and a vector of quantiles.

## Usage

``` r
bias_quantile_single_vector(observed, predicted, quantile_level, na.rm)
```

## Arguments

- observed:

  Scalar with the observed value.

- predicted:

  Vector of length N (corresponding to the number of quantiles) that
  holds predictions.

- quantile_level:

  Vector of of size N with the quantile levels for which predictions
  were made. Note that if this does not contain the median (0.5) then
  the median is imputed as being the mean of the two innermost
  quantiles.

- na.rm:

  Logical. Should missing values be removed?

## Value

scalar with the quantile bias for a single quantile prediction
