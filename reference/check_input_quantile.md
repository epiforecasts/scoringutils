# Check that inputs are correct for quantile-based forecast

Function assesses whether the inputs correspond to the requirements for
scoring quantile-based forecasts.

## Usage

``` r
check_input_quantile(observed, predicted, quantile_level)
```

## Arguments

- observed:

  Input to be checked. Should be a numeric vector with the observed
  values of size n.

- predicted:

  Input to be checked. Should be nxN matrix of predictive quantiles, n
  (number of rows) being the number of data points and N (number of
  columns) the number of quantiles per forecast. If `observed` is just a
  single number, then predicted can just be a vector of size N.

- quantile_level:

  Input to be checked. Should be a vector of size N that denotes the
  quantile levels corresponding to the columns of the prediction matrix.

## Value

Returns TRUE if the check was successful and a string with an error
message otherwise.
