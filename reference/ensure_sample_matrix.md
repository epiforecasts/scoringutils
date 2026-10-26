# Ensure that predicted samples are a matrix

Converts a vector of predictive samples (allowed as input when there is
only a single observation) into a 1xN matrix so that downstream row-wise
operations work as expected. Matrix input is returned unchanged.

## Usage

``` r
ensure_sample_matrix(predicted)
```

## Arguments

- predicted:

  A vector of size N (predictive samples for a single observation) or an
  nxN matrix of predictive samples.

## Value

An nxN matrix of predictive samples.
