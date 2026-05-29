# Print a scores object

Prints a `scores` object. Suppresses autoprinting during data.table `:=`
operations by checking data.table's internal `shouldPrint()` flag.

## Usage

``` r
# S3 method for class 'scores'
print(x, ...)
```

## Arguments

- x:

  A `scores` object

- ...:

  Additional arguments for
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

Returns `x` invisibly.
