# Test whether column names are NOT present in a data.frame

The function checks whether all column names are NOT present. If none of
the columns are present, the function returns TRUE. If one or more
columns are present, the function returns FALSE.

## Usage

``` r
test_columns_not_present(data, columns)
```

## Arguments

- data:

  A data.frame or similar to be checked

- columns:

  A character vector of column names to check

## Value

Returns TRUE if none of the columns are present and FALSE otherwise
