# Test whether all column names are present in a data.frame

The function checks whether all column names are present. If one or more
columns are missing, the function returns FALSE. If all columns are
present, the function returns TRUE.

## Usage

``` r
test_columns_present(data, columns)
```

## Arguments

- data:

  A data.frame or similar to be checked

- columns:

  A character vector of column names to check

## Value

Returns TRUE if all columns are present and FALSE otherwise
