# Check column names are present in a data.frame

The functions loops over the column names and checks whether they are
present. If an issue is encountered, the function immediately stops and
returns a message with the first issue encountered.

## Usage

``` r
check_columns_present(data, columns)
```

## Arguments

- data:

  A data.frame or similar to be checked

- columns:

  A character vector of column names to check

## Value

Returns TRUE if the check was successful and a string with an error
message otherwise.
