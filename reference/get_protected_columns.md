# Get protected columns from data

Helper function to get the names of all columns in a data frame that are
protected columns.

## Usage

``` r
get_protected_columns(data = NULL)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

## Value

A character vector with the names of protected columns in the data. If
data is `NULL` (default) then it returns a list of all columns that are
protected in scoringutils.
