# Check that there are no duplicate forecasts

Runs
[`get_duplicate_forecasts()`](https://epiforecasts.io/scoringutils/dev/reference/get_duplicate_forecasts.md)
and returns a message if an issue is encountered

## Usage

``` r
check_duplicates(data)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

## Value

Returns TRUE if the check was successful and a string with an error
message otherwise.
