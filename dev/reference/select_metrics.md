# Select metrics from a list of functions

Helper function to return only the scoring rules selected by the user
from a list of possible functions.

## Usage

``` r
select_metrics(metrics, select = NULL, exclude = NULL)
```

## Arguments

- metrics:

  A list of scoring functions.

- select:

  A character vector of scoring rules to select from the list. If
  `select` is `NULL` (the default), all possible scoring rules are
  returned.

- exclude:

  A character vector of scoring rules to exclude from the list. If
  `select` is not `NULL`, this argument is ignored.

## Value

A list of scoring functions.

## Examples

``` r
select_metrics(
  metrics = get_metrics(example_binary),
  select = "brier_score"
)
#> $brier_score
#> function (observed, predicted) 
#> {
#>     assert_input_binary(observed, predicted)
#>     observed <- as.numeric(observed) - 1
#>     brierscore <- (observed - predicted)^2
#>     return(brierscore)
#> }
#> <bytecode: 0x55ae44350670>
#> <environment: namespace:scoringutils>
#> 
select_metrics(
  metrics = get_metrics(example_binary),
  exclude = "log_score"
)
#> $brier_score
#> function (observed, predicted) 
#> {
#>     assert_input_binary(observed, predicted)
#>     observed <- as.numeric(observed) - 1
#>     brierscore <- (observed - predicted)^2
#>     return(brierscore)
#> }
#> <bytecode: 0x55ae44350670>
#> <environment: namespace:scoringutils>
#> 
```
