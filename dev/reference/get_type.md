# Get type of a vector or matrix of observed values or predictions

Internal helper function to get the type of a vector (usually of
observed or predicted values). The function checks whether the input is
a factor, or else whether it is integer (or can be coerced to integer)
or whether it's continuous.

## Usage

``` r
get_type(x)
```

## Arguments

- x:

  Input the type should be determined for.

## Value

Character vector of length one with either "classification", "integer",
or "continuous".
