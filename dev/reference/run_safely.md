# Run a function safely

This is a wrapper/helper function designed to run a function safely when
it is not completely clear what arguments could be passed to the
function.

All named arguments in `...` that are not accepted by `fun` are removed.
All unnamed arguments are passed on to the function. In case `fun`
errors, the error will be converted to a warning and `run_safely`
returns `NULL`.

`run_safely` can be useful when constructing functions to be used as
metrics in
[`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md).

## Usage

``` r
run_safely(..., fun, metric_name)
```

## Arguments

- ...:

  Arguments to pass to `fun`.

- fun:

  A function to execute.

- metric_name:

  A character string with the name of the metric. Used to provide a more
  informative warning message in case `fun` errors.

## Value

The result of `fun` or `NULL` if `fun` errors

## Examples

``` r
f <- function(x) {x}
scoringutils:::run_safely(2, fun = f, metric_name = "f")
#> [1] 2
scoringutils:::run_safely(2, y = 3, fun = f, metric_name = "f")
#> [1] 2
scoringutils:::run_safely(fun = f, metric_name = "f")
#> Warning: ! Computation for `f` failed. Error: argument "x" is missing, with no default.
#> NULL
scoringutils:::run_safely(y = 3, fun = f, metric_name = "f")
#> Warning: ! Computation for `f` failed. Error: argument "x" is missing, with no default.
#> NULL
```
