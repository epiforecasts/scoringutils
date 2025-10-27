# Helper function to convert assert statements into checks

Tries to execute an expression. Internally, this is used to see whether
assertions fail when checking inputs (i.e. to convert an `assert_*()`
statement into a check). If the expression fails, the error message is
returned. If the expression succeeds, `TRUE` is returned.

## Usage

``` r
check_try(expr)
```

## Arguments

- expr:

  an expression to be evaluated

## Value

Returns TRUE if the check was successful and a string with an error
message otherwise.
