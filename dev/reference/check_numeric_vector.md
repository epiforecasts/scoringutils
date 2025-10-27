# Check whether an input is an atomic vector of mode 'numeric'

Helper function to check whether an input is a numeric vector.

## Usage

``` r
check_numeric_vector(x, ...)
```

## Arguments

- x:

  input to check

- ...:

  Arguments passed on to
  [`checkmate::check_numeric`](https://mllg.github.io/checkmate/reference/checkNumeric.html)

  `lower`

  :   \[`numeric(1)`\]  
      Lower value all elements of `x` must be greater than or equal to.

  `upper`

  :   \[`numeric(1)`\]  
      Upper value all elements of `x` must be lower than or equal to.

  `finite`

  :   \[`logical(1)`\]  
      Check for only finite values? Default is `FALSE`.

  `any.missing`

  :   \[`logical(1)`\]  
      Are vectors with missing values allowed? Default is `TRUE`.

  `all.missing`

  :   \[`logical(1)`\]  
      Are vectors with no non-missing values allowed? Default is `TRUE`.
      Note that empty vectors do not have non-missing values.

  `len`

  :   \[`integer(1)`\]  
      Exact expected length of `x`.

  `min.len`

  :   \[`integer(1)`\]  
      Minimal length of `x`.

  `max.len`

  :   \[`integer(1)`\]  
      Maximal length of `x`.

  `unique`

  :   \[`logical(1)`\]  
      Must all values be unique? Default is `FALSE`.

  `sorted`

  :   \[`logical(1)`\]  
      Elements must be sorted in ascending order. Missing values are
      ignored.

  `names`

  :   \[`character(1)`\]  
      Check for names. See
      [`checkNamed`](https://mllg.github.io/checkmate/reference/checkNamed.html)
      for possible values. Default is “any” which performs no check at
      all. Note that you can use
      [`checkSubset`](https://mllg.github.io/checkmate/reference/checkSubset.html)
      to check for a specific set of names.

  `typed.missing`

  :   \[`logical(1)`\]  
      If set to `FALSE` (default), all types of missing values (`NA`,
      `NA_integer_`, `NA_real_`, `NA_character_` or `NA_character_`) as
      well as empty vectors are allowed while type-checking atomic
      input. Set to `TRUE` to enable strict type checking.

  `null.ok`

  :   \[`logical(1)`\]  
      If set to `TRUE`, `x` may also be `NULL`. In this case only a type
      check of `x` is performed, all additional checks are disabled.

## Value

Returns TRUE if the check was successful and a string with an error
message otherwise.
