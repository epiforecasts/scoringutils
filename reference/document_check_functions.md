# Documentation template for check functions

Documentation template for check functions

## Arguments

- data:

  A data.frame or similar to be checked

- observed:

  Input to be checked. Should be a numeric vector with the observed
  values of size n.

- columns:

  A character vector of column names to check

## Value

Returns TRUE if the check was successful and a string with an error
message otherwise.
