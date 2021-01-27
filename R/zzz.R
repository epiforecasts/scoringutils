.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Note: The definition of the weighted interval score has slightly changed in version 0.1.5. If you want to use the old definition, use the argument `count_median_twice = TRUE` in the function `eval_forecasts()`")
}
