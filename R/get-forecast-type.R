#' Get forecast type from forecast object
#' @inheritParams score
#' @return
#' Character vector of length one with the forecast type.
#' @keywords internal_input_check
get_forecast_type <- function(forecast) {
  classname <- class(forecast)
  forecast_class <- classname[grepl("forecast_", classname, fixed = TRUE)]
  if (length(forecast_class) == 1) {
    return(gsub("forecast_", "", forecast_class, fixed = TRUE))
  }
  cli_abort(
    "Input is not a valid forecast object
    (There should be a single class beginning with `forecast_`)."
  )
}


#' Assert that forecast type is as expected
#' @param data A forecast object.
#' @param actual The actual forecast type of the data
#' @param desired The desired forecast type of the data
#' @inherit document_assert_functions return
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_character
#' @keywords internal_input_check
assert_forecast_type <- function(data,
                                 actual = get_forecast_type(data),
                                 desired = NULL) {
  assert_character(desired, null.ok = TRUE)
  if (!is.null(desired) && desired != actual) {
    #nolint start: object_usage_linter keyword_quote_linter
    cli_abort(
      c(
        "!" = "Forecast type determined by scoringutils based on input:
        {.val {actual}}.",
        "i" = "Desired forecast type: {.val {desired}}."
      )
    )
    #nolint end
  }
  return(invisible(NULL))
}


#' @title Get type of a vector or matrix of observed values or predictions
#'
#' @description
#' Internal helper function to get the type of a vector (usually
#' of observed or predicted values). The function checks whether the input is
#' a factor, or else whether it is integer (or can be coerced to integer) or
#' whether it's continuous.
#' @param x Input the type should be determined for.
#' @importFrom cli cli_abort
#' @return
#' Character vector of length one with either "classification",
#' "integer", or "continuous".
#' @keywords internal_input_check
get_type <- function(x) {
  if (is.factor(x)) {
    return("classification")
  }
  assert_numeric(as.vector(x))
  if (all(is.na(as.vector(x)))) {
    cli_abort("Can't get type: all values of are {.val NA}.")
  }
  if (is.integer(x)) {
    return("integer")
  }
  if (
    isTRUE(all.equal(as.vector(x), as.integer(x))) && !all(is.na(as.integer(x)))
  ) {
    return("integer")
  } else {
    return("continuous")
  }
}
