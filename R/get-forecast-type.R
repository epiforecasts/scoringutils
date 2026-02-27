#' Get forecast type from forecast object
#' @inheritParams score
#' @return
#' Character vector of length one with the forecast type.
#' @keywords internal_input_check
get_forecast_type <- function(forecast) {
  classname <- class(forecast)
  forecast_class <- classname[grepl("forecast_", classname, fixed = TRUE)]
  if (length(forecast_class) >= 1) {
    forecast_type <- gsub(
      "forecast_", "", forecast_class[1], fixed = TRUE
    )
    if (!nzchar(forecast_type)) {
      cli_abort(
        "Input is not a valid forecast object
        (Column `forecast_` prefix found but no type suffix)."
      )
    }
    return(forecast_type)
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


#' @title Get type of a vector or matrix
#'
#' @description
#' Determine the type of a vector or matrix of values. The function checks
#' whether the input is a factor (returns "classification"), or else whether it
#' is integer (or can be coerced to integer without loss, returns "integer") or
#' whether it's continuous (returns "continuous").
#' @param x Input the type should be determined for.
#' @importFrom cli cli_abort
#' @return
#' Character vector of length one with either "classification",
#' "integer", or "continuous".
#' @export
#' @examples
#' get_vector_type(1:3)
#' get_vector_type(c(1.5, 2.3))
#' get_vector_type(factor(c("a", "b")))
get_vector_type <- function(x) {
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


#' @title Get type of the observed values in a forecast object
#'
#' @description
#' Extract the `observed` column from a forecast object and determine its type
#' using [get_vector_type()].
#' @inheritParams score
#' @return
#' Character vector of length one with either "classification",
#' "integer", or "continuous".
#' @export
#' @examples
#' get_observed_type(example_sample_continuous)
#' get_observed_type(example_binary)
get_observed_type <- function(forecast) {
  assert_forecast(forecast)
  get_vector_type(forecast$observed)
}


#' @title Get type of the predicted values in a forecast object
#'
#' @description
#' Extract the `predicted` column from a forecast object and determine its type
#' using [get_vector_type()].
#' @inheritParams score
#' @return
#' Character vector of length one with either "classification",
#' "integer", or "continuous".
#' @export
#' @examples
#' get_predicted_type(example_sample_continuous)
#' get_predicted_type(example_quantile)
get_predicted_type <- function(forecast) {
  assert_forecast(forecast)
  get_vector_type(forecast$predicted)
}
