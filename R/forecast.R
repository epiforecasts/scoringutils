#' @title General information on creating a `forecast` object
#'
#' @description
#' There are several ``as_forecast_<type>()` functions to process and validate
#' a data.frame (or similar) or similar with forecasts and observations. If
#' the input passes all input checks, those functions will be converted
#' to a `forecast` object. A forecast object is a `data.table` with a
#' class `forecast` and an additional class that depends on the forecast type.
#' Every forecast type has its own `as_forecast_<type>()` function.
#' See the details section below for more information
#' on the expected input formats.
#'
#' The `as_forecast_<type>()` functions give users some control over how their
#' data is parsed.
#' Using the arguments `observed`, `predicted`, etc. users can rename
#' existing columns of their input data to match the required columns for a
#' forecast object. Using the argument `forecast_unit`, users can specify the
#' the columns that uniquely identify a single forecast (and remove the others,
#' see docs for the internal [set_forecast_unit()] for details).
#'
#' The following functions are available:
#' - [as_forecast_point()]
#' - [as_forecast_binary()]
#' - [as_forecast_sample()]
#' - [as_forecast_quantile()]
#'
#' @param data A data.frame (or similar) with predicted and observed values.
#'   See the details section of [as_forecast()] for additional information
#'   on required input formats.
#' @param forecast_unit (optional) Name of the columns in `data` (after
#'   any renaming of columns) that denote the unit of a
#'   single forecast. See [get_forecast_unit()] for details.
#'   If `NULL` (the default), all columns that are not required columns are
#'   assumed to form the unit of a single forecast. If specified, all columns
#'   that are not part of the forecast unit (or required columns) will be removed.
#' @param observed (optional) Name of the column in `data` that contains the
#'   observed values. This column will be renamed to "observed".
#' @param predicted (optional) Name of the column in `data` that contains the
#'   predicted values. This column will be renamed to "predicted".
#' @inheritSection forecast_types Forecast types and input formats
#' @inheritSection forecast_types Forecast unit
#' @return
#' Depending on the forecast type, an object of the following class will be
#' returned:
#' - `forecast_binary` for binary forecasts
#' - `forecast_point` for point forecasts
#' - `forecast_sample` for sample-based forecasts
#' - `forecast_quantile` for quantile-based forecasts
#' @keywords as_forecast
#' @family functions to create forecast objects
#' @examples
#' as_forecast_binary(example_binary)
#' as_forecast_quantile(
#'   example_quantile,
#'   forecast_unit = c("model", "target_type", "target_end_date",
#'                     "horizon", "location")
#' )
#' @name as_forecast
NULL


#' Common functionality for `as_forecast_<type>` functions
#' @details This function splits out part of the functionality of
#' `as_forecast_<type>` that is the same for all `as_forecast_<type>` functions.
#' It renames the required columns, where appropriate, and sets the forecast
#' unit.
#' @inheritParams as_forecast
#' @keywords as_forecast
as_forecast_generic <- function(data,
                                forecast_unit = NULL,
                                observed = NULL,
                                predicted = NULL) {
  # check inputs - general
  data <- ensure_data.table(data)
  assert_character(observed, len = 1, null.ok = TRUE)
  assert_subset(observed, names(data), empty.ok = TRUE)

  assert_character(predicted, len = 1, null.ok = TRUE)
  assert_subset(predicted, names(data), empty.ok = TRUE)

  # rename columns - general
  if (!is.null(observed)) {
    setnames(data, old = observed, new = "observed")
  }
  if (!is.null(predicted)) {
    setnames(data, old = predicted, new = "predicted")
  }

  # set forecast unit (error handling is done in `set_forecast_unit()`)
  if (!is.null(forecast_unit)) {
    data <- set_forecast_unit(data, forecast_unit)
  }
  return(data)
}


#' @title Assert that input is a forecast object and passes validations
#'
#' @description
#' Assert that an object is a forecast object (i.e. a `data.table` with a class
#' `forecast` and an additional class `forecast_*` corresponding to the forecast
#' type).
#' @inheritParams as_forecast
#' @inheritParams score
#' @param forecast_type (optional) The forecast type you expect the forecasts
#'   to have. If the forecast type as determined by `scoringutils` based on the
#'   input does not match this, an error will be thrown. If `NULL` (the
#'   default), the forecast type will be inferred from the data.
#' @param verbose Logical. If `FALSE` (default is `TRUE`), no messages and
#'   warnings will be created.
#' @inheritSection forecast_types Forecast types and input formats
#' @return
#' Returns `NULL` invisibly.
#' @importFrom data.table ':=' is.data.table
#' @importFrom checkmate assert_data_frame
#' @export
#' @keywords validate-forecast-object
#' @examples
#' forecast <- as_forecast_binary(example_binary)
#' assert_forecast(forecast)
assert_forecast <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  UseMethod("assert_forecast")
}


#' @importFrom cli cli_abort
#' @rdname assert_forecast
#' @export
#' @keywords validate-forecast-object
assert_forecast.default <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  cli_abort(
    c(
      "!" = "The input needs to be a valid forecast object.",
      "i" = "Please convert to `forecast` object first (see {.fn as_forecast})." # nolint
    )
  )
}


#' @title Validation common to all forecast types
#'
#' @description
#' The function runs input checks that apply to all input data, regardless of
#' forecast type. The function
#' - asserts that the forecast is a data.table which has columns `observed` and
#' `predicted`
#' - checks the forecast type and forecast unit
#' - checks there are no duplicate forecasts
#' - if appropriate, checks the number of samples / quantiles is the same
#' for all forecasts.
#' @param data A data.table with forecasts and observed values that should
#' be validated.
#' @inheritParams assert_forecast
#' @return returns the input
#' @importFrom data.table ':=' is.data.table
#' @importFrom checkmate assert_data_table
#' @importFrom cli cli_abort cli_inform cli_warn
#' @keywords internal_input_check
assert_forecast_generic <- function(data, verbose = TRUE) {
  # check that data is a data.table and that the columns look fine
  assert_data_table(data, min.rows = 1)
  assert(check_columns_present(data, c("observed", "predicted")))
  problem <- test_columns_present(data, c("sample_id", "quantile_level"))
  if (problem) {
    cli_abort(
      c(
        "!" = "Found columns `quantile_level` and `sample_id`.
      Only one of these is allowed"
      )
    )
  }

  # check that there aren't any duplicated forecasts
  forecast_unit <- get_forecast_unit(data)
  assert(check_duplicates(data))

  # check that the number of forecasts per sample / quantile level is the same
  number_quantiles_samples <- check_number_per_forecast(data, forecast_unit)
  if (!isTRUE(number_quantiles_samples) && verbose) {
    cli_warn(number_quantiles_samples)
  }

  # check whether there are any NA values
  if (anyNA(data)) {
    if (nrow(na.omit(data)) == 0) {
      #nolint start: keyword_quote_linter
      cli_abort(
        c(
          "!" = "After removing rows with NA values in the data, no forecasts
          are left."
        )
      )
    }
    if (verbose) {
      cli_inform(
        c(
          "i" = "Some rows containing NA values may be removed.
        This is fine if not unexpected."
        )
      )
    }
    #nolint end
  }

  return(data[])
}


#' Clean forecast object
#' @description
#' The function makes it possible to silently validate an object. In addition,
#' it can return a copy of the data and remove rows with missing values.
#'
#' @inheritParams score
#' @param copy Logical, default is `FALSE`. If `TRUE`, a copy of the input data
#' is created.
#' @param na.omit Logical, default is `FALSE`. If `TRUE`, rows with missing
#' values are removed.
#' @importFrom data.table copy
#' @importFrom stats na.omit
#' @keywords internal
clean_forecast <- function(forecast, copy = FALSE, na.omit = FALSE) {
  if (copy) {
    forecast <- copy(forecast)
  }
  assert_forecast(forecast, verbose = FALSE)
  if (na.omit) {
    forecast <- na.omit(forecast)
  }
  return(forecast)
}


#' @title Class constructor for `forecast` objects
#'
#' @description
#' Construct a class based on a data.frame or similar. The constructor
#' - coerces the data into a data.table
#' - assigns a class
#'
#' @inheritParams as_forecast
#' @param classname name of the class to be created
#' @return An object of the class indicated by `classname`
#' @export
#' @keywords internal
new_forecast <- function(data, classname) {
  data <- as.data.table(data)
  class(data) <- c(classname, "forecast", class(data))
  data <- copy(data)
  return(data[])
}


#' @title Test whether an object is a forecast object
#'
#' @description
#' Test whether an object is a forecast object (see [as_forecast()] for more
#' information).
#'
#' You can test for a specific `forecast_*` class using the appropriate
#' `is_forecast_*` function.
#'
#' @param x An R object.
#' @return
#' *`is_forecast`*: `TRUE` if the object is of class `forecast`,
#' `FALSE` otherwise.
#'
#' *`is_forecast_<type>*`*: `TRUE` if the object is of class `forecast_*` in addition
#' to class `forecast`, `FALSE` otherwise.
#' @export
#' @keywords validate-forecast-object
#' @examples
#' forecast_binary <- as_forecast_binary(example_binary)
#' is_forecast(forecast_binary)
is_forecast <- function(x) {
  inherits(x, "forecast")
}


#' @export
`[.forecast` <- function(x, ...) {

  out <- NextMethod()

  # (identical(x, out) && ...length() == 1) is the best way I have found to
  #   selectively catch x[], which we don't want to revalidate. ...length()
  #   alone will skip cases with dplyr verbs and identical alone will skip cases
  #   where we used data.table := operator which will turn x into out before we
  #   arrive to this function.
  is_dt_force_print <- identical(x, out) && ...length() == 1
  #   ...length() as it still returns 1 in x[] and then skips validations in
  #   undesired situation if we set ...length() > 1
  # is.data.table: when [.data.table returns an atomic vector, it's clear it
  #   cannot be a valid forecast object, and it is likely intended by the user
  if (data.table::is.data.table(out) && !is_dt_force_print) {
    # check whether subset object passes validation
    validation <- try(
      assert_forecast(forecast = out, verbose = FALSE),
      silent = TRUE
    )
    if (inherits(validation, "try-error")) {
      cli_warn(
        c(
          "!" = "Error in validating forecast object: {validation}"
        )
      )
    }
  }

  return(out)

}

#' @export
`$<-.forecast` <- function(x, ..., value) {

  out <- NextMethod()

  # check whether subset object passes validation
  validation <- try(
    assert_forecast(forecast = out, verbose = FALSE),
    silent = TRUE
  )
  if (inherits(validation, "try-error")) {
    cli_warn(
      c(
        "!" = "Error in validating forecast object: {validation}"
      )
    )
  }

  return(out)

}

#' @export
`[[<-.forecast` <- function(x, ..., value) {

  out <- NextMethod()

  # check whether subset object passes validation
  validation <- try(
    assert_forecast(forecast = out, verbose = FALSE),
    silent = TRUE
  )
  if (inherits(validation, "try-error")) {
    cli_warn(
      c(
        "!" = "Error in validating forecast object: {validation}"
      )
    )
  }

  return(out)

}

#' @export
`[<-.forecast` <- function(x, ..., value) {

  out <- NextMethod()

  # check whether subset object passes validation
  validation <- try(
    assert_forecast(forecast = out, verbose = FALSE),
    silent = TRUE
  )
  if (inherits(validation, "try-error")) {
    cli_warn(
      c(
        "!" = "Error in validating forecast object: {validation}"
      )
    )
  }

  return(out)

}

#' @export
#' @importFrom utils head
head.forecast <- function(x, ...) {
  # We use this custom method just to unclass before forwarding to avoid
  # validation when we expect (and don't care) that objects are invalidated
  head(as.data.table(x), ...)
}

#' @export
#' @importFrom utils tail
tail.forecast <- function(x, ...) {
  # We use this custom method just to unclass before forwarding to avoid
  # validation when we expect (and don't care) that objects are invalidated
  utils::tail(as.data.table(x), ...)
}
