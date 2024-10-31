#' Common functionality for `as_forecast_<type>` functions
#' @details This function splits out part of the functionality of
#' `as_forecast_<type>` that is the same for all `as_forecast_<type>` functions.
#' It renames the required columns, where appropriate, and sets the forecast
#' unit.
#' @inheritParams as_forecast_doc_template
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
#' `forecast` and an additional class `forecast_<type>` corresponding to the
#' forecast type).
#'
#' See the corresponding `assert_forecast_<type>` functions for more details on
#' the required input formats.
#'
#' @inheritParams as_forecast_doc_template
#' @inheritParams score
#' @param forecast_type (optional) The forecast type you expect the forecasts
#'   to have. If the forecast type as determined by `scoringutils` based on the
#'   input does not match this, an error will be thrown. If `NULL` (the
#'   default), the forecast type will be inferred from the data.
#' @param verbose Logical. If `FALSE` (default is `TRUE`), no messages and
#'   warnings will be created.
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
    #nolint start: keyword_quote_linter
    c(
      "!" = "The input needs to be a valid forecast object.",
      "i" = "Please convert to `forecast` object first by calling the
      appropriate {.fn as_forecast_<type>} function)."
    )
    #nolint end
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
#' @returns returns the input
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


#' Check that all forecasts have the same number of rows
#' @description
#' Helper function that checks the number of rows (corresponding e.g to
#' quantiles or samples) per forecast.
#' If the number of quantiles or samples is the same for all forecasts, it
#' returns TRUE and a string with an error message otherwise.
#' @param forecast_unit Character vector denoting the unit of a single forecast.
#' @importFrom checkmate assert_subset
#' @inherit document_check_functions params return
#' @keywords internal_input_check
check_number_per_forecast <- function(data, forecast_unit) {
  # This function doesn't return a forecast object so it's fine to unclass it
  # to avoid validation error while subsetting
  data <- as.data.table(data)
  data <- na.omit(data)
  # check whether there are the same number of quantiles, samples --------------
  data[, scoringutils_InternalNumCheck := length(predicted), by = forecast_unit]
  n <- unique(data$scoringutils_InternalNumCheck)
  data[, scoringutils_InternalNumCheck := NULL]
  if (length(n) > 1) {
    msg <- paste0(
      "Some forecasts have different numbers of rows ",
      "(e.g. quantiles or samples). ",
      "scoringutils found: ", toString(n),
      ". This may be a problem (it can potentially distort scores, ",
      "making it more difficult to compare them), ",
      "so make sure this is intended."
    )
    return(msg)
  }
  return(TRUE)
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
#' @inheritParams as_forecast_doc_template
#' @param classname name of the class to be created
#' @returns An object of the class indicated by `classname`
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
#' Test whether an object is a forecast object.
#'
#' You can test for a specific `forecast_<type>` class using the appropriate
#' `is_forecast_<type>` function.
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

  # in addition, we also check for a maximum length. The reason is that
  # print.data.table will internally subset the data.table before printing.
  # this subsetting triggers the validation, which is not desired in this case.
  # this is a hack and ideally, we'd do things differently.
  if (nrow(out) > 30 && data.table::is.data.table(out) && !is_dt_force_print) {
    # check whether subset object passes validation
    validation <- try(
      assert_forecast(forecast = out, verbose = FALSE),
      silent = TRUE
    )
    if (inherits(validation, "try-error")) {
      cli_warn(
        #nolint start: keyword_quote_linter
        c(
          "!" = "Error in validating forecast object: {validation}.",
          "i" = "Note this error is sometimes related to `data.table`s `print`.
          Run {.help [{.fun assert_forecast}](scoringutils::assert_forecast)}
          to confirm."
        )
        #nolint end
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


#' @title Print information about a forecast object
#' @description
#' This function prints information about a forecast object,
#' including "Forecast type", "Score columns",
#' "Forecast unit".
#'
#' @param x A forecast object
#' @param ... Additional arguments for [print()].
#' @returns Returns `x` invisibly.
#' @importFrom cli cli_inform cli_warn col_blue cli_text
#' @export
#' @keywords gain-insights
#' @examples
#' dat <- as_forecast_quantile(example_quantile)
#' print(dat)
print.forecast <- function(x, ...) {

  # get forecast type, forecast unit and score columns
  forecast_type <- try(
    do.call(get_forecast_type, list(forecast = x)),
    silent = TRUE
  )
  forecast_unit <- try(
    do.call(get_forecast_unit, list(data = x)),
    silent = TRUE
  )

  # Print forecast object information
  if (inherits(forecast_type, "try-error")) {
    cli_inform(
      c(
        "!" = "Could not determine forecast type due to error in validation." #nolint
      )
    )
  } else {
    cli_text(
      col_blue(
        "Forecast type: "
      ),
      "{forecast_type}"
    )
  }

  if (inherits(forecast_unit, "try-error")) {
    cli_inform(
      c(
        "!" = "Could not determine forecast unit." #nolint
      )
    )
  } else {
    cli_text(
      col_blue(
        "Forecast unit:"
      )
    )
    cli_text(
      "{forecast_unit}"
    )
  }

  cat("\n")

  NextMethod()

  return(invisible(x))
}
