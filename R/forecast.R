#' @title Create a `forecast` object
#'
#' @description
#' Process and validate a data.frame (or similar) or similar with forecasts
#' and observations. If the input passes all input checks, it will be converted
#' to a `forecast` object. A forecast object is a `data.table` with a
#' class `forecast` and an additional class that depends on the forecast type.
#' Every forecast type has its own `as_forecast_<type>()` function.
#' See the details section below for more information
#' on the expected input formats.
#'
#' The `as_forecast_<type>()` functions give users some control over how their
#' data is parsed.
#' Using the arguments `observed`, `predicted`, `model`, etc. users can rename
#' existing columns of their input data to match the required columns for a
#' forecast object. Using the argument `forecast_unit`, users can specify the
#' the columns that uniquely identify a single forecast (and remove the others,
#' see [set_forecast_unit()] for details).
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
#' @param model (optional) Name of the column in `data` that contains the names
#'   of the models/forecasters that generated the predicted values.
#'   This column will be renamed to "model".
#' @inheritSection forecast_types Forecast types and input formats
#' @inheritSection forecast_types Forecast unit
#' @return
#' Depending on the forecast type, an object of the following class will be
#' returned:
#' - `forecast_binary` for binary forecasts
#' - `forecast_point` for point forecasts
#' - `forecast_sample` for sample-based forecasts
#' - `forecast_quantile` for quantile-based forecasts
#' @keywords check-forecasts
#' @seealso [as_forecast_point()], [as_forecast_binary()],
#'   [as_forecast_sample()], [as_forecast_quantile()]
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
#' @keywords check-forecasts
as_forecast_generic <- function(data,
                                forecast_unit = NULL,
                                observed = NULL,
                                predicted = NULL,
                                model = NULL) {
  # check inputs - general
  data <- ensure_data.table(data)
  assert_character(observed, len = 1, null.ok = TRUE)
  assert_subset(observed, names(data), empty.ok = TRUE)

  assert_character(predicted, len = 1, null.ok = TRUE)
  assert_subset(predicted, names(data), empty.ok = TRUE)

  assert_character(model, len = 1, null.ok = TRUE)
  assert_subset(model, names(data), empty.ok = TRUE)

  # rename columns - general
  if (!is.null(observed)) {
    setnames(data, old = observed, new = "observed")
  }
  if (!is.null(predicted)) {
    setnames(data, old = predicted, new = "predicted")
  }
  if (!is.null(model)) {
    setnames(data, old = model, new = "model")
  }

  # ensure that a model column is present after renaming
  ensure_model_column(data)

  # set forecast unit (error handling is done in `set_forecast_unit()`)
  if (!is.null(forecast_unit)) {
    data <- set_forecast_unit(data, forecast_unit)
  }
  return(data)
}


#' @title Create a `forecast` object for binary forecasts
#' @description
#' Create a `forecast` object for binary forecasts. See more information on
#' forecast types and expected input formats by calling `?`[as_forecast()].
#' @export
#' @inheritParams as_forecast
#' @seealso [as_forecast()], [as_forecast_point()], [as_forecast_binary()],
#'   [as_forecast_sample()], [as_forecast_quantile()]
#' @importFrom cli cli_warn
#' @keywords check-forecasts
as_forecast_binary <- function(data,
                               forecast_unit = NULL,
                               observed = NULL,
                               predicted = NULL,
                               model = NULL) {
  data <- as_forecast_generic(data, forecast_unit, observed, predicted, model)
  data <- new_forecast(data, "forecast_binary")
  assert_forecast(data)
  return(data)
}


#' @title Create a `forecast` object for point forecasts
#' @description
#' Create a `forecast` object for point forecasts. See more information on
#' forecast types and expected input formats by calling `?`[as_forecast()].
#' @inherit as_forecast params
#' @param ... Unused
#' @seealso [as_forecast()], [as_forecast_point()], [as_forecast_binary()],
#'   [as_forecast_sample()], [as_forecast_quantile()]
#' @export
#' @keywords check-forecasts
as_forecast_point <- function(data, ...) {
  UseMethod("as_forecast_point")
}


#' @rdname as_forecast_point
#' @export
#' @importFrom cli cli_warn
as_forecast_point.default <- function(data,
                                      forecast_unit = NULL,
                                      observed = NULL,
                                      predicted = NULL,
                                      model = NULL,
                                      ...) {
  data <- as_forecast_generic(data, forecast_unit, observed, predicted, model)
  data <- new_forecast(data, "forecast_point")
  assert_forecast(data)
  return(data)
}


#' @rdname as_forecast_point
#' @description
#' When converting a `forecast_quantile` object into a `forecast_point` object,
#' the 0.5 quantile is extracted and returned as the point forecast.
#' @export
#' @keywords check-forecasts
as_forecast_point.forecast_quantile <- function(data, ...) {
  assert_forecast(data, verbose = FALSE)
  assert_subset(0.5, unique(data$quantile_level))

  forecast <- data[quantile_level == 0.5]
  forecast[, "quantile_level" := NULL]

  point_forecast <- new_forecast(forecast, "forecast_point")
  return(point_forecast)
}


#' @title Create a `forecast` object for quantile-based forecasts
#' @description
#' Create a `forecast` object for quantile-based forecasts. See more information
#' on forecast types and expected input formats by calling `?`[as_forecast()].
#' @param ... Unused
#' @seealso [as_forecast()], [as_forecast_point()], [as_forecast_binary()],
#'   [as_forecast_sample()], [as_forecast_quantile()]
#' @inheritParams as_forecast
#' @export
#' @keywords check-forecasts
as_forecast_quantile <- function(data, ...) {
  UseMethod("as_forecast_quantile")
}


#' @rdname as_forecast_quantile
#' @param quantile_level (optional) Name of the column in `data` that contains
#'   the quantile level of the predicted values. This column will be renamed to
#'   "quantile_level". Only applicable to quantile-based forecasts.
#' @export
#' @importFrom cli cli_warn
as_forecast_quantile.default <- function(data,
                                         forecast_unit = NULL,
                                         observed = NULL,
                                         predicted = NULL,
                                         model = NULL,
                                         quantile_level = NULL,
                                         ...) {
  assert_character(quantile_level, len = 1, null.ok = TRUE)
  assert_subset(quantile_level, names(data), empty.ok = TRUE)
  if (!is.null(quantile_level)) {
    setnames(data, old = quantile_level, new = "quantile_level")
  }

  data <- as_forecast_generic(data, forecast_unit, observed, predicted, model)
  data <- new_forecast(data, "forecast_quantile")
  assert_forecast(data)
  return(data)
}


#' @rdname as_forecast_quantile
#' @description
#' When creating a `forecast_quantile` object from a `forecast_sample` object,
#' the quantiles are estimated by computing empircal quantiles from the samples
#' via [quantile()]. Note that empirical quantiles are a biased estimator for
#' the true quantiles in particular in the tails of the distribution and
#' when the number of available samples is low.
#' @param probs A numeric vector of quantile levels for which
#'   quantiles will be computed. Corresponds to the `probs` argument in
#'   [quantile()].
#' @param type Type argument passed down to the quantile function. For more
#'   information, see [quantile()].
#' @importFrom stats quantile
#' @importFrom methods hasArg
#' @importFrom checkmate assert_numeric
#' @export
as_forecast_quantile.forecast_sample <- function(
  data,
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
  type = 7,
  ...
) {
  forecast <- copy(data)
  assert_forecast(forecast, verbose = FALSE)
  assert_numeric(probs, min.len = 1)
  reserved_columns <- c("predicted", "sample_id")
  by <- setdiff(colnames(forecast), reserved_columns)

  quantile_level <- unique(
    round(c(probs, 1 - probs), digits = 10)
  )

  forecast <-
    forecast[, .(quantile_level = quantile_level,
                 predicted = quantile(x = predicted, probs = ..probs,
                                      type = ..type, na.rm = TRUE)),
             by = by]

  quantile_forecast <- new_forecast(forecast, "forecast_quantile")
  assert_forecast(quantile_forecast)

  return(quantile_forecast)
}


#' @title Create a `forecast` object for sample-based forecasts
#' @param sample_id (optional) Name of the column in `data` that contains the
#'   sample id. This column will be renamed to "sample_id". Only applicable to
#'   sample-based forecasts.
#' @inheritParams as_forecast
#' @export
#' @seealso [as_forecast()], [as_forecast_point()], [as_forecast_binary()],
#'   [as_forecast_sample()], [as_forecast_quantile()]
#' @importFrom cli cli_warn
#' @keywords check-forecasts
as_forecast_sample <- function(data,
                               forecast_unit = NULL,
                               observed = NULL,
                               predicted = NULL,
                               model = NULL,
                               sample_id = NULL) {
  assert_character(sample_id, len = 1, null.ok = TRUE)
  assert_subset(sample_id, names(data), empty.ok = TRUE)
  if (!is.null(sample_id)) {
    setnames(data, old = sample_id, new = "sample_id")
  }

  data <- as_forecast_generic(data, forecast_unit, observed, predicted, model)
  data <- new_forecast(data, "forecast_sample")
  assert_forecast(data)
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
#' @keywords check-forecasts
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
#' @keywords check-forecasts
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


#' @export
#' @rdname assert_forecast
#' @importFrom cli cli_abort
#' @keywords check-forecasts
assert_forecast.forecast_binary <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  forecast <- assert_forecast_generic(forecast, verbose)
  assert_forecast_type(forecast, actual = "binary", desired = forecast_type)

  columns_correct <- test_columns_not_present(
    forecast, c("sample_id", "quantile_level")
  )
  if (!columns_correct) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "Checking `forecast`: Input looks like a binary forecast, but an
         additional column called `sample_id` or `quantile` was found.",
        "i" = "Please remove the column."
      )
    )
  }
  input_check <- check_input_binary(forecast$observed, forecast$predicted)
  if (!isTRUE(input_check)) {
    cli_abort(
      c(
        "!" = "Checking `forecast`: Input looks like a binary forecast, but
             found the following issue: {input_check}"
      )
    )
    #nolint end
  }
  return(invisible(NULL))
}


#' @export
#' @rdname assert_forecast
#' @importFrom cli cli_abort
#' @keywords check-forecasts
assert_forecast.forecast_point <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  forecast <- assert_forecast_generic(forecast, verbose)
  assert_forecast_type(forecast, actual = "point", desired = forecast_type)
  #nolint start: keyword_quote_linter object_usage_linter
  input_check <- check_input_point(forecast$observed, forecast$predicted)
  if (!isTRUE(input_check)) {
    cli_abort(
      c(
        "!" = "Checking `forecast`: Input looks like a point forecast, but found
        the following issue: {input_check}"
      )
    )
    #nolint end
  }
  return(invisible(NULL))
}


#' @export
#' @rdname assert_forecast
#' @keywords check-forecasts
assert_forecast.forecast_quantile <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  forecast <- assert_forecast_generic(forecast, verbose)
  assert_forecast_type(forecast, actual = "quantile", desired = forecast_type)
  assert_numeric(forecast$quantile_level, lower = 0, upper = 1)
  return(invisible(NULL))
}


#' @export
#' @rdname assert_forecast
#' @keywords check-forecasts
assert_forecast.forecast_sample <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  forecast <- assert_forecast_generic(forecast, verbose)
  assert_forecast_type(forecast, actual = "sample", desired = forecast_type)
  return(invisible(NULL))
}


#' @title Re-validate an existing forecast object
#'
#' @description
#' The function re-validates an existing forecast object. It is similar to
#' [assert_forecast()], but returns the input data instead of an invisible
#' `NULL`. See [as_forecast()] for details on the expected input formats.
#' @inherit assert_forecast params return examples
#' @export
#' @keywords check-forecasts
validate_forecast <- function(forecast, forecast_type = NULL, verbose = TRUE) {
  assert_forecast(forecast, forecast_type, verbose)
  return(forecast)
}


#' @title Validation common to all forecast types
#'
#' @description
#' The function runs input checks that apply to all input data, regardless of
#' forecast type. The function
#' - asserts that the forecast is a data.table which has columns `observed` and
#' `predicted`, as well as a column called `model`.
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
  assert(check_columns_present(data, c("observed", "predicted", "model")))
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
#' - makes sure that a column called `model` exists and if not creates one
#' - assigns a class
#'
#' @inheritParams as_forecast
#' @param classname name of the class to be created
#' @return An object of the class indicated by `classname`
#' @export
#' @keywords internal
new_forecast <- function(data, classname) {
  data <- as.data.table(data)
  data <- ensure_model_column(data)
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
#' @keywords check-forecasts
#' @examples
#' forecast_binary <- as_forecast_binary(example_binary)
#' is_forecast(forecast_binary)
is_forecast <- function(x) {
  inherits(x, "forecast")
}

#' @export
#' @rdname is_forecast
#' @keywords check-forecasts
is_forecast_sample <- function(x) {
  inherits(x, "forecast_sample") && inherits(x, "forecast")
}

#' @export
#' @rdname is_forecast
#' @keywords check-forecasts
is_forecast_binary <- function(x) {
  inherits(x, "forecast_binary") && inherits(x, "forecast")
}

#' @export
#' @rdname is_forecast
#' @keywords check-forecasts
is_forecast_point <- function(x) {
  inherits(x, "forecast_point") && inherits(x, "forecast")
}

#' @export
#' @rdname is_forecast
#' @keywords check-forecasts
is_forecast_quantile <- function(x) {
  inherits(x, "forecast_quantile") && inherits(x, "forecast")
}
