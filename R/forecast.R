#' @title Create a `forecast` object
#'
#' @description
#' Process and validate a data.frame (or similar) or similar with forecasts
#' and observations. If the input passes all input checks, it will be converted
#' to a `forecast` object. The class of that object depends on the forecast
#' type of the input. See the details section below for more information
#' on the expected input formats.
#'
#' `as_forecast()` gives users some control over how their data is parsed.
#' Using the arguments `observed`, `predicted`, and `model`, users can rename
#' existing columns of their input data to match the required columns for a
#' forecast object. Using the argument `forecast_unit`, users can specify the
#' the columns that uniquely identify a single forecast (and remove the others,
#' see [set_forecast_unit()] for details).
#'
#' @param data A data.frame (or similar) with predicted and observed values.
#'   See the details section of [as_forecast()] for additional information
#'   on required input formats.
#' @param ... Additional arguments
#' @inheritSection forecast_types Forecast types and input formats
#' @inheritSection forecast_types Forecast unit
#' @return
#' Depending on the forecast type, an object of the following class will be
#' returned:
#' - `forecast_binary` for binary forecasts
#' - `forecast_point` for point forecasts
#' - `forecast_sample` for sample-based forecasts
#' - `forecast_quantile` for quantile-based forecasts
#' @export
#' @keywords check-forecasts
#' @examples
#' as_forecast(example_binary)
#' as_forecast(
#'   example_quantile,
#'   forecast_unit = c("model", "target_type", "target_end_date",
#'                     "horizon", "location")
#' )
as_forecast <- function(data,
                        ...) {
  UseMethod("as_forecast")
}

#' @rdname as_forecast
#' @param forecast_unit (optional) Name of the columns in `data` (after
#'   any renaming of columns done by `as_forecast()`) that denote the unit of a
#'   single forecast. See [get_forecast_unit()] for details.
#'   If `NULL` (the default), all columns that are not required columns are
#'   assumed to form the unit of a single forecast. If specified, all columns
#'   that are not part of the forecast unit (or required columns) will be removed.
#' @param forecast_type (optional) The forecast type you expect the forecasts
#'   to have. If the forecast type as determined by `scoringutils` based on the
#'   input does not match this, an error will be thrown. If `NULL` (the
#'   default), the forecast type will be inferred from the data.
#' @param observed (optional) Name of the column in `data` that contains the
#'   observed values. This column will be renamed to "observed".
#' @param predicted (optional) Name of the column in `data` that contains the
#'   predicted values. This column will be renamed to "predicted".
#' @param model (optional) Name of the column in `data` that contains the names
#'   of the models/forecasters that generated the predicted values.
#'   This column will be renamed to "model".
#' @param quantile_level (optional) Name of the column in `data` that contains
#'   the quantile level of the predicted values. This column will be renamed to
#'   "quantile_level". Only applicable to quantile-based forecasts.
#' @param sample_id (optional) Name of the column in `data` that contains the
#'   sample id. This column will be renamed to "sample_id". Only applicable to
#'   sample-based forecasts.
#' @export
#' @importFrom cli cli_warn
as_forecast.default <- function(data,
                                forecast_unit = NULL,
                                forecast_type = NULL,
                                observed = NULL,
                                predicted = NULL,
                                model = NULL,
                                quantile_level = NULL,
                                sample_id = NULL,
                                predicted_label = NULL,
                                ...) {
  # check inputs
  data <- ensure_data.table(data)
  assert_character(observed, len = 1, null.ok = TRUE)
  assert_subset(observed, names(data), empty.ok = TRUE)

  assert_character(predicted, len = 1, null.ok = TRUE)
  assert_subset(predicted, names(data), empty.ok = TRUE)

  assert_character(model, len = 1, null.ok = TRUE)
  assert_subset(model, names(data), empty.ok = TRUE)

  assert_character(quantile_level, len = 1, null.ok = TRUE)
  assert_subset(quantile_level, names(data), empty.ok = TRUE)

  assert_character(sample_id, len = 1, null.ok = TRUE)
  assert_subset(sample_id, names(data), empty.ok = TRUE)

  # rename columns
  if (!is.null(observed)) {
    setnames(data, old = observed, new = "observed")
  }
  if (!is.null(predicted)) {
    setnames(data, old = predicted, new = "predicted")
  }
  if (!is.null(model)) {
    setnames(data, old = model, new = "model")
  }
  if (!is.null(quantile_level)) {
    setnames(data, old = quantile_level, new = "quantile_level")
  }
  if (!is.null(sample_id)) {
    setnames(data, old = sample_id, new = "sample_id")
  }
  if (!is.null(predicted_label)) {
    setnames(data, old = predicted_label, new = "predicted_label")
  }

  # ensure that a model column is present after renaming
  ensure_model_column(data)

  # set forecast unit (error handling is done in `set_forecast_unit()`)
  if (!is.null(forecast_unit)) {
    data <- set_forecast_unit(data, forecast_unit)
  }

  # assert forecast type is as expected
  assert_forecast_type(data, desired = forecast_type)
  forecast_type <- get_forecast_type(data)

  # produce warning if old format is suspected
  # old quantile format
  if (forecast_type == "point" && "quantile" %in% colnames(data)) {
    #nolint start: keyword_quote_linter
    cli_warn(
      c(
        "Found column 'quantile' in the input data",
        "i" = "This column name was used before scoringutils version 2.0.0.
        Should the column be called 'quantile_level' instead?"
      ),
      .frequency = "once",
      .frequency_id = "quantile_col_present"
    )
    #nolint end
  }
  #old binary format
  if (forecast_type == "point") {
    looks_binary <- check_input_binary(factor(data$observed), data$predicted)
    if (isTRUE(looks_binary)) {
      #nolint start: keyword_quote_linter duplicate_argument_linter
      cli_warn(
        c(
          "All observations are either 1 or 0.",
          "i" = "The forecast type was classified as 'point', but it looks like
          it could be a binary forecast as well.",
          "i" = "In case you want it to be a binary forecast, convert `observed`
          to a factor. See `?as_forecast()` for more information."
        ),
        .frequency = "once",
        .frequency_id = "looks_binary"
      )
      #nolint end
    }
  }

  # construct class
  data <- new_forecast(data, paste0("forecast_", forecast_type))

  # validate class
  assert_forecast(data)
  return(data)
}


#' @title Assert that input is a forecast object and passes validations
#'
#' @description
#' Methods for the different classes run [assert_forecast_generic()], which performs
#' checks that are the same for all forecast types and then perform specific
#' checks for the specific forecast type.
#' @inheritParams as_forecast
#' @inheritParams score
#' @param verbose Logical. If `FALSE` (default is `TRUE`), no messages and
#'   warnings will be created.
#' @inheritSection forecast_types Forecast types and input formats
#' @return
#' Depending on the forecast type, an object of class
#' `forecast_binary`, `forecast_point`, `forecast_sample` or
#' `forecast_quantile`.
#' @importFrom data.table ':=' is.data.table
#' @importFrom checkmate assert_data_frame
#' @export
#' @keywords check-forecasts
#' @examples
#' forecast <- as_forecast(example_binary)
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
      "!" = "The input needs to be a forecast object.",
      "i" = "Please run `as_forecast()` first." # nolint
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


#' @export
#' @keywords check-forecasts
#' @importFrom checkmate assert_names
assert_forecast.forecast_nominal <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  forecast <- assert_forecast_generic(forecast, verbose)
  assert(check_columns_present(forecast, "predicted_label"))
  assert_names(colnames(forecast), disjunct.from = c("sample_id", "quantile_level"))
  assert_forecast_type(forecast, actual = "nominal", desired = forecast_type)
  return(forecast[])
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
  class(data) <- c(classname, class(data))
  data <- copy(data)
  return(data[])
}


#' @title Test whether an object is a forecast object
#'
#' @description
#' Generic function to test whether an object is of class `forecast_*`. You
#' can also test for a specific `forecast_*` class using the appropriate
#' `is_forecast.forecast_*` method. For example, to check whether an object is
#' of class `forecast_quantile`, you would use
#' `scoringutils:::is_forecast.forecast_quantile()`.
#'
#' @param x An R object.
#' @param ... Additional arguments
#' @return `TRUE` if the object is of class `forecast_*`, `FALSE` otherwise.
#' @export
#' @keywords check-forecasts
#' @examples
#' forecast_binary <- as_forecast(example_binary)
#' is_forecast(forecast_binary)
is_forecast <- function(x, ...) {
  UseMethod("is_forecast")
}

#' @export
#' @rdname is_forecast
#' @keywords check-forecasts
is_forecast.default <- function(x, ...) {
  return(FALSE)
}

#' @export
#' @rdname is_forecast
#' @keywords check-forecasts
is_forecast.forecast_sample <- function(x, ...) {
  inherits(x, "forecast_sample")
}

#' @export
#' @rdname is_forecast
#' @keywords check-forecasts
is_forecast.forecast_binary <- function(x, ...) {
  inherits(x, "forecast_binary")
}

#' @export
#' @rdname is_forecast
#' @keywords check-forecasts
is_forecast.forecast_point <- function(x, ...) {
  inherits(x, "forecast_point")
}

#' @export
#' @rdname is_forecast
#' @keywords check-forecasts
is_forecast.forecast_quantile <- function(x, ...) {
  inherits(x, "forecast_quantile")
}
