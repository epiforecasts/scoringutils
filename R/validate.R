#' @title Create a `forecast` Object
#' @description Convert a data.frame or similar of forecasts into an object of
#' class `forecast_*` and validate it.
#'
#' `as_forecast()`
#' - allows users to specify the current names of the columns that correspond
#' to the columns required by `scoringutils` (`observed`, `predicted`,
#' `model`, as well `quantile_level` for quantile-based forecasts and
#' `sample_id` for sample-based forecasts). `as_forecast()` renames the
#' existing columns.
#' - allows users to specify the unit of a single forecast. It removes all
#' columns that are neither part of the forecast unit nor a required column
#' (see [set_forecast_unit()] for details)
#' - Determines the forecast type (binary, point, sample-based or
#' quantile-based) from the input data (using the function
#' [get_forecast_type()].
#' - Constructs a forecast object of the appropriate class
#' (`forecast_binary`, `forecast_point`, `forecast_sample`, or
#' `forecast_quantile`, using the function [new_forecast()]).
#' - Calls [validate_forecast()] on the newly created forecast object to
#' validate it
#' @inheritParams score
#' @inheritSection forecast_types Forecast types and input format
#' @return Depending on the forecast type, an object of class
#' `forecast_binary`, `forecast_point`, `forecast_sample` or
#' `forecast_quantile`.
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
#' any renaming of columns done by `as_forecast()`) that denote the unit of a
#' single forecast. See [get_forecast_unit()] for details.
#' If `NULL` (the default), all columns that are not required columns are
#' assumed to form the unit of a single forecast. If specified, all columns
#' that are not part of the forecast unit (or required columns) will be removed.
#' @param forecast_type (optional) The forecast type you expect the forecasts
#' to have. If the forecast type as determined by `scoringutils` based on the
#' input does not match this, an error will be thrown. If `NULL` (the default),
#' the forecast type will be inferred from the data.
#' @param observed (optional) Name of the column in `data` that contains the
#' observed values. This column will be renamed to "observed".
#' @param predicted (optional) Name of the column in `data` that contains the
#' predicted values. This column will be renamed to "predicted".
#' @param model (optional) Name of the column in `data` that contains the names
#' of the models/forecasters that generated the predicted values.
#' This column will be renamed to "model".
#' @param quantile_level (optional) Name of the column in `data` that contains
#' the quantile level of the predicted values. This column will be renamed to
#' "quantile_level". Only applicable to quantile-based forecasts.
#' @param sample_id (optional) Name of the column in `data` that contains the
#' sample id. This column will be renamed to "sample_id". Only applicable to
#' sample-based forecasts.
#' @export
as_forecast.default <- function(data,
                                forecast_unit = NULL,
                                forecast_type = NULL,
                                observed = NULL,
                                predicted = NULL,
                                model = NULL,
                                quantile_level = NULL,
                                sample_id = NULL,
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

  # assert that the correct column names are present after renaming
  assert(check_data_columns(data))

  # set forecast unit (error handling is done in `set_forecast_unit()`)
  if (!is.null(forecast_unit)) {
    data <- set_forecast_unit(data, forecast_unit)
  }

  # find forecast type
  desired <- forecast_type
  forecast_type <- get_forecast_type(data)

  if (!is.null(desired) && desired != forecast_type) {
    stop(
      "Forecast type determined by scoringutils based on input: `",
      forecast_type,
      "`. Desired forecast type: `", desired, "`."
    )
  }

  # construct class
  data <- new_forecast(data, paste0("forecast_", forecast_type))

  # validate class
  validate_forecast(data)
}


#' @title Validate input data
#'
#' @description
#' Methods for the different classes run [validate_general()], which performs
#' checks that are the same for all forecast types and then perform specific
#' checks for the specific forecast type.
#' @inheritParams score
#' @inheritSection forecast_types Forecast types and input format
#' @return Depending on the forecast type, an object of class
#' `forecast_binary`, `forecast_point`, `forecast_sample` or
#' `forecast_quantile`.
#' @importFrom data.table ':=' is.data.table
#' @importFrom checkmate assert_data_frame
#' @export
#' @keywords check-forecasts
#' @examples
#' forecast <- as_forecast(example_binary)
#' validate_forecast(forecast)
validate_forecast <- function(data, ...) {
  UseMethod("validate_forecast")
}


#' @export
#' @rdname validate_forecast
#' @keywords check-forecasts
validate_forecast.forecast_binary <- function(data, ...) {
  data <- validate_general(data)

  columns_correct <- test_columns_not_present(
    data, c("sample_id", "quantile_level")
  )
  if (!columns_correct) {
    stop("Checking `data`: Input looks like a binary forecast, but an",
         "additional column called `sample_id` or `quantile_level` was found.",
         "Please remove the column.")
  }
  input_check <- check_input_binary(data$observed, data$predicted)
  if (!is.logical(input_check)) {
    stop("Checking `data`:",
         "Input looks like a binary forecast, but found the following issue: ",
         input_check)
  }
  return(data[])
}


#' @export
#' @rdname validate_forecast
#' @keywords check-forecasts
validate_forecast.forecast_point <- function(data, ...) {
  data <- validate_general(data)

  input_check <- check_input_point(data$observed, data$predicted)
  if (!is.logical(input_check)) {
    stop("Checking `data`:",
         "Input looks like a point forecast, but found the following issue: ",
         input_check)
  }
  return(data[])
}


#' @export
#' @rdname validate_forecast
#' @keywords check-forecasts
validate_forecast.forecast_quantile <- function(data, ...) {
  data <- validate_general(data)
  assert_numeric(data$quantile_level, lower = 0, upper = 1)
  return(data[])
}


#' @export
#' @rdname validate_forecast
#' @keywords check-forecasts
validate_forecast.forecast_sample <- function(data, ...) {
  data <- validate_general(data)
  return(data[])
}


#' @title Apply scoringutls input checks that are the same across forecast types
#'
#' @description
#' The function runs input checks that apply to all input data, regardless of
#' forecast type. The function
#' - asserts that the data is a data.table which has columns `observed` and
#' `predicted`, as well as a column called `model`.
#' - checks the forecast type and forecast unit
#' - checks there are no duplicate forecasts
#' - if appropriate, checks the number of samples / quantiles is the same
#' for all forecasts
#' @inheritParams get_forecast_counts
#' @return returns the input, with a few new attributes that hold additional
#' information, messages and warnings
#' @importFrom data.table ':=' is.data.table
#' @importFrom checkmate assert_data_table
#' @export
#' @keywords internal_input_check
validate_general <- function(data) {
  # check that data is a data.table and that the columns look fine
  assert_data_table(data)
  assert(check_data_columns(data))
  data <- assure_model_column(data)

  # check that there aren't any duplicated forecasts
  forecast_unit <- get_forecast_unit(data)
  assert(check_duplicates(data, forecast_unit = forecast_unit))

  # check that the number of forecasts per sample / quantile level is the same
  number_quantiles_samples <- check_number_per_forecast(data, forecast_unit)
  if (!is.logical(number_quantiles_samples)) {
    warning(number_quantiles_samples)
  }

  # check whether there are any NA values
  if (anyNA(data)) {
    if (nrow(na.omit(data)) == 0) {
      stop(
        "After removing rows with NA values in the data, no forecasts are left."
      )
    }
    message(
      "Some rows containing NA values may be removed. ",
      "This is fine if not unexpected."
    )
  }

  return(data[])
}


#' @title Class constructor for scoringutils objects
#'
#' @description
#' Construct a class based on a data.frame or similar. The constructor
#' - coerces the data into a data.table
#' - makes sure that a column called `model` exists and if not creates one
#' - assigns a class
#'
#' @inheritParams get_forecast_counts
#' @param classname name of the class to be created
#' @return An object of the class indicated by `classname`
#' @export
#' @keywords internal
new_forecast <- function(data, classname) {
  data <- as.data.table(data)
  data <- assure_model_column(data)
  class(data) <- c(classname, class(data))
  data <- copy(data)
  return(data[])
}


#' @title Test Whether An Object Is Of Class `forecast_*`
#'
#' @description
#' Generic function to test whether an object is of class `forecast_*`. You
#' can also test for a specific `forecast_*` class using the appropriate
#' `is_forecast.forecast_*` method. For example, to check whether an object is
#' of class `forecast_quantile`, you would use
#' `scoringutils:::is_forecast.forecast_quantile()`.
#'
#' @param x An R object.
#' @param ... additional arguments
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


#' @title Validate metrics
#'
#' @description This function validates whether the list of metrics is a list
#' of valid functions.
#'
#' The function is used in [score()] to make sure that all metrics are valid
#' functions
#'
#' @param metrics A named list with metrics. Every element should be a scoring
#' function to be applied to the data.
#'
#' @return A named list of metrics, with those filtered out that are not
#' valid functions
#' @importFrom checkmate assert_list test_list check_function
#' @keywords internal_input_check
validate_metrics <- function(metrics) {

  assert_list(metrics, min.len = 1, names = "named")

  for (i in seq_along(metrics)) {
    check_fun <- check_function(metrics[[i]])
    if (!is.logical(check_fun)) {
      warning("`Metrics` element number ", i, " is not a valid function")
      names(metrics)[i] <- "scoringutils_delete"
    }
  }
  metrics[names(metrics) == "scoringutils_delete"] <- NULL

  assert_list(metrics, min.len = 1, .var.name = "valid metrics")

  return(metrics)
}
