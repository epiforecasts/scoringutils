#' @title Validate input data
#'
#' @description
#'
#' The default method, [validate.default()] only runs basic input checks.
#' It's main purpose is to determine the forecast type (binary, point,
#' sample-based or quantile-based) based on the input data. It then constructs
#' an appropriate class and calls [validate()] again which dispatches to the
#' appropriate method.
#'
#' Methods for the different classes run [validate_general()], which performs
#' checks that are the same for all forecast types and then perform specific
#' checks for the specific forecast type.
#'
#' You can find more information about input formats in the vignette.
#' To summarise, the data should come in one of four different formats:
#' - A format for binary predictions (see [example_binary])
#' - A format for point predictions (see [example_point])
#' - A sample-based format for discrete or continuous predictions
#' (see [example_continuous] and [example_integer])
#' - A quantile-based format (see [example_quantile])
#'
#' @inheritParams score
#' @return Depending on the forecast type, an object of class
#' `scoringutils_binary`, `scoringutils_point`, `scoringutils_sample` or
#' `scoringutils_quantile`.
#' @importFrom data.table ':=' is.data.table
#' @importFrom checkmate assert_data_frame
#' @export
#' @keywords validate
validate <- function(data, ...) {
  UseMethod("validate")
}

#' @rdname validate
#' @export
validate.default <- function(data, ...) {
  assert(check_data_columns(data))

  # find forecast type
  forecast_type <- get_forecast_type(data)

  # construct class
  data <- new_scoringutils(data, paste0("scoringutils_", forecast_type))

  # validate class
  validate(data)
}

#' @rdname validate
#' @export
validate.scoringutils_binary <- function(data, ...) {
  data <- validate_general(data)

  columns_correct <- test_columns_not_present(data, c("sample_id", "quantile"))
  if (!columns_correct) {
    stop("Checking `data`: Input looks like a binary forecast, but an",
         "additional column called `sample_id` or `quantile` was found.",
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

#' @rdname validate
#' @export
validate.scoringutils_point <- function(data, ...) {
  data <- validate_general(data)

  input_check <- check_input_point(data$observed, data$predicted)
  if (!is.logical(input_check)) {
    stop("Checking `data`:",
         "Input looks like a point forecast, but found the following issue: ",
         input_check)
  }
  return(data[])
}

#' @rdname validate
#' @export
validate.scoringutils_quantile <- function(data, ...) {
  data <- validate_general(data)
  assert_numeric(data$quantile, lower = 0, upper = 1)
  return(data[])
}

#' @rdname validate
#' @export
validate.scoringutils_sample <- function(data, ...) {
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
#' @inheritParams available_forecasts
#' @return returns the input, with a few new attributes that hold additional
#' information, messages and warnings
#' @importFrom data.table ':=' is.data.table setattr
#' @importFrom checkmate assert_data_table
#' @export
#' @keywords validate
validate_general <- function(data) {
  # check that data is a data.table and that the columns look fine
  assert_data_table(data)
  assert(check_data_columns(data))
  data <- assure_model_column(data)

  # assign forecast type and unit as an attribute and make sure there is no clash
  forecast_type <- get_forecast_type(data)
  assert(check_attribute_conflict(data, "forecast_type", forecast_type))
  setattr(data, "forecast_type", forecast_type)
  forecast_unit <- get_forecast_unit(data)
  assert(check_attribute_conflict(data, "forecast_unit", forecast_unit))
  setattr(data, "forecast_unit", forecast_unit)

  # check that there aren't any duplicated forecasts
  assert(check_duplicates(data, forecast_unit = forecast_unit))

  # check that the number of forecasts per sample / quantile is the same
  number_quantiles_samples <- check_number_per_forecast(data, forecast_unit)
  if (!is.logical(number_quantiles_samples)) {
    setattr(data, "warnings", number_quantiles_samples)
  }

  # check whether there are any NA values in the predicted or observed values
  messages <- c(
    check_no_NA_present(data, "predicted"),
    check_no_NA_present(data, "observed")
  )
  if (!is.logical(messages)) {
    messages <- messages[messages != "TRUE"]
    setattr(data, "messages", messages)
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
#' @inheritParams available_forecasts
#' @param classname name of the class to be created
#' @return An object of the class indicated by `classname`
#' @export
#' @keywords validate
new_scoringutils <- function(data, classname) {
  data <- as.data.table(data)
  data <- assure_model_column(data)
  class(data) <- c(classname, class(data))
  data <- copy(data)
  return(data[])
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
