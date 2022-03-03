#' @title Check forecasts
#'
#' @description Function to check the input data before running
#' [score()].
#'
#' The data should come in one of three different formats:
#' - A format for binary predictions (see [example_binary])
#' - A sample-based format for discrete or continuous predictions
#' (see [example_continuous] and [example_integer])
#' - A quantile-based format (see [example_quantile])
#'
#' @seealso Function to move from sample-based to quantile format:
#' [sample_to_quantile()]
#' @inheritParams avail_forecasts
#' @return A list with elements that give information about what `scoringutils`
#' thinks you are trying to do and potential issues.
#'
#' - `target_type` the type of the prediction target as inferred from the
#' input: 'binary', if all values in `true_value` are either 0 or 1 and values
#'  in `prediction` are between 0 and 1, 'discrete' if all true values are
#'  integers.
#' and 'continuous' if not.
#' - `prediction_type` inferred type of the prediction. 'quantile', if there is
#' a column called 'quantile', else 'discrete' if all values in `prediction`
#' are integer, else 'continuous.
#' - `forecast_unit` unit of a single forecast, i.e. the grouping that uniquely
#' defines a single forecast. This is assumed to be all
#' present columns apart from the following protected columns:
#' `c("prediction", "true_value", "sample", "quantile","range", "boundary")`.
#' It is important that you remove all unnecessary columns before scoring.
#' - `rows_per_forecast` a data.frame that shows how many rows (usually
#' quantiles or samples there are available per forecast. If a forecast model
#' has several entries, then there a forecasts with differing numbers of
#' quantiles / samples.
#' - `unique_values` A data.frame that shows how many unique values there are
#' present per model and column in the data. This doesn't directly show missing
#' values, but rather the maximum number of unique values across the whole data.
#' - `warnings` A vector with warnings. These can be ignored if you know what
#' you are doing.
#' - `errors` A vector with issues that will cause an error when running
#' [score()].
#' - `messages` A verbal explanation of the information provided above.
#'
#' @importFrom data.table ':=' is.data.table
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @export
#' @keywords check-forecasts
#' @examples
#' check <- check_forecasts(example_quantile)
#' print(check)
#' check_forecasts(example_binary)
check_forecasts <- function(data) {

  # create lists to store results ----------------------------------------------
  out <- list()
  warnings <- list()
  errors <- list()
  messages <- list()


  # check data columns ---------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("Input should be a data.frame or similar")
  }
  data <- data.table::as.data.table(data)

  # make sure true_value and prediction are present
  if (!all(c("true_value", "prediction") %in% colnames(data))) {
    stop("Data needs to have columns called `true_value` and `prediction`")
  }

  # check whether any column name is a scoringutils metric
  if (any(colnames(data) %in% available_metrics())) {
    warnings <- c(
      warnings,
      "At least one column in the data corresponds to the name of a metric that will be computed by scoringutils. This may be a problem. Please check `available_metrics()`" # nolint
    )
  }

  # check whether there is a model column present
  if (!("model" %in% colnames(data))) {
    messages <- c(
      messages,
      paste(
        "There is no column called `model` in the data.",
        "scoringutils therefore thinks that all forecasts come from the same model" # nolint
      )
    )
    data[, model := "Unspecified model"]
  }


  # remove rows where prediction or true value are NA --------------------------
  if (anyNA(data$true_value)) {
    messages <- c(
      messages,
      "Some values for `true_value` are NA in the data provided and the corresponding rows were removed. This may indicate a problem if unexpected." # nolint
    )
  }
  if (anyNA(data$prediction)) {
    messages <- c(
      messages,
      "Some values for `prediction` are NA in the data provided and the corresponding rows were removed. This may indicate a problem if unexpected." # nolint
    )
  }
  data <- data[!is.na(true_value) & !is.na(prediction)]

  if (nrow(data) == 0) {
    stop("After removing all NA true values and predictions, there were no observations left")
  }


  # get information about the forecasts ----------------------------------------
  forecast_unit <- get_unit_of_forecast(data)
  target_type <- get_target_type(data)
  prediction_type <- get_prediction_type(data)


  # check whether a column called 'quantile' or 'sample' is present ------------
  if (!any(c("quantile", "sample") %in% colnames(data))) {
    if (!target_type == "binary") {
      errors <- c(
        errors,
        "This forecast does not seem to be for a binary prediction target, so we need a column called quantile or sample" # nolint
      )
    }
  }


  # check duplicate forecasts --------------------------------------------------
  # check whether there is more than one prediction for the same target, i.e.
  # the length of prediction is greater 1 for a sample / quantile for
  # a single forecast
  type <- c("sample", "quantile")[c("sample", "quantile") %in% colnames(data)]
  data[, InternalDuplicateCheck := .N, by = c(forecast_unit, type)]

  if (any(data$InternalDuplicateCheck > 1)) {
    errors <- c(
      errors,
      paste(
        "There are instances with more than one forecast for the same target.",
        "This can't be right and needs to be resolved. Maybe you need to check",
        "the unit of a single forecast and add missing columns?"
      )
    )
    out[["duplicate_forecasts"]] <- data[InternalDuplicateCheck > 1]
  }
  data[, InternalDuplicateCheck := NULL]


  # check whether there are the same number of quantiles, samples --------------
  data[, InternalNumCheck := length(prediction), by = forecast_unit]
  n <- unique(data$InternalNumCheck)
  if (length(n) > 1) {
    warnings <- c(
      warnings,
      paste0(
        "Some forecasts have different numbers of rows (e.g. quantiles or samples). ", # nolint
        "scoringutils found: ", paste(n, collapse = ", "),
        ". This is not necessarily a problem, but make sure this is intended."
      )
    )
  }
  data[, InternalNumCheck := NULL]


  # store info so it can be accessed by the user -------------------------------
  out[["cleaned_data"]] <- data

  # info to be printed
  out[["info"]] <- c(
    paste0(
      "Forecasts are for a `", target_type, "` target ",
      "using a `", prediction_type, "` prediction format."
    ),
    paste0(
      "The unit of a single forecast is defined by `",
      paste(forecast_unit, collapse = "`, `"), "`. ",
      "If this is not as intended, please DELETE UNNECESSARY columns or add new ones." # nolint
    )
  )

  # available unique values per model for the different columns
  cols <- forecast_unit[forecast_unit != "model"]
  out[["unique_values"]] <-
    data[, lapply(.SD, FUN = function(x) length(unique(x))), by = "model"]

  # forecast infos
  out[["forecast_unit"]] <- forecast_unit
  out[["target_type"]] <- target_type
  out[["prediction_type"]] <- prediction_type

  out[["messages"]] <- unlist(messages)
  out[["warnings"]] <- unlist(warnings)
  out[["errors"]] <- unlist(errors)


  # generate messages, warnings, errors ----------------------------------------
  if (length(messages) > 0) {
    msg <- paste("The following messages were produced when checking inputs:",
      messages,
      sep = "\n"
    )
    message(msg)
  }
  if (length(warnings) > 0) {
    msg <- paste("The following warnings were produced when checking inputs:",
      warnings,
      sep = "\n"
    )
    warning(msg)
  }
  if (length(errors) > 0) {
    print("Duplicate forecasts:")
    print(out[["duplicate_forecasts"]])
    msg <- paste("The following errors were produced when checking inputs:",
      errors,
      sep = "\n"
    )
    stop(msg)
  }

  # return check results
  class(out) <- c("scoringutils_check", "list")
  return(out)
}


#' @title Print output from `check_forecasts()`
#'
#' @description Helper function that prints the output generated by
#' [check_forecasts()]
#'
#' @param x An object of class 'scoringutils_check' as produced by
#' [check_forecasts()]
#' @param ... additional arguments (not used here)
#'
#' @return NULL
#' @export
#' @keywords check-forecasts
#' @examples
#' check <- check_forecasts(example_quantile)
#' print(check)
print.scoringutils_check <- function(x, ...) {
  cat(paste0(
    "\nBased on your input, scoringutils thinks:\n",
    paste(x$info, collapse = "\n"),
    "\n\n"
  ))

  print.default(x[c("forecast_unit", "target_type", "prediction_type")])

  cat("Cleaned data, rows with NA values in prediction or true_value removed:\n")
  print.default(x["cleaned_data"])

  cat("Number of unique values per column per model:\n")
  print.default(x["unique_values"])

  colnames <- names(x)[names(x) %in% c("messages", "warnings", "errors")]
  if (length(colnames) > 0) {
    print.default(x[colnames])
  }

  return(invisible(x))
}
