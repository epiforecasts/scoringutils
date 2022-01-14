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
#' @param data A data.frame or similar as would be used for [score()]
#'
#' @return A list with elements that give information about what `scoringutils`
#' thinks you are trying to do and potential issues.
#'
#' - `target_type` the type of the prediction target as inferred from the
#' input: 'binary', if all values in `true_value` are either 0 or 1 and values in
#' `prediction` are between 0 and 1, 'discrete' if all true values are integers
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
#'
#' @examples
#' library(scoringutils)
#' check <- check_forecasts(example_quantile)
#' print(check)
#' check_forecasts(example_binary)
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @export
#' @keywords check-forecasts

check_forecasts <- function(data) {
  check <- list()
  msg <- list()
  warnings <- list()
  errors <- list()

  # check data looks ok and remove columns with no prediction or no true value
  data <- withCallingHandlers(
    tryCatch(
      check_clean_data(data),
      error = function(e) {
        errors <<- c(errors, e$message)
      }
    ),
    warning = function(w) {
      warnings <<- c(warnings, w$message)
      tryInvokeRestart("muffleWarning")
    }
  )
  if (length(errors) > 0 | !is.data.table(data)) {
    stop(
      "Can't check input. The following error has been produced:\n",
      paste(errors, collapse = "\n")
    )
  }

  check[["target_type"]] <- get_target_type(data)
  check[["prediction_type"]] <- get_prediction_type(data)

  msg <- c(
    msg,
    paste0(
      "Forecasts are for a `", check[["target_type"]], "` target ",
      "using a `", check[["prediction_type"]], "` prediction format."
    )
  )

  # obtain unit of a single forecast
  forecast_unit <- get_unit_of_forecast(data)
  check[["forecast_unit"]] <- forecast_unit

  msg <- c(
    msg,
    paste0(
      "The unit of a single forecast is defined by `",
      paste(check[["forecast_unit"]], collapse = "`, `"), "`. ",
      "If this is not as intended, please DELETE UNNECESSARY columns or add new ones."
    )
  )

  # check what format is has right now and tell user to convert it.
  if (!any(c("quantile", "sample") %in% colnames(data))) {
    if (!check[["target_type"]] == "binary") {
      errors <- c(
        errors,
        "This forecast does not seem to be for a binary prediction target, so we need a column called quantile or sample"
      )
    }
  }

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
    check[["duplicate_forecasts"]] <- data[InternalDuplicateCheck > 1]
  }
  data[, InternalDuplicateCheck := NULL]

  # check whether there is a model column present. And if not, state what that means
  if (!("model" %in% colnames(data))) {
    msg <- c(
      msg,
      paste(
        "There is no column called `model` in the data.",
        "scoringutils therefore thinks that all forecasts come from the same model"
      )
    )
    data[, model := "Unspecified model"]
  }

  # some checks whether there are the same number of quantiles, samples
  data[, InternalNumCheck := length(prediction), by = forecast_unit]
  n <- unique(data$InternalNumCheck)
  if (length(n) > 1) {
    warnings <- c(
      warnings,
      paste0(
        "Some forecasts have different numbers of rows (e.g. quantiles or samples). ",
        "scoringutils found: ", paste(n, collapse = ", "),
        ". This is not necessarily a problem, but make sure this is intended."
      )
    )
  }
  data[, InternalNumCheck := NULL]

  # get available unique values per model for the different columns
  cols <- forecast_unit[forecast_unit != "model"]
  check[["unique_values"]] <-
    data[, lapply(.SD, FUN = function(x) length(unique(x))), by = "model"]

  check[["messages"]] <- unlist(msg)
  check[["warnings"]] <- unlist(warnings)
  check[["errors"]] <- unlist(errors)

  class(check) <- c("scoringutils_check", "list")

  return(check)
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

print.scoringutils_check <- function(x, ...) {
  print_elements <- names(x)[!(names(x) %in% c("messages"))]
  print.default(x[print_elements])

  cat(paste0(
    "\nBased on your input, scoringutils thinks:\n",
    paste(x$messages, collapse = "\n")
  ))
  cat(
    "\n$unique_values shows how many unique values there are per column per model",
    "(across the entire data)."
  )

  if (length(x$warnings) > 0) {
    cat(paste0(
      "\n\n",
      "You should be aware of the following warnings:\n",
      paste(x$warnings, collapse = "\n")
    ))
  }

  if (length(x$errors) > 0) {
    cat(paste0(
      "\n\n",
      "The following things will likely result in an error: ",
      paste(x$errors, collapse = "\n")
    ))
  }
  return(invisible(x))
}
