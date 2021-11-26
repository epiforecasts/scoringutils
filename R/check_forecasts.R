#' @title Check forecasts
#'
#' @description Function to check the input data before running
#' [eval_forecasts()].
#'
#' The data should come in one of three different formats:
#' - A format for binary predictions (see [binary_example_data])
#' - A sample-based format for discrete or continuous predictions
#' (see [continuous_example_data] and [integer_example_data])
#' - A quantile-based format (see [quantile_example_data])
#'
#' @seealso Functions to move between different formats:
#' [range_long_to_quantile()], [range_wide_to_long()]
#' @param data A data.frame or similar as would be used for [eval_forecasts()]
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
#' [eval_forecasts()].
#' - `messages` A verbal explanation of the information provided above.
#'
#' @importFrom data.table ':=' is.data.table
#'
#' @examples
#' library(scoringutils)
#' check <- check_forecasts(quantile_example_data)
#' print(check)
#' check_forecasts(binary_example_data)
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @export

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
  obs_unit <- get_unit_of_forecast(data)
  check[["forecast_unit"]] <- obs_unit

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
    if ("range" %in% colnames(data) | any(grepl("lower_", colnames(data)))) {
      errors <- c(
        errors,
        "It seems like you have a format based on forecast intervals (see `example_data_long`, `example_data_semi_wide`, `example_data_wide`). You need to convert this to a quantile-based format first using `range_wide_to_long()` and `range_long_to_quantile()`"
      )
    } else if (!check[["target_type"]] == "binary") {
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
  data[, InternalDuplicateCheck := .N, by = c(obs_unit, type)]

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
  data[, InternalNumCheck := length(prediction), by = obs_unit]
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
  check[["rows_per_forecast"]] <-
    data[, .(rows_per_forecast = unique(InternalNumCheck)), by = model]
  data[, InternalNumCheck := NULL]

  # get available unique values per model for the different columns
  cols <- obs_unit[obs_unit != "model"]
  check[["unique_values"]] <-
    data[, vapply(.SD, FUN = function(x) length(unique(x)), integer(1)), by = "model"]

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

print.scoringutils_check <- function(x, ...) {
  print_elements <- names(x)[!(names(x) %in% c("messages"))]
  print.default(x[print_elements])

  cat(paste0(
    "\nBased on your input, scoringutils thinks:\n",
    paste(x$messages, collapse = "\n")
  ))
  cat("\n$rows_per_forecast shows how many rows (usually quantiles or samples are available per forecast.")
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
      "The following things will likely result in an error:",
      paste(x$errors, collapse = "\n")
    ))
  }
  return(invisible(x))
}



#' @title Get prediction type of a forecast
#'
#' @description Internal helper function to get the prediction type of a
#' forecast. That is inferred based on the properties of the values in the
#' `prediction` column.
#'
#' @inheritParams check_forecasts
#'
#' @return Character vector of length one with either "quantile", "integer", or
#' "continuous".
#'
#' @keywords internal

get_prediction_type <- function(data) {
  if ("quantile" %in% names(data)) {
    return("quantile")
  } else if (all.equal(data$prediction, as.integer(data$prediction)) == TRUE) {
    return("integer")
  } else {
    return("continuous")
  }
}


#' @title Get type of the target true values of a forecast
#'
#' @description Internal helper function to get the type of the target
#' true values of a forecast. That is inferred based on the which columns
#' are present in the data.
#'
#' @inheritParams check_forecasts
#'
#' @return Character vector of length one with either "binary", "integer", or
#' "continous"
#'
#' @keywords internal

get_target_type <- function(data) {
  if (isTRUE(all.equal(data$true_value, as.integer(data$true_value)))) {
    if (all(data$true_value %in% c(0, 1)) &&
        all(data$prediction >= 0) && all(data$prediction <= 1)) {
      return("binary")
    } else {
      return("integer")
    }
  } else {
    return("continuous")
  }
}


#' @title Clean forecast data
#'
#' @description Helper function to check that the input is in fact a data.frame
#' or similar and remove rows with no value for `prediction` or `true_value`
#'
#' @param data A data.frame or similar as it gets passed to [eval_forecasts()].
#' @param verbose Boolean (default is `TRUE`), whether or not to print warnings
#'
#' @return A data.table with NA values in `true_value` or `prediction` removed.
#'
#' @importFrom data.table as.data.table
#'
#' @keywords internal

check_clean_data <- function(data, verbose = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input should be a data.frame or similar")
  }
  data <- as.data.table(data)

  # make sure necessary columns are present
  if (!all(c("true_value", "prediction") %in% colnames(data))) {
    stop("Data needs to have columns called `true_value` and `prediction`")
  }

  if (any(colnames(data) %in% available_metrics())) {
    warning("At least one column in the data corresponds to the name of a ",
            "metric that will be computed by scoringutils. This may be a ",
            "problem. Please check `available_metrics()`")
  }

  # remove rows where prediction or true value are NA
  if (anyNA(data$true_value)) {
    if (verbose) {
      warning("Some values for `true_value` are NA in the data provided")
    }
  }
  data <- data[!is.na(true_value)]

  if (anyNA(data$prediction)) {
    if (verbose) {
      warning("Some values for `prediction` are NA in the data provided")
    }
  }
  data <- data[!is.na(prediction)]
  if (nrow(data) == 0) {
    stop("After removing all NA true values and predictions, there were no observations left")
  }
  return(data)
}


#' @title Get unit of a single forecast
#'
#' @description Helper function to get the unit of a single forecast, i.e.
#' the column names that define where a single forecast was made for
#'
#' @inheritParams check_forecasts
#'
#' @return A character vector with the column names that define the unit of
#' a single forecast
#'
#' @keywords internal

get_unit_of_forecast <- function(data) {
  protected_columns <- c(
    "prediction", "true_value", "sample", "quantile",
    "range", "boundary", available_metrics()
  )
  obs_unit <- setdiff(colnames(data), protected_columns)
  return(obs_unit)
}

