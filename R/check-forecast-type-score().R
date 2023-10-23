#' @title Infer the type of a forecast based on a data.frame
#'
#' @description Internal helper function to get the type of the forecast.
#' Options are "sample-based", "quantile-based", "binary" or "point" forecast.
#' The function runs additional checks to make sure the data satisfies
#' requirements and throws an informative error if any issues are found.
#'
#' @inheritParams check_forecasts
#'
#' @return Character vector of length one with either "binary", "quantile",
#' "sample" or "point".
#'
#' @keywords internal

get_forecast_type <- function(data) {
  # error if both quantile and sample_id column are present
  both_cols_present <- test_columns_present(data, c("sample_id", "quantile"))
  if (both_cols_present) {
    stop(
      "Checking `data`:",
      "found column `quantile` as well as `sample_id`. Only one of these is allowed."
    )
  }

  if (test_forecast_is_binary(data)) {
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
    return("binary")
  }

  if (test_forecast_is_quantile(data)) {
    input_check <- check_input_quantile(data$observed, data$predicted, data$quantile)
    if (!is.logical(input_check)) {
      stop("Checking `data`:",
           "Input looks like a quantile forecast, but found the following issue: ",
           input_check)
    }
    return("quantile")
  }

  if (test_forecast_is_sample(data)) {
    input_check <- check_input_sample(data$observed, data$predicted)
    if (!is.logical(input_check)) {
      stop("Checking `data`:",
           "Input looks like a sample-based forecast, but found the following issue: ",
           input_check)
    }
    return("sample")
  }
  if (test_forecast_is_point(data)) {
    input_check <- check_input_point(data$observed, data$predicted)
    if (!is.logical(input_check)) {
      stop("Checking `data`:",
           "Input looks like a point forecast, but found the following issue: ",
           input_check)
    }
    return("point")
  }

  # more helfpul things: is there a column sample_id? A column quantile?
  stop("Checking `data`: input doesn't satisfy the criteria for any forecast type.",
       "Are you missing a column `quantile` or `sample_id`?",
       "Please check the vignette for additional info.")
}

#' Test whether data could be a binary forecast.
#' @description Checks type of the necessary columns.
#' @param data A data.frame or similar to be checked
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords check-inputs
test_forecast_is_binary <- function(data) {
  observed_correct <- test_factor(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  return(observed_correct && predicted_correct)
}

#' Test whether data could be a sample-based forecast.
#' @description Checks type of the necessary columns.
#' @param data A data.frame or similar to be checked
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords check-inputs
test_forecast_is_sample <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_present(data, "sample_id")
  return(observed_correct && predicted_correct && columns_correct)
}

#' Test whether data could be a point forecast.
#' @description Checks type of the necessary columns.
#' @param data A data.frame or similar to be checked
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords check-inputs
test_forecast_is_point <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_not_present(data, c("sample_id", "quantile"))
  return(observed_correct && predicted_correct && columns_correct)
}

#' Test whether data could be a quantile forecast.
#' @description Checks type of the necessary columns.
#' @param data A data.frame or similar to be checked
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords check-inputs
test_forecast_is_quantile <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_present(data, "quantile")
  return(observed_correct && predicted_correct && columns_correct)
}



#' Check column names are present in a data.frame
#' @param data A data.frame or similar to be checked
#' @param columns names of columns to be checked
#' @return Returns string with a message with the first issue encountered if
#'  any of the column names are not in data, otherwise returns TRUE
#'
#' @keywords check-inputs
check_columns_present <- function(data, columns) {
  colnames <- colnames(data)
  for (x in columns){
    if (!(x %in% colnames)) {
      msg <- paste0("Data needs to have a column called '", x, "'")
      return(msg)
    }
  }
  return(TRUE)
}

#' Test whether all column names are present in a data.frame
#' @param data A data.frame or similar to be checked
#' @param columns names of columns to be checked
#' @return Returns TRUE if all columns are present and FALSE otherwise
#' @keywords internal
test_columns_present <- function(data, columns) {
  check <- check_columns_present(data, columns)
  return(is.logical(check))
}

#' Test whether column names are NOT present in a data.frame
#' @param data A data.frame or similar to be checked
#' @param columns names of columns to be checked
#' @return Returns TRUE if none of the columns are present and FALSE otherwise
#' @keywords internal
test_columns_not_present <- function(data, columns) {
  if (any(columns %in% colnames(data))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
