#' @title Calculate Geometric Mean
#'
#' @param x numeric vector of values for which to calculate the geometric mean
#' @return the geometric mean of the values in `x`
#'
#' @keywords internal
geom_mean_helper <- function(x) {
  geom_mean <- exp(mean(log(x[!is.na(x)])))
  return(geom_mean)
}


globalVariables(c(
  "..index",
  "..quantiles",
  "..type",
  ".",
  ".SD",
  "adj_pval",
  "ae_point",
  "ae_median",
  "boundary",
  "bias",
  "brier_score",
  "component_value",
  "..colnames_x",
  "..colnames_y",
  "..samplecols",
  "compare_against",
  "count",
  "coverage_deviation",
  "CRPS",
  "crps",
  "DSS",
  "dss",
  "fill_col",
  "identifCol",
  "Interval_Score",
  "overprediction",
  "underprediction",
  "quantile_coverage",
  "LogS",
  "calibration",
  "coverage",
  "hist",
  "InternalDuplicateCheck",
  "InternalNumCheck",
  "log_score",
  "lower",
  "mad",
  "mean_scores_ratio",
  "metric",
  "metrics_select",
  "metrics",
  "model",
  "n_obs",
  "n_obs wis_component_name",
  "Number forecasts",
  "pit_p_val",
  "pit_value",
  "point",
  "prediction",
  "pval",
  "quantile",
  "ratio",
  "rel_to_baseline",
  "relative_skill",
  "rn",
  "se_mean",
  "sharpness",
  "theta",
  "true_value",
  "type",
  "upper",
  "value",
  "value_scaled",
  "var_of_interest",
  "variable",
  "wis_component_name",
  "x",
  "y",
  "g"
))


#' @title Available metrics in scoringutils
#'
#' @return A vector with the name of all available metrics
#' @export
#' @keywords info

available_metrics <- function() {
  return(unique(scoringutils::metrics$Name))
}

#' @title Simple permutation test
#'
#' @description #' The implementation of the permutation test follows the
#' function
#' `permutationTest` from the `surveillance` package by Michael Höhle,
#' Andrea Riebler and Michaela Paul.
#'
#' @return p-value of the permutation test
#' @keywords internal
permutation_test <- function(scores1,
                             scores2,
                             n_permutation = 999,
                             one_sided = FALSE,
                             comparison_mode = c("difference", "ratio")) {
  nTime <- length(scores1)
  meanscores1 <- mean(scores1)
  meanscores2 <- mean(scores2)
  comparison_mode <- match.arg(comparison_mode)
  if (comparison_mode == "ratio") {
    # distinguish between on-sided and two-sided:
    testStat_observed <- ifelse(one_sided,
      meanscores1 / meanscores2,
      max(meanscores1 / meanscores2, meanscores2 / meanscores1)
    )
  } else {
    testStat_observed <- ifelse(one_sided, meanscores1 - meanscores2, abs(meanscores1 - meanscores2))
  }
  testStat_permuted <- replicate(n_permutation, {
    sel <- rbinom(nTime, size = 1, prob = 0.5)
    g1 <- (sum(scores1[sel == 0]) + sum(scores2[sel == 1])) / nTime
    g2 <- (sum(scores1[sel == 1]) + sum(scores2[sel == 0])) / nTime
    if (comparison_mode == "ratio") {
      ifelse(one_sided, g1 / g2, max(g1 / g2, g2 / g1))
    } else {
      ifelse(one_sided, g1 - g2, abs(g1 - g2))
    }
  })
  pVal <- (1 + sum(testStat_permuted >= testStat_observed)) / (n_permutation + 1)
  # plus ones to make sure p-val is never 0?
  return(pVal)
}


#' Delete Columns From a Data.table
#'
#' @description take a vector of column names and delete the columns if they
#' are present in the data.table
#' @param df A data.table or data.frame from which columns shall be deleted
#' @param cols_to_delete character vector with names of columns to be deleted
#' @param make_unique whether to make the data set unique after removing columns
#' @importFrom data.table as.data.table
#' @return A data.table
#'
#' @keywords internal
#'
delete_columns <- function(df, cols_to_delete, make_unique = FALSE) {
  df <- data.table::as.data.table(df)
  delete_columns <- names(df)[names(df) %in% cols_to_delete]
  if (length(delete_columns) > 0) {
    if (make_unique) {
      df <- unique(df[, eval(delete_columns) := NULL])
    } else {
      df <- df[, eval(delete_columns) := NULL]
    }
  }
  return(df)
}

#' @title Check if predictions are quantile forecasts
#'
#' @description Internal helper function to check if a data frame contains
#' quantile forecast predictions. This is determined by checking if the
#' "quantile" column is present.
#'
#' @param data Data frame containing forecast predictions
#'
#' @return Logical indicating whether predictions are quantile forecasts
#'
#' @keywords internal

prediction_is_quantile <- function(data) {
  if (is.data.frame(data)) {
    if ("quantile" %in% names(data)) {
      return(TRUE)
    }
    return(FALSE)
  }
  stop("Input is not a data.frame")
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
  if (is.data.frame(data)) {
    if ("quantile" %in% names(data)) {
      return("quantile")
    } else {
      if ("prediction" %in% names(data)) {
        data <- data$prediction
      }else {
        stop("Input does not contain a column named 'prediction'")
      }
    }
  }

  if (isTRUE(all.equal(as.vector(data), as.integer(data))) &&
        !all(is.na(as.integer(data)))) {
    return("integer")
  } else if (suppressWarnings(!all(is.na(as.numeric(data))))) {
    return("continuous")
  } else {
    stop("Input is not numeric and cannot be coerced to numeric")
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
#' "continuous"
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

get_forecast_unit <- function(data) {

  protected_columns <- get_protected_columns(data)
  if (prediction_is_quantile(data)) {
    protected_columns <- setdiff(protected_columns, "sample")
  }
  forecast_unit <- setdiff(colnames(data), protected_columns)
  return(forecast_unit)
}


#' @title Get protected columns from a data frame
#'
#' @description Helper function to get the names of all columns in a data frame
#' that are protected columns.
#'
#' @inheritParams check_forecasts
#'
#' @return A character vector with the names of protected columns in the data
#'
#' @keywords internal

get_protected_columns <- function(data) {

  datacols <- colnames(data)
  protected_columns <- c(
    "prediction", "true_value", "sample", "quantile", "upper", "lower",
    "pit_value", "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )

  # only return protected columns that are present
  protected_columns <- intersect(
    datacols,
    protected_columns
  )

  return(protected_columns)
}


#' @title Check whether object has been checked with check_forecasts()
#'
#' @description Helper function to determine whether an object has been checked
#' by and passed [check_forecasts()].
#'
#' @param data An object of class `scoringutils_check()` as produced by
#' [check_forecasts()].
#'
#' @importFrom methods is
#'
#' @return Logical, either TRUE or FALSE
#'
#' @keywords internal

is_scoringutils_check <- function(data) {

  result <- is(data, "scoringutils_check")

  if (result &&
        any(is.null(data$cleaned_data), is.null(data$prediction_type),
            is.null(data$forecast_unit), is.null(data$target_type))) {
    stop("Input seems to be an output of `scoringutils_check()`, ",
         "but at least one of the required list items ",
         "'cleaned_data', 'prediction_type', 'forecast_unit', or
         'target_type' is missing. Try running check_forecasts() again.")
  }

  return(result)
}
