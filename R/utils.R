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
    } else if (isTRUE(
      all.equal(data$prediction, as.integer(data$prediction)))
    ) {
      return("integer")
    } else {
      return("continuous")
    }
  } else {
    if (isTRUE(all.equal(data, as.integer(data)))) {
      return("integer")
    } else {
      return("continuous")
    }
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
#' @param prediction_type The prediction type of the forecast. This is used to
#' adjust the list of protected columns.
#'
#' @inheritParams check_forecasts
#'
#' @return A character vector with the column names that define the unit of
#' a single forecast
#'
#' @keywords internal

get_forecast_unit <- function(data, prediction_type) {

  protected_columns <- c(
    "prediction", "true_value", "sample", "quantile", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    names(data)[grepl("coverage_", names(data))]
  )
  if (!missing(prediction_type)) {
    if (prediction_type == "quantile") {
      protected_columns <- setdiff(protected_columns, "sample")
    }
  }
  forecast_unit <- setdiff(colnames(data), protected_columns)
  return(forecast_unit)
}
