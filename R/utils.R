#' @title Check Variable is not NULL
#'
#' @description
#' Check whether a certain variable is not `NULL` and return the name of that
#' variable and the function call where the variable is missing. This function
#' is a helper function that should only be called within other functions
#' @param ... The variables to check
#' @return The function returns `NULL`, but throws an error if the variable is
#' missing.
#'
#' @keywords internal
check_not_null <- function(...) {
  vars <- list(...)
  varnames <- names(vars)

  for (i in 1:length(vars)) {
    varname = varnames[i]
    if (is.null(vars[[i]])) {
      calling_function <- deparse1(sys.calls()[[sys.nframe()-1]])
      stop(paste0("variable '", varname,
                  "' is `NULL` in the following function call: '",
                  calling_function, "'"))
    }
  }
  return(invisible(NULL))
}





#' @title Check Length
#'
#' @description
#' Check whether variables all have the same length
#' @param ... The variables to check
#' @param one_allowed logical, allow arguments of length one that can be recycled
#'
#' @return The function returns `NULL`, but throws an error if variable lengths
#' differ
#'
#' @keywords internal
check_equal_length <- function(...,
                               one_allowed = TRUE) {
  vars <- list(...)
  lengths <- sapply(vars,
         FUN = function(x) {
           length(x)
         })

  lengths <- unique(lengths)

  if (one_allowed) {
    lengths <- lengths[lengths != 1]
  }

  if (length(unique(lengths)) != 1) {
    calling_function <- deparse1(sys.calls()[[sys.nframe()-1]])
    stop(paste0("Arguments passed to the following function call: '",
                calling_function,
                "' should have the same length (or length one). Arguments have the following lengths: ",
                paste0(lengths, collapse = ", ")))
  }
  return(invisible(NULL))
}

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


globalVariables(c("..index",
                  ".",
                  ".SD",
                  "adj_pval",
                  "ae_point",
                  "aem",
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
                  "DSS",
                  "fill_col",
                  "identif",
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
                  "mean_scores_ratio",
                  "metric",
                  "metrics_select",
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
                  "g"))


#' @title Available metrics in scoringutils
#'
#' @return A vector with the name of all available metrics
#' @export

available_metrics <- function() {
  available_metrics <- c("ae_point", "aem", "log_score", "sharpness",
                         "dispersion", "bias", "dss", "crps", "brier_score",
                         "coverage", "coverage_deviation", "quantile_coverage",
                         "pit_p_val", "pit_sd","interval_score",
                         "underprediction", "overprediction", "relative_skill",
                         "scaled_rel_skill")

  metrics <- list()
  ae_point <- list(
    id = "ae_point",
    name = "Absolute error of the point forecast",
    description = ""
  )
  aem <- list(
    id = "",
    name = "",
    description = ""
  )
  log_score <- list(
    id = "",
    name = "",
    description = ""
  )

  sharpness <- list(
    id = "",
    name = "",
    description = ""
  )
  bias <- list(
    id = "",
    name = "",
    description = ""
  )
  dss <- list(
    id = "",
    name = "",
    description = ""
  )
  crps <- list(
    id = "",
    name = "",
    description = ""
  )

  brier_score <-  list(
    id = "",
    name = "",
    description = ""
  )
  coverage <- list(
    id = "",
    name = "",
    description = ""
  )
  coverage_deviation <- list(
    id = "",
    name = "",
    description = ""
  )
  quantile_coverage <- list(
    id = "",
    name = "",
    description = ""
  )
  pit_values <- list(
    id = "",
    name = "",
    description = ""
  )
  interval_score <- list(
    id = "",
    name = "",
    description = ""
  )
  underprediction <- list(
    id = "",
    name = "",
    description = ""
  )
  overprediction <- list(
    id = "",
    name = "",
    description = ""
  )
  relative_skill <- list(
    id = "",
    name = "",
    description = ""
  )
  scaled_rel_skill <- list(
    id = "",
    name = "",
    description = ""
  )

  return(available_metrics)
}


permutation_test <- function(scores1,
                             scores2,
                             nPermutation = 999,
                             oneSided = FALSE,
                             comparison_mode = c("difference", "ratio")) {
  nTime = length(scores1)
  meanscores1 <- mean(scores1)
  meanscores2 <- mean(scores2)
  if (comparison_mode[1] == "ratio") {
    # distinguish between on-sided and two-sided:
    testStat_observed <- ifelse(oneSided,
                                meanscores1 / meanscores2,
                                max(meanscores1 / meanscores2, meanscores2 / meanscores1))
  } else {
    testStat_observed <- ifelse(oneSided, meanscores1 - meanscores2, abs(meanscores1 - meanscores2))
  }
  testStat_permuted <- replicate(nPermutation, {
    sel <- rbinom(nTime, size = 1, prob = 0.5)
    g1 <- (sum(scores1[sel == 0]) + sum(scores2[sel == 1]))/nTime
    g2 <- (sum(scores1[sel == 1]) + sum(scores2[sel == 0]))/nTime
    if (comparison_mode[1] == "ratio") {
      ifelse(oneSided, g1 / g2, max(g1 / g2, g2/g1))
    } else {
      ifelse(oneSided, g1 - g2, abs(g1 - g2))
    }
  })
  # abs needs to be removed here (messes with one sided vs two-sided)
  pVal <- (1 + sum(testStat_permuted >= testStat_observed))/(nPermutation + 1)
  # plus ones to make sure p-val is never 0?
  return(pVal)
}


#' Delete Columns From a Data.table
#'
#' @description take a vector of column names and delete the columns if they
#' are present in the data.table
#' @param df A data.table or data.frame from which columns shall be deleted
#' @param cols_to_delete character vector with names of columns to be deleted
#' @importFrom data.table as.data.table
#' @return A data.table
#'
#' @keywords internal
delete_columns <- function(df, cols_to_delete) {
  df <- data.table::as.data.table(df)
  delete_columns <- names(df)[names(df) %in% cols_to_delete]
  if (length(delete_columns) > 0) {
    df <- unique(df[, eval(delete_columns) := NULL])
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
#' @param data A data.frame or similar as it gets passed to [score()].
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
    "prediction", "true_value", "sample", "quantile", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics()
  )
  forecast_unit <- setdiff(colnames(data), protected_columns)
  return(forecast_unit)
}

#' @title Check whether the desired metrics are available in scoringutils
#'
#' @description Helper function to check whether desired metrics are
#' available. If the input is `NULL`, all metrics will be returned.
#'
#' @param metrics character vector with desired metrics
#'
#' @return A character vector with metrics that can be used for downstream
#' computation
#'
#' @keywords internal

check_metrics <- function(metrics) {
  # use all available metrics if none are given
  if (is.null(metrics)) {
    metrics <- available_metrics()
  }

  # check desired metrics are actually available in scoringutils
  available_metrics <- available_metrics()
  if (!all(metrics %in% available_metrics)) {
    msg <- paste("The following metrics are not available:",
                 paste(setdiff(metrics, available_metrics), collapse = ", "))
    warning(msg)
  }
  return(metrics)
}

