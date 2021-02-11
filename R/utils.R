#' @title Check Variable is not NULL
#'
#' @description
#' Check whether a certain variable is not `NULL` and return the name of that
#' variable and the function call where the variable is missing. This function
#' is a helper function that should only be called within other functions
#' @param ... The variables to check
#' @return The function returns `NULL`, but throws an error if the variable is
#' missing.
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
                  "brier_score",
                  "component_value",
                  "..colnames_x",
                  "..colnames_y",
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
                  "id",
                  "log_score",
                  "lower",
                  "mean_scores_ratio",
                  "metric",
                  "metrics_select",
                  "model",
                  "n_obs",
                  "n_obs wis_component_name",
                  "pit_p_val",
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


list_of_avail_metrics <- function() {
  available_metrics <- c("ae_point", "aem", "log_score", "sharpness", "bias", "dss", "crps",
                         "coverage", "coverage_deviation", "quantile_coverage",
                         "pit_p_val", "pit_sd","interval_score",
                         "underprediction", "overprediction", "relative_skill",
                         "scaled_rel_skill")

  return(available_metrics)
}



#' @title Extract Elements From a List of Lists
#'
#' @description
#' Extract corresponding elements from a list of lists.
#' @param list the list of lists
#' @param what character with the name of the element to extract from every
#' individual list element of `list`
#' @return A list with the extracted element from every sublist
#' missing.
extract_from_list <- function(list, what) {
  out <- lapply(list,
                FUN = function(list_element) {
                  return(list_element[[what]])
                })
  return(out)
}





#' Update a List
#'
#' @description `r lifecycle::badge("stable")`
#' Used to handle updating settings in a list. For example when making
#' changes to `interval_score_arguments` in `eval_forecasts()`
#' @param defaults A list of default settings
#' @param optional A list of optional settings to override defaults
#' @return A list
#' @export
update_list <- function(defaults = list(), optional = list()) {
  if (length(optional) != 0) {
    defaults <- defaults[setdiff(names(defaults), names(optional))]
    updated <- c(defaults, optional)
  } else {
    updated <- defaults
  }
  return(updated)
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
delete_columns <- function(df, cols_to_delete) {
  df <- data.table::as.data.table(df)
  delete_columns <- names(df)[names(df) %in% cols_to_delete]
  if (length(delete_columns) > 0) {
    df <- unique(df[, eval(delete_columns) := NULL])
  }
  return(df)
}

