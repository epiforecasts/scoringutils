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


globalVariables(c(".",
                  ".SD",
                  "aem",
                  "boundary",
                  "brier_score",
                  "component_value",
                  "..colnames_x",
                  "..colnames_y",
                  "count",
                  "coverage_deviation",
                  "CRPS",
                  "DSS",
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
                  "metric",
                  "metrics_select",
                  "model",
                  "n_obs",
                  "n_obs wis_component_name",
                  "pit_p_val",
                  "prediction",
                  "quantile",
                  "rn",
                  "true_value",
                  "type",
                  "upper",
                  "value",
                  "value_scaled",
                  "variable",
                  "wis_component_name",
                  "x",
                  "y",
                  "g"))


list_of_avail_metrics <- function() {
  available_metrics <- c("aem", "log_score", "sharpness", "bias", "dss", "crps",
                         "coverage", "coverage_deviation", "quantile_coverage",
                         "pit_p_val", "pit_sd","interval_score",
                         "underprediction", "overprediction")

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



