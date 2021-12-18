
#' @title Add Quantiles to Predictions When Summarising
#'
#' @description
#' Helper function used within score
#' @param dt the data.table operated on
#' @param varnames names of the variables for which to calculate quantiles
#' @param quantiles the desired quantiles
#' @param by grouping variable in [score()]
#'
#' @return `data.table` with quantiles added
#'
#' @keywords internal
add_quantiles <- function(dt, quantiles, summarise_by) {
  # make sure that the desired varnames are actually present
  varnames <- intersect(available_metrics(), colnames(dt))
  for (varname in varnames) {
    dt[, paste0(varname, "_", quantiles) := as.list(quantile(get(varname),
                                                             probs = quantiles,
                                                             na.rm = TRUE)),
       by = summarise_by]
  }
  return(dt[])
}


#' @title Add Standard Deviation to Predictions When Summarising
#'
#' @description
#' Helper function used within score
#' @param dt the data.table operated on
#' @param varnames names of the variables for which to calculate the sd
#' @param by grouping variable in [score()]
#' @importFrom data.table `%like%`
#' @return `data.table` with sd added
#'
#' @keywords internal
add_sd <- function(dt, summarise_by) {
  # make sure that the desired varnames are actually present
  varnames <- intersect(available_metrics(), colnames(dt))
  for (varname in varnames) {
    dt[, paste0(varname, "_sd") := sd(get(varname), na.rm = TRUE),
       by = summarise_by]
  }
  return(dt[])
}



#' @title Check input parameters for [score()]
#'
#' @description A helper function to check the input parameters for
#' [score()].
#'
#' @inheritParams score
#'
#' @keywords internal
check_score_params <- function(data,
                               forecast_unit,
                               metrics,
                               summarise_by,
                               compute_relative_skill,
                               baseline,
                               rel_skill_metric) {

  # check that the arguments in by and summarise_by are actually present
  if (!all(c(forecast_unit, summarise_by) %in% c(colnames(data), "range", "quantile"))) {
    not_present <- setdiff(unique(c(forecast_unit, summarise_by)),
                           c(colnames(data), "range", "quantile"))
    msg <- paste0("The following items in `summarise_by` are not",
                  "valid column names of the data: '",
                  paste(not_present, collapse = ", "),
                  "'. Check and run `score()` again")
    stop(msg)
  }

  # check metrics to be computed
  available_metrics <- available_metrics()
  if (!all(metrics %in% available_metrics)) {
    msg <- paste("The following metrics are not currently implemented and",
                 "will not be computed:",
                 paste(setdiff(metrics, available_metrics), collapse = ", "))
    warning(msg)
  }

  # error handling for relative skill computation
  if (compute_relative_skill) {
    if (!("model" %in% colnames(data))) {
      warning("to compute relative skills, there must column present called 'model'. Relative skill will not be computed")
      compute_relative_skill <- FALSE
    }
    models <- unique(data$model)
    if (length(models) < 2 + (!is.null(baseline))) {
      warning("you need more than one model non-baseline model to make model comparisons. Relative skill will not be computed")
      compute_relative_skill <- FALSE
    }
    if (!is.null(baseline) && !(baseline %in% models)) {
      warning("The baseline you provided for the relative skill is not one of the models in the data. Relative skill will not be computed")
      compute_relative_skill <- FALSE
    }
    if (rel_skill_metric != "auto" && !(rel_skill_metric %in% available_metrics())) {
      warning("argument 'rel_skill_metric' must either be 'auto' or one of the metrics that can be computed. Relative skill will not be computed")
      compute_relative_skill <- FALSE
    }
  }
  return(compute_relative_skill)
}
