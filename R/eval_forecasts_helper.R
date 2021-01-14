
#' @title Add Quantiles to Predictions When Summarising
#'
#' @description
#' Helper function used within eval_forecasts
#' @param dt the data.table operated on
#' @param varnames names of the variables for which to calculate quantiles
#' @param quantiles the desired quantiles
#' @param by grouping variable in `eval_forecasts()
#'
#' @return `data.table` with quantiles added
add_quantiles <- function(dt, varnames, quantiles, by) {
  # make sure that the desired varnames are actually present
  varnames <- intersect(varnames, colnames(dt))
  for (varname in varnames) {
    dt[, paste0(varname, "_", quantiles) := as.list(quantile(get(varname),
                                                             probs = quantiles,
                                                             na.rm = TRUE)),
       by = c(by)]
  }
  return(dt)
}


#' @title Add Standard Deviation to Predictions When Summarising
#'
#' @description
#' Helper function used within eval_forecasts
#' @param dt the data.table operated on
#' @param varnames names of the variables for which to calculate the sd
#' @param by grouping variable in `eval_forecasts()
#' @importFrom data.table `%like%`
#' @return `data.table` with sd added
add_sd <- function(dt, varnames, by) {
  # make sure that the desired varnames are actually present
  varnames <- intersect(varnames, colnames(dt))
  for (varname in varnames) {
    dt[, paste0(varname, "_sd") := sd(get(varname), na.rm = TRUE), by = by]
  }
  return(dt)
}
