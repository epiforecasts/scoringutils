
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


