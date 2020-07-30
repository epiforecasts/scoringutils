#' @title Pivot Quantile Forecasts From Long to Wide Format
#'
#' @description
#' Given a data.frame that follows the structure shown in
#' \code{\link{quantile_example_data_long}}, the function outputs the same
#' data in a long format as (as shown in
#' \code{\link{quantile_example_data_wide}}). This can be useful e.g. for
#' plotting.
#'
#' @param data a data.frame following the specifications from
#' \code{\link{eval_forecasts}}) for quantile forecasts. For an example, see
#' \code{\link{quantile_example_data_long}})
#' @return a data.frame in wide format
#' @importFrom data.table dcast
#' @export
#' @examples
#' long <- scoringutils::quantile_example_data_long
#' wide <- scoringutils::quantile_to_wide(long)
#'

quantile_to_wide <- function(data) {
  data.table::dcast(data, ... ~ boundary + range,
                    value.var = "predictions")
}



#' @title Pivot Quantile Forecasts From Wide to Long Format
#'
#' @description
#' Given a data.frame that follows the structure shown in
#' \code{\link{quantile_example_data_wide}}, the function outputs the same
#' data in a long format as (as shown in
#' \code{\link{quantile_example_data_long}}). This can be useful e.g. for
#' plotting.
#'
#' @param data a data.frame following the specifications from
#' \code{\link{eval_forecasts}}) for quantile forecasts. For an example, see
#' \code{\link{quantile_example_data_wide}})
#' @return a data.frame in long format
#' @importFrom data.table melt
#' @export
#' @examples
#' wide <- scoringutils::quantile_example_data_wide
#' long <- scoringutils::quantile_to_long(wide)
#'

quantile_to_long <- function(data) {
  colnames <- colnames(data)
  ranges <- colnames[grepl("lower", colnames) | grepl("upper", colnames)]

  data <- data.table::melt(data,
                           id.vars = c("id", "true_values", "model"),
                           measure.vars = ranges,
                           variable.name = "range",
                           value.name = "predictions")
  data[, boundary := gsub("_.*", "", range)]
  data[, range := as.numeric(gsub("^.*?_","", range))]

  return(data)
}
